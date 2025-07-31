# R/preprocessing.R
# Data preprocessing and transformation functions

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(lubridate)
  library(zoo)
  library(logger)
  library(here)
})

#' Main function to prepare register data
#' 
#' @param raw_data List. Raw data from DHIS2 API (output from load_dhis2_data)
#' @param metadata_file Character. Path to organization unit metadata file
#' @return data.frame. Processed register data
#' 
prepare_register <- function(raw_data, metadata_file = NULL) {
  
  log_info("Starting data preprocessing...")
  
  # Extract data and metadata
  event_data_df <- raw_data$data
  label_map <- raw_data$metadata
  
  if (nrow(event_data_df) == 0) {
    stop("No data available for processing")
  }
  
  # Apply morbidity code mapping
  event_data_df <- apply_morbidity_mapping(event_data_df, label_map)
  
  # Select and rename columns
  register <- select_and_rename_columns(event_data_df)
  
  # Apply data type conversions
  register <- convert_data_types(register)
  
  # Load and join organizational metadata
  if (!is.null(metadata_file) && file.exists(metadata_file)) {
    register <- join_organization_metadata(register, metadata_file)
  } else {
    log_warn("Metadata file not provided or doesn't exist. Skipping organizational metadata.")
  }
  
  # Process age groups
  register <- process_age_groups(register)
  
  # Process dates
  register <- process_dates(register)
  
  # Standardize categorical variables
  register <- standardize_categorical_variables(register)
  
  # Create disease categories
  register <- create_disease_categories(register)
  
  # Create indicator variables
  register <- create_indicator_variables(register)
  
  log_info("Data preprocessing completed. Final dataset: {nrow(register)} rows, {ncol(register)} columns")
  
  return(register)
}

#' Apply morbidity code mapping using metadata
#' 
#' @param df data.frame. Event data
#' @param label_map Named character vector. Code to label mapping
#' @return data.frame. Data with mapped morbidity labels
#' 
apply_morbidity_mapping <- function(df, label_map) {
  
  disease_col <- "SY - Morbidity classification"
  
  if (!disease_col %in% colnames(df)) {
    log_warn("Morbidity classification column not found: {disease_col}")
    return(df)
  }
  
  if (length(label_map) == 0) {
    log_warn("No metadata mapping available")
    return(df)
  }
  
  # Apply mapping
  df[[disease_col]] <- label_map[df[[disease_col]]]
  
  # Count successful mappings
  mapped_count <- sum(!is.na(df[[disease_col]]))
  log_info("Applied morbidity mapping: {mapped_count}/{nrow(df)} records mapped")
  
  return(df)
}

#' Select and rename columns to standard names
#' 
#' @param df data.frame. Event data
#' @return data.frame. Data with selected and renamed columns
#' 
select_and_rename_columns <- function(df) {
  
  # Define column mapping
  column_mapping <- list(
    "Organisation unit name" = "orgunit",
    "Date of visit" = "datevisit",
    "Gender" = "sex",
    "Age group 1 (0-5,6-17,18-59,60+) - SY" = "age_group",
    "Age group 2 (0-11m, 1-4, 5-14,15-49,50-60,60+) - SY" = "age_group_2",
    "Age group 3 (< 5 , 5-14,15-18,19-49,50+) - SY" = "age_group_3",
    "SY -  Age Group (0-59, 5-17, 18-49, 50 and Above)" = "age_group_4",
    "Resident Status" = "resident",
    "SY - Morbidity classification" = "morbidity",
    "Disease Category - SY" = "disease_category_raw",
    "Type of Case (Trauma/Non-trauma)" = "type_case",
    "Type of Trauma Case - Health - SY" = "trauma_type",
    "Patient Presents With Disability (Y/N) " = "disability",
    "Visit Number - SYR" = "visit_number",
    "Visit Type" = "visit_type"
  )
  
  # Select available columns
  available_columns <- intersect(names(column_mapping), colnames(df))
  missing_columns <- setdiff(names(column_mapping), colnames(df))
  
  if (length(missing_columns) > 0) {
    log_warn("Missing columns: {paste(missing_columns, collapse = ', ')}")
  }
  
  # Select and rename
  df_selected <- df %>%
    select(all_of(available_columns)) %>%
    rename(!!!column_mapping[available_columns])
  
  log_info("Selected {ncol(df_selected)} columns for processing")
  
  return(df_selected)
}

#' Convert data types for proper analysis
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with converted types
#' 
convert_data_types <- function(df) {
  
  # Convert to factors
  factor_columns <- c("orgunit", "sex", "resident", "morbidity", "disease_category_raw",
                     "type_case", "trauma_type", "disability", "visit_number", "visit_type")
  
  for (col in intersect(factor_columns, colnames(df))) {
    df[[col]] <- as.factor(df[[col]])
  }
  
  # Convert dates (handle multiple possible formats)
  if ("datevisit" %in% colnames(df)) {
    df$datevisit <- convert_date_column(df$datevisit)
  }
  
  log_info("Converted data types for analysis")
  
  return(df)
}

#' Convert date column handling multiple formats
#' 
#' @param date_col Character vector. Date column
#' @return Date vector. Converted dates
#' 
convert_date_column <- function(date_col) {
  
  # Try different date formats
  date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d")
  
  for (fmt in date_formats) {
    converted <- as.Date(date_col, format = fmt)
    if (sum(!is.na(converted)) > 0) {
      log_info("Successfully converted dates using format: {fmt}")
      return(converted)
    }
  }
  
  log_warn("Could not convert date column - trying automatic parsing")
  return(as.Date(date_col))
}

#' Join organizational unit metadata
#' 
#' @param df data.frame. Register data
#' @param metadata_file Character. Path to metadata Excel file
#' @return data.frame. Data with organizational metadata joined
#' 
join_organization_metadata <- function(df, metadata_file) {
  
  tryCatch({
    log_info("Loading organizational metadata from: {metadata_file}")
    metadata <- readxl::read_excel(metadata_file)
    
    # Standardize join column name
    if ("orgunit" %in% colnames(metadata)) {
      join_col <- "orgunit"
    } else if ("Organisation unit name" %in% colnames(metadata)) {
      join_col <- "Organisation unit name"
      metadata <- metadata %>% rename(orgunit = `Organisation unit name`)
    } else {
      log_warn("Could not find matching column in metadata file")
      return(df)
    }
    
    # Perform join
    original_rows <- nrow(df)
    df_joined <- df %>%
      left_join(metadata, by = "orgunit")
    
    # Check join success
    matched_rows <- sum(!is.na(df_joined$admin1))
    log_info("Joined organizational metadata: {matched_rows}/{original_rows} rows matched")
    
    return(df_joined)
    
  }, error = function(e) {
    log_error("Failed to load organizational metadata: {e$message}")
    return(df)
  })
}

#' Process and standardize age groups
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with processed age groups
#' 
process_age_groups <- function(df) {
  
  if (!"age_group" %in% colnames(df)) {
    log_warn("Age group column not found")
    return(df)
  }
  
  df <- df %>%
    mutate(
      age_group_new = case_when(
        age_group %in% c("<5", "0-5 y") ~ "0-5 y",
        age_group %in% c("5 - 17", "6-17 y") ~ "6-17 y",
        age_group == "18 - 30" ~ "18-30 y",
        age_group == "31 - 59" ~ "31-59 y",
        age_group == "18-59 y" ~ "18-59 y",
        age_group %in% c("60+", "≥60 y") ~ "60+ y",
        TRUE ~ as.character(age_group)
      )
    ) %>%
    mutate(
      age_group_new = factor(
        age_group_new, 
        levels = c("0-5 y", "6-17 y", "18-30 y", "31-59 y", "18-59 y", "60+ y")
      )
    )
  
  log_info("Processed age groups")
  
  return(df)
}

#' Process dates and create time variables
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with processed dates
#' 
process_dates <- function(df) {
  
  if (!"datevisit" %in% colnames(df)) {
    log_warn("Date visit column not found")
    return(df)
  }
  
  df <- df %>%
    mutate(
      # Create standardized date
      datevisitnew = case_when(
        !is.na(datevisit) ~ datevisit,
        TRUE ~ NA_Date_
      ),
      # Extract time components
      year = year(datevisitnew),
      month = format(datevisitnew, "%Y-%m"),
      quarter = as.yearqtr(datevisitnew),
      # Create month date for plotting
      month2 = as.Date(paste0(month, "-01"))
    )
  
  log_info("Processed dates and created time variables")
  
  return(df)
}

#' Standardize categorical variables
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with standardized categories
#' 
standardize_categorical_variables <- function(df) {
  
  # Standardize facility types
  if ("facilitytype" %in% colnames(df)) {
    df <- df %>%
      mutate(
        facilitytype = case_when(
          facilitytype == "Clinic" ~ "PHC",
          facilitytype == "MMT" ~ "MMU",
          TRUE ~ facilitytype
        )
      )
  }
  
  # Standardize trauma case types
  if ("type_case" %in% colnames(df)) {
    df <- df %>%
      mutate(
        type_case = case_when(
          type_case == "Non Trauma" ~ "Non-trauma",
          TRUE ~ as.character(type_case)
        )
      )
  }
  
  # Standardize administrative divisions
  if ("admin1" %in% colnames(df)) {
    df <- df %>%
      mutate(
        admin1 = case_when(
          admin1 == "Al-Hasakah" ~ "Al-Hasakeh",
          TRUE ~ admin1
        )
      )
  }
  
  if ("admin2" %in% colnames(df)) {
    df <- df %>%
      mutate(
        admin2 = case_when(
          admin2 == "Al Quasir" ~ "Al-Qusayr",
          TRUE ~ admin2
        )
      )
  }
  
  log_info("Standardized categorical variables")
  
  return(df)
}

#' Create disease categories from morbidity classifications
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with disease categories
#' 
create_disease_categories <- function(df) {
  
  if (!"morbidity" %in% colnames(df)) {
    log_warn("Morbidity column not found - skipping disease categorization")
    return(df)
  }
  
  # Define morbidity categories
  morbidity_categories <- get_morbidity_categories()
  
  # Create mapping function
  map_morbidity_to_category <- function(morbidity) {
    if (is.na(morbidity)) return("Unknown")
    
    for (cat in names(morbidity_categories)) {
      if (morbidity %in% morbidity_categories[[cat]]) {
        return(cat)
      }
    }
    return("Uncategorized")
  }
  
  # Apply categorization
  df <- df %>%
    mutate(morbidity_category = sapply(morbidity, map_morbidity_to_category))
  
  # Count categories
  category_counts <- table(df$morbidity_category)
  log_info("Created disease categories: {paste(names(category_counts), category_counts, sep='=', collapse=', ')}")
  
  # Handle disease category from raw data if available
  if ("disease_category_raw" %in% colnames(df)) {
    df <- df %>%
      mutate(
        disease_cat = case_when(
          disease_category_raw == "Blood and Immune Mechanism diseases" ~ "Blood and Immune Mechanism",
          disease_category_raw == "Cardiovascular diseases" ~ "Cardiovascular",
          disease_category_raw == "Dermatology diseases" ~ "Dermatology",
          disease_category_raw == "Digestive diseases" ~ "Digestive",
          disease_category_raw == "Endocrine/Metabolic and Nutritional diseases" ~ "Endocrine/Metabolic and Nutritional",
          disease_category_raw == "Mental health diseases" ~ "Mental health",
          disease_category_raw == "Musculoskeletal diseases" ~ "Musculoskeletal",
          disease_category_raw == "Neurological diseases" ~ "Neurological",
          disease_category_raw == "Ophthalmology diseases" ~ "Ophthalmology",
          disease_category_raw == "Respiratory diseases" ~ "Respiratory",
          disease_category_raw == "Urological and Nephrology diseases" ~ "Urological and Nephrology",
          TRUE ~ as.character(disease_category_raw)
        )
      )
  }
  
  return(df)
}

#' Get morbidity categories definition
#' 
#' @return List. Named list of morbidity categories and their conditions
#' 
get_morbidity_categories <- function() {
  list(
    "Infectious Diseases" = c(
      "Acute bloody diarrhea", "Acute Watery Diarrhea", "COVID-19 suspected case", 
      "Tuberculosis", "Suspected Tuberculosis", "Pneumonia", "Influenza Like Illness", 
      "Helminthiasis", "Scabies/other acariasis", "Typhoid", "Rubella (German measles)", 
      "Malaria (Suspected)", "Suspected Measles", "Suspected Cholera (Acute Watery diarrhea)",
      "Suspected Diphtheria", "Diarrhoea and Gastroenteritis of Presumed Infectious Origin (Not Cholera or Dysentery)", 
      "Leishmaniasis", "Hepatitis B", "Hepatitis C", "Hepatitis E", "Amebiasis", "Rabies", "HIV",
      "Other diseases - infectious diseases", "SARI", "Varicella Chickenpox",
      "Suspected Meningitis", "Measles (Suspected)", "STIs", "Suspected Varicella [chickenpox]", 
      "Acute diarrhea", "Acute hepatitis A", "Typhoid and paratyphoid fevers", 
      "Acute bloody diarrhea (suspected shigellosis)", "Mumps", "Herpes zoster", 
      "Acute junidice syndorme", "Suspected Whooping cough", "Suspected Rubella [German measles]", 
      "Acute Watery Diarrhea (Suspected Cholera)", "Acute Jaundice Syndrome (Hepatitis A)", 
      "Acute Bloody Diarrhea (Suspected Shigellosis/Dysentery)", "TB", "Meningitis (Suspected)", 
      "Acute Flaccid Paralysis (Suspected Polio)", "Whooping Cough (Diphtheria)", 
      "Herpes Simplex Infections", "Herpes Zoster Infections (Shingles)", "Tetanus", 
      "Other viral hepatitis"
    ),
    "Non-Communicable Diseases (NCDs)" = c(
      "Diabetes mellitus", "Hypertension", "Chronic obstructive pulmonary disease (COPD)", 
      "Stroke/cerebrovascular accident", "Asthma", "Heart failure", "Cancer", 
      "Chronic renal Failure", "Chronic Ischaemic Heart Disease", "Lipid disorder", 
      "Arthritis", "Gout", "Hypothyroidism myxoedema", "Hyperthyroidism thyrotoxicosis", 
      "Benign prostatic hyperplasia", "Parkinsonism", "Migraine", "Tension-type headache", 
      "Iron defeciency Anaemia", "Overweight and obesity", "Other NCDs (Including Cancers)", 
      "Hallucinations, Unspecified", "Osteoporosis", "Polyosteoarthritis", "Delirium", 
      "Other diseases - Cardiovascular diseases", "Chronic ischemic heart disease", 
      "Hypotension", "Hypertensive crisis", "Acute myocardial infarction", 
      "Secondary hypertension", "Acute Myocardial Infarction", "Other Cardiovascular Disorders", 
      "Chronic Renal disease (CKD)", "Diabetes", "Heart Failure/Pulmonary Edema", 
      "Cerebrovascular Infarction (Stroke)", "Acute Renal failure"
    ),
    "Trauma and Injuries" = c(
      "Fractures", "Superficial injury", "Burns", "Burn", "Gunshot wounds", "Blast injuries", 
      "Traumatic amputation", "Crush injury", "Conflict-related Injuries", "Contusions", 
      "Firearm injury", "Limb amputation excluding digits", "Traumatic Amputation",
      "Fractures and trauma", "Injury of facial nerve", 
      "Injury of Nerves and Spinal Cord at Neck Level", "Blast Injuries", "Crush Injury", 
      "Other injuries"
    ),
    "Mental Health Disorders" = c(
      "Anxiety Disorders", "Depression", "Schizophrenia", "Bipolar disorder", "PTSD", 
      "Substance Use Disorder", "Dementia", "Psychosis/Schizophrenia", 
      "Somatoform disorders", "Reaction to Severe Stress, and Adjustment Disorders", 
      "Homicidal and suicidal ideations", "Delirium", "Other Mental Health and Behavioural Disorders",
      "Mental Behavioural Disorders Due to Multiple Drug Use and Use of Other Psychoactive Substances", 
      "Other diseases - Mental health diseases", "Depressive disorder", "Anorexia nervosa", 
      "Post-traumatic stress disorder (PTSD)", "Attention deficit hyperactivity disorder (ADHD)", 
      "Bipolar Affective Disorder"
    ),
    "Maternal and Child Health" = c(
      "Normal Vaginal Delivery", "Obstetric Complication", "Caesarean section", 
      "SRH Service", "Breast Lump", "Gynecologic Condition", "Menstrual disorder", 
      "Endometriosis", "Neonatal Tetanus", "Vaginitis vulvitis"
    ),
    "Malnutrition and Micronutrient Deficiency" = c(
      "Complicated severe acute Malnutrition", "Moderate acute Malnutrition", 
      "Severe Malnutrition", "Uncomplicated severe acute Malnutrition", "Underweight", 
      "Malnutrition", "Vitamin deficiency", "Anemia", "Other anemias", 
      "Other diseases - Endocrine Metabolic and Nutritional"
    ),
    "Skin and Subcutaneous Conditions" = c(
      "Dermatitis", "Impetigo", "Psoriasis", "Cellulitis", "Scabies", 
      "Skin Diseases (Scabies, Lice, Fungal Infections)", "Urticaria", "Tinea",
      "Burns (First or Second Degree)", "Other Skin and Subcutaneous Tissue Disorders", 
      "Other diseases - dermatology", 
      "Dermatitis (Infective or Eczematoid Dermatitis, including Diaper Rash)"
    ),
    "Ear, Nose, and Throat (ENT) Conditions" = c(
      "Acute otitis media", "Chronic otitis media", "Otitis externa", 
      "Acute chronic sinusitis", "Allergic rhinitis", "Nasal polyp", 
      "Acute tonsillitis", "Strep throat", "Eustachian salpingitis", 
      "Otitis Media, Unspecified", "Perforation ear drum", "Otosclerosis", 
      "Polyp of middle ear", "Bleeding ear", "Other Ear Disorders"
    ),
    "Eye Conditions" = c(
      "Dry eye syndrome", "Cataract", "Glaucoma", "Blepharitis stye chalazion", 
      "Eye infection inflammation other", "Conjunctivitis", "Refractive error",
      "Pterygium of eye", "Retinal vein or artery occlusion", "Diabetic retinopathy", 
      "Other Eye Disorders", "Cataract, Unspecified"
    ),
    "General Symptoms and Undiagnosed" = c(
      "Fever of unknown origin", "Dehydration", "General weakness", "No disease", 
      "Adverse effect medical agent", "Physiological jaundice", "Ascites",
      "Other disease - General and unspecified"
    ),
    "Surgical Conditions" = c(
      "Appendicitis", "Cholecystitis cholelithiasis", "Umbilical hernia", 
      "Inguinal hernia", "Perforation ear drum", "Pelvic inflammatory disease"
    ),
    "Digestive Disorders" = c(
      "Gastro-esophageal reflux disease", "Peptic ulcer", 
      "Gastro Oesophageal Reflux Disease/ Gastric or Duodenal Ulcer", 
      "Irritable bowel syndrome", "Other Gastrointestinal Disorders", 
      "Other diseases - Digestive diseases"
    ),
    "Blood and Immune Disorders" = c(
      "Iron defeciency Anaemia", "Sickle-cell disorders Thalassemia", 
      "Vitamin B12 folate deficiency anaemia", "Other diseases - Blood and immune mechanism"
    ),
    "Respiratory Diseases" = c(
      "LRTI", "Other Respiratory Disorders", "COPD (Chronic Bronchitis, Emphysema)",
      "Chronic bronchitis", "Acute Upper respiratory infection", "URTI", 
      "Other diseases - Respiratory diseases"
    ),
    "Neurological Disorders" = c(
      "Epilepsy", "Epilepsy and recurrent seizures", "Multiple sclerosis", "Cluster headache", 
      "Headaches/Migraine", "Other headache syndromes", "Vertiginous syndrome",
      "Trigeminal neuralgia", "Other Neurological Disorders", 
      "Other diseases - Neurological diseases"
    ),
    "Other" = c(
      "Death", "Disability", "Alcohol abuse", "Albinism", "Problems related to lifestyle", 
      "Snake Venom/Bite", "Others", "Referral", "Bedwetting enuresis", 
      "Hearing loss", "Growth delay", "Foreign body", "Foreign body in ear",
      "Social and behaviour related problems", "Pediculosis (Lice)"
    )
  )
}

#' Create indicator variables for specific conditions
#' 
#' @param df data.frame. Register data
#' @return data.frame. Data with indicator variables
#' 
create_indicator_variables <- function(df) {
  
  if (!"morbidity" %in% colnames(df)) {
    log_warn("Morbidity column not found - skipping indicator variable creation")
    return(df)
  }
  
  df <- df %>%
    mutate(
      # Cholera cases
      cholera_case = as.numeric(morbidity %in% c(
        "Suspected Cholera (Acute Watery diarrhea)", 
        "Suspected Cholera", 
        "Acute Watery Diarrhea (Suspected Cholera)"
      )),
      
      # Polio/Acute Flaccid Paralysis cases
      polio_case = as.numeric(morbidity %in% c(
        "Acute Flaccid Paralysis (Suspected Polio)",
        "Acute Flaccid Paralysis"
      )),
      
      # Measles cases
      measles_case = as.numeric(morbidity %in% c(
        "Measles (Suspected)", 
        "Suspected Measles"
      )),
      
      # Measles, mumps, or meningitis cases
      mmr_case = as.numeric(morbidity %in% c(
        "Measles (Suspected)", 
        "Suspected Measles",
        "Meningitis (Suspected)",
        "Mumps"
      )),
      
      # Malnutrition cases
      malnutrition_case = as.numeric(morbidity %in% c(
        "Complicated severe acute Malnutrition",
        "Moderate acute Malnutrition",
        "Severe Malnutrition",
        "Uncomplicated severe acute Malnutrition",
        "Underweight"
      ))
    )
  
  log_info("Created indicator variables for key conditions")
  
  return(df)
}

#' Validate processed register data
#' 
#' @param df data.frame. Processed register data
#' @return List. Validation results
#' 
validate_register_data <- function(df) {
  
  validation_results <- list(
    total_rows = nrow(df),
    total_columns = ncol(df),
    missing_dates = sum(is.na(df$datevisitnew)),
    date_range = if("datevisitnew" %in% colnames(df)) range(df$datevisitnew, na.rm = TRUE) else c(NA, NA),
    unique_facilities = if("orgunit" %in% colnames(df)) length(unique(df$orgunit)) else NA,
    morbidity_categories = if("morbidity_category" %in% colnames(df)) length(unique(df$morbidity_category)) else NA
  )
  
  # Check for potential data quality issues
  warnings <- c()
  
  if (validation_results$missing_dates > nrow(df) * 0.1) {
    warnings <- c(warnings, "More than 10% of records have missing dates")
  }
  
  if ("morbidity_category" %in% colnames(df)) {
    uncategorized_pct <- sum(df$morbidity_category == "Uncategorized", na.rm = TRUE) / nrow(df)
    if (uncategorized_pct > 0.1) {
      warnings <- c(warnings, paste("More than 10% of morbidities are uncategorized:", round(uncategorized_pct * 100, 1), "%"))
    }
  }
  
  validation_results$warnings <- warnings
  
  log_info("Data validation completed: {validation_results$total_rows} rows, {length(warnings)} warnings")
  
  return(validation_results)
}