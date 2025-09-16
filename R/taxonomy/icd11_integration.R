# icd11_integration.R - ICD-11 API Integration and Cache System
# Integrates with WHO ICD-11 API for standardized disease classification

library(httr)
library(jsonlite)
library(dplyr)
library(here)
library(digest)
library(yaml)

# ICD-11 API Configuration
ICD11_CONFIG <- list(
  base_url = "https://id.who.int/icd/release/11/2023-01",  # Latest release
  token_url = "https://icdaccessmanagement.who.int/connect/token",
  client_id = Sys.getenv("ICD11_CLIENT_ID"),  # Set via environment variable
  client_secret = Sys.getenv("ICD11_CLIENT_SECRET"),
  cache_dir = here("data", "icd11_cache"),
  cache_expiry_days = 30,
  timeout = 30
)

#' Initialize ICD-11 integration system
#' @param force_token_refresh Force refresh of access token
#' @return TRUE if initialization successful
initialize_icd11_system <- function(force_token_refresh = FALSE) {
  
  cat("🌍 Initializing ICD-11 integration system...\n")
  
  # Create cache directory
  if (!dir.exists(ICD11_CONFIG$cache_dir)) {
    dir.create(ICD11_CONFIG$cache_dir, recursive = TRUE)
    cat("📁 Created ICD-11 cache directory\n")
  }
  
  # Check API credentials
  if (ICD11_CONFIG$client_id == "" || ICD11_CONFIG$client_secret == "") {
    cat("⚠️ ICD-11 API credentials not set. Using cached data only.\n")
    cat("💡 Set ICD11_CLIENT_ID and ICD11_CLIENT_SECRET environment variables\n")
    cat("💡 Register at: https://icd.who.int/icdapi\n")
    return(FALSE)
  }
  
  # Get or refresh access token
  token <- get_icd11_access_token(force_refresh = force_token_refresh)
  
  if (is.null(token)) {
    cat("❌ Failed to obtain ICD-11 access token\n")
    return(FALSE)
  }
  
  cat("✅ ICD-11 system initialized successfully\n")
  return(TRUE)
}

#' Get ICD-11 access token
#' @param force_refresh Force token refresh even if cached token exists
#' @return Access token string or NULL if failed
get_icd11_access_token <- function(force_refresh = FALSE) {
  
  token_file <- file.path(ICD11_CONFIG$cache_dir, "access_token.rds")
  
  # Check for existing valid token
  if (!force_refresh && file.exists(token_file)) {
    token_data <- readRDS(token_file)
    
    # Check if token is still valid (expires in 1 hour, refresh after 50 minutes)
    if (Sys.time() < (token_data$created + 50 * 60)) {
      cat("🔑 Using cached ICD-11 access token\n")
      return(token_data$token)
    }
  }
  
  cat("🔑 Requesting new ICD-11 access token...\n")
  
  # Request new token
  response <- tryCatch({
    httr::POST(
      ICD11_CONFIG$token_url,
      body = list(
        client_id = ICD11_CONFIG$client_id,
        client_secret = ICD11_CONFIG$client_secret,
        scope = "icdapi_access",
        grant_type = "client_credentials"
      ),
      encode = "form",
      httr::timeout(ICD11_CONFIG$timeout)
    )
  }, error = function(e) {
    cat("❌ Token request failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("❌ Failed to obtain access token. Status:", 
        if (!is.null(response)) httr::status_code(response) else "No response", "\n")
    return(NULL)
  }
  
  # Parse token response
  token_info <- httr::content(response, as = "parsed")
  
  if (is.null(token_info$access_token)) {
    cat("❌ No access token in response\n")
    return(NULL)
  }
  
  # Cache the token
  token_data <- list(
    token = token_info$access_token,
    created = Sys.time(),
    expires_in = token_info$expires_in
  )
  
  saveRDS(token_data, token_file)
  cat("✅ ICD-11 access token obtained and cached\n")
  
  return(token_data$token)
}

#' Search ICD-11 for diseases by name
#' @param query Search query (disease name)
#' @param max_results Maximum number of results to return
#' @param use_cache Use cached results if available
#' @return Data frame with ICD-11 search results
search_icd11_diseases <- function(query, max_results = 10, use_cache = TRUE) {
  
  if (nchar(query) < 2) {
    warning("Query too short. Minimum 2 characters required.")
    return(data.frame())
  }
  
  # Generate cache key
  cache_key <- digest::digest(paste(query, max_results), algo = "md5")
  cache_file <- file.path(ICD11_CONFIG$cache_dir, paste0("search_", cache_key, ".rds"))
  
  # Check cache first
  if (use_cache && file.exists(cache_file)) {
    cache_info <- file.info(cache_file)
    if (Sys.time() - cache_info$mtime < ICD11_CONFIG$cache_expiry_days * 24 * 3600) {
      cat("📚 Using cached search results for:", query, "\n")
      return(readRDS(cache_file))
    }
  }
  
  # Get access token
  token <- get_icd11_access_token()
  if (is.null(token)) {
    cat("⚠️ No access token available. Using cached data only.\n")
    if (file.exists(cache_file)) {
      return(readRDS(cache_file))
    }
    return(data.frame())
  }
  
  cat("🔍 Searching ICD-11 for:", query, "\n")
  
  # Build search URL
  search_url <- paste0(
    ICD11_CONFIG$base_url, 
    "/mms/search?q=", 
    URLencode(query),
    "&includeKeywordResult=true",
    "&useFlexisearch=true",
    "&flatResults=false"
  )
  
  # Make API request
  response <- tryCatch({
    httr::GET(
      search_url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        `API-Version` = "v2",
        `Accept-Language` = "en"
      ),
      httr::timeout(ICD11_CONFIG$timeout)
    )
  }, error = function(e) {
    cat("❌ Search request failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("❌ Search failed. Status:", 
        if (!is.null(response)) httr::status_code(response) else "No response", "\n")
    return(data.frame())
  }
  
  # Parse search results
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  search_data <- jsonlite::fromJSON(content, flatten = TRUE)
  
  if (is.null(search_data$destinationEntities) || length(search_data$destinationEntities) == 0) {
    cat("ℹ️ No results found for:", query, "\n")
    return(data.frame())
  }
  
  # Process results
  results <- search_data$destinationEntities
  
  # Handle potential data structure issues
  n_results <- nrow(results)
  if (is.null(n_results)) n_results <- length(results)
  
  # Safely extract fields with proper length handling
  safe_extract <- function(field, default_val = NA) {
    if (is.null(field) || length(field) == 0) {
      return(rep(default_val, n_results))
    } else if (length(field) == 1 && n_results > 1) {
      return(rep(field, n_results))
    } else {
      return(field)
    }
  }
  
  # Extract relevant fields safely
  processed_results <- data.frame(
    query = rep(query, n_results),
    icd11_code = safe_extract(extract_icd_code(results$theCode)),
    icd11_id = safe_extract(results$id),
    title = safe_extract(clean_html_markup(results$title)),
    definition = safe_extract(clean_html_markup(results$definition), NA),
    synonyms = if (!is.null(results$synonym)) {
      sapply(results$synonym, function(x) {
        if(is.null(x) || length(x) == 0) NA 
        else if(is.list(x) && "label" %in% names(x)) clean_html_markup(paste(x$label, collapse = "; "))
        else if(is.character(x)) clean_html_markup(paste(x, collapse = "; "))
        else NA
      })
    } else {
      rep(NA, n_results)
    },
    category = safe_extract(clean_html_markup(extract_category(results$breadcrumb))),
    api_url = safe_extract(results$`@id`),
    search_score = safe_extract(results$score, 1),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(search_score)) %>%
    head(max_results)
  
  # Cache results
  saveRDS(processed_results, cache_file)
  cat("✅ Found", nrow(processed_results), "results for:", query, "\n")
  
  return(processed_results)
}

#' Get detailed information for specific ICD-11 code
#' @param icd_code ICD-11 code (e.g., "1A00", "BD10.0")
#' @param use_cache Use cached results if available
#' @return List with detailed ICD-11 information
get_icd11_details <- function(icd_code, use_cache = TRUE) {
  
  cache_key <- digest::digest(icd_code, algo = "md5")
  cache_file <- file.path(ICD11_CONFIG$cache_dir, paste0("details_", cache_key, ".rds"))
  
  # Check cache
  if (use_cache && file.exists(cache_file)) {
    cache_info <- file.info(cache_file)
    if (Sys.time() - cache_info$mtime < ICD11_CONFIG$cache_expiry_days * 24 * 3600) {
      return(readRDS(cache_file))
    }
  }
  
  # Get access token
  token <- get_icd11_access_token()
  if (is.null(token)) {
    if (file.exists(cache_file)) {
      return(readRDS(cache_file))
    }
    return(NULL)
  }
  
  cat("📋 Getting ICD-11 details for:", icd_code, "\n")
  
  # Build details URL
  details_url <- paste0(ICD11_CONFIG$base_url, "/mms/codeinfo/", icd_code)
  
  # Make API request
  response <- tryCatch({
    httr::GET(
      details_url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        `API-Version` = "v2",
        `Accept-Language` = "en"
      ),
      httr::timeout(ICD11_CONFIG$timeout)
    )
  }, error = function(e) {
    cat("❌ Details request failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("❌ Details request failed. Status:", 
        if (!is.null(response)) httr::status_code(response) else "No response", "\n")
    return(NULL)
  }
  
  # Parse details
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  details_data <- jsonlite::fromJSON(content, flatten = TRUE)
  
  # Cache details
  saveRDS(details_data, cache_file)
  
  return(details_data)
}

#' Extract ICD code from theCode field
extract_icd_code <- function(the_code) {
  if (is.null(the_code) || length(the_code) == 0) return(NA)
  # Extract code from URL-like format
  sapply(the_code, function(x) {
    if (is.character(x) && grepl("/", x)) {
      return(basename(x))
    }
    return(x)
  })
}

#' Extract category from breadcrumb
extract_category <- function(breadcrumb) {
  if (is.null(breadcrumb)) return(NA)
  
  sapply(breadcrumb, function(x) {
    if (is.list(x) && length(x) > 1) {
      # Get the second level category (first is usually too general)
      if (length(x) >= 2) {
        return(x[[2]]$label)
      }
    }
    return(NA)
  })
}

#' Batch search multiple diseases
#' @param disease_list Vector of disease names to search
#' @param max_results_per_disease Maximum results per disease
#' @return Combined data frame of all search results
batch_search_icd11 <- function(disease_list, max_results_per_disease = 3) {
  
  cat("🔄 Batch searching", length(disease_list), "diseases in ICD-11...\n")
  
  all_results <- data.frame()
  
  for (i in seq_along(disease_list)) {
    disease <- disease_list[i]
    cat(sprintf("(%d/%d) Searching: %s\n", i, length(disease_list), disease))
    
    results <- search_icd11_diseases(disease, max_results = max_results_per_disease)
    
    if (nrow(results) > 0) {
      all_results <- rbind(all_results, results)
    }
    
    # Small delay to be respectful to the API
    Sys.sleep(0.5)
  }
  
  cat("✅ Batch search completed:", nrow(all_results), "total results\n")
  return(all_results)
}

#' Explore ICD-11 data elements from API responses
#' @param sample_diseases Vector of diseases to explore (defaults to common diseases)
#' @param include_details Whether to fetch detailed information for each result
#' @return List containing data structure analysis
explore_icd11_data_elements <- function(sample_diseases = NULL, include_details = TRUE) {
  
  cat("🔍 Exploring ICD-11 API data elements...\n")
  cat(rep("=", 50), "\n")
  
  # Default sample diseases if none provided
  if (is.null(sample_diseases)) {
    sample_diseases <- c("malaria", "pneumonia", "cholera", "diabetes", "hypertension")
  }
  
  # Initialize system
  if (!initialize_icd11_system()) {
    cat("❌ ICD-11 system initialization failed\n")
    return(list(error = "System initialization failed"))
  }
  
  exploration_results <- list(
    sample_diseases = sample_diseases,
    search_results = list(),
    detailed_results = list(),
    data_elements = list(),
    summary = list()
  )
  
  # 1. Explore search results structure
  cat("\n1️⃣ Exploring search results data structure...\n")
  
  for (disease in sample_diseases) {
    cat("Searching:", disease, "\n")
    
    search_results <- search_icd11_diseases(disease, max_results = 3, use_cache = FALSE)
    
    if (nrow(search_results) > 0) {
      exploration_results$search_results[[disease]] <- search_results
      cat("✅ Found", nrow(search_results), "results for", disease, "\n")
    } else {
      cat("⚠️ No results for", disease, "\n")
    }
    
    Sys.sleep(0.5) # Be respectful to API
  }
  
  # 2. Explore detailed entity structure  
  if (include_details) {
    cat("\n2️⃣ Exploring detailed entity data structure...\n")
    
    # Get details for first result of each disease
    for (disease in names(exploration_results$search_results)) {
      results <- exploration_results$search_results[[disease]]
      
      if (nrow(results) > 0 && !is.na(results$icd11_code[1])) {
        cat("Getting details for:", results$icd11_code[1], "\n")
        
        details <- get_icd11_details(results$icd11_code[1], use_cache = FALSE)
        
        if (!is.null(details)) {
          exploration_results$detailed_results[[disease]] <- details
          cat("✅ Got detailed data for", results$icd11_code[1], "\n")
        }
        
        Sys.sleep(0.5)
      }
    }
  }
  
  # 3. Analyze data elements
  cat("\n3️⃣ Analyzing available data elements...\n")
  exploration_results$data_elements <- analyze_icd11_data_elements(exploration_results)
  
  # 4. Create summary
  exploration_results$summary <- create_data_elements_summary(exploration_results)
  
  cat("\n✅ Exploration completed!\n")
  cat(rep("=", 50), "\n")
  
  return(exploration_results)
}

#' Analyze ICD-11 data elements from API responses
#' @param exploration_results Results from explore_icd11_data_elements
#' @return List of analyzed data elements
analyze_icd11_data_elements <- function(exploration_results) {
  
  data_elements <- list(
    search_fields = list(),
    detail_fields = list(),
    common_fields = list(),
    classification_fields = list()
  )
  
  # Analyze search result fields
  if (length(exploration_results$search_results) > 0) {
    all_search_results <- bind_rows(exploration_results$search_results, .id = "disease")
    
    data_elements$search_fields <- list(
      field_names = names(all_search_results),
      sample_data = head(all_search_results, 3),
      field_types = sapply(all_search_results, class),
      unique_categories = unique(all_search_results$icd11_category),
      code_patterns = unique(substr(all_search_results$icd11_code, 1, 2))
    )
  }
  
  # Analyze detailed result fields
  if (length(exploration_results$detailed_results) > 0) {
    # Get all unique field names across detailed results
    all_detail_fields <- unique(unlist(lapply(exploration_results$detailed_results, names)))
    
    data_elements$detail_fields <- list(
      available_fields = all_detail_fields,
      field_frequency = table(unlist(lapply(exploration_results$detailed_results, names))),
      sample_structure = exploration_results$detailed_results[[1]]
    )
    
    # Analyze specific classification-relevant fields
    classification_fields <- c("title", "definition", "synonym", "exclusion", "inclusion", 
                             "parent", "child", "browserUrl", "classKind", "breadcrumb",
                             "indexTerm", "postcoordinationScale", "relatedEntitiesInMaternalChapter")
    
    present_classification_fields <- intersect(classification_fields, all_detail_fields)
    
    data_elements$classification_fields <- list(
      recommended_fields = classification_fields,
      available_fields = present_classification_fields,
      missing_fields = setdiff(classification_fields, all_detail_fields)
    )
  }
  
  return(data_elements)
}

#' Create summary of available data elements
#' @param exploration_results Results from exploration
#' @return Summary list
create_data_elements_summary <- function(exploration_results) {
  
  summary <- list(
    total_diseases_searched = length(exploration_results$sample_diseases),
    successful_searches = length(exploration_results$search_results),
    detailed_entities = length(exploration_results$detailed_results)
  )
  
  if (length(exploration_results$search_results) > 0) {
    all_results <- bind_rows(exploration_results$search_results, .id = "disease")
    
    summary$search_summary <- list(
      total_results = nrow(all_results),
      unique_categories = length(unique(all_results$icd11_category)),
      categories = unique(all_results$icd11_category),
      code_structure = list(
        single_letter = sum(nchar(all_results$icd11_code) == 4, na.rm = TRUE),
        with_decimal = sum(grepl("\\.", all_results$icd11_code), na.rm = TRUE),
        chapter_distribution = table(substr(all_results$icd11_code, 1, 1))
      )
    )
  }
  
  return(summary)
}

#' Print exploration results in readable format
#' @param exploration_results Results from explore_icd11_data_elements
print_icd11_data_elements <- function(exploration_results) {
  
  cat("\n📊 ICD-11 DATA ELEMENTS EXPLORATION RESULTS\n")
  cat(rep("=", 60), "\n")
  
  # Summary
  cat("\n📈 SUMMARY\n")
  cat("Diseases searched:", exploration_results$summary$total_diseases_searched, "\n")
  cat("Successful searches:", exploration_results$summary$successful_searches, "\n")
  cat("Detailed entities retrieved:", exploration_results$summary$detailed_entities, "\n")
  
  if (!is.null(exploration_results$summary$search_summary)) {
    cat("Total search results:", exploration_results$summary$search_summary$total_results, "\n")
    cat("Unique categories:", exploration_results$summary$search_summary$unique_categories, "\n")
  }
  
  # Available data elements from search
  if (length(exploration_results$data_elements$search_fields) > 0) {
    cat("\n🔍 SEARCH RESULT DATA ELEMENTS\n")
    cat("Available fields:\n")
    for (field in exploration_results$data_elements$search_fields$field_names) {
      field_type <- exploration_results$data_elements$search_fields$field_types[[field]]
      cat(sprintf("  - %s (%s)\n", field, field_type))
    }
  }
  
  # Available data elements from details
  if (length(exploration_results$data_elements$detail_fields) > 0) {
    cat("\n🔬 DETAILED ENTITY DATA ELEMENTS\n")
    cat("Available fields (", length(exploration_results$data_elements$detail_fields$available_fields), " total):\n")
    
    # Show most common fields first
    field_freq <- exploration_results$data_elements$detail_fields$field_frequency
    sorted_fields <- names(sort(field_freq, decreasing = TRUE))
    
    for (field in head(sorted_fields, 20)) {  # Show top 20 most common
      freq <- field_freq[[field]]
      cat(sprintf("  - %s (appears in %d/%d entities)\n", field, freq, length(exploration_results$detailed_results)))
    }
    
    if (length(sorted_fields) > 20) {
      cat(sprintf("  ... and %d more fields\n", length(sorted_fields) - 20))
    }
  }
  
  # Classification-specific fields
  if (length(exploration_results$data_elements$classification_fields) > 0) {
    cat("\n🏷️ CLASSIFICATION-RELEVANT FIELDS\n")
    cat("Available for classification:\n")
    for (field in exploration_results$data_elements$classification_fields$available_fields) {
      cat(sprintf("  ✅ %s\n", field))
    }
    
    if (length(exploration_results$data_elements$classification_fields$missing_fields) > 0) {
      cat("Not available in sample:\n")
      for (field in exploration_results$data_elements$classification_fields$missing_fields) {
        cat(sprintf("  ❌ %s\n", field))
      }
    }
  }
  
  # Categories found
  if (!is.null(exploration_results$data_elements$search_fields$unique_categories)) {
    cat("\n📂 ICD-11 CATEGORIES FOUND\n")
    categories <- exploration_results$data_elements$search_fields$unique_categories
    for (i in seq_along(categories)) {
      cat(sprintf("%d. %s\n", i, categories[i]))
    }
  }
  
  # Code patterns
  if (!is.null(exploration_results$summary$search_summary$code_structure)) {
    cat("\n🔢 CODE STRUCTURE ANALYSIS\n")
    chapter_dist <- exploration_results$summary$search_summary$code_structure$chapter_distribution
    cat("Chapter distribution (first letter of codes):\n")
    for (chapter in names(chapter_dist)) {
      cat(sprintf("  %s: %d codes\n", chapter, chapter_dist[[chapter]]))
    }
  }
  
  cat(rep("=", 60), "\n")
}

#' Explore ICD-11 classification structure and metadata
#' @param explore_chapters Explore chapter-level structure
#' @param explore_foundation Explore Foundation API endpoints
#' @return List of classification metadata and structure
explore_icd11_classification_structure <- function(explore_chapters = TRUE, explore_foundation = FALSE) {
  
  cat("🏗️ Exploring ICD-11 Classification Structure & Metadata...\n")
  cat(rep("=", 60), "\n")
  
  if (!initialize_icd11_system()) {
    cat("❌ ICD-11 system initialization failed\n")
    return(list(error = "System initialization failed"))
  }
  
  structure_results <- list(
    chapters = list(),
    postcoordination = list(),
    extensions = list(),
    foundation_entities = list(),
    api_endpoints = list(),
    classification_metadata = list()
  )
  
  # 1. Explore ICD-11 Chapter Structure
  if (explore_chapters) {
    cat("\n1️⃣ Exploring ICD-11 Chapter Structure...\n")
    structure_results$chapters <- explore_icd11_chapters()
  }
  
  # 2. Explore Postcoordination Axes/Extensions
  cat("\n2️⃣ Exploring Postcoordination and Extension Axes...\n")
  structure_results$extensions <- explore_postcoordination_axes()
  
  # 3. Explore available API endpoints
  cat("\n3️⃣ Exploring Available API Endpoints...\n")
  structure_results$api_endpoints <- explore_icd11_endpoints()
  
  # 4. Get classification metadata
  cat("\n4️⃣ Getting Classification Metadata...\n")
  structure_results$classification_metadata <- get_icd11_metadata()
  
  # 5. Foundation layer (if requested)
  if (explore_foundation) {
    cat("\n5️⃣ Exploring Foundation Layer...\n")
    structure_results$foundation_entities <- explore_foundation_layer()
  }
  
  cat("\n✅ Classification structure exploration completed!\n")
  return(structure_results)
}

#' Explore ICD-11 chapter structure
explore_icd11_chapters <- function() {
  
  token <- get_icd11_access_token()
  if (is.null(token)) return(list(error = "No token"))
  
  # Get root entities (chapters)
  chapters_url <- paste0(ICD11_CONFIG$base_url, "/mms")
  
  response <- httr::GET(
    chapters_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      `API-Version` = "v2",
      `Accept-Language` = "en"
    )
  )
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    chapters_data <- jsonlite::fromJSON(content, flatten = FALSE)
    
    cat("📚 Found ICD-11 structure\n")
    cat("📋 Raw response structure:\n")
    cat("  Fields available:", paste(names(chapters_data), collapse = ", "), "\n")
    
    # Handle different possible response structures
    chapter_info <- NULL
    
    if (!is.null(chapters_data$child)) {
      # If child is a list/data.frame
      if (is.data.frame(chapters_data$child)) {
        chapter_info <- data.frame(
          title = chapters_data$child$title,
          code = sapply(chapters_data$child$`@id`, function(x) basename(x)),
          url = chapters_data$child$`@id`,
          stringsAsFactors = FALSE
        )
      } else if (is.list(chapters_data$child)) {
        # If child is a list of entities
        titles <- sapply(chapters_data$child, function(x) x$title %||% "Unknown")
        ids <- sapply(chapters_data$child, function(x) x$`@id` %||% "Unknown")
        codes <- sapply(ids, function(x) basename(x))
        
        chapter_info <- data.frame(
          title = titles,
          code = codes,
          url = ids,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Return structure information even if we can't parse chapters
    result <- list(
      raw_structure = names(chapters_data),
      raw_data = chapters_data
    )
    
    if (!is.null(chapter_info)) {
      result$total_chapters <- nrow(chapter_info)
      result$chapters <- chapter_info
      cat("✅ Parsed", nrow(chapter_info), "chapters\n")
    } else {
      cat("⚠️ Could not parse chapter structure, returning raw data\n")
    }
    
    return(result)
  } else {
    cat("❌ Failed to get chapters, status:", httr::status_code(response), "\n")
    return(list(error = paste("HTTP", httr::status_code(response))))
  }
}

#' Explore postcoordination axes and extensions
explore_postcoordination_axes <- function() {
  
  token <- get_icd11_access_token()
  if (is.null(token)) return(list(error = "No token"))
  
  # Try different postcoordination endpoints
  postcoord_endpoints <- c(
    "postcoordinationscale",
    "extensions", 
    "axes"
  )
  
  axes_info <- list()
  
  for (endpoint in postcoord_endpoints) {
    url <- paste0(ICD11_CONFIG$base_url, "/mms/", endpoint)
    
    response <- httr::GET(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        `API-Version` = "v2"
      )
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      axes_data <- jsonlite::fromJSON(content, flatten = TRUE)
      axes_info[[endpoint]] <- axes_data
      cat("✅ Found", endpoint, "data\n")
    } else {
      cat("❌", endpoint, "not available (", httr::status_code(response), ")\n")
    }
  }
  
  return(axes_info)
}

#' Explore available ICD-11 API endpoints
explore_icd11_endpoints <- function() {
  
  base_endpoints <- list(
    search = list(
      url = paste0(ICD11_CONFIG$base_url, "/mms/search"),
      description = "Search for entities by text",
      parameters = c("q", "includeKeywordResult", "useFlexisearch", "flatResults")
    ),
    codeinfo = list(
      url = paste0(ICD11_CONFIG$base_url, "/mms/codeinfo/{code}"),
      description = "Get detailed information about specific code",
      parameters = c("include", "flexiblemode")
    ),
    lookup = list(
      url = paste0(ICD11_CONFIG$base_url, "/mms/lookup"),
      description = "Lookup codes",
      parameters = c("q")
    ),
    autocode = list(
      url = paste0(ICD11_CONFIG$base_url, "/mms/autocode"),
      description = "Automatic coding suggestions",
      parameters = c("text")
    ),
    release = list(
      url = paste0(ICD11_CONFIG$base_url, "/mms/release"),
      description = "Get release information",
      parameters = c()
    )
  )
  
  return(base_endpoints)
}

#' Get ICD-11 classification metadata
get_icd11_metadata <- function() {
  
  token <- get_icd11_access_token()
  if (is.null(token)) return(list(error = "No token"))
  
  # Get release/version information
  release_url <- paste0(ICD11_CONFIG$base_url, "/mms/release")
  
  response <- httr::GET(
    release_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      `API-Version` = "v2"
    )
  )
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    release_data <- jsonlite::fromJSON(content, flatten = TRUE)
    
    return(list(
      release_info = release_data,
      api_version = "v2",
      base_url = ICD11_CONFIG$base_url,
      classification_type = "MMS (Mortality and Morbidity Statistics)"
    ))
  }
  
  return(list(error = "Failed to get metadata"))
}

#' Explore Foundation layer (detailed entity information)
explore_foundation_layer <- function() {
  
  # Foundation API uses different base URL
  foundation_base <- gsub("/mms", "/foundation", ICD11_CONFIG$base_url)
  
  token <- get_icd11_access_token()
  if (is.null(token)) return(list(error = "No token"))
  
  # Try to get foundation structure
  foundation_url <- foundation_base
  
  response <- httr::GET(
    foundation_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      `API-Version` = "v2"
    )
  )
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    foundation_data <- jsonlite::fromJSON(content, flatten = TRUE)
    
    return(list(
      foundation_available = TRUE,
      foundation_url = foundation_base,
      structure = foundation_data
    ))
  } else {
    cat("ℹ️ Foundation layer not accessible (status:", httr::status_code(response), ")\n")
    return(list(foundation_available = FALSE))
  }
}

#' Print classification structure results
print_icd11_classification_structure <- function(structure_results) {
  
  cat("\n🏗️ ICD-11 CLASSIFICATION STRUCTURE ANALYSIS\n")
  cat(rep("=", 70), "\n")
  
  # Chapters
  if (!is.null(structure_results$chapters$chapters)) {
    cat("\n📚 ICD-11 CHAPTERS\n")
    chapters <- structure_results$chapters$chapters
    for (i in 1:nrow(chapters)) {
      cat(sprintf("%s - %s\n", chapters$code[i], chapters$title[i]))
    }
  }
  
  # API Endpoints
  if (!is.null(structure_results$api_endpoints)) {
    cat("\n🔗 AVAILABLE API ENDPOINTS\n")
    endpoints <- structure_results$api_endpoints
    for (name in names(endpoints)) {
      endpoint <- endpoints[[name]]
      cat(sprintf("• %s: %s\n", toupper(name), endpoint$description))
      if (length(endpoint$parameters) > 0) {
        cat(sprintf("  Parameters: %s\n", paste(endpoint$parameters, collapse = ", ")))
      }
    }
  }
  
  # Extensions/Postcoordination
  if (length(structure_results$extensions) > 0) {
    cat("\n🎯 POSTCOORDINATION & EXTENSIONS\n")
    for (ext_name in names(structure_results$extensions)) {
      if (!is.null(structure_results$extensions[[ext_name]])) {
        cat(sprintf("• %s: Available\n", toupper(ext_name)))
      }
    }
  }
  
  # Metadata
  if (!is.null(structure_results$classification_metadata$release_info)) {
    cat("\n📋 CLASSIFICATION METADATA\n")
    meta <- structure_results$classification_metadata
    cat("• Classification Type:", meta$classification_type, "\n")
    cat("• API Version:", meta$api_version, "\n")
    cat("• Base URL:", meta$base_url, "\n")
  }
  
  # Foundation
  if (!is.null(structure_results$foundation_entities)) {
    foundation_status <- if (structure_results$foundation_entities$foundation_available) "Available" else "Not Available"
    cat("\n🏛️ FOUNDATION LAYER\n")
    cat("• Status:", foundation_status, "\n")
  }
  
  cat(rep("=", 70), "\n")
}

#' Get raw API response for detailed analysis
#' @param icd_code ICD-11 code to examine
#' @return Raw API response as list
get_raw_icd11_response <- function(icd_code) {
  
  token <- get_icd11_access_token()
  if (is.null(token)) {
    cat("❌ No access token available\n")
    return(NULL)
  }
  
  cat("🔍 Getting raw API response for:", icd_code, "\n")
  
  # Build details URL
  details_url <- paste0(ICD11_CONFIG$base_url, "/mms/codeinfo/", icd_code)
  
  # Make API request
  response <- tryCatch({
    httr::GET(
      details_url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        `API-Version` = "v2",
        `Accept-Language` = "en"
      ),
      httr::timeout(ICD11_CONFIG$timeout)
    )
  }, error = function(e) {
    cat("❌ Request failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("❌ Request failed. Status:", 
        if (!is.null(response)) httr::status_code(response) else "No response", "\n")
    return(NULL)
  }
  
  # Parse raw response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  raw_data <- jsonlite::fromJSON(content, flatten = FALSE)  # Don't flatten to see structure
  
  return(raw_data)
}

#' Get detailed chapter information from ICD-11 API
#' @param max_chapters Maximum number of chapters to explore
#' @return List with chapter details and available data elements
get_icd11_chapter_details <- function(max_chapters = 5) {
  
  cat("📚 Getting detailed ICD-11 chapter information...\n")
  
  token <- get_icd11_access_token()
  if (is.null(token)) return(list(error = "No token"))
  
  # First get the root structure
  chapters_url <- paste0(ICD11_CONFIG$base_url, "/mms")
  
  response <- httr::GET(
    chapters_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/json",
      `API-Version` = "v2",
      `Accept-Language` = "en"
    )
  )
  
  if (httr::status_code(response) != 200) {
    return(list(error = "Failed to get root structure"))
  }
  
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  root_data <- jsonlite::fromJSON(content, flatten = FALSE)
  
  if (is.null(root_data$child) || length(root_data$child) == 0) {
    return(list(error = "No chapters found"))
  }
  
  cat("📋 Found", length(root_data$child), "chapters, exploring first", min(max_chapters, length(root_data$child)), "\n")
  
  chapter_details <- list()
  data_elements <- list()
  
  # Get details for each chapter
  for (i in seq_len(min(max_chapters, length(root_data$child)))) {
    chapter_url <- root_data$child[i]
    cat("Getting chapter", i, ":", chapter_url, "\n")
    
    chapter_response <- httr::GET(
      chapter_url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json",
        `API-Version` = "v2",
        `Accept-Language` = "en"
      )
    )
    
    if (httr::status_code(chapter_response) == 200) {
      chapter_content <- httr::content(chapter_response, as = "text", encoding = "UTF-8")
      chapter_data <- jsonlite::fromJSON(chapter_content, flatten = FALSE)
      
      chapter_details[[i]] <- chapter_data
      
      # Extract available data elements
      data_elements[[i]] <- list(
        title = chapter_data$title,
        code = basename(chapter_data$`@id`),
        available_fields = names(chapter_data),
        has_children = !is.null(chapter_data$child),
        child_count = if (!is.null(chapter_data$child)) length(chapter_data$child) else 0,
        definition = chapter_data$definition,
        inclusion = chapter_data$inclusion,
        exclusion = chapter_data$exclusion,
        postcoordination = chapter_data$postcoordinationScale
      )
      
      cat("✅ Chapter", i, ":", 
          if (!is.null(chapter_data$title$`@value`)) chapter_data$title$`@value` else "Unknown", "\n")
      
      Sys.sleep(0.5) # Be respectful to API
    } else {
      cat("❌ Failed to get chapter", i, "- Status:", httr::status_code(chapter_response), "\n")
    }
  }
  
  return(list(
    total_chapters = length(root_data$child),
    explored_chapters = length(chapter_details),
    chapter_details = chapter_details,
    data_elements = data_elements,
    root_data = root_data
  ))
}

#' Analyze ICD-11 data elements from detailed exploration
#' @param chapter_details Result from get_icd11_chapter_details
#' @return Analysis of available data elements
analyze_icd11_data_elements_detailed <- function(chapter_details) {
  
  cat("🔍 Analyzing ICD-11 data elements from detailed exploration...\n")
  
  if (length(chapter_details$data_elements) == 0) {
    return(list(error = "No data elements to analyze"))
  }
  
  # Collect all unique field names across chapters
  all_fields <- unique(unlist(lapply(chapter_details$data_elements, function(x) x$available_fields)))
  
  # Field frequency analysis
  field_frequency <- table(unlist(lapply(chapter_details$data_elements, function(x) x$available_fields)))
  
  # Classification-relevant fields found
  classification_fields <- c("title", "definition", "inclusion", "exclusion", "parent", "child", 
                           "breadcrumb", "classKind", "postcoordinationScale", "indexTerm")
  
  found_classification_fields <- intersect(all_fields, classification_fields)
  missing_classification_fields <- setdiff(classification_fields, all_fields)
  
  # Data structure analysis
  structure_analysis <- list(
    total_unique_fields = length(all_fields),
    field_frequency = field_frequency,
    common_fields = names(field_frequency[field_frequency >= length(chapter_details$data_elements) * 0.8]),
    classification_fields_found = found_classification_fields,
    classification_fields_missing = missing_classification_fields,
    chapters_with_children = sum(sapply(chapter_details$data_elements, function(x) x$has_children)),
    chapters_with_definitions = sum(sapply(chapter_details$data_elements, function(x) !is.null(x$definition))),
    chapters_with_postcoordination = sum(sapply(chapter_details$data_elements, function(x) !is.null(x$postcoordination)))
  )
  
  return(structure_analysis)
}

#' Print detailed ICD-11 data elements analysis
#' @param chapter_details Result from get_icd11_chapter_details
#' @param analysis Result from analyze_icd11_data_elements_detailed
print_detailed_icd11_analysis <- function(chapter_details, analysis) {
  
  cat("\n🔬 DETAILED ICD-11 DATA ELEMENTS ANALYSIS\n")
  cat(rep("=", 80), "\n")
  
  # Summary
  cat("\n📊 SUMMARY\n")
  cat("Total chapters in ICD-11:", chapter_details$total_chapters, "\n")
  cat("Chapters explored:", chapter_details$explored_chapters, "\n")
  cat("Unique data fields found:", analysis$total_unique_fields, "\n")
  
  # Chapter details
  cat("\n📚 CHAPTER DETAILS\n")
  for (i in seq_along(chapter_details$data_elements)) {
    element <- chapter_details$data_elements[[i]]
    title <- if (!is.null(element$title$`@value`)) element$title$`@value` else "Unknown Title"
    cat(sprintf("%d. %s (Code: %s)\n", i, title, element$code))
    cat(sprintf("   - Fields: %d | Children: %d | Has Definition: %s\n", 
                length(element$available_fields), 
                element$child_count,
                !is.null(element$definition)))
  }
  
  # Available data elements
  cat("\n🔍 ALL AVAILABLE DATA ELEMENTS\n")
  sorted_fields <- names(sort(analysis$field_frequency, decreasing = TRUE))
  for (field in sorted_fields) {
    freq <- analysis$field_frequency[[field]]
    cat(sprintf("• %s (appears in %d/%d chapters)\n", field, freq, chapter_details$explored_chapters))
  }
  
  # Classification-specific elements
  cat("\n🏷️ CLASSIFICATION-RELEVANT ELEMENTS\n")
  cat("Available:\n")
  for (field in analysis$classification_fields_found) {
    cat(sprintf("  ✅ %s\n", field))
  }
  
  if (length(analysis$classification_fields_missing) > 0) {
    cat("Not found in sample:\n")
    for (field in analysis$classification_fields_missing) {
      cat(sprintf("  ❌ %s\n", field))
    }
  }
  
  # Structure insights
  cat("\n📋 STRUCTURAL INSIGHTS\n")
  cat("Chapters with hierarchical children:", analysis$chapters_with_children, "\n")
  cat("Chapters with definitions:", analysis$chapters_with_definitions, "\n")
  cat("Chapters with postcoordination:", analysis$chapters_with_postcoordination, "\n")
  
  cat(rep("=", 80), "\n")
}

#' Automatically find and add unmapped diseases using ICD-11 API
#' @param unmapped_diseases Vector of unmapped disease names
#' @param mappings_file Path to existing mappings YAML file
#' @param batch_size Number of diseases to process at once (default 20)
#' @param min_confidence Minimum confidence level to accept ("high", "medium", "low")
#' @return Results of the mapping expansion
auto_expand_disease_mappings <- function(unmapped_diseases, 
                                        mappings_file = here("taxonomy", "icd11", "disease_mappings.yml"),
                                        batch_size = 20, 
                                        min_confidence = "medium") {
  
  cat("🤖 Automatically expanding disease mappings using ICD-11 API...\n")
  cat(rep("=", 60), "\n")
  
  if (!initialize_icd11_system()) {
    cat("❌ ICD-11 API not available\n")
    return(list(error = "ICD-11 API unavailable"))
  }
  
  if (length(unmapped_diseases) == 0) {
    cat("ℹ️ No unmapped diseases provided\n")
    return(list(message = "No diseases to map"))
  }
  
  cat("🔍 Processing", length(unmapped_diseases), "unmapped diseases\n")
  cat("📦 Batch size:", batch_size, "| Min confidence:", min_confidence, "\n")
  
  # Process diseases in batches
  all_new_mappings <- data.frame()
  total_batches <- ceiling(length(unmapped_diseases) / batch_size)
  
  for (batch_num in 1:total_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(unmapped_diseases))
    batch_diseases <- unmapped_diseases[start_idx:end_idx]
    
    cat(sprintf("\n📦 Batch %d/%d: Processing %d diseases\n", 
                batch_num, total_batches, length(batch_diseases)))
    
    # Search ICD-11 for this batch
    batch_results <- batch_search_icd11(batch_diseases, max_results_per_disease = 1)
    
    if (nrow(batch_results) > 0) {
      # Filter by minimum confidence
      filtered_results <- filter_by_confidence(batch_results, min_confidence)
      
      if (nrow(filtered_results) > 0) {
        all_new_mappings <- rbind(all_new_mappings, filtered_results)
        cat("✅ Found", nrow(filtered_results), "mappings meeting criteria\n")
      } else {
        cat("⚠️ No mappings met minimum confidence threshold\n")
      }
    } else {
      cat("❌ No mappings found in this batch\n")
    }
    
    # Brief pause between batches
    if (batch_num < total_batches) Sys.sleep(1)
  }
  
  # Add new mappings to file
  if (nrow(all_new_mappings) > 0) {
    cat(sprintf("\n📄 Adding %d new mappings to %s\n", 
                nrow(all_new_mappings), basename(mappings_file)))
    
    update_results <- add_mappings_to_yaml(all_new_mappings, mappings_file)
    
    # Print summary
    print_mapping_summary(all_new_mappings, update_results)
    
    return(list(
      original_count = length(unmapped_diseases),
      new_mappings = all_new_mappings,
      mappings_added = nrow(all_new_mappings),
      success_rate = round((nrow(all_new_mappings) / length(unmapped_diseases)) * 100, 1),
      update_results = update_results
    ))
  } else {
    cat("\n❌ No mappings found that meet the criteria\n")
    return(list(
      original_count = length(unmapped_diseases),
      mappings_added = 0,
      success_rate = 0
    ))
  }
}

#' Filter mappings by confidence level
#' @param mappings Data frame of mappings with confidence column
#' @param min_confidence Minimum confidence level
#' @return Filtered mappings
filter_by_confidence <- function(mappings, min_confidence) {
  
  confidence_levels <- c("low" = 1, "medium" = 2, "high" = 3)
  min_level <- confidence_levels[[min_confidence]]
  
  if (is.na(min_level)) {
    cat("⚠️ Invalid confidence level, accepting all mappings\n")
    return(mappings)
  }
  
  mappings %>%
    filter(confidence %in% names(confidence_levels)[confidence_levels >= min_level])
}

#' Add new mappings to YAML file
#' @param new_mappings Data frame with new mappings
#' @param mappings_file Path to YAML file
#' @return Update results
add_mappings_to_yaml <- function(new_mappings, mappings_file) {
  
  # Load existing file or create new structure
  if (file.exists(mappings_file)) {
    existing_data <- yaml::read_yaml(mappings_file)
  } else {
    existing_data <- list(
      metadata = list(
        created = as.character(Sys.Date()),
        system = "ICD-11 Auto-Expansion",
        icd11_version = "2023-01"
      ),
      statistics = list(),
      mappings = list(
        high_confidence = list(),
        medium_confidence = list(),
        needs_review = list()
      )
    )
  }
  
  # Add new mappings
  added_by_confidence <- list(high = 0, medium = 0, low = 0)
  
  for (i in 1:nrow(new_mappings)) {
    disease_name <- new_mappings$query[i]
    confidence <- new_mappings$confidence[i]
    
    # Determine section
    section <- case_when(
      confidence == "high" ~ "high_confidence",
      confidence == "medium" ~ "medium_confidence",
      TRUE ~ "needs_review"
    )
    
    # Add mapping
    existing_data$mappings[[section]][[disease_name]] <- list(
      icd11_code = new_mappings$icd11_code[i],
      icd11_title = new_mappings$title[i],
      icd11_category = new_mappings$category[i],
      confidence = confidence,
      source = "icd11_api_auto",
      mapped_date = as.character(Sys.Date()),
      search_score = new_mappings$search_score[i]
    )
    
    added_by_confidence[[confidence]] <- added_by_confidence[[confidence]] + 1
  }
  
  # Update statistics
  existing_data$statistics <- list(
    high_confidence = length(existing_data$mappings$high_confidence),
    medium_confidence = length(existing_data$mappings$medium_confidence),
    needs_review = length(existing_data$mappings$needs_review),
    total_mappings = length(existing_data$mappings$high_confidence) + 
                    length(existing_data$mappings$medium_confidence) + 
                    length(existing_data$mappings$needs_review),
    last_auto_expanded = as.character(Sys.Date()),
    api_available = TRUE
  )
  
  existing_data$metadata$last_updated <- as.character(Sys.Date())
  
  # Save file
  yaml::write_yaml(existing_data, mappings_file)
  
  return(list(
    file_updated = TRUE,
    added_by_confidence = added_by_confidence,
    final_statistics = existing_data$statistics
  ))
}

#' Print mapping expansion summary
#' @param new_mappings New mappings data frame
#' @param update_results Results from file update
print_mapping_summary <- function(new_mappings, update_results) {
  
  cat("\n📊 AUTO-EXPANSION RESULTS\n")
  cat(rep("=", 40), "\n")
  
  cat("New mappings added:", nrow(new_mappings), "\n")
  
  if (!is.null(update_results$added_by_confidence)) {
    cat("\nBy confidence level:\n")
    for (conf in names(update_results$added_by_confidence)) {
      count <- update_results$added_by_confidence[[conf]]
      if (count > 0) {
        cat(sprintf("  %s: %d\n", conf, count))
      }
    }
  }
  
  if (!is.null(update_results$final_statistics)) {
    cat("\nTotal mappings now:", update_results$final_statistics$total_mappings, "\n")
  }
  
  # Show top categories
  if (nrow(new_mappings) > 0) {
    cat("\nTop categories added:\n")
    category_counts <- table(new_mappings$category)
    top_categories <- sort(category_counts, decreasing = TRUE)[1:min(5, length(category_counts))]
    
    for (i in seq_along(top_categories)) {
      cat(sprintf("  • %s: %d\n", names(top_categories)[i], top_categories[i]))
    }
  }
  
  cat(rep("=", 40), "\n")
}

#' Extract unmapped diseases from surveillance data and auto-expand
#' @param data_file Path to surveillance data file (.rds or .csv)
#' @param disease_column Name of column containing disease names
#' @param min_cases Minimum case count to consider for mapping
#' @param max_diseases Maximum number of diseases to map (most frequent first)
#' @return Auto-expansion results
auto_expand_from_data <- function(data_file, disease_column = "morbidity", 
                                 min_cases = 10, max_diseases = 100) {
  
  cat("📂 Auto-expanding from surveillance data file\n")
  cat("File:", data_file, "\n")
  cat("Disease column:", disease_column, "\n")
  
  # Load existing mappings to identify unmapped diseases
  mappings_file <- here("taxonomy", "icd11", "disease_mappings.yml")
  existing_mapped <- c()
  
  if (file.exists(mappings_file)) {
    existing_data <- yaml::read_yaml(mappings_file)
    
    # Extract all mapped disease names
    for (section in c("high_confidence", "medium_confidence", "needs_review")) {
      if (!is.null(existing_data$mappings[[section]])) {
        existing_mapped <- c(existing_mapped, names(existing_data$mappings[[section]]))
      }
    }
    
    cat("📚 Found", length(existing_mapped), "existing mappings\n")
  }
  
  # Load and analyze surveillance data
  if (grepl("\\.rds$", data_file, ignore.case = TRUE)) {
    surveillance_data <- readRDS(data_file)
  } else if (grepl("\\.csv$", data_file, ignore.case = TRUE)) {
    surveillance_data <- read.csv(data_file, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format. Use .rds or .csv")
  }
  
  if (!disease_column %in% names(surveillance_data)) {
    stop("Disease column '", disease_column, "' not found in data")
  }
  
  # Find unmapped diseases
  disease_counts <- surveillance_data %>%
    filter(!is.na(.data[[disease_column]]) & .data[[disease_column]] != "") %>%
    count(.data[[disease_column]], name = "cases", sort = TRUE) %>%
    filter(cases >= min_cases) %>%
    rename(disease = !!disease_column)
  
  unmapped_diseases <- disease_counts %>%
    filter(!disease %in% existing_mapped) %>%
    head(max_diseases)
  
  cat("🔍 Found", nrow(unmapped_diseases), "unmapped diseases with >=", min_cases, "cases\n")
  
  if (nrow(unmapped_diseases) == 0) {
    cat("✅ All diseases are already mapped!\n")
    return(list(message = "No unmapped diseases found"))
  }
  
  # Show top unmapped diseases
  cat("\nTop unmapped diseases:\n")
  print(head(unmapped_diseases, 10))
  
  # Auto-expand mappings
  results <- auto_expand_disease_mappings(
    unmapped_diseases = unmapped_diseases$disease,
    mappings_file = mappings_file,
    batch_size = 20,
    min_confidence = "medium"
  )
  
  return(results)
}

#' Clean HTML markup from text strings
#' @param text Character vector or single string that may contain HTML tags
#' @return Cleaned text with HTML tags removed
clean_html_markup <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(text)
  }
  
  # Remove HTML tags like <em class='found'>, </em>, etc.
  cleaned <- gsub("<[^>]*>", "", text)
  
  # Remove extra whitespace that might be left
  cleaned <- gsub("\\s+", " ", cleaned)
  
  # Trim leading/trailing whitespace
  cleaned <- trimws(cleaned)
  
  return(cleaned)
}

#' Clean up existing YAML mappings that contain HTML markup
#' @param yaml_file Path to the YAML mappings file
#' @return Updated mappings with cleaned titles
cleanup_existing_mappings <- function(yaml_file = here("taxonomy", "icd11", "disease_mappings.yml")) {
  
  cat("🧹 Cleaning HTML markup from existing mappings...\n")
  
  if (!file.exists(yaml_file)) {
    stop("YAML file not found: ", yaml_file)
  }
  
  mappings <- yaml::read_yaml(yaml_file)
  
  if (is.null(mappings$mappings)) {
    cat("⚠️ No mappings found in file\n")
    return(mappings)
  }
  
  cleaned_count <- 0
  
  # Handle the nested structure with confidence levels
  confidence_levels <- names(mappings$mappings)
  
  for (confidence_level in confidence_levels) {
    if (is.null(mappings$mappings[[confidence_level]])) next
    
    for (disease_name in names(mappings$mappings[[confidence_level]])) {
      disease_entry <- mappings$mappings[[confidence_level]][[disease_name]]
    
      if (!is.null(disease_entry$icd11_title) && grepl("<[^>]*>", disease_entry$icd11_title)) {
        old_title <- disease_entry$icd11_title
        clean_title <- clean_html_markup(old_title)
        mappings$mappings[[confidence_level]][[disease_name]]$icd11_title <- clean_title
        cleaned_count <- cleaned_count + 1
        cat("  Cleaned:", disease_name, "\n")
        cat("    Before:", old_title, "\n") 
        cat("    After: ", clean_title, "\n\n")
      }
      
      if (!is.null(disease_entry$icd11_definition) && grepl("<[^>]*>", disease_entry$icd11_definition)) {
        mappings$mappings[[confidence_level]][[disease_name]]$icd11_definition <- clean_html_markup(disease_entry$icd11_definition)
        cleaned_count <- cleaned_count + 1
      }
      
      if (!is.null(disease_entry$icd11_category) && grepl("<[^>]*>", disease_entry$icd11_category)) {
        mappings$mappings[[confidence_level]][[disease_name]]$icd11_category <- clean_html_markup(disease_entry$icd11_category)
        cleaned_count <- cleaned_count + 1
      }
    }
  }
  
  if (cleaned_count > 0) {
    yaml::write_yaml(mappings, yaml_file)
    cat("✅ Cleaned", cleaned_count, "HTML markup entries and saved to", yaml_file, "\n")
  } else {
    cat("✅ No HTML markup found in existing mappings\n")
  }
  
  return(mappings)
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

cat("✅ ICD-11 integration functions loaded\n")
cat("💡 Run auto_expand_from_data('your_data.rds') to automatically expand mappings\n")
cat("🧹 Run cleanup_existing_mappings() to clean HTML markup from existing mappings\n")
