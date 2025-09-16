# ai_prompts.R - AI prompt templates and building functions
# Consolidated from R/enhanced_ai.R
# VERSION: 1.0.0

library(stringr)

# =============================================================================
# PROMPT TEMPLATE SYSTEM
# =============================================================================

#' Get enhanced prompt templates with user customization support
#' @param ai_config AI configuration with user customization
#' @return List of prompt templates
get_enhanced_prompt_templates <- function(ai_config = NULL) {
  # Get customizable templates from user configuration
  if (!is.null(ai_config) && !is.null(ai_config$user_customization) && 
      !is.null(ai_config$user_customization$prompts)) {
    custom_templates <- ai_config$user_customization$prompts
  } else {
    custom_templates <- list()
  }
  
  # Default templates with deployment-specific adaptations
  default_templates <- list(
    monthly_summary = "Analyze this monthly epidemiological surveillance summary for {setting} serving {population}:

DATA SUMMARY:
- Total consultations: {data_summary$case_count}
- Active health facilities: {data_summary$facilities}
- Geographic coverage: {data_summary$regions} regions/governorates
- Disease categories: {data_summary$diseases}
- Recent activity: {data_summary$recent_trends}

OPERATIONAL CONTEXT:
- Setting: {setting_description}
- Surveillance priorities: {surveillance_priorities}
- Key constraints: {operational_constraints}
- Endemic diseases of concern: {endemic_diseases}

Provide a concise epidemiological assessment focusing on:
1. Key health trends and patterns
2. Disease burden distribution
3. Operational implications
4. Priority areas requiring attention

Keep response under 400 words, professional medical tone.",

    geographic_pattern = "Analyze geographic disease distribution patterns for {setting} surveillance:

CONTEXT: {setting_description} serving {population}
GEOGRAPHIC DATA: {geo_data}

Provide insights on:
1. Geographic clustering of diseases
2. Regional health disparities  
3. Access and coverage gaps
4. Priority locations for intervention

Focus on actionable geographic intelligence for health programming. Maximum 300 words.",

    demographic_insight = "Analyze demographic health patterns:

SETTING: {setting}
POPULATION: {population}
HIGH-PRIORITY GROUPS: {high_priority}
OPERATIONAL CONTEXT: {context_specific_vulnerabilities}

DEMOGRAPHIC DATA: {demo_data}

Provide insights on:
1. Age/gender-specific health patterns
2. Vulnerable population health status
3. Demographic risk factors
4. Targeted intervention recommendations

Maximum 300 words, focus on vulnerable populations.",

    disease_trend = "Analyze epidemiological trends for {disease_name}:

SURVEILLANCE CONTEXT:
- Setting: {setting}
- Population: {population}
- Alert threshold: {alert_threshold}x baseline
- Context: {context_name}
- High priority concerns: {high_priority}
- Endemic disease background: {endemic_diseases}

TREND ANALYSIS:
{trend_data}
- Recent 30-day average: {recent_average}
- Baseline comparison: {baseline_average} 
- Trend ratio: {trend_ratio}

Provide:
1. Epidemiological interpretation
2. Risk assessment
3. Surveillance recommendations
4. Intervention priorities

Maximum 250 words, clinical epidemiological tone.",

    facility_performance = "Analyze health facility performance:

OPERATIONAL CONTEXT: {setting}
FACILITY METRICS:
- Total facilities: {total_facilities}
- High-load facilities: {high_load_facilities}
- Average consultations per facility: {avg_consultations_per_facility}
- Maximum facility load: {max_facility_load}
- Load concentration (top 5 facilities): {load_concentration_ratio}%
- Peak facility: {peak_facility}

DETAILED DATA: {facility_data}

Assess:
1. Service delivery patterns
2. Capacity utilization
3. Equity of access
4. Resource optimization opportunities

Maximum 300 words, operational focus.",

    outbreak_alert = "OUTBREAK SURVEILLANCE ALERT ANALYSIS

ALERT PARAMETERS:
- Outbreak threshold: {alert_threshold}x baseline
- Geographic spread threshold: {geo_threshold} regions
- Surveillance context: {setting}
- Target population: {population}

ALERT DATA:
{alert_data}

Provide immediate epidemiological assessment:
1. Outbreak risk level (LOW/MODERATE/HIGH)
2. Geographic spread assessment
3. Immediate response priorities
4. Surveillance intensification needs

URGENT - Maximum 200 words, actionable recommendations."
  )
  
  # Merge custom templates over defaults
  templates <- modifyList(default_templates, custom_templates)
  return(templates)
}

#' Fill template with values using placeholder replacement
#' @param template Template string with {placeholders}
#' @param values Named list of values to substitute
#' @return Filled template string
.fill_template <- function(template, values) {
  if (is.null(template) || !is.character(template)) {
    return("")
  }
  
  filled <- template
  
  # Handle nested values (e.g., data_summary$case_count)
  for (name in names(values)) {
    value <- values[[name]]
    
    # Convert value to string safely
    if (is.null(value)) {
      value_str <- "Not available"
    } else if (is.list(value)) {
      # For nested lists, create simple key-value pairs
      nested_strs <- character()
      for (nested_name in names(value)) {
        nested_val <- value[[nested_name]]
        if (!is.null(nested_val)) {
          nested_strs <- c(nested_strs, paste0(nested_name, ": ", as.character(nested_val)))
        }
      }
      if (length(nested_strs) > 0) {
        value_str <- paste(nested_strs, collapse = ", ")
      } else {
        value_str <- "No data available"
      }
    } else {
      value_str <- as.character(value)
    }
    
    # Replace both simple {name} and nested {name$subkey} patterns
    pattern1 <- paste0("\\{", name, "\\}")
    filled <- stringr::str_replace_all(filled, pattern1, value_str)
    
    # Handle nested patterns like {data_summary$case_count}
    if (is.list(value)) {
      for (subkey in names(value)) {
        subval <- value[[subkey]]
        if (!is.null(subval)) {
          pattern2 <- paste0("\\{", name, "\\$", subkey, "\\}")
          filled <- stringr::str_replace_all(filled, pattern2, as.character(subval))
        }
      }
    }
  }
  
  # Clean up any remaining placeholders
  filled <- stringr::str_replace_all(filled, "\\{[^}]+\\}", "Not available")
  
  return(filled)
}

#' Build enhanced prompt from template and values
#' @param type Template type (monthly_summary, geographic_pattern, etc.)
#' @param values Named list of values to fill template
#' @param ai_config AI configuration for custom templates
#' @return Filled prompt string
build_enhanced_prompt <- function(type, values = list(), ai_config = NULL) {
  templates <- get_enhanced_prompt_templates(ai_config)
  if (!type %in% names(templates)) {
    stop("Unknown prompt type: ", type, ". Available types: ", paste(names(templates), collapse = ", "))
  }
  .fill_template(templates[[type]], values)
}

cat("✅ AI Prompts module loaded successfully\n")