# modules/shiny_modules.R
# Reusable helpers + optional modules (no sample data, no side effects)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(openxlsx)
  library(lubridate)
  library(stringr)
  library(scales)
  library(plotly)
  library(DT)
})

# -----------------------------------------------------------------------------
# Helpers to standardize filter behavior with your existing sidebar inputs
# -----------------------------------------------------------------------------

# Update sidebar filter choices from a data.frame (safe if columns are missing)
update_sidebar_filters <- function(session, df) {
  req(session, is.data.frame(df))
  
  # Date range
  if ("datevisit" %in% names(df)) {
    dates <- range(df$datevisit, na.rm = TRUE)
    if (is.finite(dates[1]) && is.finite(dates[2])) {
      updateDateRangeInput(
        session, "date_range",
        start = max(dates[2] - 90, dates[1]),
        end   = dates[2],
        min   = dates[1],
        max   = dates[2]
      )
    }
  }
  
  # Facilities
  if ("orgunit" %in% names(df)) {
    facilities <- sort(unique(df$orgunit[!is.na(df$orgunit)]))
    updateSelectInput(session, "selected_facilities",
                      choices = facilities, selected = NULL)
  }
  
  # Diseases (category)
  if ("category_canonical_disease_imc" %in% names(df)) {
    diseases <- sort(unique(df$category_canonical_disease_imc[!is.na(df$category_canonical_disease_imc)]))
    updateSelectInput(session, "selected_diseases",
                      choices = diseases, selected = NULL)
  }
  
  # Regions (admin1)
  if ("admin1" %in% names(df)) {
    regions <- sort(unique(df$admin1[!is.na(df$admin1)]))
    updateSelectInput(session, "selected_regions",
                      choices = regions, selected = NULL)
  }
}

# Apply all sidebar filters to a data.frame (returns filtered df)
apply_all_filters <- function(input, df) {
  req(is.data.frame(df))
  
  out <- df
  
  # Date filter
  if (!is.null(input$date_range) && "datevisit" %in% names(out)) {
    out <- out %>%
      filter(datevisit >= input$date_range[1],
             datevisit <= input$date_range[2])
  }
  
  # Facility filter
  if (!is.null(input$selected_facilities) &&
      length(input$selected_facilities) > 0 &&
      "orgunit" %in% names(out)) {
    out <- out %>% filter(orgunit %in% input$selected_facilities)
  }
  
  # Disease filter (category)
  if (!is.null(input$selected_diseases) &&
      length(input$selected_diseases) > 0 &&
      "category_canonical_disease_imc" %in% names(out)) {
    out <- out %>% filter(category_canonical_disease_imc %in% input$selected_diseases)
  }
  
  # Region filter
  if (!is.null(input$selected_regions) &&
      length(input$selected_regions) > 0 &&
      "admin1" %in% names(out)) {
    out <- out %>% filter(admin1 %in% input$selected_regions)
  }
  
  out
}

# -----------------------------------------------------------------------------
# OPTIONAL: Export module (uses your filtered data)
# Mount only if you add its UI somewhere (e.g., Export tab or sidebar)
# -----------------------------------------------------------------------------

exportDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Export"),
    fluidRow(
      column(6, downloadButton(ns("download_csv"),   "Download CSV")),
      column(6, downloadButton(ns("download_excel"), "Download Excel"))
    ),
    br(),
    textOutput(ns("preview"))
  )
}

exportDataServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    output$preview <- renderText({
      req(data_reactive())
      df <- data_reactive()
      paste0(
        "Rows: ", format(nrow(df), big.mark = ","),
        " | Columns: ", ncol(df),
        if ("datevisit" %in% names(df)) {
          rng <- range(df$datevisit, na.rm = TRUE)
          paste0(" | Date range: ",
                 format(rng[1], "%Y-%m-%d"), " to ", format(rng[2], "%Y-%m-%d"))
        } else ""
      )
    })
    
    output$download_csv <- downloadHandler(
      filename = function() paste0("epi_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) write.csv(data_reactive(), file, row.names = FALSE)
    )
    
    output$download_excel <- downloadHandler(
      filename = function() paste0("epi_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content  = function(file) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(data_reactive(), file)
        } else {
          # fallback to csv if openxlsx unavailable
          write.csv(data_reactive(), sub("\\.xlsx$", ".csv", file), row.names = FALSE)
        }
      }
    )
  })
}
# modules/shiny_modules.R
# Reusable helpers + optional modules (no sample data, no side effects)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(openxlsx)
  library(lubridate)
  library(stringr)
  library(scales)
  library(plotly)
  library(DT)
})

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

# Return the first column name from `candidates` that exists in df (or NULL)
.first_existing_col <- function(df, candidates) {
  hits <- intersect(candidates, names(df))
  if (length(hits) > 0) hits[[1]] else NULL
}

# Coerce a vector to Date safely
.as_date_safe <- function(x) {
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::as_date(x))
  }
  out
}

# Derive a default age group when an explicit column is missing
.derive_age_group <- function(age_numeric) {
  cut(suppressWarnings(as.numeric(age_numeric)),
      breaks = c(-Inf, 4, 14, 49, Inf),
      labels = c("0–4","5–14","15–49","50+"))
}

# -----------------------------------------------------------------------------
# Helpers to standardize filter behavior with your existing sidebar inputs
# -----------------------------------------------------------------------------

# Update sidebar filter choices from a data.frame (safe if columns are missing)
update_sidebar_filters <- function(session, df) {
  req(session, is.data.frame(df))

  # Date range (detect best candidate column)
  date_col <- .first_existing_col(df, c("datevisit", "datevisitnew", "visit_date", "date"))
  if (!is.null(date_col)) {
    dv <- .as_date_safe(df[[date_col]])
    if (any(!is.na(dv))) {
      rng <- range(dv, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2])) {
        updateDateRangeInput(
          session, "date_range",
          start = max(rng[2] - 90, rng[1]),
          end   = rng[2],
          min   = rng[1],
          max   = rng[2]
        )
      }
    }
  }

  # Facilities
  if ("orgunit" %in% names(df)) {
    facilities <- sort(unique(df$orgunit[!is.na(df$orgunit)]))
    updateSelectInput(session, "selected_facilities", choices = facilities, selected = NULL)
  }

  # Diseases (category)
  if ("category_canonical_disease_imc" %in% names(df)) {
    diseases <- sort(unique(df$category_canonical_disease_imc[!is.na(df$category_canonical_disease_imc)]))
    updateSelectInput(session, "selected_diseases", choices = diseases, selected = NULL)
  }

  # Regions (admin1)
  if ("admin1" %in% names(df)) {
    regions <- sort(unique(df$admin1[!is.na(df$admin1)]))
    updateSelectInput(session, "selected_regions", choices = regions, selected = NULL)
  }

  # Districts (admin2)
  if ("admin2" %in% names(df)) {
    admin2s <- sort(unique(df$admin2[!is.na(df$admin2)]))
    updateSelectInput(session, "selected_admin2", choices = admin2s, selected = NULL)
  }

  # Sex
  sex_col <- .first_existing_col(df, c("sex", "gender", "client_sex"))
  if (!is.null(sex_col)) {
    sx <- sort(unique(df[[sex_col]][!is.na(df[[sex_col]])]))
    updateSelectInput(session, "selected_sex", choices = sx, selected = NULL)
  }

  # Age group (prefer existing column, otherwise derive from age)
  agegrp_col <- .first_existing_col(df, c("age_group", "agegrp", "age_group5", "age_group10"))
  if (!is.null(agegrp_col)) {
    ag <- df[[agegrp_col]]
    ag <- sort(unique(as.character(ag[!is.na(ag)])))
    updateSelectInput(session, "selected_agegrp", choices = ag, selected = NULL)
  } else if ("age" %in% names(df)) {
    ag <- .derive_age_group(df$age)
    ag <- sort(unique(as.character(ag[!is.na(ag)])))
    updateSelectInput(session, "selected_agegrp", choices = ag, selected = NULL)
  }
}

# Apply all sidebar filters to a data.frame (returns filtered df)
apply_all_filters <- function(input, df) {
  req(is.data.frame(df))
  out <- df

  # --- Date filter ---
  date_col <- .first_existing_col(out, c("datevisit", "datevisitnew", "visit_date", "date"))
  if (!is.null(date_col) && !is.null(input$date_range)) {
    dv <- .as_date_safe(out[[date_col]])
    keep <- !is.na(dv) & dv >= input$date_range[1] & dv <= input$date_range[2]
    out <- out[keep, , drop = FALSE]
  }

  # --- Location filters ---
  if (!is.null(input$selected_regions) && length(input$selected_regions) > 0 && "admin1" %in% names(out)) {
    out <- out %>% dplyr::filter(.data$admin1 %in% input$selected_regions)
  }
  if (!is.null(input$selected_admin2) && length(input$selected_admin2) > 0 && "admin2" %in% names(out)) {
    out <- out %>% dplyr::filter(.data$admin2 %in% input$selected_admin2)
  }
  if (!is.null(input$selected_facilities) && length(input$selected_facilities) > 0 && "orgunit" %in% names(out)) {
    out <- out %>% dplyr::filter(.data$orgunit %in% input$selected_facilities)
  }

  # --- Sex filter ---
  sex_col <- .first_existing_col(out, c("sex", "gender", "client_sex"))
  if (!is.null(sex_col) && !is.null(input$selected_sex) && length(input$selected_sex) > 0) {
    out <- out %>% dplyr::filter(.data[[sex_col]] %in% input$selected_sex)
  }

  # --- Age group filter ---
  agegrp_col <- .first_existing_col(out, c("age_group", "agegrp", "age_group5", "age_group10"))
  if (!is.null(agegrp_col) && !is.null(input$selected_agegrp) && length(input$selected_agegrp) > 0) {
    out <- out %>% dplyr::filter(.data[[agegrp_col]] %in% input$selected_agegrp)
  } else if ("age" %in% names(out) && !is.null(input$selected_agegrp) && length(input$selected_agegrp) > 0) {
    ag <- .derive_age_group(out$age)
    out <- out[as.character(ag) %in% input$selected_agegrp, , drop = FALSE]
  }

  out
}

# -----------------------------------------------------------------------------
# Cascading options helper (call from server observers)
# -----------------------------------------------------------------------------

# Update admin2 and facility choices based on selected admin1/admin2
update_cascading_locations <- function(session, df, selected_admin1 = NULL, selected_admin2 = NULL) {
  req(session, is.data.frame(df))
  d <- df

  # Narrow by admin1
  if (!is.null(selected_admin1) && length(selected_admin1) > 0 && "admin1" %in% names(d)) {
    d <- d[d$admin1 %in% selected_admin1, , drop = FALSE]
  }

  # Update admin2 choices
  if ("admin2" %in% names(df)) {
    admin2s <- sort(unique(d$admin2[!is.na(d$admin2)]))
    updateSelectInput(session, "selected_admin2", choices = admin2s, selected = NULL)
  }

  # If admin2 provided, narrow further
  if (!is.null(selected_admin2) && length(selected_admin2) > 0 && "admin2" %in% names(d)) {
    d <- d[d$admin2 %in% selected_admin2, , drop = FALSE]
  }

  # Update facilities
  if ("orgunit" %in% names(df)) {
    facs <- sort(unique(d$orgunit[!is.na(d$orgunit)]))
    updateSelectInput(session, "selected_facilities", choices = facs, selected = NULL)
  }
}

# -----------------------------------------------------------------------------
# OPTIONAL: Export module (uses your filtered data)
# Mount only if you add its UI somewhere (e.g., Export tab or sidebar)
# -----------------------------------------------------------------------------

exportDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Export"),
    fluidRow(
      column(6, downloadButton(ns("download_csv"),   "Download CSV")),
      column(6, downloadButton(ns("download_excel"), "Download Excel"))
    ),
    br(),
    textOutput(ns("preview"))
  )
}

exportDataServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    output$preview <- renderText({
      req(data_reactive())
      df <- data_reactive()
      date_col <- .first_existing_col(df, c("datevisit", "datevisitnew", "visit_date", "date"))
      paste0(
        "Rows: ", format(nrow(df), big.mark = ","),
        " | Columns: ", ncol(df),
        if (!is.null(date_col)) {
          dv <- .as_date_safe(df[[date_col]])
          rng <- range(dv, na.rm = TRUE)
          if (all(is.finite(rng))) paste0(" | Date range: ",
                                          format(rng[1], "%Y-%m-%d"), " to ", format(rng[2], "%Y-%m-%d")) else ""
        } else ""
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("epi_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) write.csv(data_reactive(), file, row.names = FALSE)
    )

    output$download_excel <- downloadHandler(
      filename = function() paste0("epi_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content  = function(file) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(data_reactive(), file)
        } else {
          # fallback to csv if openxlsx unavailable
          write.csv(data_reactive(), sub("\\.xlsx$", ".csv", file), row.names = FALSE)
        }
      }
    )
  })
}