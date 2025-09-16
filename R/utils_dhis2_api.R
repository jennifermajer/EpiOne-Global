# R/utils_dhis2_api.R — Simple, robust DHIS2 API helpers used by data_loader.R
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(glue)
  library(logger)
})

# Null coalescing
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# Build analytics/events query URL from config
.build_analytics_url <- function(cfg) {
  base <- cfg$dhis2$base_url
  if (!grepl("/$", base)) base <- paste0(base, "/")
  
  ep <- cfg$dhis2$endpoint %||% NULL
  if (!is.null(ep) && nzchar(ep)) {
    if (startsWith(ep, "http")) return(ep)
    return(paste0(base, ep))
  }
  
  prog <- cfg$parameters$program
  if (is.null(prog) || !nzchar(prog)) stop("Program ID missing in cfg$parameters$program")
  paste0(base, "analytics/events/query/", prog, ".json")
}

# Build query parameters from config
.build_query_params <- function(cfg) {
  params <- list()
  params$startDate <- cfg$parameters$date_start
  params$endDate   <- cfg$parameters$date_end
  
  if (!is.null(cfg$parameters$stage) && nzchar(cfg$parameters$stage)) {
    params$stage <- cfg$parameters$stage
  }
  if (!is.null(cfg$parameters$output_type))   params$outputType   <- cfg$parameters$output_type
  if (!is.null(cfg$parameters$paging))        params$paging       <- tolower(as.character(cfg$parameters$paging))
  if (!is.null(cfg$parameters$data_id_scheme)) params$dataIdScheme <- cfg$parameters$data_id_scheme
  
  # Dimension vectors (we'll flatten later)
  ous   <- cfg$dimensions$organizational_unit %||% NULL
  other <- cfg$dimensions$other_dimensions %||% NULL
  pinds <- cfg$dimensions$program_indicators %||% NULL
  
  dims <- character()
  if (!is.null(ous) && length(ous)) dims <- c(dims, glue("ou:{paste0(ous, collapse=';')}"))
  if (!is.null(other) && length(other)) dims <- c(dims, other)
  if (!is.null(pinds) && length(pinds)) dims <- c(dims, pinds)
  
  if (length(dims)) params$dimension <- unique(dims)
  params
}

# Convert query list into only scalar entries (repeat names for vector values)
.flatten_query <- function(q) {
  out <- list()
  for (nm in names(q)) {
    val <- q[[nm]]
    if (length(val) <= 1) {
      out[[nm]] <- val
    } else {
      # repeat same name for each value
      pieces <- as.list(as.character(val))
      names(pieces) <- rep(nm, length(pieces))
      out <- c(out, pieces)
    }
  }
  out
}

# Auth credentials from cfg or env
.dhis2_credentials <- function(cfg) {
  user <- if (!is.null(cfg$dhis2$username) && nzchar(cfg$dhis2$username)) cfg$dhis2$username else Sys.getenv("DHIS2_USERNAME", "")
  pass <- if (!is.null(cfg$dhis2$password) && nzchar(cfg$dhis2$password)) cfg$dhis2$password else Sys.getenv("DHIS2_PASSWORD", "")
  if (!nzchar(user) || !nzchar(pass)) stop("DHIS2 credentials not set (username/password).")
  list(user = user, pass = pass)
}

# Perform GET with basic retry, return parsed JSON (list) without vector simplification
.fetch_json <- function(url, query, creds, timeout_sec = 300, attempts = 3) {
  query <- .flatten_query(query)  # <-- critical fix: avoid vector entries in query=
  last_err <- NULL
  for (i in seq_len(attempts)) {
    resp <- tryCatch({
      httr::GET(
        url,
        httr::authenticate(creds$user, creds$pass, type = "basic"),
        query = query,
        httr::timeout(timeout_sec)
      )
    }, error = function(e) e)
    
    if (inherits(resp, "error")) {
      last_err <- resp
      log_warn("Attempt {i} failed: {resp$message}")
      next
    }
    sc <- httr::status_code(resp)
    if (sc == 200) {
      txt <- httr::content(resp, "text", encoding = "UTF-8")
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    } else {
      # try to pull server message but be resilient
      body_txt <- tryCatch(
        httr::content(resp, "text", encoding = "UTF-8"),
        error = function(e) "<no body>"
      )
      last_err <- simpleError(glue("HTTP {sc}: {body_txt}"))
      log_warn("Attempt {i} failed: {last_err$message}")
    }
  }
  stop(last_err)
}

# PUBLIC: Fetch DHIS2 events. Returns list(raw_json=..., metadata=..., data=NULL or data.frame)
fetch_dhis2_events <- function(cfg) {
  url   <- .build_analytics_url(cfg)
  query <- .build_query_params(cfg)
  creds <- .dhis2_credentials(cfg)
  
  log_info("🔗 GET {url}")
  log_info("🔎 Query params: {paste(names(query), collapse=', ')}")
  
  data_json <- .fetch_json(
    url,
    query,
    creds,
    timeout_sec = cfg$dhis2$timeout %||% 300,
    attempts    = cfg$dhis2$retry_attempts %||% 3
  )
  
  list(
    raw_json = data_json,
    metadata = data_json$metaData %||% list()
    # Optionally: data = build_events_df_from_analytics(data_json)
  )
}

# Convert DHIS2 analytics/events JSON -> data.frame
build_events_df_from_analytics <- function(x) {
  # Expect x$headers (list of name entries) and x$rows (list of character vectors)
  hdrs <- vapply(x$headers, function(h) h$name %||% h$column %||% "col", character(1))
  rows <- x$rows %||% list()
  if (!length(rows)) return(data.frame())

  m <- do.call(rbind, lapply(rows, function(r) as.character(r)))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- make.names(hdrs, unique = TRUE)

  # Try to normalize common columns
  to_date <- function(v) suppressWarnings(as.Date(v))
  date_cols <- grep("date|eventDate|occur|incident|created", names(df), ignore.case = TRUE, value = TRUE)
  for (nm in date_cols) {
    df[[nm]] <- to_date(df[[nm]])
  }

  # Common renames if present
  rn <- function(old, new) if (old %in% names(df)) names(df)[names(df) == old] <<- new
  rn("orgUnitName", "orgunit")
  rn("ouname",      "orgunit")
  rn("ou",          "orgunit")
  rn("eventDate",   "datevisit")
  rn("executionDate","datevisit")
  rn("occurDate",   "datevisit")
  rn("dx",          "morbidity")
  rn("morbidity_category", "morbidity_category")  # noop if absent

  df
}

# Simple wrapper returning a "register"-like data.frame used by the app
dhis2_fetch_register <- function(config = app_cfg) {
  ev <- fetch_dhis2_events(config)
  if (is.null(ev$raw_json)) return(data.frame())
  build_events_df_from_analytics(ev$raw_json)
}