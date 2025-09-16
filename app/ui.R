# app/ui.R — Enhanced UI with Complete Tab Navigation (Data Summary Removed)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(shinyjs)
  library(leaflet)
  library(DT)
  library(plotly)
})

# Enhanced CSS with Disease Trends Styling (same as before)
custom_css <- tags$style(HTML("
  /* Your existing CSS styles remain the same */
  .content-wrapper { 
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    min-height: 100vh;
  }
  
  .main-header .logo { 
    font-weight: 600; 
    background: linear-gradient(45deg, #667eea 0%, #764ba2 100%);
  }
  
  .main-header .navbar {
    background: linear-gradient(45deg, #667eea 0%, #764ba2 100%);
  }
  
  .box { 
    border-radius: 12px; 
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    border-top: 3px solid #667eea;
    transition: transform 0.2s ease;
  }
  
  /* Chat container and other styles remain the same */
  .chat-container {
    height: 650px;
    border: 2px solid #667eea;
    border-radius: 16px;
    display: flex;
    flex-direction: column;
    background: white;
    box-shadow: 0 8px 25px rgba(102, 126, 234, 0.15);
  }
  
  .chat-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 20px;
    border-radius: 14px 14px 0 0;
    position: relative;
    overflow: hidden;
  }
  
  .epibot-logo {
    width: 45px;
    height: 45px;
    border-radius: 50%;
    margin-right: 15px;
    vertical-align: middle;
    border: 3px solid rgba(255,255,255,0.3);
    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    transition: transform 0.3s ease;
  }

  /* Geographic analysis specific styling */
  .geographic-controls {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }

  /* Enhanced map container styling */
  .map-container {
    border: 2px solid #667eea;
    border-radius: 16px;
    overflow: hidden;
    box-shadow: 0 8px 25px rgba(102, 126, 234, 0.15);
    background: white;
  }

  /* Tab styling for geographic analysis */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #667eea;
  }

  .nav-tabs-custom > .nav-tabs > li.active > a {
    background-color: #667eea;
    color: white;
  }

  /* Enhanced table styling for geographic summary */
  .geographic-summary-table {
    background: white;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  }

  .geographic-summary-table .table {
    margin-bottom: 0;
  }

  .geographic-summary-table .table thead th {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border: none;
    padding: 15px 12px;
    font-weight: 600;
  }

  .geographic-summary-table .table tbody tr:hover {
    background-color: #f8f9fa;
    transform: translateY(-1px);
    transition: all 0.2s ease;
  }

  /* Chart container enhancements */
  .chart-container {
    background: white;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    margin-bottom: 20px;
    transition: transform 0.2s ease, box-shadow 0.2s ease;
  }

  .chart-container:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(0,0,0,0.15);
  }

  /* Statistics panel styling */
  .stats-panel {
    background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
    border-left: 4px solid #667eea;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 15px;
  }

  .stats-panel h5 {
    color: #667eea;
    font-weight: 600;
    margin-bottom: 15px;
    border-bottom: 2px solid #e9ecef;
    padding-bottom: 8px;
  }

  /* Metric cards for geographic analysis */
  .geo-metric-card {
    background: white;
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    border-left: 4px solid #667eea;
    transition: all 0.3s ease;
  }

  .geo-metric-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 25px rgba(102, 126, 234, 0.2);
  }

  .geo-metric-card .metric-value {
    font-size: 2.5em;
    font-weight: 700;
    color: #667eea;
    margin-bottom: 5px;
  }

  .geo-metric-card .metric-label {
    font-size: 1.1em;
    color: #6c757d;
    font-weight: 500;
  }

  .geo-metric-card .metric-change {
    font-size: 0.9em;
    margin-top: 10px;
  }

  .metric-change.positive {
    color: #28a745;
  }

  .metric-change.negative {
    color: #dc3545;
  }

  .metric-change.stable {
    color: #6c757d;
  }

  /* Branding */
  .header-branding-slot {
    padding: 8px 14px !important;
  }

  .header-branding {
    display: flex;
    align-items: center;
    gap: 12px;
  }

  .header-branding img {
    height: 36px;
    width: auto;
    border-radius: 6px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.15);
    background: #fff;
    padding: 4px 6px;
  }

  .branding-banner {
    margin: 0 0 18px 0;
    padding: 14px 18px;
    border-radius: 10px;
    background: linear-gradient(135deg, #eef2ff 0%, #e0e7ff 100%);
    border-left: 4px solid #4f46e5;
    display: flex;
    align-items: center;
    gap: 16px;
    color: #1f2937;
    font-size: 14px;
    box-shadow: 0 3px 12px rgba(79,70,229,0.15);
  }

  .branding-banner img {
    height: 42px;
    width: auto;
    border-radius: 6px;
    background: #fff;
    padding: 4px 6px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.15);
  }

  .branding-banner strong {
    color: #4338ca;
  }

  /* Alert styling for geographic analysis */
  .geo-alert {
    padding: 15px 20px;
    border-radius: 10px;
    margin-bottom: 15px;
    border-left: 4px solid;
    font-weight: 500;
  }

  .geo-alert.high-risk {
    background-color: #ffeaa7;
    border-left-color: #e17055;
    color: #2d3436;
  }

  .geo-alert.medium-risk {
    background-color: #ffeaa7;
    border-left-color: #fdcb6e;
    color: #2d3436;
  }

  .geo-alert.low-risk {
    background-color: #d1f2eb;
    border-left-color: #00b894;
    color: #2d3436;
  }

  /* Enhanced download button */
  .download-enhanced {
    background: linear-gradient(135deg, #00b894 0%, #00a085 100%);
    border: none;
    color: white;
    padding: 10px 20px;
    border-radius: 8px;
    font-weight: 600;
    transition: all 0.3s ease;
    box-shadow: 0 4px 12px rgba(0, 184, 148, 0.3);
  }

  .download-enhanced:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(0, 184, 148, 0.4);
    background: linear-gradient(135deg, #00a085 0%, #00b894 100%);
  }

  /* Enhanced styling for disease trends tab */
  .trend-metric-card {
    background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
    border-left: 4px solid #667eea;
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 15px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }

  .trend-analysis-box {
    background: white;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    margin-bottom: 20px;
  }

  .disease-priority-high {
    background-color: #ffebee !important;
    border-left: 4px solid #f44336;
  }

  .disease-priority-medium {
    background-color: #fff3e0 !important;
    border-left: 4px solid #ff9800;
  }

  .disease-priority-low {
    background-color: #e8f5e8 !important;
    border-left: 4px solid #4caf50;
  }

  .trends-control-panel {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }

  /* Responsive adjustments for geographic analysis */
  @media (max-width: 768px) {
    .geographic-controls {
      padding: 15px;
    }
    
    .geo-metric-card {
      margin-bottom: 15px;
      padding: 15px;
    }
    
    .geo-metric-card .metric-value {
      font-size: 2em;
    }
    
    .chart-container {
      padding: 15px;
    }
    
    .trends-control-panel {
      padding: 15px;
    }
  }

  /* Custom styling for plotly charts in geographic analysis */
  .plotly-container {
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
  }

  /* Tab content styling */
  .tab-content {
    background: white;
    border-radius: 0 0 12px 12px;
    padding: 20px;
    min-height: 400px;
  }

  /* Enhanced selectInput styling for geographic filters */
  .geographic-controls .form-group {
    margin-bottom: 20px;
  }

  .geographic-controls .form-control {
    border: 2px solid #e9ecef;
    border-radius: 8px;
    padding: 10px 15px;
    font-size: 14px;
    transition: all 0.3s ease;
  }

  .geographic-controls .form-control:focus {
    border-color: #667eea;
    box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
  }

  .geographic-controls label {
    font-weight: 600;
    color: #495057;
    margin-bottom: 8px;
  }

  /* Enhanced box styling for geographic analysis */
  .box.geographic-enhanced {
    border: none;
    border-radius: 16px;
    box-shadow: 0 8px 25px rgba(0,0,0,0.1);
    overflow: hidden;
  }

  .box.geographic-enhanced .box-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 20px;
    border-radius: 16px 16px 0 0;
  }

  .box.geographic-enhanced .box-header .box-title {
    font-size: 18px;
    font-weight: 600;
    margin: 0;
  }

  .box.geographic-enhanced .box-body {
    padding: 25px;
    background: white;
  }

  /* Loading spinner for geographic analysis */
  .geo-loading {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 300px;
    background: #f8f9fa;
    border-radius: 12px;
  }

  .geo-loading .spinner {
    width: 40px;
    height: 40px;
    border: 4px solid #e9ecef;
    border-top: 4px solid #667eea;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  /* Enhanced table styling for geographic data */
  .geographic-data-table {
    background: white;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
  }

  .geographic-data-table table {
    margin-bottom: 0;
    width: 100%;
  }

  .geographic-data-table th {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    font-weight: 600;
    padding: 15px 12px;
    border: none;
    text-align: left;
  }

  .geographic-data-table td {
    padding: 12px;
    border-bottom: 1px solid #e9ecef;
    vertical-align: middle;
  }

  .geographic-data-table tr:hover {
    background-color: #f8f9fa;
    transition: background-color 0.2s ease;
  }

  .geographic-data-table tr:last-child td {
    border-bottom: none;
  }

  /* Trauma percentage color coding */
  .trauma-high {
    background-color: #ffebee !important;
    color: #c62828;
    font-weight: 600;
  }

  .trauma-medium {
    background-color: #fff3e0 !important;
    color: #ef6c00;
    font-weight: 600;
  }

  .trauma-low {
    background-color: #e8f5e8 !important;
    color: #2e7d32;
    font-weight: 600;
  }

  /* Enhanced legend styling */
  .plot-legend {
    background: rgba(255, 255, 255, 0.95);
    border: 1px solid #dee2e6;
    border-radius: 8px;
    padding: 10px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }

  /* Geographic analysis specific animations */
  .geo-fade-in {
    animation: fadeIn 0.6s ease-in;
  }

  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(20px); }
    to { opacity: 1; transform: translateY(0); }
  }

  .geo-slide-in {
    animation: slideIn 0.5s ease-out;
  }

  @keyframes slideIn {
    from { transform: translateX(-30px); opacity: 0; }
    to { transform: translateX(0); opacity: 1; }
  }

  /* Enhanced progress indicators */
  .geo-progress {
    background: #e9ecef;
    border-radius: 10px;
    overflow: hidden;
    height: 8px;
    margin: 10px 0;
  }

  .geo-progress-bar {
    height: 100%;
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
    transition: width 0.6s ease;
    border-radius: 10px;
  }

  /* Status indicators */
  .status-indicator {
    display: inline-block;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    margin-right: 8px;
  }

  .status-indicator.high {
    background-color: #dc3545;
    box-shadow: 0 0 0 2px rgba(220, 53, 69, 0.2);
  }

  .status-indicator.medium {
    background-color: #ffc107;
    box-shadow: 0 0 0 2px rgba(255, 193, 7, 0.2);
  }

  .status-indicator.low {
    background-color: #28a745;
    box-shadow: 0 0 0 2px rgba(40, 167, 69, 0.2);
  }

  /* Enhanced tooltip styling */
  .geo-tooltip {
    background: rgba(0, 0, 0, 0.9);
    color: white;
    padding: 10px 15px;
    border-radius: 8px;
    font-size: 12px;
    max-width: 250px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
  }

  /* Accessibility improvements */
  .sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
  }

  /* High contrast mode support */
  @media (prefers-contrast: high) {
    .box.geographic-enhanced {
      border: 2px solid #000;
    }
    
    .geographic-controls .form-control {
      border: 2px solid #000;
    }
    
    .geo-metric-card {
      border: 2px solid #000;
    }
  }
"))

# Main UI with all new modules
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display:flex; align-items:center; gap:10px;",
      tags$img(src = "epione_logo.png", height = 38, style = "background:#fff;border-radius:6px;padding:4px 6px;box-shadow:0 1px 4px rgba(0,0,0,0.15);"),
      tags$span(
        icon("hospital", style = "margin-right: 8px;"),
        "IMC Syria Epidemiological Dashboard",
        style = "font-size: 18px; font-weight: 600;"
      )
    ),
    tags$li(class = "dropdown header-branding-slot", uiOutput("header_branding")),
    tags$li(class = "dropdown", uiOutput("synthetic_badge")),
    tags$li(class = "dropdown",
            tags$a(href = "docs/index.html", target = "_blank",
                   icon("book"), HTML("&nbsp;Docs")))
  ),
  
  # Sidebar
  dashboardSidebar(
    useShinyjs(),
    custom_css,
    
    sidebarMenu(
      id = "sidebar_menu",
      
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("EpiBot Assistant", tabName = "epibot", icon = icon("robot")),
      menuItem("Disease Surveillance", tabName = "surveillance", icon = icon("shield-virus")),
      menuItem("Disease Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("map-marked-alt")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Advanced Analytics", tabName = "advanced", icon = icon("brain"),
        menuSubItem("Predictive Analytics", tabName = "predictive", icon = icon("chart-line")),
        menuSubItem("Disease Correlations", tabName = "correlations", icon = icon("project-diagram")),
        menuSubItem("Climate Integration", tabName = "climate", icon = icon("cloud-sun"))
      ),
      
      # Filters section (your existing filter code)
      br(),
      div(style = "padding: 0 15px;",
          h4("Filters", class = "text-light"),
          
          # Date Range
          div(style = "margin-bottom: 15px;",
              dateRangeInput(
                "date_range",
                "📅 Date Range:",
                start = Sys.Date() - 365,
                end = Sys.Date()
              )
          ),
          
          # Other filters...
          selectInput("selected_sex", "⚥ Sex:", choices = NULL, multiple = TRUE),
          selectInput("selected_age_groups", "🎯 Age Group:", choices = NULL, multiple = TRUE),
          selectInput("selected_regions", "🗺️ Governorate:", choices = NULL, multiple = TRUE),
          selectInput("selected_facilities", "🏥 Facility:", choices = NULL, multiple = TRUE)
      ),
      
      # Quick Actions
      br(),
      div(class = "sidebar-section", style = "padding: 0 15px;",
          h5("Quick Actions", class = "text-light"),
          
          actionButton("refresh_data", HTML('<i class="fa fa-sync-alt"></i> Refresh Data'),
                      class = "btn-primary btn-sm btn-block"),
          actionButton("generate_ai_summary", HTML('<i class="fa fa-robot"></i> Generate AI Summary'),
                      class = "btn-info btn-sm btn-block"),
          downloadButton("download_report", HTML('<i class="fa fa-file-alt"></i> Download Report'),
                        class = "btn-success btn-sm btn-block"),
          conditionalPanel(
            condition = "output.synthetic_mode_on != 'true'",
            downloadButton("download_data", HTML('<i class="fa fa-download"></i> Download Data (CSV)'),
                          class = "btn-secondary btn-sm btn-block")
          ),
          conditionalPanel(
            condition = "output.synthetic_mode_on == 'true'",
            selectInput("synthetic_export_choice", "Export:",
                        choices = list(
                          "Synthetic Fixture (CSV)" = "synthetic",
                          "Current Filtered Data (CSV)" = "filtered"
                        ), selected = "synthetic"),
            helpText(HTML(paste(
              "<b>Synthetic Fixture</b>: Freshly generated test dataset including all required fields and taxonomy flags.",
              "<br/><b>Current Filtered Data</b>: The data currently shown in the dashboard after applying filters.",
              sep = ""))),
            downloadButton("download_synthetic_export", HTML('<i class="fa fa-download"></i> Download Selected Export'),
                          class = "btn-secondary btn-sm btn-block")
          )
      )
    )
  ),
  
  # Body with all new module UIs
  dashboardBody(
    useShinyjs(),
    uiOutput("branding_banner"),
    
    tabItems(
      
      # ================= OVERVIEW TAB =================
      tabItem(tabName = "overview",
              mod_overview_ui("overview")
      ),
      
      # ================= EPIBOT TAB =================
      tabItem(tabName = "epibot",
        fluidRow(
          column(8,
            box(
              title = tagList(
                tags$img(src = "epibot_bmo_1024.png", alt = "EpiBot logo", style = "height:32px; margin-right:8px;"),
                "EpiBot AI Assistant"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = NULL,

              # Centered avatar
              div(
                style = "text-align:center; margin-bottom:15px;",
                tags$img(src = "epibot_bmo_1024.png", alt = "EpiBot", 
                         style = "max-width:150px; border-radius:50%; box-shadow:0 2px 8px rgba(0,0,0,0.2);")
              ),

              # EpiBot module UI
              mod_epibot_ui("epibot")
            )
          ),
          column(4,
                 box(
                   title = "AI Capabilities",
                   status = "info",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "300px",
                   div(
                     h5("🎯 Available Capabilities:"),
                     tags$ul(
                       tags$li(icon("chart-line"), " Real-time disease trend analysis"),
                       tags$li(icon("map-marked-alt"), " Geographic pattern detection"), 
                       tags$li(icon("exclamation-triangle"), " Outbreak alert generation"),
                       tags$li(icon("users"), " Population health insights"),
                       tags$li(icon("hospital"), " Facility performance analysis")
                     )
                   )
                 ),
                 
                 # Summary cards for AI context
                 mod_summary_cards_ui("epibot_summary", title = "Data Summary for AI", show_refresh = FALSE)
          )
        )
      ),
      
      # ================= DISEASE SURVEILLANCE TAB =================
      tabItem(tabName = "surveillance",
              mod_disease_surveillance_ui("surveillance", height = "500px")
      ),
      
      # ================= TRENDS TAB =================
      tabItem(tabName = "trends",
              mod_time_trends_ui("trends", height = "500px")
      ),
      
      # ================= GEOGRAPHIC TAB =================
      tabItem(tabName = "geographic",
              mod_geographic_ui("geographic", height = "700px")
      ),
      
      # ================= SETTINGS TAB =================
      tabItem(tabName = "settings",
              mod_settings_ui("settings")
      ),

      # ================= ADVANCED ANALYTICS TABS =================
      
      # Predictive Analytics Tab
      tabItem(tabName = "predictive",
        fluidRow(
          column(12,
            box(
              title = tagList(
                icon("chart-line", style = "margin-right: 8px;"),
                "Predictive Analytics Dashboard"
              ),
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              
              # Check if module exists
              conditionalPanel(
                condition = "output.predictive_module_available",
                predictive_analytics_UI("predictive_analytics")
              ),
              conditionalPanel(
                condition = "!output.predictive_module_available",
                div(class = "alert alert-warning",
                  icon("exclamation-triangle"), " Predictive Analytics module not available.",
                  br(), "Please ensure the module file is properly loaded."
                )
              )
            )
          )
        )
      ),
      
      # Multi-Disease Correlation Tab  
      tabItem(tabName = "correlations",
        fluidRow(
          column(12,
            box(
              title = tagList(
                icon("project-diagram", style = "margin-right: 8px;"),
                "Multi-Disease Correlation Analysis"
              ),
              status = "warning",
              solidHeader = TRUE,
              width = NULL,
              
              # Check if module exists
              conditionalPanel(
                condition = "output.correlation_module_available",
                multi_disease_correlation_UI("disease_correlations")
              ),
              conditionalPanel(
                condition = "!output.correlation_module_available",
                div(class = "alert alert-warning",
                  icon("exclamation-triangle"), " Multi-Disease Correlation module not available.",
                  br(), "Please ensure the module file is properly loaded."
                )
              )
            )
          )
        )
      ),
      
      # Climate Integration Tab
      tabItem(tabName = "climate",
        fluidRow(
          column(12,
            box(
              title = tagList(
                icon("cloud-sun", style = "margin-right: 8px;"),
                "Climate-Health Integration"
              ),
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              
              # Check if module exists
              conditionalPanel(
                condition = "output.climate_module_available",
                climate_integration_UI("climate_integration")
              ),
              conditionalPanel(
                condition = "!output.climate_module_available",
                div(class = "alert alert-warning",
                  icon("exclamation-triangle"), " Climate Integration module not available.",
                  br(), "Please ensure the module file is properly loaded and API keys are configured."
                )
              )
            )
          )
        )
      )
    )
  )
)
