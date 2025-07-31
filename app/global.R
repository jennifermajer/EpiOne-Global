# Global configuration for Shiny application
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)

# Source all R files
source("R/data_loader.R")
source("R/preprocessing.R") 
source("R/visualization_helpers.R")

# Load configuration
cfg <- config::get()