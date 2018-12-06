library(shiny)
library(leaflet)
library(plotly)

farm_laborers <- read.csv("./farm_laborers.csv")
kind_of_work <- as.character(unique(farm_laborers$kind_of_work))

shinyUI(fluidPage(
  # Application title
  fluidPage(
    tags$style(type="text/css", 
               "h2 { width: 100%; text-align:center;}"),
    column(width = 10, offset = 1, 
      titlePanel("Registry System for Basic Sectors in Agriculture, Department of Budget and Management, Philippines")
    ),
    br(),
    column(width = 12,
      pre("Instructions: \n1. Select from the list of controls on the left and choose between map or bar plot below. \n2. Plots will automatically change information shown based on control values selected. \n3. It takes time to query server information so please wait for it to load.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "kind_of_work", label = "Kind of Work: ", choices = kind_of_work),
      radioButtons(inputId = "sex", label = "Sex: ",
                   choices = c("Total" = "total",
                               "Male" = "male",
                               "Female" = "female")),
      radioButtons(inputId = "type_of_work", label = "Type of Work: ",
                   choices = c("Farm Laborers Only" = "farm_laborers_only",
                               "Farmers and Fishermen" = "farm_laborers_and_fishermen"))
    ),
    mainPanel(
      tags$style(type="text/css", "#map_title, #barplot_title { width: 100%; text-align:center; font-size: 13pt;}"),
      tabsetPanel(
        type = "tabs",
        tabPanel(title = "Map", 
          textOutput(outputId = "map_title", inline = FALSE), 
          leafletOutput(outputId = "map_plot"),
          br(),
          textOutput(outputId = "total")
        ),
        tabPanel(title = "Bar Plot", 
          textOutput(outputId = "barplot_title", inline = FALSE),
          plotlyOutput(outputId = "barplot"),
          br(),
          textOutput(outputId = "total2")
        )
      )
    )
  )
))