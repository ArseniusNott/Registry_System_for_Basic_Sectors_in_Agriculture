library(shiny)
library(leaflet)
library(dplyr)

# Define server logic required to draw the map
shinyServer(function(input, output) {
  # load the data
  farm_laborers <- read.csv("./farm_laborers.csv")
  
  # input_parameters
  input_parameters <- reactive({
    # control values
    kind_of_work <- input$kind_of_work
    sex <- input$sex
    type_of_work <- input$type_of_work
    
    return(c(kind_of_work, sex, type_of_work))
  })
  
  # selected data based on input parameters
  selected_data <- reactive({
    # control values
    kind_of_work <- input$kind_of_work
    sex <- input$sex
    type_of_work <- input$type_of_work
    
    # This is the column where the number of workers per work type and work kind
    # are recorded. This will be shown on the popup per province
    column_to_select <- paste(sex, type_of_work, sep = "_")
    
    # This is the column that shows the total farm laborers per sex
    total_farm_laborers_by_sex <- paste(sex, "farm_laborers", sep = "_")
    
    # select columns that are essential to draw the choropleth
    columns_to_select <- c("province", "kind_of_work", total_farm_laborers_by_sex, 
                           column_to_select, "lat", "lng")
    
    # filter based on the kind of work and columns to select
    farm_laborers_filtered <- 
      farm_laborers[farm_laborers$kind_of_work == kind_of_work, columns_to_select]
  })
  
  # show plot title for each plot
  show_plot_title <- reactive({
    # input parameters
    input_parameters <- input_parameters()
    kind_of_work <- input_parameters[1]
    sex <- input_parameters[2]
    type_of_work <- input_parameters[3]
    
    data <- selected_data()
    
    plot.title <- paste(gsub("_", " ", names(data)[4]), "per province doing"
                        , kind_of_work, "work")
    toupper(plot.title)
  })
  
  output$map_plot <- renderLeaflet({
    # input parameters
    input_parameters <- input_parameters()
    kind_of_work <- input_parameters[1]
    sex <- input_parameters[2]
    type_of_work <- input_parameters[3]
    
    # create the map
    # select farm_laborers_filtered rows EXCEPT the first row.
    # It will be used as an aggregator outside the plot since that only show the
    # total across 20 provinces
    # get this data from the reactive function above
    data <- selected_data()
    data <- data[2:dim(data)[1], ]
    
    column_to_select <- paste(sex, type_of_work, sep = "_")
    
    farm_laborers_map <- 
      data %>%
      subset(select = c("lat", "lng")) %>%
      leaflet(width = "100%") %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(popup = paste(sep = "<br/>",
                               paste("<b>", data$province, "</b>"),
                               paste("Count: ", data[, column_to_select])))
    
    # show the map
    farm_laborers_map
  })
  
  # change map title
  output$map_title <- reactive({
    show_plot_title()
  })
  
  # change barplot title
  # change map title
  output$barplot_title <- reactive({
    show_plot_title()
  })
  
  output$barplot <- renderPlotly({
    # input parameters
    input_parameters <- input_parameters()
    kind_of_work <- input_parameters[1]
    sex <- input_parameters[2]
    type_of_work <- input_parameters[3]
    
    # Plotly layout parameters
    # Margin
    m <- list(
      l = 90,
      r = 10,
      b = 125,
      t = 100,
      pad = 10
    )
    # font style
    general.font <- list(
      family='Old Standard TT, serif',
      size=8,
      color='black'
    )
    # x label
    x <- list(
      tickangle=-90,
      tickfont = general.font,
      tickformat = '.0f',
      automargin = TRUE,
      title = 'Province'
    )
    # y label
    y <- list(
      tickfont = general.font,
      tickformat = '.0f',
      automargin = TRUE,
      title = 'Count'
    )
    
    # create the map
    # select farm_laborers_filtered rows EXCEPT the first row.
    # It will be used as an aggregator outside the plot since that only show the
    # total across 20 provinces
    # get this data from the reactive function above
    data <- selected_data()
    data <- data[2:dim(data)[1], c(1, 2, 4)]
    data$province <- as.character(data$province)
    
    y.formula <- as.formula(paste("~", names(data)[3]))
    
    pl <- plot_ly(data = data, x = ~province, y = y.formula, type = "bar") %>%
      layout(xaxis = x, yaxis = y, margin = m)
    pl$elementId <- NULL
    pl
  })
  
  # Show overall count at the bottom of each plot (map and bar plots)
  overall_count <- reactive({
    # input parameters
    input_parameters <- input_parameters()
    kind_of_work <- input_parameters[1]
    sex <- input_parameters[2]
    
    data <- selected_data()
    
    # This is the column that shows the total farm laborers per sex
    total_farm_laborers_by_sex <- paste(sex, "farm_laborers", sep = "_")
    
    overall_count <- paste(gsub("_", " ", names(data)[4]), "DOING", 
                           kind_of_work, "work (overall):", 
                           data[1, total_farm_laborers_by_sex])
    toupper(overall_count)
  })
  output$total <- reactive({
    overall_count()
  })
  output$total2 <- reactive({
    overall_count()
  })
})
