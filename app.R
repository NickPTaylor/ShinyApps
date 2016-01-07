library(readr)
library(dplyr)
library(leaflet)
library(shiny)

# load data --------------------------------------------------------------------
all_data <- read_csv("data.csv") %>% select(-c(Icon)) %>% 
  rename(lat = Lat, lng = Long, stage = Stage) %>% 
  mutate(log_scale = 
         ifelse(is.na(MW), 0, 1000 * -1000 * log(1 - MW/sum(MW, na.rm = T)))
  )
oper_data <- all_data %>% 
  filter(stage == 'Operational')

# set up color palettes for legend ---------------------------------------------
mw_palette <- colorNumeric(palette = "BuGn", domain = all_data$MW, n = 5)
stage_palette <- colorFactor(palette = c("black", "red", "green", "blue"), 
                             domain = as.factor(all_data$stage))

# generate labels for markers
create_label <- function(name, stage, mw) {
  lab <- paste(sep = "<br/>",
               paste("<b>", name, "</b>"),
               paste("Status:", stage),
               ifelse(is.na(mw), "", paste("Output:", mw)))
}

# user interface ---------------------------------------------------------------
ui <- bootstrapPage (
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 100, left = 10,
    
    sliderInput(
      inputId = "rng", 
      label = "Select Range of Power Output to Display", 
      min = 0, max = max(oper_data$MW), value = c(0, max(oper_data$MW))
    ), 
    checkboxInput(
      inputId = "scl",
      label = "Scale Operational Plant Markers by Power Output" 
    ),
    checkboxGroupInput(
      inputId = "lyrs", label = "Display Layers",
      choices = levels(as.factor(all_data$stage)), 
      selected = levels(as.factor(all_data$stage))
    )
  )
)

# server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # reactive expression for filtering data -------------------------------------
  filter_data <- reactive({
    all_data %>% 
      filter(stage %in% input$lyrs) %>% 
      filter(is.na(MW) | (MW > input$rng[1] & MW < input$rng[2]))
    })
  
  # base map output ------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(all_data) %>% 
      addTiles() %>%  
      setView(lng = -4.3, lat = 55, zoom = 6)  
  })

  # react to filtering of markers for all data ---------------------------------
  observe({
    
    # map proxy ----------------------------------------------------------------
    proxy <- leafletProxy("map", data = filter_data()) %>% 
       clearShapes() %>% 
       clearGroup("oper")
    
    # update filtered base points ---------------------------------------------- 
    proxy %>%   
      addCircles(popup = ~create_label(name, stage, MW),
                 fillOpacity = 1, radius = 1, col = ~stage_palette(stage),
                 group = "all_stage") %>% 
      addLegend("bottomleft", pal = stage_palette, values = ~stage, 
                title = "Stage of Development", layerId = "stg")
   
    # toggle scaled operational points ----------------------------------------- 
    if (input$scl & 'Operational' %in% input$lyrs){
      proxy %>% 
        addCircles(data = filter_data() %>% filter(stage == 'Operational'),
                   radius = ~log_scale, fillOpacity = 0.5, 
                   popup = ~create_label(name, stage, MW),  
                   fillColor = ~mw_palette(MW),weight = 1, col = "green",
                   group = "oper") %>% 
        addLegend("bottomright", pal = mw_palette, values = ~oper_data$MW, 
                title = "Ouput (MW)", 
                layerId = "oper")
    } else {
      proxy %>%
        removeControl("oper") %>% 
        clearGroup("oper")
    } 
  })
  
#   # plot output --------------------------------------------------------------
#   output$plot <- renderPlot({
#     hist(filter_data()$MW) 
#   })
}

shinyApp(ui = ui, server = server)