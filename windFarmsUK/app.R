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
oper_data_mw <- all_data %>% filter(stage == 'Operational') %>% select(MW)

# set up color palettes for legend ---------------------------------------------
mw_palette <- colorNumeric(palette = "BuGn", domain = oper_data_mw, n = 5)
stage_palette <- colorFactor(palette = c("black", "red", "green", "blue"), 
                             domain = as.factor(all_data$stage))

# generate labels for markers
create_label <- function(name, stage, mw) {
  lab <- paste(sep = "<br/>",
               paste("<b>", name, "</b>"),
               paste("Status:", stage),
               ifelse(is.na(mw), "", sprintf("Output: %.2f MW", mw)))
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
      min = 0, max = max(oper_data$MW), value = c(0, max(oper_data_mw))
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

  # react to filtering of data -------------------------------------------------
  observe({
 
    # map proxy ----------------------------------------------------------------
    proxy <- leafletProxy("map", data = filter_data()) %>% 
       clearShapes() %>% 
       clearGroup("scaled_operational") %>% 
       clearGroup("all")
    
    # update base points ------------------------------------------------------- 
    proxy %>%   
      addCircles(popup = ~create_label(name, stage, MW),
                 fillOpacity = 1, radius = 1, col = ~stage_palette(stage),
                 group = "all") %>% 
      addLegend("bottomleft", pal = stage_palette, values = ~stage, 
                title = "Stage of Development", layerId = "all")
    
    # update scaled points of operational plant --------------------------------
    if ('Operational' %in% input$lyrs) {
      proxy %>%  
        addCircles(data = filter_data() %>% filter(stage == 'Operational'),
                   radius = ~log_scale, fillOpacity = 0.5, 
                   popup = ~create_label(name, stage, MW),  
                   fillColor = ~mw_palette(MW),weight = 1, col = "green",
                   group = "scaled_operational")  
    }
  })       
  
  # react to toggling of display of scaeled data -------------------------------  
  observeEvent(input$scl, {  
   
    # map proxy ---------------------------------------------------------------- 
    proxy <- leafletProxy("map", data = filter_data()) #%>%
   
    # toggle scaled operational points ----------------------------------------- 
    if (input$scl){
      proxy %>% 
        showGroup("scaled_operational") %>% 
        addLegend("bottomright", pal = mw_palette, 
                  values = unlist(oper_data_mw), 
                  title = "Ouput (MW)", 
                  layerId = "scaled_operational")
    } else {
      proxy %>%
        hideGroup("scaled_operational") %>% 
        removeControl("scaled_operational")
    } 
  })
  
#   # plot output --------------------------------------------------------------
#   output$plot <- renderPlot({
#     hist(filter_data()$MW) 
#   })
}

shinyApp(ui = ui, server = server)