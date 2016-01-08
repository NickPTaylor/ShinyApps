library(readr)
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)

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
ui <- dashboardPage(
  dashboardHeader(title = 'UK Wind Farms'),
  dashboardSidebar(
    h1('Description'),
    p(
      'This application shows the locations of UK wind farms categorired 
      by stage of development.  The use can select the stages of development 
      to display.  In addition, for operational wind turbines, the user can 
      choose to scale the markers by power output.  The scaling is log based
      since the vast majority of wind farms are small scale but there are a few
      with very high output relatively.  Furthermore, the user can choose to
      filter to show wind farms within a specified range of power outputs.
      '
    ),
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
  ),
  dashboardBody(
    tags$head(tags$style("#map{height:90vh !important;}")),
    fluidRow(
      leafletOutput("map")
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
      setView(lng = -4.3, lat = 55, zoom = 5) 
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