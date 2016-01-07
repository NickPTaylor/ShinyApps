library(readr)
library(dplyr)
library(leaflet)
library(shiny)

# load data --------------------------------------------------------------------

wind_data <- read_csv('data.csv') %>% 
  select(-c(Icon)) %>% 
  rename(latitude = Lat, longitude = Long, stage = Stage) %>% 
  mutate(log_scale = 5000 * log(MW + 1))
# user interface ---------------------------------------------------------------

ui <- fluidPage(
  headerPanel("UK Wind Power"),
  sidebarPanel(
    sliderInput(inputId = 'lvls', label = "Number of Quantile Levels",
                value = 4, min = 2, max = 10),
    sliderInput(inputId = 'rng',
                label = "Select Range of Power Output to Display", 
                dragRange = TRUE, min = 0, max = 350, value = c(0, 350)),
    checkboxInput(inputId = 'scl', 
                  label = "Scale Operational Plant Markers by Power Output",
                  value = TRUE),
    checkboxInput(inputId = 'nop', 
                  label = "Display Non-Operational Plant",
                  value = TRUE), 
    checkboxGroupInput(inputId = 'lyrs', "Display Layers",
                       choices = c("Construction","Planning", "Consented"))
  ),
  mainPanel(
    leafletOutput(outputId = "map", height = 600)
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output){
    
    mw_palette <- colorQuantile(palette = c('BuGn'), 
                                    domain = wind_data$MW, n = 5)
    stage_palette <- colorFactor(palette = c('black', 'red', 'green', 'blue'), 
                                 domain = as.factor(wind_data$stage))
    
    filter_data <- reactive({
       wind_data %>% filter(is.na(MW) | (MW > input$rng[1] & MW < input$rng[2]))
    })
     
    
    output$map <- renderLeaflet({
      leaflet(filter_data()) %>%
        addProviderTiles('Stamen.TonerLite', group = 'TonerLite') %>%
        fitBounds(-10, 50, 0, 60) %>%
        addCircles(data = filter(filter_data(), stage == 'Operational'), radius = ~log_scale,
                   fill = TRUE, stroke = FALSE, color = ~mw_palette(MW), 
                   fillOpacity = 0.5,
                   group = 'operational_scaled') %>%
        addCircles(group = 'non_operational',
                   popup =  ~ifelse(is.na(MW), name, paste(name, "\n", MW, "MW")),
                   fillOpacity = 1, radius = 1, col = ~stage_palette(stage))  
    })

    observe({
      proxy <- leafletProxy('map', data = filter_data()) 
     
      if(input$nop) {
        proxy %>% 
          showGroup('non_operational') %>% 
          addLegend('topleft', pal = stage_palette, values = ~stage, 
                    title = 'Stage of Development', layerId = 'non_operational')    
     } else {
        proxy %>% 
          hideGroup('non_operational') %>% 
          removeControl('non_operational')
     } 
   })
  
   observe({
     proxy <- leafletProxy('map', data = filter_data())
     
     if(input$scl){
       proxy %>% 
         showGroup('operational_scaled') %>% 
         addLegend('bottomleft', pal = mw_palette, values = ~MW, 
                title = 'Output in MW', layerId = 'operational_scaled')  
     } else {
       proxy %>% 
         hideGroup('operational_scaled') %>% 
         removeControl('operational_scaled')
     }
   }) 
}

shinyApp(ui = ui, server = server)
