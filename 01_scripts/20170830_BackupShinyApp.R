#
# This is a Shiny web application for showing Brazilian Labour Market statistics
#

library(shiny)
library(plotly)

map <- readRDS("../00_data/map.rds")
db <- readRDS("../00_data/db.rds")
map_data = map@data

# Define the UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Brazilian Labor Data", 
             windowTitle = "Brazilian labor Data"),
  
  # Sidebar with:
  #  slider for years
  #  filter for state and movement
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Choose the year:",
                  min = 2009,
                  max = 2016,
                  value = 2015,
                  step = 1,
                  dragRange = F,
                  sep = ""),
      selectInput("mov",
                  "Choose the movement type:",
                  multiple = F,
                  choices = c(
                    "Hires" = "Hires", 
                    "Separations" = "Separations", 
                    "Net Hires" = "Net_Hires"
                  ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 h3(textOutput("subtitle")),
                 leafletOutput("map")),
        tabPanel("Docs", verbatimTextOutput("docs"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # setup the map
  map_to_plot <- map
  
  # setup the data
  mp <- reactive({
    map_to_plot@data <-
      left_join(map_data,
                db[db$Year == input$year, ],
                by = c("CD_GEOCUF" = "State"))
  }) 
  
  # write down the subtitle
  output$subtitle <- renderText({
    paste0(ifelse(input$mov == 'Net_Hires', 'Net Hires', input$mov), ':')
  })
  
  # set up the labels
  labs <- 
    lapply(
      seq(reactive({nrow(mp@data)})), 
      function(i) {
        reactive({
          paste0('<p>', mp@data[i, "NM_ESTADO"], '</p>',
                 '<p>', reactive({mov_label <- ifelse(input$mov == 'Net_Hires', 'Net Hires', input$mov)}), 
                 mp@data[i, mov], '</p>'
          )
        })
      })
  
  output$map <- renderLeaflet({
    leaflet(reactive({mp})) %>% 
      addPolygons(
        color = "#111111",
        weight = .5,
        smoothFactor = .5,
        fillColor = ~colorQuantile("GnBu", as.numeric(as.character(CD_GEOCUF)))(as.numeric(as.character(CD_GEOCUF))),
        highlightOptions = highlightOptions(color = "white", weight = 3,
                                            bringToFront = TRUE),
        label = ~lapply(labs, HTML)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

