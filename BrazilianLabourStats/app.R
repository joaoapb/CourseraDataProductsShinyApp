#
# This is a Shiny web application for showing Brazilian Labour Market statistics
#

# load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# load the data
map <- readRDS("../00_data/map.rds")
db <- readRDS("../00_data/db.rds")

# change a little detail on the data
db$value[which(db$mov == 'Separations')] <- abs(db$value[which(db$mov == 'Separations')])

# creates legend labels
# niceprint <- function(x, decimal.mark=".", big.mark=",", digits = 2, ...) {
#   formatC(unclass(x), decimal.mark=decimal.mark, big.mark=big.mark, digits = digits, format = "f")
# }
# brHires <- c(min(db$value[which(db$mov == 'Hires')]), 
#              mean(db$value[which(db$mov == 'Hires')]),
#              max(db$value[which(db$mov == 'Hires')]))
# legHires <- c(
#   niceprint(min(db$value[which(db$mov == 'Hires')])/1000),
#   niceprint(mean(db$value[which(db$mov == 'Hires')])/1000),
#   niceprint(max(db$value[which(db$mov == 'Hires')])/1000))
# 
# brSep <- c(min(db$value[which(db$mov == 'Separations')]), 
#            mean(db$value[which(db$mov == 'Separations')]),
#            max(db$value[which(db$mov == 'Separations')]))
# legSep <- c(
#   niceprint(min(db$value[which(db$mov == 'Separations')])/1000),
#   niceprint(mean(db$value[which(db$mov == 'Separations')])/1000),
#   niceprint(max(db$value[which(db$mov == 'Separations')])/1000))
# 
# brNet <- c(min(db$value[which(db$mov == 'Net Hires')]), 
#            mean(db$value[which(db$mov == 'Net Hires')]),
#            max(db$value[which(db$mov == 'Net Hires')]))
# legNet <- c(
#   niceprint(min(db$value[which(db$mov == 'Net Hires')])/1000),
#   niceprint(mean(db$value[which(db$mov == 'Net Hires')])/1000),
#   niceprint(max(db$value[which(db$mov == 'Net Hires')])/1000))

# creates color palettes
palHires <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
scHires <- scale_colour_gradientn(colours = palHires(100), 
                                  limits=c(min(db$value[which(db$mov == 'Hires')]), 
                                           max(db$value[which(db$mov == 'Hires')])))

palSep <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
scSep <- scale_colour_gradientn(colours = palSep(100), 
                                limits=c(min(db$value[which(db$mov == 'Separations')]), 
                                         max(db$value[which(db$mov == 'Separations')])))

palNet <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
scNet <- scale_colour_gradientn(colours = palNet(100), 
                                limits=c(min(db$value[which(db$mov == 'Net Hires')]), 
                                         max(db$value[which(db$mov == 'Net Hires')])))

map@data$id <- rownames(map@data)
map_fixed = fortify(map) %>% left_join(map@data)

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
                    plotOutput("map")),
           tabPanel("Documentation", htmlOutput("docs"))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #  setup the Docs tab
  output$docs <- renderUI({
    t1 <- '<h3>Documentation</h3>'
    t2 <- '<h4>Data</h3>'
    t3 <- '<p>The data presented comes from CAGED, a Brazilian registry of movements in the labor market. Created and maintained by the Brazilian Work and Employment Ministry, the CAGED microdata is available in a <a href="ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/">FTP</a>.</p>'
    t4 <- '<p>The data was aggregated by year and State, but keep in mind that the microdata allows for a much more complex analysis. For example, the microdata has one line for each hiring or separation, in a given month and city. Other info included are: Economic Activity Code, Ocupation Code, income and time on the job, for example.</p>'
    t6 <- '<h4>How to use</h3>'
    t7 <- '<p>Select a year on the slider, and a movement on the dropdown list, and the map will be updated.</p>'
    t8 <- '<p>What does each movement mean:</p>'
    t9 <- '<ul><li>Hires: Any addition to an establishment\'s payroll, including newly hired and rehired employees.</li><li>Separations: Separation of an employee from an establishment (voluntary, involuntary, or other).</li><li>Net Hires: Hires - separations</li></ul>'
    t10 <- '<p></p>'
    t11 <- '<h4>Observations</h3>'
    t12 <- '<p>The scale changes with the year and the movement! (It is on my To Do to change this).</p><p>The code, as well as the RDS, are available on my <a href="https://github.com/joaoapb">GitHub</a>.</p>'  
    
    HTML(paste(t1, t2, t3, t4,t6, t7, t8, t9, t10, t11, t12, sep = '<br/>'))
    })
  
  # select the color scale
  sc <- reactive({
    if(input$mov == 'Net_Hires') {
      sc = scNet
    } else if(input$mov == 'Hires') {
      sc = scHires
    } else {
      sc = scSep
    }
    sc
  })
  
  # get the data to plot
  data_join <- reactive({
    movi <- ifelse(input$mov == 'Net_Hires',
                   'Net Hires',
                   input$mov)
    data_join <- 
      map_fixed %>% 
      left_join(db %>% 
                  filter(Year == input$year,
                         mov == movi),
                by = c('CD_GEOCUF' = 'State'))
    data_join
  })
  
  # render the plot
  output$map <- renderPlot({
    to_plot <- data_join()
    p <-
      ggplot(to_plot) +
      geom_polygon(
        aes(
          x = long,
          y = lat,
          group = group,
          fill = value/1000
        ),
        color = 'white'
      ) +
      coord_map() +
      sc() +
      theme_map() +
      labs(fill = "Thousands") 
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)