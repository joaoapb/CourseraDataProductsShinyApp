#
# This is a Shiny web application for showing Brazilian Labour Market statistics
#

# load packages
library(mapproj)
library(sp)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# load the data
map <- readRDS("./data/map.rds")
db <- readRDS("./data/db.rds")

# change a little detail on the data
db$value[which(db$mov == 'Separations')] <- abs(db$value[which(db$mov == 'Separations')])

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
                 plotOutput("map")),
        tabPanel("Charts", 
                 radioButtons('choice1', 'Choose the data',
                              choices = list('Nominal', 'Year over Year, %')),
                 radioButtons('choice2', 'Choose the model', inline = T,
                              choices = list('Linear', 'Local Polynomial')),
                 plotOutput("chart")),
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
    t2 <- '<h4>Data</h4>'
    t3 <- '<p>The data presented comes from CAGED, a Brazilian registry of movements in the labor market. Created and maintained by the Brazilian Work and Employment Ministry, the CAGED microdata is available in a <a href="ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/">FTP</a>.</p>'
    t4 <- '<p>The data was aggregated by year and State, but keep in mind that the microdata allows for a much more complex analysis. For example, the microdata has one line for each hiring or separation, in a given month and city. Other info included are: Economic Activity Code, Ocupation Code, income and time on the job, for example.</p>'
    t5 <- '<h4>Tabs</h4>'
    t6 <- '<p><b>Map</b>: Show a map of Brazil, colored by the number of movements in the State. Use the slide to select the year and the dropdown to select the movement type.</p>'
    t7 <- '<p>What does each movement mean:</p>'
    t8 <- '<ul><li>Hires: Any addition to an establishment\'s payroll, including newly hired and rehired employees.</li><li>Separations: Separation of an employee from an establishment (voluntary, involuntary, or other).</li><li>Net Hires: Hires - separations</li></ul>'
    t9 <- '<p><b>Chart</b>: show a chart with the nominal value or the year over year variation, with a regression line over it. You choose the method!<p>'
    t10 <- '<p><b>Docs</b>: This documentation.<p>'
    t11 <- '<p></p>'
    t12 <- '<h4>Observations</h4>'
    t13 <- '<p>The scale changes with the year and the movement! (It is on my To Do to change this).</p><p>The code, as well as the RDS, are available on my <a href="https://github.com/joaoapb">GitHub</a>.</p>'  
    
    HTML(paste(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, sep = '<br/>'))
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
  
  # get the data for the chart
  data_chart <- reactive({
    movi <- ifelse(input$mov == 'Net_Hires',
                   'Net Hires',
                   input$mov)
    
    data_chart <- 
      db %>% 
      filter(mov == movi) %>% 
      group_by(Year) %>% 
      summarise(value = sum(value)) %>% 
      mutate(var = NA)
    
    data_chart$var[2:nrow(data_chart)] <- 
      data_chart$value[2:nrow(data_chart)] / data_chart$value[1:nrow(data_chart)-1] - 1
      
    if (input$choice1 == 'Nominal') {
      data_chart$vv = data_chart$value
    } else {
      data_chart$vv = data_chart$var
    }
    
    data_chart
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
  
  output$chart <- renderPlot({
    chart <- data_chart()
    smooth <- reactive({
      if (input$choice2 == 'Linear') {
        geom_smooth(method = 'lm', aes(x = Year, y = vv))
      } else {
        geom_smooth(method = 'loess', aes(x = Year, y = vv))
      }
    })
    pc <-
      ggplot(chart) +
      geom_line(
        aes(
          x = Year,
          y = vv
        )
      ) +
      smooth() +
      labs(x = 'Year',
           y = 'Value')
    
    print(pc)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)