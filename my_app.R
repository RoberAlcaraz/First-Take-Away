# Roberto Jesús Alcaraz Molina
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly, shinythemes, shinyjs, DT, leaflet, knitr,
               Stat2Data, dplyr, patchwork, ggpubr, htmlwidgets, shinythemes, GGally, ggforce, maps, network, viridis)
vehicles <- readRDS("vehicles_data.RDS")

introPanel <- tabPanel("Craiglist's Vehicles",
                       sidebarLayout(position = "right",
                         sidebarPanel(
                           h3(strong("Description of the variables: ")),
                           HTML(paste0("<ul><li>", 
                                       code("price"), ": indicates the price of the vehicle.</li><li>", 
                                       code("year"), ": indicates the year of the vehicle.</li><li>", 
                                       code("manufacturer"), ": indicates the class of the vehicle.</li><li>", 
                                       code("condition"), ": condition of the car (like new, good, etc).</li><li>",
                                       code("fuel"), ": fuel that consumes each car.</li><li>",
                                       code("odometer"), ": indicates the kms of a car.</li><li>", 
                                       code("title_status"), ": indicates if the car is able to drive or not.</li><li>", 
                                       code("transmission"), ": indicates the transmission of the cars.</li><li>",
                                       code("drive"), ": indicates the wheel drive of the vehicle.</li><li>",
                                       code("lat"), ": indicates the latitude of the location of the vehicle.</li><li>",
                                       code("long"), ": indicates the longitude of the location of the vehicle.</li></ul>")
                                )
                           ),
                         mainPanel(
                           h1(strong("Introduction")),
                           p("Craigslist is the world’s largest collection of used vehicles for sale. This data set includes every used vehicle
                             entry within the United States on Craiglist, from the year 1900 until today. This data set has been taken
                             from the website",
                             a(href = "https://www.kaggle.com/", "Kaggle"), "."),
                           p("We will focus on the price of the vehicles, being the aim of this project on the one hand to ",
                             strong("explain"), " the predictors that affect the price of the cars, and on the other hand, to ",
                             strong("classify"), " these cars taking into account their prices. Moreover, we classify them in two
                             classes: the ones that are ",
                             strong("easy (Easy)"), " to sell, whose value will be lower than $20 mil, and the ",
                             strong("difficult (Diff)"), "whose value is greater than $20 mil"),
                           p("For a start, we will do a descriptive analysis to see the behavior of our variables and how to work with them,
                             as well as cleaning our data set. Then, we will apply different statistical tools on our data set to get the best
                             possible information about it in order to make the best conclusions."),
                           br()
                           )
                         )
                       )

dataPanel <- tabPanel(
  "Data description",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "vars", label = "Select the variable: ", choices = c("price"=1, "year"=2, "manufacturer"=3,
                                                                                 "condition"=4, "fuel"=5, "odometer"=6,
                                                                                 "title_status"=7, "transmission"=8, "drive"=9,
                                                                                 "lat"=10, "long"=11)),
      verbatimTextOutput("summary")
    ),
    mainPanel(
      h3(strong("Data frame description")),
      dataTableOutput("data")
    )
  )
)

plotPanel <- tabPanel(
  "Descriptive analysis",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("In the following panel, we are able to plot two graphics to see the relationship between our
        target variable, the ", code("price"), ", and the rest of predictors: a ", strong("histogram"),
        "for the numerical variables and a ", strong("barplot"), " for the categorical ones."),
      h4("Select the numerical variable and the number of bins to be plotted in the histogram"),
      selectInput("hist_var", label = "", choices = c("year", "odometer")),
      sliderInput("n_bins", label = NULL, min = 2, max = 30, value = 10),
      h4("Select the categorical variable to be plotted in the barplot"),
      selectInput("bar_var", label = "", choices = c("manufacturer", "condition", "fuel",
                                                      "title_status", "transmission", "drive"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

plotlyPanel <- tabPanel(
  "title",
  useShinyjs(),
  splitLayout(
    plotlyOutput("plotly1"),
    plotlyOutput("plotly2")
  )
)

# newPanel <- tabPanel(
#   "title",
#   useShinyjs(),
#   sidebarLayout(
#     sidebarPanel(
#       
#     ),
#     
#     mainPanel(
#       
#     )
#   )
# )




ui <- navbarPage("Roberto J. Alcaraz Molina",
                 theme = shinytheme("sandstone"),
                 introPanel,
                 dataPanel,
                 plotPanel,
                 plotlyPanel
)

server <- function(input, output){
  
  output$summary <- renderPrint({
    summary(na.omit(vehicles[, as.numeric(input$vars)]))
  })
  
  output$data <- renderDataTable({
    vehicles
  })
  
  output$plot <- renderPlot({
    p1 <- ggplot(data = vehicles, aes_string(x = input$hist_var)) +
      geom_histogram(bins = input$n_bins, fill = "orange", color = "black") +
      ggtitle("Histogram") +
      theme_bw()
    
    p2 <- ggplot(vehicles, aes_string(x = "price", fill = input$bar_var)) +
      geom_bar() +
      ggtitle("Barplot") +
      ylab(" ") +
      theme_bw()
    
    ggarrange(p1, p2, widths = c(1, 1.25))
  })
  
  
  output$plotly1 <- renderPlotly({
    usa_points = vehicles[(vehicles$long < -55 & vehicles$long > -130 & vehicles$lat > 25 & vehicles$lat < 50), c("state", "long", "lat")]
    usa_points = na.omit(usa_points)
    
    USA = map_data("usa")
    
    points = ggplot() +
      geom_polygon(data = USA, aes(x = long, y = lat, group = group), color = "black", fill = "#f9f9f9", size = 0.2) +
      geom_point(data = usa_points, aes(x = long, y = lat, color = state)) + theme_bw() +
      theme(legend.position = "none") + xlab(" ") + ylab(" ")
    
    points
  })
  
  output$plotly2 <- renderPlotly({
    usa_points <- vehicles[(vehicles$long < -55 & vehicles$long > -130 & vehicles$lat > 25 & vehicles$lat < 50), c("state", "long", "lat")]
    usa_points <- na.omit(usa_points)
    
    USA = map_data("usa")
    data = usa_points %>%
      group_by(state) %>%
      summarise(count = n(), mean_lat = mean(lat), mean_long = mean(long))
    
    quant = ggplot()+
      geom_polygon(data = USA, aes(x = long, y = lat, group = group), color = "black", fill = "#f9f9f9", size = 0.2) +
      geom_point(data = data, 
                 aes(x = mean_long, y = mean_lat, size = count, color = count),) +
      scale_size_continuous(range = c(1,15)) +
      scale_color_viridis() + theme_bw() + xlab(" ") + ylab(" ")
    #theme(legend.position = "none")
    
    quant
  })
  
  
}

shinyApp(ui = ui, server = server)






