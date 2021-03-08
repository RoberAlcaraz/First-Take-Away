# Roberto Jesús Alcaraz Molina
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly,
               shinythemes, shinyjs, DT, leaflet, knitr, Stat2Data, 
               dplyr, patchwork, ggpubr, htmlwidgets, shinythemes)
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
                                       code("drive"), ": indicates the wheel drive of the vehicle.</li></ul>")
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

plotPanel <- tabPanel("Descriptive analysis")




ui <- navbarPage("Roberto J. Alcaraz Molina",
                 theme = shinytheme("sandstone"),
                 introPanel
)

server <- function(input, output){
  output$data <- renderDataTable({
    vehicles
  })
}

shinyApp(ui = ui, server = server)






