# Roberto Jesús Alcaraz Molina
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly,
               shinythemes, shinyjs, DT, leaflet, knitr, Stat2Data, 
               dplyr, patchwork, ggpubr, htmlwidgets, shinythemes)

introPanel <- tabPanel("Introduction")


ui <- navbarPage("Roberto J. Alcaraz Molina",
                 theme = shinytheme("sandstone"),
                 introPanel
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)






