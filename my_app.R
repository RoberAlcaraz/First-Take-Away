# Roberto Jesús Alcaraz Molina
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly, shinythemes, shinyjs, DT, leaflet, knitr,
               Stat2Data, dplyr, patchwork, ggpubr, htmlwidgets, shinythemes, GGally, ggforce, maps, network, viridis,
               devtools, kaggler, caret, reticulate, mice)

# py_install("kaggle")
# kaggle <- import("kaggle")
# kaggle$api$authenticate()
# kaggle$api$dataset_download_files("austinreese/craigslist-carstrucks-data", "vehicles.csv", unzip = T)

# DATA PRE-PROCESSING
# ###############################################################################
# vehicles = read.csv("vehicles.csv/vehicles.csv")
# #Firstly, we can observe some variables that are not useful in our study like 
# # the ID, all that are related with URLs and the descriptions of each car. We 
# # are going to proceed to eliminate them.
# 
# vehicles = vehicles[, -c(1, 2, 3, 4, 5, 16, 20, 21, 22, 26)]
# # Now, we remove some outliers that appear in the price
# 
# q = quantile(vehicles$price, probs = c(0.125, 0.9995))
# vehicles = vehicles[vehicles$price > q[1] & vehicles$price < q[2],]
# 
# # Then, we will use a method called *Predictive Mean Matching* (PMM), which 
# # estimates a regression of $x_j$ taking as predictors the rest of the 
# # variables. Therefore, our variables ``year`` and ``odometer`` will look like:
# 
# X = mice(vehicles[, c("price", "year", "odometer")], 
#          method = "pmm", m = 5, printFlag = F)
# X = complete(X)
# vehicles[, c("price", "year", "odometer")] = X
# 
# # For the variable of the ``manufacturer``, since it has 44 different levels but
# # we know that it is hugely important, we will modify them as follows: we will 
# # consider 4 categories (> \$30k, between \$30k and \$20k, between \$20k and
# # \$10k and < \$10k), which will be named class **A**, **B**, **C** and **D**. 
# # For the cars that have no manufacturer, we will assign them one category or 
# # another depending on their price and for the rest, we will calculate the mean 
# # of each manufacturer and we will divide them in the same categories. 
# # Therefore, our variable ``manufacturer`` will look like:
# 
# v = which(vehicles$manufacturer == "")
# for (i in v){
#   vehicles$manufacturer[i] = if (vehicles$price[i] >= 30000){vehicles$manufacturer[i] = "A"}
#   else if (vehicles$price[i] < 30000 & vehicles$price[i] >= 20000){vehicles$manufacturer[i] = "B"}
#   else if (vehicles$price[i] < 20000 & vehicles$price[i] >= 10000){vehicles$manufacturer[i] = "C"}
#   else {vehicles$manufacturer[i] = "D"}
# }
# 
# a = which(vehicles$manufacturer == "A")  
# b = which(vehicles$manufacturer == "B")  
# c = which(vehicles$manufacturer == "C")  
# d = which(vehicles$manufacturer == "D")  
# 
# mean_manufacturers = vehicles[-c(a,b,c,d),] %>%
#   group_by(manufacturer) %>%
#   summarise_at(vars(price), funs(mean(., na.rm=T)))
# 
# A = which(mean_manufacturers$price >= 30000)
# B = which(mean_manufacturers$price < 30000 & mean_manufacturers$price >= 20000)
# C = which(mean_manufacturers$price < 20000 & mean_manufacturers$price >= 10000)
# D = which(mean_manufacturers$price < 10000)
# 
# 
# 
# for (i in 1:nrow(vehicles)){
#   if(vehicles$manufacturer[i] %in% mean_manufacturers$manufacturer[A])
#   {vehicles$manufacturer[i] = "A"}
#   else if(vehicles$manufacturer[i] %in% mean_manufacturers$manufacturer[B])
#   {vehicles$manufacturer[i] = "B"} 
#   else if(vehicles$manufacturer[i] %in% mean_manufacturers$manufacturer[C])
#   {vehicles$manufacturer[i] = "C"}
#   else if(vehicles$manufacturer[i] %in% mean_manufacturers$manufacturer[D])
#   {vehicles$manufacturer[i] = "D"}
# }
# 
# # Also, we edit more variables:
# 
# vehicles$manufacturer = as.factor(vehicles$manufacturer)      
# vehicles$condition = as.factor(vehicles$condition)
# vehicles$cylinders = as.factor(vehicles$cylinders)
# vehicles$fuel = as.factor(vehicles$fuel)
# vehicles$title_status = factor(vehicles$title_status, 
#                                levels = c("clean", "lien", "missing", "parts only", "rebuilt", "salvage"), 
#                                labels = c("ok", "ok", "notok", "notok", "ok", "notok"))   
# vehicles$transmission = as.factor(vehicles$transmission)
# vehicles$drive = as.factor(vehicles$drive)
# vehicles$size = as.factor(vehicles$size)  
# 
# # Then, for the categorical predictors we will compute the NA's as we have 
# # explained before for the numerical ones.
# 
# cat_predictors = vehicles[, c("condition", "cylinders", "fuel", "title_status", 
#                               "transmission", "drive", "size")]
# 
# cat_predictors$condition[which(cat_predictors$condition == "")] = NA
# cat_predictors$cylinders[which(cat_predictors$cylinders == "")] = NA
# cat_predictors$fuel[which(cat_predictors$fuel == "")] = NA
# cat_predictors$title_status[which(cat_predictors$title_status == "")] = NA
# cat_predictors$transmission[which(cat_predictors$transmission == "")] = NA
# cat_predictors$drive[which(cat_predictors$drive == "")] = NA
# cat_predictors$size[which(cat_predictors$size == "")] = NA
# 
# X = mice(cat_predictors, method = "pmm", m = 5, printFlag = F)
# X = complete(X)
# 
# vehicles[, c("condition", "cylinders", "fuel", "title_status", "transmission", "drive", "size")] = X
# 
# vehicles$condition = factor(vehicles$condition,
#                             levels = c("new", "like new", "excellent", "good", "fair", "salvage"))
# summary(vehicles$condition)
# vehicles$cylinders = factor(vehicles$cylinders,
#                             levels = c("12 cylinders", "10 cylinders", "8 cylinders", "6 cylinders", "5 cylinders", "4 cylinders", "3 cylinders", "other"))
# vehicles$fuel = factor(vehicles$fuel)
# vehicles$title_status = factor(vehicles$title_status)   
# vehicles$transmission = factor(vehicles$transmission)
# vehicles$drive = factor(vehicles$drive)
# vehicles$size = factor(vehicles$size,
#                        levels = c("full-size", "mid-size", "compact", "sub-compact"))
# 
# # Finally, we modify our target variable in order to do a classification problem.
# 
# vehicles$price = ifelse(vehicles$price <= 20000, "Easy", "Diff")
# 
# vehicles$price = as.factor(vehicles$price)
# vehicles$price = factor(vehicles$price, levels = c("Easy", "Diff"))
# 
# # Since there are some variables that are not useful to the analysis, we will
# # remove them.
# 
# vehicles = vehicles[, -c(4, 6, 12, 13)]
# 
# # And then, we have too many observations, therefore we will take a sample of
# # 8000 observations
# 
# sample = createDataPartition(vehicles$price, p = 0.02, list = F)
# 
# vehicles = vehicles[sample, ]
# saveRDS(vehicles, file = "vehicles_data.RDS")

#################################################
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

statsPanel <- tabPanel(
  "Statistical Models",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Statistical classification and Machine Learning")),
      p(""),
      br(),
      h4("Select the partition of the data that goes in the training set: "),
      sliderInput("data_part", label = NULL, min = 0.5, max = 0.9, step = 0.05, value = 0.7),
      br(),
      h4("Select the method of resampling: "),
      radioButtons("control", label = NULL, choices = c("5-fold cross validation (CV)" = "cv",
                                                        "5 times bootstrap" = "boot")),
      h4("Select the model: "),
      radioButtons("model", label = NULL, choices = c("Generalized Linear Model (glm)" = "glm",
                                                      "K Nearest Neighbors (knn)" = "knn",
                                                      "Random Forest (rf)" = "rf",
                                                      "Boosted Logistic Regression (LogitBoost)" = "LogitBoost",
                                                      "Neural network (nnet)" = "nnet")),
      actionButton("do", "Go!")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of the model", verbatimTextOutput("mod")),
        tabPanel("Confusion Matrix", verbatimTextOutput("confMat")),
        tabPanel("Variable Importance", plotOutput("varImp"))
      )
      )
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
                 plotlyPanel,
                 statsPanel
)

# SERVER FUNCTION
model.fitting <- function(partition, control, model){
  
  set.seed(0)
  training <- createDataPartition(vehicles$price, p = partition, list = F)
  vehiclesTrain <- vehicles[training, ]
  vehiclesTest <- vehicles[-training, ]
  
  ctrl <- trainControl(method = control,
                       number = 5, classProbs = T, verboseIter = F)
  
  form <- as.formula(price ~ year + odometer + manufacturer + condition + fuel + title_status + transmission + drive)
  fit.mod <- train(form, 
                   method = model,
                   data = vehiclesTrain, metric = "Accuracy",
                   trControl = ctrl,
                   preProcess = c("BoxCox", "center", "scale"))
  
  pred <- predict(fit.mod, vehiclesTest)
  CM <- confusionMatrix(pred, vehiclesTest$price)
  
  invisible(list(fit = fit.mod, confMatrix = CM))
}

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
  
  fit.mod <- eventReactive(input$do, {
    model.fitting(input$data_part, input$control, input$model)
  })
  
  
  output$mod <- renderPrint({
      fit.mod()$fit
  })
  
  output$confMat <- renderPrint(
    fit.mod()$confMatrix
  )
  
  output$varImp <- renderPlot({
    plot_imp <- varImp(fit.mod()$fit, scale = T)
    plot(plot_imp, scales = list(y = list(cex = .95)), top = 5)
  })
}

shinyApp(ui = ui, server = server)






