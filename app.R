#install.packages(c("shiny","shinydashboard","regclass", "e1071","randomForest","xgboost","ggplot2"))

  library(shiny)
  library(shinydashboard)
  
  ui <- dashboardPage(
    dashboardHeader(title = "FEATURE SELECTION", titleWidth = 250),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("list-alt")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Random Forest", tabName = "RF", icon = icon("bar-chart-o")),
        menuItem("XGBOOST", tabName = "XGB", icon = icon("bar-chart-o")),
        menuItem("Cluster", tabName = "CL", icon = icon("bar-chart-o")),
        menuItem("PCA", tabName = "PCA", icon = icon("bar-chart-o"))
      )
    ),
  
    dashboardBody(
      tabItems(
        
        # First tab content
        tabItem(tabName = "about", 
                strong("How to select or reduce features:"),
                br(),
                br(),
                p("(1) Random Forest Method."),
                p("(2) XGBOOST Methond."),
                p("(3) Cluster Method."),
                p("(4) Principal Component Analysis.")
                
        ),
        # Second tab content
        tabItem(tabName = "data",
                dataTableOutput("Mydata")),
        
        # Third tab content
        tabItem(tabName = "RF", h2("Random Forest Method:"),
                
                numericInput("RFnum", 
                             h5("Select features by numbers:"), 
                             value = 1),
                
                p("Features:"),
                
                htmlOutput("RFnumvar"),
  
                plotOutput("RFfig")
  
        ),
        
        # Fourth tab content
        tabItem(tabName = "XGB", h2("XGBoost Method:"),
                
                numericInput("XGBnum", 
                             h5("Select features by numbers:"), 
                             value = 1),
                p("Features:"),
                
                htmlOutput("XGBnumvar"),
                
                plotOutput("XGBfig")
        ),
        
        # Fifth tab content
        tabItem(tabName = "CL", h2("Cluster Method:"),
                
                p("Elbow Curve:"),
                
                plotOutput("kfig1"),
                
                p("K-means clustering:"),
                
                plotOutput("kfig2")
                
        ),
        
        # Sixth tab content
        tabItem(tabName = "PCA", h2("Principal Component Analysis"),
                
                plotOutput("pcafig")
        )
  
        )
      )
    )
  
  
  
  server <- function(input, output) { 
    library(regclass)
    library(randomForest)
    library(xgboost)
    library(e1071)
    
    data(LAUNCH)
    
    LAUCH.data <- LAUNCH
    
    # Create Success
    LAUCH.data$Success <- factor(ifelse(LAUCH.data$Profit>4.5, "Yes","No"))
    
    # Delete Profit column
    LAUCH.data$Profit <- NULL
    
    # Remove all columns that contain a single value
    to.delete <- which(unlist(lapply(LAUCH.data, function(x)length(unique(x))))==1)
    LAUCH.data <- LAUCH.data[,-to.delete]
    
    
    # Random Forest
    set.seed(1234)
    RF <- randomForest(Success~.,data=LAUCH.data, ntree=500)
    
    # XGBoost
    Mtar <- ifelse(LAUCH.data$Success=='Yes', 1,0)
    LAUCH.data$Success <- NULL
    
    # Convert categorical variables into indicator variables with the folloing command
    MLAUNCH <- model.matrix(~.-1,data=LAUCH.data)
    
    set.seed(1234)
    bst <- xgboost(data = MLAUNCH, label = Mtar, max_depth = 100, 
                   eta = 1, nthread = 20, nrounds = 20, objective = "binary:logistic")
    xgbfit <- xgb.importance(model = bst)
    
    xgblist <- xgbfit$Feature
    
    library("ggplot2")
    xgbdata <- data.frame(Feature = xgbfit$Feature, Gain=xgbfit$Gain)
  
    # PCA
    pca <- princomp(LAUCH.data, cor=T, score=T)
    
    data <- data.matrix(LAUCH.data)
    
    # K-means cluster method:
    
    #Elbow Curve
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:15) wss[i]<-sum(kmeans(data,centers=i)$withinss)

    
    #K-means clustering
    cl <- kmeans(data, 3, nstart = 25)

    
    # output data
    output$Mydata <- renderDataTable({LAUNCH})
    
    output$RFnumvar <- renderText( paste(names(summarize_tree(RF)$importance[1:input$RFnum]) ))
    
    output$RFfig <- renderPlot(
      
      barplot(summarize_tree(RF)$importance)
    )
    
    output$XGBnumvar <- renderText(paste(xgblist[1:input$XGBnum]))
    
    output$XGBfig <- renderPlot(
      
      ggplot(data=xgbdata, aes(x=Feature, y=Gain)) + geom_bar(stat="identity") + labs(title = "Features' Gain Information")
           
    )
    
    output$kfig1 <- renderPlot(
      plot(1:15, wss, type = "b", xlab="Number of Clusters", ylab="Within Sum of Squares", main="Elbow Curve")
    )
      
    output$kfig2 <- renderPlot(
      plot(data, col=(cl$cluster+1), main="K-means result with 3 clusters", pch=1, cex=1, las=1)
    )
        
    output$pcafig <- renderPlot(
      plot(pca, type='l')
    )
    
  }
  shinyApp(ui, server)