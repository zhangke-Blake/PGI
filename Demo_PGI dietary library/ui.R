
# install.packages("shiny")
library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tabsetPanel( #为任意数量的tabPanels()创建容器
    tabPanel("PGI",
             # App title ----
             # titlePanel("Personalizd Glycemic Sensitivity Index (PGI)"), 
             # Sidebar layout with input and output definitions ----
             sidebarLayout( 
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 h3("Please insert GL of meals.", align="left"), 
                 sliderInput(inputId = "GL1", label = "GL of meal1", 5, min = 0, max=100),
                 sliderInput(inputId = "GL2", label = "GL of meal2", 15, min = 0, max=100),
                 sliderInput(inputId = "GL3", label = "GL of meal3", 20, min = 0, max=100), 
                 sliderInput(inputId = "GL4", label = "GL of meal4", 40, min = 0, max=100),
                 sliderInput(inputId = "GL5", label = "GL of meal5", 70, min = 0, max=100),
                  
                 br(),
                 h3("Please insert measured postprandial glucose-iAUC of meals.", align="left"),  
                 numericInput(inputId = "iAUC1", label = expression("Postprandial iAUC of meal1"), 0.1, min = 0, max=30), 
                 numericInput(inputId = "iAUC2", label = expression("Postprandial iAUC of meal2"), 1, min = 0, max=30), 
                 numericInput(inputId = "iAUC3", label = expression("Postprandial iAUC of meal3"), 3, min = 0, max=30),
                 numericInput(inputId = "iAUC4", label = expression("Postprandial iAUC of meal4"), 5, min = 0, max=30), 
                 numericInput(inputId = "iAUC5", label = expression("Postprandial iAUC of meal5"), 7, min = 0, max=30),
                 
                 br(),
                 h3("Please upload the PGI database.", align="left"), 
                 helpText("PGI database is utilized for ranking. \n(.csv will be accepted.)"),
                 fileInput("database", label = "", buttonLabel = "Browse...", multiple = TRUE, accept = c(".csv", ".tsv", ".txt")),
                 
                 ),
               
               # Main panel for displaying outputs ----
               mainPanel(  
                 fluidRow(
                   column(12,  align="center",
                     br(),
                     h3("Your personalizd glycemic sensitivity index (PGI)", align = "left"),
                     helpText("Type meal GLs and corresponding iAUCs, then you will get your own PGI.", align = "left"),
                     hr(), br(),
                     plotOutput(outputId = "PGI1", width = "60%"),
                     
                     br(),br(),br(),
                     h3("Your PGI ranks in the Chinese population as ", align = "left"),
                     helpText("Please upload the PGI_database.csv file, then you will know the ranking.", align = "left"),
                     hr(), br(),
                     plotOutput(outputId = "PGI2", width = "60%"), 
                     
                     br(),br(),br()
                     ) 
                   )
                 ) 
               )
             ),
    
    
    
    
    tabPanel("Library",
             # App title ----
             titlePanel("Personalizd dietary library & iUL-carb"),  
             
             sidebarLayout( 
               sidebarPanel( 
                 # numericInput(inputId = "PGI_val", label = "Your PGI is ", 0, min = 0, max=100),
                 
                 br(),
                 h3("Please upload the GI database.", align="left"), 
                 helpText("GI database is required for iUL-carb calculation. 
                          It is also accepted to enrich the GI database by replacing the file with your own .csv file. 
                          (.csv will be accepted.)"), 
                 fileInput("GI_database", label = "", buttonLabel = "Browse...", multiple = TRUE, accept = c(".csv", ".tsv", ".txt")),
                 
                 selectInput("mealName", 
                             label = "Choose a type of food",
                             choices = c("Refined grain", "Whole grain",
                                         "IF toast", "Pure glucose","IF dinner1","IF dinner2"),
                             selected = "Refined grain"),
                  
                 # textInput(inputId = "mealName", label = "Food name","refined grain")
                 ), 
               
               mainPanel( 
                 h4("Your PGI is ", align="left"), 
                 textOutput(outputId = "iUL_PGI"),
                 
                 br(), 
                 h4("The meal GI of selected food is ", align="left"), 
                 helpText("Please upload the GI_database.csv file.", align = "left"),
                 textOutput(outputId = "mealGI"),
                 
                 br(), 
                 h4("The individual upper limits for carbohydrate intake (iUL-carb) is ", align="left"), 
                 helpText("Please upload the GI_database.csv file, then you will know the iUL-carb.", align = "left"),
                 textOutput(outputId = "iUL")
                 )
               )
             )
    )
  )











