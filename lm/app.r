#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(textOutput("title")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # Horizontal line ----
            tags$hr(),
            # Button to plot linear model
            actionButton("go","Plot Linear Model")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           verbatimTextOutput("R2"),
           verbatimTextOutput("CO"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Setting all the reactive values at the start to null
    lmdata <- reactiveValues(model = NULL,
        rsq = NULL,
        coef = NULL,
        Sl = NULL
    )
    # Reading the uploaded CSV file as a data frame labeled DataInput
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    # Creating a Dynamic title based on the CSV file uploaded
    output$title <- renderText({
    if (is.null(input$file1)) {
        "Data from:"
    } else {
        paste("Data from:", input$file1$name)
    }
 })
    observeEvent(input$go,{update_lm()

    })
    # Creating the Linear model function and isolating the R^2, the y coefficient, and the Slope from the summary data.
    update_lm <- function(){
        req(dataInput())
        
        lmdata$model <- lm( y ~ x, data = dataInput())
        lmdata$rsq <- summary(lmdata$model)$r.squared
        lmdata$coef <- summary(lmdata$model)$coefficients[1]
        lmdata$Sl <- summary(lmdata$model)$coefficients[2]

    }
    # Creating the first plot of the UI: the distribution of data
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y,
            main = paste("Distribution Plot of", input$file1$name),
             xlab = colnames(dataInput())[1],
             ylab = colnames(dataInput())[2],
             col = "black",
             border = "white")
    })
    # Creating the Second plot of the UI: the linear model plot (I.E the distribution with the line from the linear model
    output$lmPlot <- renderPlot({
        req(lmdata$model)
        plot(dataInput()$x,dataInput()$y,
            main = paste("Linear Model Plot of", input$file1$name),
             xlab = colnames(dataInput())[1],
             ylab = colnames(dataInput())[2],
             col = "black",
             border = "white")
            abline(lmdata$model, col = 'red')
    # Adding the R^2 , Slope, and Y Coefficient to the linear model plot
         text(x = min(dataInput()$x), y = max(dataInput()$y),
         labels = paste("R-squared =", round(lmdata$rsq, 3)),
         adj = c(0, 1), pos = 4)
    
    text(x = min(dataInput()$x), y = max(dataInput()$y) * 0.95,
         labels = paste("Slope =", round(lmdata$Sl, 3)),
         adj = c(0, 1), pos = 4)
    
    text(x = min(dataInput()$x), y = max(dataInput()$y) * 0.9,
         labels = paste("Y Coefficient =", round(lmdata$coef, 3)),
         adj = c(0, 1), pos = 4)
        
    })
# Creating a seperate line of text for the R^2, slope, and y Coefficient
    output$R2 <- renderText({
        paste ("R^2 =", lmdata$rsq,"Slope :", lmdata$Sl)
        
        }) 
    
    output$CO<- renderText({
        paste ( "Y Coefficient :", lmdata$coef)
        
        })  
   
    
   # Rendering the data from the CSV
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
