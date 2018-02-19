################################################################################
# app.R
#
# This is a Shiny web application showcasing the predictNext predictive text
# model.

library(shiny)


source("predictNext.R")

# Load training Ngrams
load("../data/train/dts_pruned_8.rda")

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Predictive Text Model"),
    
    # Text instructions
    HTML(paste0(
        "This app is a predictive text model, using 1-, 2-, ..., 5-grams to ",
        "predict the next word, ranking the predictions using a method ",
        "called \"Stupid Backoff\", as described in ",
        "<a href=\"http://www.aclweb.org/anthology/D07-1090.pdf\">'Large ",
        "Language Models in Machine Translation'</a> by T. Brants et al, in ",
        "EMNLP/CoNLL 2007.<br><hr>"
    )),
    
    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            textInput(
                "X",
                "Enter text below:"
            )
        ),
            
        # Show best prediction and table of top five predictions
        mainPanel(
            HTML("<b>Prediction:</b>"),
            htmlOutput("next_word"),
            HTML("<hr>"),
            tableOutput("predictions")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Reactive expression to get predictions
    predictions <- reactive({
        out <- predictNext(input$X, dts)
        
        names(out) <- c("Score", "Prediction")
        
        return(out)
    })
    
    # Output the top prediction appended to the input
    output$next_word <- renderUI((
        HTML(paste0(
            input$X,
            " <i>",
            predictions()$Prediction[1], 
            "</i>"
        ))
        
    ))
    
    # Output the top five predictions with their SBO score
    output$predictions <- renderTable(
        {predictions()},
        striped=TRUE,
        digits=4
    )
}

# Run the application 
shinyApp(ui = ui, server = server)