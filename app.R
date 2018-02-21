################################################################################
# app.R
#
# This is a Shiny web application showcasing Yet Another Predictive Text
# Model (YAP™).

library(shiny)

source("predictNext.R")

# Load training Ngrams
load("www/dts_pruned_8.rda")

################################################################################
# Define UI for application
ui <- fluidPage(
    
    theme="style.css",
    
    # Application title
    titlePanel(HTML("<h1>Yet Another Predictive Text Model (YAP&trade;)</h1>"),
               windowTitle="YAP TM"),
    
    # Text instructions
    HTML(paste0(
        "The YAP&trade; app uses a 5-gram language model to predict the next ",
        "word from user input, ranking the predictions using a method called ",
        "\"Stupid Backoff\", as described in ",
        "<a href=\"http://www.aclweb.org/anthology/D07-1090.pdf\">'Large ",
        "Language Models in Machine Translation'</a> by T. Brants et al, in ",
        "EMNLP/CoNLL 2007.<br><hr>"
    )),
    
    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            width=5,
            
            wellPanel(
                textInput(
                    "X",
                    "Enter text below:"
                )
            ),
            
            wellPanel(
                HTML("<b>Prediction:</b>"),
                
                htmlOutput("next_word")
            ),
            
            # Built with Shiny by RStudio
            h5("Built with",
               a(href="https://shiny.rstudio.com",
                 img(src="http://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                     height="30px")),
               "by",
               a(href="https://www.rstudio.com",
                 img(src="http://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                     height="30px")),
               "."
            )
        ),
            
        # Show best prediction and table of top five predictions
        mainPanel(
            width=7,
            
            tableOutput("predictions")
        )
    ),
    
    HTML("<hr>"),
    
    HTML(paste0(
        "YAP&trade; was implemented as the capstone project for the Johns ",
        "Hopkins University Coursera ",
        "<a href=\"https://www.coursera.org/specializations/jhu-data-science\">",
        "Data Science Specialization</a>.<p><br>",
        "The R code underlying this app can be found on ",
        "<a href=\"https://github.com/sweitzen/yap-tm\">GitHub</a>.",
        "<br>Visit the author on ",
        "<a href=\"https://www.linkedin.com/in/sweitzen/\">LinkedIn</a>."
    ))
    
    #tags$script(HTML("src=\"//platform.linkedin.com/in.js\" type=\"text/javascript\">")),
    #tags$script(HTML("type=\"IN/MemberProfile\" data-id=\"https://www.linkedin.com/in/sweitzen\" data-format=\"inline\" data-related=\"false\">"))
    
    #HTML(paste0(
    #    "<script src=\"//platform.linkedin.com/in.js\" type=\"text/javascript\"></script>",
    #    "<script type=\"IN/MemberProfile\" data-id=\"https://www.linkedin.com/in/sweitzen\" data-format=\"inline\" data-related=\"false\"></script>"
    #)),
    
    #tags$script(src="//platform.linkedin.com/in.js", type="text/javascript"),
    #tags$script(type="IN/MemberProfile", data-id="https://www.linkedin.com/in/sweitzen", data-format="inline", data-related="false")

    #<script src="//platform.linkedin.com/in.js" type="text/javascript"></script>
    #<script type="IN/MemberProfile" data-id="https://www.linkedin.com/in/sweitzen" data-format="inline" data-related="false"></script>
    
    #<script src="http://platform.linkedin.com/in.js" type="text/javascript"></script>
    #<script type="IN/MemberProfile" data-id="http://www.linkedin.com/in/sweitzen" data-format="hover" data-text="Scott Weitzenhoffer"></script>
)

################################################################################
# Define server logic
server <- function(input, output) {
    
    # Reactive expression to get predictions
    predictions <- reactive({
        req(input$X)
        
        out <- predictNext(input$X, dts)
        names(out) <- c("Score", "Prediction")
        
       return(out[, c("Prediction", "Score")])
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
        digits=4
    )
}

# Run the application 
shinyApp(ui = ui, server = server)