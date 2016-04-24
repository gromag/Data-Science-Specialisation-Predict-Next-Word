
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(markdown)

shinyUI(navbarPage(
        
        # Application title
        title = "Predicting next word with Natural Language Processing",
        
        tabPanel('App',
                 
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                         sidebarPanel(
                                 textInput("sentence",
                                           value="", label = "Enter a sentence:"),
                                 actionButton("fetchButton", "Predict Next Word"),
                                 
                                 h3("Further settings"),
                                 
                                 
                                 sliderInput("max",
                                             "Number of Words:",
                                             min = 1,  max = 10,  value = 10)
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                                 tabsetPanel(
                                         tabPanel("Words list", h3("All words sorted by relevance"), dataTableOutput("commonTable"))
#                                          tabPanel("Words cloud", plotOutput("plot")),
#                                          tabPanel("Statistics", br(), plotOutput("plot"))
#                                          
                                 )
                         )
                 )
        ),
        tabPanel('Usage', includeMarkdown("README.md"))
))
