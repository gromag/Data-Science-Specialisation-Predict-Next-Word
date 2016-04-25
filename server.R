

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RCurl)
source("R/capstone.R")
library(RSQLite)


#capstone.loadDataTablesRemote()

shinyServer(function(input, output, session) {
        
        
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.unigramDataS.RData"))
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.bigramDataS.RData"))
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.trigramDataS.RData"))
        
        # fetch text from webpage
        sentence <- reactive(input$sentence)
        
        # filtering based on widget settings
        filteredTerms <- reactive({
                # Making this reactive on fetchButton only
                input$fetchButton
                
                r <- capstone.predictNextWord(sentence(), unigramData, bigramData, trigramData)
                r <- head(r, n = input$max)
                r
        })
        
        
        # Output for tabular data
        output$commonTable <- renderDataTable({
                if (input$fetchButton != 0) {
                        t <- filteredTerms()
                        
                        isolate({
                                t[, .("Next word" = wordsLast, "Kneser-Ney Probability" = pKN, "MLE Probability" = pMLE)]
                        })
                }
        })
        
})
