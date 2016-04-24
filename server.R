

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RCurl)
source("R/capstone.R")

capstone.loadDataTables()

shinyServer(function(input, output, session) {

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
        

        # Outputting for the word cloud
        output$plot <- renderPlot({
                if (input$fetchButton != 0) {
                        t <- filteredTerms()
                        
                        if (length(t) > 1) {
                                wordcloud_rep(
                                        t$wordsLast, t, scale = c(4,0.5),
                                        max.words = input$max,
                                        colors = brewer.pal(8, "Spectral")
                                )
                        }
                        
                }
                
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
