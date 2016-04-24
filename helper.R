library(tm)
library(wordcloud)
library(memoise)
library(XML)

# Using "memoise" to automatically cache the results
# refer to http://shiny.rstudio.com/gallery/word-cloud.html
getTermMatrix <- memoise(function(text) {
        
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, content_transformer(tolower))
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,
                          c(stopwords("SMART"), "and", "but", "the"))
        
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 1))
        
        m = as.matrix(myDTM)
        
        sort(rowSums(m), decreasing = TRUE)
})

stripMarkup <- function(text) {
        return(gsub("<.*?>", "", text))
}

# Removing an entire HTML node including its content, eg. <script> or <style>
stripNodeAndContent <- function(text, tag) {
        r <- paste0("<", tag, ".+?</", tag, ">")
        return(gsub(r, "", text))
}
stripSpace <- function(text){
        return(gsub("\\s+", " ", text))
}

stripComment <- function(text){
        return(gsub("/\\*.*?\\*/", " ", text))
}

cleanMarkup <- function(text){
        
        stripComment(
                stripSpace(
                        stripMarkup(
                                stripNodeAndContent(                
                                        stripNodeAndContent(
                                                stripNodeAndContent(text, "script"),
                                                "style"),
                                        "head")
                        )))
}

# extracting a HTML Doc of a URL
getText <- function(url){
        text <- getURL(url)
        
        cleanMarkup(text)
}

