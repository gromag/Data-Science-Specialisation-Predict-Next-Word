library(tm)

getCorpus <- function(text){

        corpus <- Corpus(VectorSource(text))
        # corpus <- tm_map(corpus, removePunctuation)
        # corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, PlainTextDocument)
        
        corpus
}