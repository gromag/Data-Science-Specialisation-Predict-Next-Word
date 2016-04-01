library(tm)
library(RWeka)
library(slam)

parseTextIntoFrequencyTable <- function(text, freq = 0, gramSize = 3L){
        
        
        tic()
        s <- segment(text, what = "other", delimiter = "(?<=\\</s\\>)", perl = TRUE)
        toc()
        tic()
        unls <- unlist(s)
        toc()
        tic()
        t <-  tokenize(unls, what = "fastestword")
        toc()
        tic()
        ng <- ngrams(t, n = gramSize, concatenator=" ")
        toc()
        tic()
        grams <- unlist(ng)
        toc()
        tic()
        t <- table(grams)
        toc()
        
        t
}