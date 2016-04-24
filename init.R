library(SnowballC)
#library(tm)
library(quanteda)
library(tictoc)
library(RWeka)
library(slam)



init <- function(){
        sapply(list.files(pattern = "[.]R$", path="R", full.names = T), source)
}

init()


t <- readLines("data/samples/artifact.txt", encoding = "UTF-8")
s <- segment(t, what = "other", delimiter = "(?<=\\</s\\>)", perl = TRUE)
unls <- unlist(s)
mydfm <- dfm(t, ngrams=3, verbose = FALSE, what = "fasterword", removePunct = FALSE, concatenator=" ")
t <-  tokenize(unls, what = "fastestword")
ng <- ngrams(t, n = 3, concatenator = " ")
grams <- unlist(ng)
t <- table(grams)


gram3 <- parseTextIntoFrequencyTable(text.clean, gramSize = 3)
gram2 <- parseTextIntoFrequencyTable(text.clean, gramSize = 2)
gram1 <- parseTextIntoFrequencyTable(text.clean, gramSize = 1)


