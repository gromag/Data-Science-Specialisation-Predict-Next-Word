library(tm)
library(RWeka)
library(slam)
library(ggplot2)
library(tictoc)

#Loading files
sapply(list.files(pattern = "[.]R$", path="R", full.names = T), source)

samplePercent <- 0.001

blogText <- readLines("data/raw/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = T)

s.intervals = cut_number(1:length(blogText), n = 5)

trainingSet <- blogText[(1:length(blogText))[s.intervals] < 5]

testSet <- blogText[(1:length(blogText))[s.intervals] == 5]

#trainingSet <- sample(blogText, round(0.001 * length(blogText)) )

length(trainingSet)

writeLines(trainingSet, "data/samples/en_US.blogs.train08.txt")

writeLines(testSet, "data/samples/en_US.blogs.test02.txt")

trainingSet <- readLines("data/samples/en_US.blogs.train08.txt", encoding = "UTF-8", skipNul = T)

s <- sampleLines(trainingSet, seed=123)

s[4:10]

debug(ensemble.initPredictors)


tic()
pred1  <- ensemble.initPredictors(trainingSet, clusterSize = 10)
toc()

tic()
pred2  <- ensemble.initPredictors(trainingSet, clusterSize = 20)
toc()

tic()
pred3  <- ensemble.initPredictors(trainingSet, clusterSize = 30)
toc()

tic()
pred4  <- ensemble.initPredictors(trainingSet, clusterSize = 1, samplePercent = 1)
toc()

tic()
pred4.C  <- ensemble.initPredictors(trainingSet, clusterSize = 10, samplePercent = 0.01)
toc()

save(pred4.C, file="data/pred4C.01.RData")

tic()
pred5  <- ensemble.initPredictors(trainingSet,  clusterSize = 1,  samplePercent = 0.2)
toc()

tic()
pred6  <- ensemble.initPredictors(trainingSet,  clusterSize = 1, samplePercent = 0.3)
toc()

tic()
pred10.C  <- ensemble.initPredictors(trainingSet, clusterSize = 20, samplePercent = 0.05)
toc()

save(pred10.C, file="data/pred10C.1.RData")

pred <- pred4
pred <- pred4.C

p <- ensemble.predict("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", pred); p[1:10,]
p <- ensemble.predict("You're the reason why I smile everyday. Can you follow me please? It would mean the", pred); p[1:10,]
p <- ensemble.predict("Hey sunshine, can you follow me and make me the", pred); p[1:10,]
p <- ensemble.predict("Very early observations on the Bills game: Offense still struggling but the", pred); p[1:10,]
p <- ensemble.predict("Go on a romantic date at the", pred); p[1:10,]
p <- ensemble.predict("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", pred); p[1:10,]
p <- ensemble.predict("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", pred)
p <- ensemble.predict("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", pred); p[1:10,]
p <- ensemble.predict("Be grateful for the good times and keep the faith during the", pred); p[1:10,]
p <- ensemble.predict("If this isn't the cutest thing you've ever seen, then you must be", pred); p[1:10,]

print("End predictions")



rm(blogText)
gc()

sample.s  <- sampleLines(blogText, seed = 123)

sample.s[1]

corpus <- getCorpus(sample.s)

save(corpus, file="data/blog70PercCorpusSample.RData")

unigramTokeniser <- function(x){ NGramTokenizer(x, Weka_control(min=1, max =1))}
bigramTokeniser <- function(x) {NGramTokenizer(x, Weka_control(min=2, max=2))}
trigramTokeniser <- function(x){NGramTokenizer(x, Weka_control(min=3, max=3))}
quadrigramTokenise <- function(x){NGramTokenizer(x, Weka_control(min=4, max=4))}
pentagramTokenise <- function(x){NGramTokenizer(x, Weka_control(min=5, max=5))}

unidf  <- parseCorpusIntoDf(corpus, tokenizer = unigramTokeniser)
save(unidf, file="data/unidf.RData")


bidf <- parseCorpusIntoDf(corpus, tokenizer = bigramTokeniser)
save(bidf, file="data/bidf.RData")

tridf <-parseCorpusIntoDf(corpus, tokenizer = trigramTokeniser)
save(tridf, file="data/tridf.RData")


quadf <- parseCorpusIntoDf(corpus, tokenizer = quadrigramTokenise)
save(quadf, file="data/quadf.RData")


pentdf <- parseCorpusIntoDf(corpus, tokenizer = pentagramTokenise)
save(pentdf, file="data/pentdf.RData")

load("data/pentdf.RData")
load("data/quadf.RData")
load("data/tridf.RData")
load("data/bidf.RData")
load("data/unidf.RData")



plotFreq(unidf)
plotFreq(bidf)
plotFreq(tridf)
plotFreq(quadf)
plotFreq(pentdf)






