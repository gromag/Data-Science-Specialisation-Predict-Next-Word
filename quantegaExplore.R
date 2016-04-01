library(SnowballC)
library(quanteda)
library(tictoc)

text <- "<s> I'm a sentence and I'd better be formatted properly! </s> <s> I'm a second sentence </s> <s> So I'd better be correctly split since I'm a sentence and I've always wanted to be </s>"


#Loading files
sapply(list.files(pattern = "[.]R$", path="R", full.names = T), source)


trainingSet <- readLines("data/samples/en_US.blogs.train08.txt", encoding = "UTF-8", skipNul = T)

trainingSet.Clean <- cleanText(trainingSet)

trainingSet.3g <- parseTextIntoFrequencyTable(trainingSet.Clean, gramSize = 3L)
trainingSet.4g <- parseTextIntoFrequencyTable(trainingSet.Clean, gramSize = 4L)



#s <- wordstem(tokens, language = "porter")




summary(c)

tic()
qc <- corpus(text)
toc()


tic()
mydfm  <- dfm(qc, ngram=3, removeNumbers = F, stem=T, keepFeatures="\\</?s\\>")
toc()
mydfm


tic()
s <- segment(trainingSet, what = "other", delimiter = "(?<=\\</s\\>)", perl = TRUE)
toc()
tic()
unls <- unlist(s)
toc()
tic()
t <-  tokenize(unls, what = "fastestword")
toc()
tic()
ng <- ngrams(t, n = 3L, concatenator=" ")
toc()
tic()
trigrams <- unlist(ng)
toc()
tic()
t <- table(trigrams)
toc()
t[grep("<s> I'm a", names(t))]

str(trigrams)




dim(mydfm)

mydfm[1:10,1:10]

s <- names(colSums(mydfm))

s <- summary[order(-summary)]

barplot(head(s, n=20))


tic()
c <- Corpus(VectorSource(trainingSet))
toc()

summary(qc, showmeta = T)



