#installing dev version of data.table

#install.packages("data.table", type = "source", repos = "https://Rdatatable.github.io/data.table")

library(data.table)
library(SnowballC)
library(quanteda)
library(tictoc)

# Loading files
sapply(list.files(pattern = "[.]R$", path="R", full.names = T), source)

text1 <- capstone.getText()
text <- capstone.cleanText(text1)

sentences <- unlist(tokenize(text, what = "sentence"))
sentences <- capstone.removeProfanity(sentences)
sentences <- capstone.removePunctuation(sentences)
sentences <- capstone.addSentenceDelimiters(sentences)
sentences <- tolower(sentences)

save(sentences, file="data/samples/sentences-blog-news-twitter.01Pc.RData")

#extracting frequencies
unigramData <- capstone.getUnigramFrequencies(sentences)
bigramData <- capstone.getBigramFrequencies(sentences)
trigramData <- capstone.getTrigramFrequencies(sentences)


#if words are not in the unigram, remove them 
#as this will mess up the KN probability calculation
bigramData <- bigramData[wordsLast %in% unigramData$words]
bigramData <- bigramData[wordsMin1 %in% unigramData$words]

#if words are not in the unigram and bigrame remote them
#as this will mess up the KN probability calculation
trigramData <- trigramData[(wordsLast %in% unigramData$words)]
trigramData <- trigramData[(wordsMin1 %in% bigramData$words)]

unigramData[, c("pMLE") := .(freq / sum(freq))]

capstone.addMLE(bigramData)
#run this before calculating continuation probability of the unigram
capstone.addLastWordFreq(bigramData)

capstone.addMLE(trigramData)
capstone.addLastWordFreq(trigramData)

delta <- 0.75

capstone.addUnigramPK(unigramData, bigramData)

capstone.addPKN(bigramData, unigramData, delta)

capstone.addPKN(trigramData, bigramData, delta)

setorder(unigramData, -pKN)
setorder(bigramData, -pKN)
setorder(trigramData, -pKN)

capstone.saveDataTables()

