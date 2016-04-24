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

unigramData <- capstone.getUnigramFrequencies(sentences)

wordsCount  <- dim(unigramData)[1]

bigramData <- capstone.getBigramFrequencies(sentences)

bigramData <- bigramData[wordsLast %in% unigramData$words]
bigramData <- bigramData[wordsMin1 %in% unigramData$words]


bigramData[, c("wordsMin1Count", "pMLE") := list(sum(freq), freq /sum(freq)), by = .(wordsMin1)]
bigramData[, c("wordsLastCount") := list(sum(freq)), by = .(wordsLast)]

# wordsMin1Count  <- bigramData[, .(freq = sum(freq)), by = .(wordsMin1) ]
# setkey(wordsMin1Count, wordsMin1)


#bigramData$addOneSmoothProb <- (bigramData$count + 1) / (wordsCount + ngramMin1Count[J(bigramData$ngramMin1), count])

trigramData <- capstone.getTrigramFrequencies(sentences)

trigramData <- trigramData[(wordsLast %in% unigramData$words)]
trigramData <- trigramData[(wordsMin1 %in% bigramData$words)]



trigramData[, c("wordsMin1Count", "pMLE") := list(sum(freq), freq /sum(freq)), by = .(wordsMin1)]

# wordsMin1Count  <- trigramData[, .(freq = sum(freq)), by = .(wordsMin1) ]
# setkey(wordsMin1Count, wordsMin1)



# trigramData$pMLE <- trigramData$freq / wordsMin1Count[J(trigramData$wordsMin1), freq]
#trigramData$addOneSmoothProb <- (trigramData$count + 1) / (wordsCount + ngramMin2ToMin1Count[J(trigramData$ngramMin2ToMin1), count])

#Calculating the continuation count
u  <- unigramData[, .(words)]
setkey(u, words)
b  <- bigramData[, .(wordsLast, wordsLastCount)]
setkey(b, wordsLast)
bGrouped <- unique(b)
#left outer join u (left)
ub <- bGrouped[J(u)]
ub[is.na(wordsLastCount), c("wordsLastCount") := 0]





N1Cont <- ub$wordsLastCount

# N1Cont <- sapply(unigramData$words, function(x){dim(bigramData[wordsLast == x,])[1]})

#setkey will reorder in same order as N1Count
setkey(unigramData, words)
unigramData$pKN <- N1Cont / sum(N1Cont)
unigramData$pMLE <- unigramData$freq / sum(unigramData$freq)

setorder(unigramData, -pKN)

# bigramData[, c("contextSum", "continuationCount") :=  list(sum(freq), length(freq)), by = wordsMin1]
# 
# bigramData[, c("lambda") :=  (delta/contextSum * continuationCount),]
# 
# b <- bigramData[, .(wordsLast)]
# u <- unigramData[, .(words, pKN)]
# 
# setkey(b, wordsLast)
# setkey(u, words)
# 
# ub <- u[b]
# 
# setkey(ub, words)
# 
# setkey(bigramData, wordsLast)
# 
# bigramData$pContKN <- ub$pKN

# PContWi <- sapply(bigramData$wordsLast, function(lw){ unigramData[words ==lw, pKN] })
# names(PContWi) <- bigramData$wordsLast

# bigramData[, c("pKN") := .((sapply(freq, function(f){max(f - delta, 0)}) / contextSum) + lambda + pContKN)]

# bigramData$pKN <- (sapply(bigramData$freq, function(b){ max( b - delta, 0)}) / bigramContextSum) + lambda2 * PContWi

delta <- 0.75

bigramData$pKN <- capstone.calculatePKN(bigramData, unigramData, delta)

setorder(bigramData, -pKN)

trigramData$pKN <- capstone.calculatePKN(trigramData, bigramData, delta)

setorder(trigramData, -pKN)

head(trigramData)

save(unigramData, file ="data/quanteda.unigramData.RData")
save(bigramData, file ="data/quanteda.bigramData.RData")
save(trigramData, file ="data/quanteda.trigramData.RData")


