library(data.table)
library(SnowballC)
library(quanteda)
#library(tictoc)

capstone.getText <- function(seed = 123){
        
        #         text <- "<s> I'm a sentence and I'd better be formatted properly </s> <s> I'm a second sentence </s> <s> So I'd better be correctly split since I'm a sentence and I've always wanted to be </s>"
        #         text <- "<s>I'm a sentence and I'd better be formatted properly</s><s>I'm a second sentence</s>"
        # text <- "Prof. O'Neil, I'm a sentence and I'd better be formatted properly. I'm a second sentence. So I'd better be correctly split since I'm a sentence and I've always wanted to be."
        
        message("reading blog")
        blogText <- readLines("data/raw/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = T)
        message("reading News")
        newsText <- readLines("data/raw/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = T)
        message("reading Twitter")
        twitterText <- readLines("data/raw/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = T)
        
        set.seed(seed)
        
        message("Sampling 1% of the three copies")
        text <- c(
                sample(blogText, round(0.05 * length(blogText))),
                sample(newsText, round(0.1 * length(newsText))),
                sample(twitterText, round(0.1 * length(twitterText)))
        )
        
        
        text
}
capstone.readCharConversionTable <- function(){
        r  <- read.table("data/replaceTable.txt", sep="\t", header =T, stringsAsFactors = F, quote="", encoding = "UTF-8")
        r  <- as.data.table(r)
        r[, lapply(.SD, paste0, collapse="|"), by = Repl]
}
capstone.cleanText <- function(text){
        
        r <- capstone.readCharConversionTable()
        
        rLength <- dim(r)[1]
        
        for(i in 1:rLength){
                
                message(paste("Replacing", r[i]$Symb, "with", r[i]$Repl))
                
                text <- gsub(r[i]$Symb, r[i]$Repl, text, perl=T)
        }
        
        message(paste("Replacing multiple quotes with single"))
        text  <- gsub("'+", "'", text, perl=T)
        
        # Replace words like doin' with doing
        message(paste("Replacing \\w{2}in' with doing"))
        text  <- gsub("(\\w{2})in'", "\\1ing", text, perl=T)
        text  <- gsub("(\\w{2})in\\s", "\\1ing ", text, perl=T)
        message(paste("Replacing isn t couldn t with isn't couldn't etc"))
        text  <- gsub("(\\w{2})n\\st(\\s|[.,;!?])", "\\1n't\\2", text, perl=T)
        
        message(paste("Removing single quotes when these are not used in context like I'd, we'd etc"))
        text <- gsub("(?<=\\W|^)'+|'+(?=\\W|$)", "", text, perl=T)
        message(paste("Removing dashes that are not word separators"))
        text <- gsub("(?<=\\W|^)-+|-+(?=\\W|$)", "", text, perl=T)
        
        text
}
capstone.removeProfanity <- function(sentences){
        
        r <- readLines("data/swearWords.csv", encoding = "UTF-8")
        
        sentences <- gsub(r[1], "<censored>", sentences, perl=T)
        
        sentences
}
capstone.addSentenceDelimiters <- function(sentences){
        
        sentences <- sapply(sentences, function(x){paste0("<s> ", x, " </s>")})
        names(sentences) <- NULL
        
        sentences
}
capstone.removePunctuation <- function(sentences){
        
        sentences <- sapply(sentences, function(x){ gsub("[^'\\-\\w]+", " ", x, perl=T)})
        sentences <- sapply(sentences, function(x){ gsub("\\s+", " ", x, perl=T)})
        sentences <- sapply(sentences, function(x){ gsub("^\\s|\\s$", "", x, perl=T)})
        names(sentences) <- NULL
        sentences
}
capstone.getUnknownWords <- function(ngrams, counts, minCounts){
        ngrams.new <-  sapply(seq_along(ngrams), function(i){ if(counts[i] >= minCounts) { ngrams[i]}else{"<UNK>"}})
        ngrams.new
}
capstone.getLastNWords <- function(sentence, n=1){
        f <- strsplit(sentence, " ")[[1]]
        l <- length(f)
        n <- if(n > l) l else if(n < 1) 0 else n - 1
        s <- f[(l-n):l]
        paste(s, collapse = " ")
}
capstone.addMLE <- function(ngram){
        
        ngram[, c("pMLE") := list(freq /sum(freq)), by = .(wordsMin1)]
        
}
capstone.addLastWordFreq <- function(ngram){
        
        ngram[, c("wordsLastCount") := list(sum(freq)), by = .(wordsLast)]
}
capstone.LaplaceProb <- function(ngrams, counts){}
capstone.getContextCount <- function(higherOrderLastWords, word){
        
        contexts <- which(higherOrderLastWords == word)
        length(contexts)
}
capstone.addUnigramPK <- function(unigramData, bigramData){
        
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
        
        #setkey will reorder in same order as N1Count
        setkey(unigramData, words)
        unigramData[, c("pKN") := N1Cont / sum(N1Cont),]
}
capstone.addPKN <- function(higherOrderNgram, lowerOrderNgram, delta){
        
        higherOrderNgram[, c("contextSum", "continuationCount") :=  list(sum(freq), length(freq)), by = .(wordsMin1)]
        message("contextSum and continuationCount calculated")
        
        higherOrderNgram[, c("lambda") :=  ((delta/contextSum) * continuationCount),]
        message("lambda calculated")
        
        h <- higherOrderNgram[, .(wordsLowerNGram)]
        l <- lowerOrderNgram[, .(words, pKN)]
        
        setkey(h, wordsLowerNGram)
        setkey(l, words)
        
        hl <- l[h]
        
        setkey(hl, words)
        
        setkey(higherOrderNgram, wordsLowerNGram)
        
        higherOrderNgram[, c("pContKN") := hl$pKN]
        
        message("pContKN added")
        
        higherOrderNgram[, c("pKN") := .((sapply(freq, function(f){max(f - delta, 0)}) / contextSum) + lambda * pContKN)]
        
        message("pKN calculated")
        
}
capstone.benchMarkPredictNextWord <- function(sentence, u, b, t){
        
        capstone.predictNextWord(sentence, u, b, t)[1:3]$wordsLast
}
capstone.predictNextWord <- function(sentence, u,b,t){
        suppressMessages(
                
                sentence <- tolower(
                        #capstone.removePunctuation(
                        #       capstone.removeProfanity(
                        #capstone.cleanText(sentence)
                        sentence
                        #      )
                        #)
                )
        )
        sentence <- paste("<s>", sentence)
        
        hLen <- length(strsplit(sentence, " ")[[1]])
        
        nGramSize <- if(hLen > 1) 3 else if(hLen > 0) 2 else 1
        
        
        if(nGramSize == 3){
                
                hist <- capstone.getLastNWords(sentence, n = (nGramSize - 1))
                
                r <- t[wordsMin1 == hist]
                
                if(dim(r)[1] > 0){
                        
                        return(r)
                }
                
                nGramSize = 2
        }
        
        
        if(nGramSize == 2){
                
                hist <- capstone.getLastNWords(sentence, n = (nGramSize - 1)) 
                
                r <- b[wordsMin1 == hist]
                
                if(dim(r)[1] > 0){
                        
                        return(r)
                }
                
                nGramSize = 1
        }
        
        if(nGramSize == 1){
                
                r <- u[1:100]
                
                return(r)
        }
        
}
capstone.calculateProbability <- function(wi, hist, d, uni, bi, bifw){
        #         words <- paste(hist, w, " ")
        #         
        #         biFreq <- bi[which(bi$word == words),]$freq
        #         biContextSum <- sum(bi[grep(paste0("^", hist, "\\s"), bi$word),]$freq) 
        #         biContinuationCount <- length(which(bifw == hist))
        #         lamb <- (d / biContextSum) * biContinuationCount
        #         pContWi <- t.PKN1[which(names(t.PKN1) == w)] 
        #         
        #         (max(biFreq - d, 0) / biContextSum) + lamb * pContWi
}
capstone.getUnigramFrequencies <- function(sentences){
        
        unigramDfm  <- dfm(sentences, ngram=1, what="fasterword", removePunct=F, removeNumbers = F, stem=F, concatenator=" ")
        
        unigramCounts <- colSums(unigramDfm)
        unigramCounts <- unigramCounts[order(-unigramCounts)]
        
        unigramData <- data.table(
                words = names(unigramCounts), 
                wordsLast = names(unigramCounts),
                freq = unigramCounts, pMLE = 0, pKN = 0)
        
        setkey(unigramData, words)
        
        setorder(unigramData, -freq)
        
        unigramData
}
capstone.getBigramFrequencies <- function(sentences){
        
        bigramDfm <- dfm(sentences, ngram=2, what="fasterword", removePunct=F, removeNumbers = F, stem=F, concatenator=" ")
        
        bigramCounts <- colSums(bigramDfm)
        bigramCounts <- bigramCounts[order(-bigramCounts)]
        
        bigramData <- data.table(
                words = names(bigramCounts), 
                freq  = bigramCounts, 
                wordsMin1 = gsub("\\s+[^\\s]+$", "", names(bigramCounts), perl=T), 
                wordsLast = gsub("^(?:[^\\s]+\\s+)", "", names(bigramCounts), perl=T), 
                wordsLowerNGram = gsub("^(?:[^\\s]+\\s+)", "", names(bigramCounts), perl=T), 
                pMLE = 0, pGT = 0, pKN = 0, pAdd1 = 0 )
        
        setkey(bigramData, words, wordsMin1, wordsLast)
        
        setorder(bigramData, -freq)
        
        bigramData
}
capstone.getTrigramFrequencies <- function(sentences){
        
        trigramDfm  <- dfm(sentences, ngram=3, what="fasterword", removePunct=F, removeNumbers = F, stem=F, concatenator=" ")
        
        trigramCounts <- colSums(trigramDfm)
        trigramCounts <- trigramCounts[order(-trigramCounts)]
        
        trigramData <- data.table(
                words = names(trigramCounts), 
                freq  = trigramCounts,
                wordsMin1 = gsub("\\s+[^\\s]+$", "", names(trigramCounts), perl=T), 
                wordsLast = gsub("^(?:[^\\s]+\\s+){2}", "", names(trigramCounts), perl=T), 
                wordsLowerNGram = gsub("^(?:[^\\s]+\\s+)", "", names(trigramCounts), perl=T), 
                pMLE = 0, pGT = 0, pKN = 0, pAdd1 = 0 )
        
        setkey(trigramData, words, wordsMin1, wordsLast)
        setorder(trigramData, -freq)
        
        trigramData
}

capstone.saveDataTables <- function(){
        save(unigramData, file ="data/quanteda.unigramDataS.RData")
        save(bigramData, file ="data/quanteda.bigramDataS.RData")
        save(trigramData, file ="data/quanteda.trigramDataS.RData")
}

capstone.loadDataTablesRemote <- function(){
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.unigramDataS.RData"))
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.bigramDataS.RData"))
        load(url("http://s3.amazonaws.com/giusepperomagnuolo.datascience.capstone/quanteda.trigramDataS.RData"))
        
        setorder(unigramData, -pKN)
        setorder(bigramData, -pKN)
        setorder(trigramData, -pKN)
        
        unigramData <<- unigramData
        bigramData <<- bigramData
        trigramData <<- trigramData}

capstone.loadDataTables <- function(){
        load(file="data/quanteda.unigramData.RData")
        load(file="data/quanteda.bigramData.RData")
        load(file="data/quanteda.trigramData.RData")
        
        setorder(unigramData, -pKN)
        setorder(bigramData, -pKN)
        setorder(trigramData, -pKN)
        
        unigramData <<- unigramData
        bigramData <<- bigramData
        trigramData <<- trigramData
}

capstone.publishShiny <- function(all=F){
        
        if(!dir.exists("_publish")){
                message("Creating _publish dir")
                dir.create("_publish")
                message("Creating data dir")
                dir.create("_publish/data")
                message("Creating R dir")
                dir.create("_Publish/R")
        }
        
        copyF <- function(files, path){
                for(i in 1:length(files)){
                        message(paste("Creating", files[i]))
                        file.copy(files[i], paste0(path, files[i]), overwrite = T)
                }
        }
        
        rootFiles <- c("server.R", "ui.R", "DESCRIPTION", "README.md", "LICENCE", "Capstone Project.Rproj")
        copyF(rootFiles, "_publish/")
        
        copyF(c("R/capstone.R"), "_publish/")
        
        if(all){
                dataFiles <- list.files("data", pattern = "*.(RData|txt|csv)",full.names = T)
                copyF(dataFiles, "_publish/")
        }
        
        
        copyF(c("data/nlp.db"), "_publish/")
        
        
}
