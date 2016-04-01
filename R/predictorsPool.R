

predictPhraseBi <- function(initialSentence, bi){
        sent <- initialSentence
        predict.sent <- initialSentence
        
        count <- 0
        
        
        while(TRUE && count < 10){
                
                count <- count + 1
                result <-  predictBigrams(predict.sent, bi)
                
                rlength <- length(result$names)
                
                if(rlength>0){
                        
                        m <- mean(result$probability)
                        r <- result[which(abs(result$probability-m)==min(abs(result$probability-m))),]$names
                        nextWord <- strsplit(as.character(r), " ")[[1]][2]
                        if(nextWord == "</s>") {
                                nextWord <- if(rlength>1) strsplit(as.character(result[sample(1:rlength, size=1),]$names), " ")[[1]][2] else ""
                                count = 51
                        }
                        
                        predict.sent <- paste(predict.sent, nextWord)
                }else{
                        count = 51
                }
        }
        
        predict.sent
}


predictPhrase <- function(initialSentence, bi){
        
        sent <- initialSentence
        predict.sent <- ""
        
        count <- 0
        
        while(sent != predict.sent && count < 50){
                
                count <- count + 1
                sent <- if(count == 1) sent else predict.sent
                predict.sent <- predict(sent, bi)
        }
        
        sent
}

predict <- function(history, tri){
        
        history.short <- strsplit(history, " ")[[1]]
        
        history.short <- paste(history.short[(length(history.short)-1):length(history.short)], collapse = " ")
        
        regex <- paste0(c("^", history.short, "\\s"), collapse = "")
        
        t  <- tri[grep(regex, tri$names),]
        
        nextWord <- strsplit(as.character(t[order(-t$frequency),][1,1]), " ")[[1]][3]
        
        if (!is.na(nextWord)){
                history <- paste(history, nextWord, sep=" ")
        }
        
        
        history
}

predictBigrams <- function(sentence, bi){
        history <- sentence
        
        history <- strsplit(history, " ")[[1]]
        
        history <- c("<s>", history)
        
        totalProb <- 1
        
        for(i in 2:length(history)){
                
                currentWord = history[i]
                prevWord = history[i -1]
                
                bigram <- paste(prevWord, currentWord, sep=" ")
                
                regex <- paste0("^", prevWord, "\\s", collapse = "")
                
                condProb = bi[bi$names == bigram,]$frequency / sum(bidf[grep(regex, bidf$names),]$frequency)
                
                if(length(condProb) == 0){
                        condProb = totalProb * 0.001
                }
                
                totalProb = totalProb * condProb
        }
        
        regex <- paste0(c("^", history[length(history)], "\\s"), collapse = "")
        
        candidateWords <- bi[grep(regex, bi$names), ]
        
        candidateWords$probability <- candidateWords$frequency * totalProb
        
        candidateWords[order(-candidateWords$probability),]
}

predictNGram <- function(sentence, pdf, qdf, tdf, bdf, udf){
        
        history <- gsub(",", "",sentence)
        
        history <- strsplit(history, " ")[[1]]
        
        hLen <- length(history)
        
        nGramSize <- if(hLen > 3) 5 else if(hLen > 2) 4 else if(hLen > 1) 3 else 2
        
        result <- c()
        
        while((is.null(dim(result)[1]) || dim(result)[1] == 0) && nGramSize > 0 ){
                
                df <- if(nGramSize == 5) pdf else if(nGramSize == 4) qdf else if(nGramSize == 3) tdf else if(nGramSize == 2) bdf else udf
                
                history.recent <- history[(hLen+2-nGramSize):hLen] 
                
                search <- paste(history.recent, collapse = " ")
                
                regex <- paste0("^", search, "\\s")
                
                result <- df[grep(regex, df$names, perl = T),]
                
                if(dim(result)[1] == 0){
                        nGramSize <- nGramSize - 1
                }
        }
        
        result
        
}