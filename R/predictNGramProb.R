predictNGramProb <- function(sentence, pred = list(pdf = NULL, qdf = NULL, tdf = NULL, bdf = NULL, udf = NULL)){
        
        history <- gsub(",", "",sentence)
        
        history <- tolower(history)
        
        history <- paste("<s>", history)
        
        hLen <- length(strsplit(history, " ")[[1]])
        
        nGramSize <- if(hLen > 3 && !is.null(pred$pdf)) 5 else if(hLen > 2 && !is.null(pred$qdf)) 4 else if(hLen > 1 && !is.null(pred$tdf)) 3 else if(!is.null(pred$bdf)) 2 else 0
        
        probTbl  <- data.frame(words=character(0), frequency = numeric(0), probability = numeric(0), stringsAsFactors = F )
        
        
        while(nGramSize > 0 ){
                
                df <- if(nGramSize == 5) pred$pdf else if(nGramSize == 4) pred$qdf else if(nGramSize == 3) pred$tdf else if(nGramSize == 2) pred$bdf else pred$udf
                
                if(!is.null(df)){
                        
                        words <- getLastWord(history, n = (nGramSize - 1)) 
                        
                        regex <- paste0("^", words, "(?:\\s|$)")
                        
                        result <- if(nGramSize > 1) df[grep(regex, df$words, perl = T),] else df[order(-df$frequency),][1,]
                        
                        rLen <- dim(result)[1]
                        
                        if(rLen > 0){
                                result <- result[order(-result$frequency),]
                                
                                wNMin1Freq <- sum(result$frequency)
                                
                                pTblLen <- (dim(probTbl)[1])
                                
                                for(i in 1:rLen){
                                        
                                        word <- getLastWord(as.character(result[i,]$words))
                                        #word <- as.character(result[i,]$words)
                                        wNFreq <- result[i,]$frequency
                                        probTbl[pTblLen+i,] <- list(name=word, frequency = wNFreq, probability = ((wNFreq/wNMin1Freq)*(10^nGramSize)) )
                                }
                        }
                }
                
                if(nGramSize == 1){
                        
                }
                
                nGramSize <- nGramSize - 1
        }
        
        if(dim(probTbl)[1] > 0){
                
                jointProb <- aggregate(probTbl$probability ~ probTbl$words, data=probTbl, sum)
                
                names(jointProb) <- c("words", "probability")
                
                return(head(jointProb[order(-jointProb$probability),], n=100))
        }
        
        return(NULL)
        
}