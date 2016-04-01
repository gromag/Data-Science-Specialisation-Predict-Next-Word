ensembleQ.initPredictors <- function(text, clusterSize = 2, samplePercent = 0.001){
        
       ens.list <- list()
        
        s  <- sampleLines(text, samplePercent = (samplePercent * clusterSize), seed = (clusterSize * 100))
        
        s <- cleanText(s)
        
        message(paste("sample size: ", length(s)))
        
        s.intervals = cut_number(1:length(s), n = clusterSize)
        
        for(c in 1:clusterSize){
                
                message(paste("Cluster: ", c))
                
                text <- s[(1:length(s))[s.intervals] == c]
                
                
                
                unidf  <- parseTextIntoFrequencyTable(text, gramSize = 1)
                
                message("unidf ...done")

                bidf <- parseTextIntoFrequencyTable(text, gramSize = 2)

                message("bidf ...done")
                
                tridf <-parseTextIntoFrequencyTable(text, gramSize = 3)

                message("tridf ...done")
                
                quadf <- parseTextIntoFrequencyTable(text, gramSize = 4)

                message("quadf ...done")
                
                pentdf <- parseTextIntoFrequencyTable(text, gramSize = 5)
                
                message("pentdf ...done")
                
                dfs = list(udf = unidf, bdf = bidf, tdf = tridf, qdf = quadf, pdf = pentdf)
                
                ens.list[[c]] <- dfs
        }
        
        ens.list
}

ensembleQ.predict <- function(sentence, ens.list){
        
        if(is.null(ens.list)){
                return(NULL)
        }
        
        probTbl  <- data.frame(words=character(0), probability = numeric(0), stringsAsFactors = F )
        
        for(i in 1:length(ens.list)){
                
                message(paste("Ensemble: ", i))
                
                p <- ens.list[[i]]
                
                result <- predictNGramProb(sentence, pred = p )
                
                message("Predicted")
                
                if(!is.null(result)){
                        probTbl <- merge(probTbl, result, all.x = T, all.y = T)        
                }
                
        }
        
        
        if(dim(probTbl)[1] > 0){
                
                jointProb <- aggregate(probTbl$probability ~ probTbl$words, data=probTbl, sum)
                
                names(jointProb) <- c("words", "probability")
                
                return(head(jointProb[order(-jointProb$probability),], n = 100))
        }
        
        return(NULL)
        
}

ensembleQ.constructPhrase <- function(sentence, ens.list){
        
        text <- sentence
        
        for(i in 1:30){ 
                prediction <- ensemble.predict(text, ens.list)
                if(!is.null(prediction)){
                        word <- getLastWord(prediction[1]$words)
                        
                        if(word == "</s>" || word == "<s>"){
                                break;
                        }
                        
                        text <- paste(text, word)
                        

                }
        }
        
        text
}