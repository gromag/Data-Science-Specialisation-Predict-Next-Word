ensemble.initPredictors <- function(text, clusterSize = 2, samplePercent = 0.001){
        
        unigramTokeniser <- function(x){ NGramTokenizer(x, Weka_control(min=1, max =1, delimiters=" \\r\\n\\t.,;:\"()?!"))}
        bigramTokeniser <- function(x) {NGramTokenizer(x, Weka_control(min=2, max=2, delimiters=" \\r\\n\\t.,;:\"()?!"))}
        trigramTokeniser <- function(x){NGramTokenizer(x, Weka_control(min=3, max=3, delimiters=" \\r\\n\\t.,;:\"()?!"))}
        quadrigramTokenise <- function(x){NGramTokenizer(x, Weka_control(min=4, max=4, delimiters=" \\r\\n\\t.,;:\"()?!"))}
        pentagramTokenise <- function(x){NGramTokenizer(x, Weka_control(min=5, max=5, delimiters=" \\r\\n\\t.,;:\"()?!"))}
        
        ens.list <- list()
        
        s  <- sampleLines(text, samplePercent = (samplePercent * clusterSize), seed = (clusterSize * 100))
        
        s <- cleanText(s)
        
        message(paste("sample size: ", length(s)))
        
        s.intervals = cut_number(1:length(s), n = clusterSize)
        
        for(c in 1:clusterSize){
                
                message(paste("Cluster: ", c))
                
                sample.s <- s[(1:length(s))[s.intervals] == c]
                
                corpus <- getCorpus(sample.s)
                
                unidf  <- parseCorpusIntoDf(corpus, tokenizer = unigramTokeniser)
                
                message("unidf ...done")

                bidf <- parseCorpusIntoDf(corpus, tokenizer = bigramTokeniser)

                message("bidf ...done")
                
                tridf <-parseCorpusIntoDf(corpus, tokenizer = trigramTokeniser)

                message("tridf ...done")
                
                quadf <- parseCorpusIntoDf(corpus, tokenizer = quadrigramTokenise)

                message("quadf ...done")
                
                pentdf <- parseCorpusIntoDf(corpus, tokenizer = pentagramTokenise)
                
                message("pentdf ...done")
                
                dfs = list(udf = unidf, bdf = bidf, tdf = tridf, qdf = quadf, pdf = pentdf)
                
                ens.list[[c]] <- dfs
        }
        
        ens.list
}

ensemble.predict <- function(sentence, ens.list){
        
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

ensemble.constructPhrase <- function(sentence, ens.list){
        
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