
getTermDocumentMatrix <- function(corpus, ngrams){
        myDTM = TermDocumentMatrix(corpus,
                                   control = list(
                                           minWordLength = 1, 
                                           tokenize = function(x){ NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))}))
        
        myDTM
}