countWords <- function(lines){
        countWords  <- function(l) {sapply(gregexpr("\\W+", l), length) + 1}
        
        wordsNo <- Reduce("+", lapply(lines, countWords))
        
        wordsNo
}