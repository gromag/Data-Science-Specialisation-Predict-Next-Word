

getLastWord <- function(sentence, n=1){
        f <- strsplit(sentence, " ")[[1]]
        l <- length(f)
        n <- if(n > l) l else if(n < 1) 0 else n - 1
        s <- f[(l-n):l]
        paste(s, collapse = " ")
}
