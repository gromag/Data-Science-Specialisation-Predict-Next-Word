

comparisonStats <- function(blogLines, newsLines, twitterLines ){
        
        a <- data.frame(Corpus = character(3), DocumentsCount = numeric(3), WordsCount = numeric(3),  stringsAsFactors = FALSE)
        
        a$DocumentsCount[1] <- length(blogLines)
        a$WordsCount[1] <- countWords(blogLines)
        a$Corpus[1] <- "Blog"
        
        a$DocumentsCount[2] <- length(newsLines)
        a$WordsCount[2] <- countWords(newsLines)
        a$Corpus[2] <- "News"
        
        a$DocumentsCount[3] <- length(twitterLines)
        a$WordsCount[3] <- countWords(twitterLines)
        a$Corpus[3] <- "Twitter"
        
        a
}