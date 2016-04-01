library(tm)
library(RWeka)
library(slam)

parseCorpusIntoDf <- function(corpus, freq = 0, tokenizer = function(x){NGramTokenizer(x, Weka_control(min=3, max=3))}){
        
        
        tdm <- TermDocumentMatrix(corpus, control=list(tokenize=tokenizer))
        tdm.sum <- rollup(tdm, 2, FUN = sum)
        tdm.tf <- findFreqTerms(tdm.sum, lowfreq = freq)
        tdm.tf <- sort(rowSums(as.matrix(tdm.sum[tdm.tf,])), decreasing = T)
        tdm.tf <- data.frame(words = names(tdm.tf), frequency = tdm.tf)
        
        tdm.tf <- tdm.tf[!grepl("\\</s\\>\\s\\<s\\>",tdm.tf$words, perl=T),]
        
        tdm.tf <- tdm.tf[!grepl("(\\<s\\>)(?:\\s\\1){1,}",tdm.tf$words, perl=T),]
        tdm.tf <- tdm.tf[!grepl("(\\</s\\>)(?:\\s\\1){1,}",tdm.tf$words, perl=T),]
        
        tdm.tf
}