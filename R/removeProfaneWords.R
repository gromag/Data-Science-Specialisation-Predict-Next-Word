#Profanity filtering - removing profanity and other words you do not want to predict.


removeProfaneWords <- function(lines, profaneWords){
        
        prof <- paste(profaneWords, collapse = "|")
        prof <- paste("\\W(", prof)
        prof <- paste(prof, ")\\W")
        
        lines <- gsub(prof, "", lines)
}