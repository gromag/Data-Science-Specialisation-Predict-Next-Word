cleanText <- function(text){
        
        text  <- enc2native(text)
        
        # removing double inverted opening and closing quotes 
        # Quote substitution idea borrowed from http://rpubs.com/mszczepaniak/163777
        # Handle right single quotes: 387317 instances in the blog file
        # http://stackoverflow.com/questions/2477452/%C3%A2%E2%82%AC%E2%84%A2-showing-on-page-instead-of
        text <- gsub("(\xE2\x80\x99)", "'", text, perl=TRUE)
        # Handle other chars that are like single quotes:
        text <- gsub("\\<U\\+0027\\>|\\<U\\+00B4\\>|\\<U\\+0092\\>|\\<U\\+0060\\>|\\<U\\+02BB\\>|\\<U\\+02BC\\>|\\<U\\+2018\\>|\\<U\\+2019\\>",
                         "'", text, perl=TRUE)
        text <- gsub("\\<U\\+[^\\>]+\\>", "", text, perl=T)
        
        # Replace words like doin' with doing
        text  <- gsub("(?<=\\w{2})in'", "ing", text, perl=T)
        # Remove single quotes when these are not used in context like I'd, we'd etc
        text  <- gsub("(?<=\\W)'+|(?=\\W)", "", text, perl=T)
        # Temp replace dash before non words substitution
        text  <- gsub("(\\w)-(\\w)", "\\1\001\\2", text, fixed=T)
        # Replace all non words characters with sentence terminators
        text  <- gsub("[^'\\s\\w\\<\\>]+", " </s> <s> ", text, perl=T)
        # Reinstate dash
        text  <- gsub("\001", "-", text, fixed=T)
        text  <- gsub("[^'\\s\\w]+\\s?(?=\\</s\\>)", "", text, perl=T)
        text  <- gsub("</s>\\s<s>\\s$|$", "</s>", text, perl=T)
        text  <- gsub("^", "<s> ", text, perl=T)
        text  <- gsub("\\s+", " ", text, perl=T)
        # replace multiple closing and opening with single closing and opening
        text  <- gsub("(\\</s\\>\\s\\<s\\>\\s){2,}", "</s> <s>", text, perl=T)
        
        # Replace multiple consecutive quotes with single
        text  <- gsub("'+", "'", text, perl=T)
        # Replace multiple consecutive dashes with single
        text  <- gsub("-+","-", text, perl=T )
        
        text  <- gsub("<s> </s>", "", text, perl=T)

        text 
}


