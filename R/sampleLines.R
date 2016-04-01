
sampleLines <- function(text, samplePercent = 0.001, seed=NULL){
        
        if(!is.null(seed)){
                set.seed(seed)
        }
        
        sample  <- sample(text, round(samplePercent * length(text)) )

}