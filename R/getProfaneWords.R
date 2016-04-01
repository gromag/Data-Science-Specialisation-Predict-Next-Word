getProfaneWords <- function(path){
        strsplit(readLines(path), ",")[[1]]
}