capstone.download <- function(){
        
        #Variables declaration
        
        datasetUrl              <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        dataFolder              <- "./data"
        datasetArchive          <- paste(dataFolder, "/dataset.zip", sep="")
        rawDataFolder           <- paste(dataFolder, "/raw", sep="")
        
        message("Downloading file from remote location")
        download.file(datasetUrl, datasetArchive)
        message("Unarchiving file")
        unzip(zipfile = datasetArchive, exdir = rawDataFolder)
        
}

capstone.quiz1.lineLength <- function(){
        
        processFile = function(filepath) {
                con = file(filepath, "r")
                size = 0
                count = 0
                while ( TRUE ) {
                        
                        count = count + 1
                        line = readLines(con, n = 1)
                        linesNo = length(line)
                        lineLength = nchar(line)

                        if (linesNo  == 0 ) {
                                break
                        }
                        
                        if(lineLength > size){
                                size = lineLength
                        }
                        
                }
                
                close(con)
                
                size
        }
        
        
        twitter <- processFile("en_US/en_US.twitter.txt")
        blogs <- processFile("en_US/en_US.blogs.txt")
        news <- processFile("en_US/en_US.news.txt")
        
        print(paste("Twitter size", twitter, sep=" "))
        print(paste("Blogs size", blogs, sep=" "))
        print(paste("News size", news, sep=" "))
}

capstone.quiz1.loveHateRatio <- function(){
        processFile = function(filepath) {
                con = file(filepath, "r")
                count = 0
                hateCount = 0
                loveCount = 0
                while ( TRUE ) {
                        
                        count = count + 1
                        lines = readLines(con, n = 100, skipNul = TRUE)
                        linesNo = length(lines)
                        
                        if (linesNo  == 0 ) {
                                break
                        }
                        
                        loveCount = loveCount + sum(grepl("love", lines))
                        hateCount = hateCount + sum(grepl("hate", lines))
                        
                        
                }
                
                close(con)
                
                loveCount/hateCount
        }
        
        
        processFile("en_US/en_US.twitter.txt")
}


capstone.quiz1.biostats <- function(str){
        processFile = function(filepath) {
                con = file(filepath, "r")
                count = 0
                bios = c()
                while ( TRUE) {
                        
                        count = count + 1
                        lines = readLines(con, n = 100, skipNul = TRUE)
                        linesNo = length(lines)
                        
                        if (linesNo  == 0 ) {
                                break
                        }
                        
                        bios = c(bios, lines[grep(str, lines)])
                        
                        
                }
                
                close(con)
                
                bios
                
        }
        
        processFile("en_US/en_US.twitter.txt")
        
        processFile("en_US/en_US.twitter.txt")
}