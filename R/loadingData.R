#Loading the data:
#Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily need to load the entire 
#dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a smaller subset 
#of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. You can also loop over 
#each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks
#at a time. Reading pieces of the file at a time will require the use of a file connection in R. For example, the 
#following code could be used to read the first few lines of the English Twitter 
#dataset:con <- file("en_US.twitter.txt", "r") readLines(con, 1) ## Read the first line of text readLines(con, 1) 
# ## Read the next line of text readLines(con, 5) ## Read in the next 5 lines of text close(con) ## It's important to 
#close the connection when you are done