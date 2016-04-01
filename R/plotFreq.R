library(ggplot2)

plotFreq <- function(df){
        g <- ggplot(data = head(df, 10), aes(x = words, y = frequency))
        g <- g + geom_bar(stat="Identity", fill="gray", colour="black")
        g <- g + geom_text(aes(label=frequency), vjust=-0.1)
        g <- g + theme(axis.text.x = element_text(angle=45, hjust = 2))
        g
}