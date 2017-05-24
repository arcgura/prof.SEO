install.packages("ggplot2")
install.packages("ggrepel")
install.packages("dplyr")
library(ggplot2)
library(ggrepel)
library(dplyr)
data<-read.table("/Users/jd5/Dropbox/R/data/example1.txt",skip =1, sep = "\t", quote = "", col.names =c("region","dx"))
dx<-as.data.frame(table(data$dx))
region<-as.data.frame(table(data$region))
names(dx) <- c("dx", "freq")
names(region) <- c("region", "freq")
dx_filter <- filter(dx_data, freq >= 10)




region %>% slice(1:50) %>%
ggplot + aes(x=1,y=1, size=freq, label = region) + geom_text_repel(segment.size = 0, force = 10) +
    scale_size(range=c(1,20), guide = FALSE) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x="",y="") +
    theme_classic()


install.packages("wordcloud")

library(wordcloud)
example1 <- read.table("/Users/jd5/Dropbox/R/data/example1.txt", header=T, sep="\t",  quote = "", strip.white = TRUE)
a <- example1$dx_ER01_c
b <- sort(table(a),decreasing=T)

wordcloud(words = a, freq=b, min.freq = 1,
    max.words=200, random.order=FALSE, rot.per=0.35, 
    colors=brewer.pal(8, "Dark2"))


