library(tm)
library(wordcloud)

#Read the feedback file
text <- unlist(read.delim("feedback.txt", header=F, sep=" "))
text <- text[text != ""]
text <- text[order(text)]

#Does some data cleaning
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)

#Creates the frequency matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

#Fixes "easy to follow" as a single sentence
n <- d$freq[d$word == "follow"]
d$freq[d$word == "easy"] <- d$freq[d$word == "easy"] - n 
d <- d[d$word != "follow", ]

d <- rbind(d, c("easy to follow", n))
d$freq <- as.numeric(d$freq)


set.seed(42)

png("../assets/feedback_wordcloud.png")
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.color=TRUE, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# I then crop the image with GIMP to remove the large whiteborder