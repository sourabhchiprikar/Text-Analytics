library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(tidyverse)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(RCurl)


setwd("C:\\Users\\soura\\Desktop\\R")
DataMarket<-read.csv("marketing1.csv")

str(DataMarket)

DataMarket$Review.details..text.[1]

corpus <- SimpleCorpus(VectorSource(DataMarket$Review.details..text.))

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, content_transformer(tolower))

# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus[[1]]$meta

corpus <- tm_map(corpus, stemDocument)

DTMM <- DocumentTermMatrix(corpus)
view(DTMM)

inspect(DTMM)


sums1 <- as.data.frame(colSums(as.matrix(DTMM)))
sums1 <- rownames_to_column(sums1) 
colnames(sums1) <- c("term", "count")
sums1 <- arrange(sums1, desc(count))
head <- sums1[1:100,]
wordcloud(words = head$term, freq = head$count, min.freq = 800,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



sent1 <- analyzeSentiment(DTMM, language = "english")
# were going to just select the Harvard-IV dictionary results ..  
sent1 <- sent1[,1:4]
#Organizing it as a dataframe
sent1 <- as.data.frame(sent1)

summary(sent$SentimentGI)

str(DataMarket)

final1 <- bind_cols(DataMarket, sent1)
# now lets get the top 5 
final1 %>% group_by(Hotel.Name) %>%
  summarize(sent1 = mean(SentimentGI)) %>%
  arrange(desc(sent1)) %>%
  head(n= 5)


final1 %>% group_by(Hotel.Name) %>%
  summarize(sent1 = mean(SentimentGI)) %>%
  arrange(sent1) %>%
  head(n= 5)
DataMarket$Review.details..text.<-as.character(DataMarket$Review.details..text.)
sent2 <- get_nrc_sentiment(DataMarket$Review.details..text.)
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")
ggplot(sent3, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment of Customers (O Hotel)") + theme(plot.title = element_text(hjust=0.5))

