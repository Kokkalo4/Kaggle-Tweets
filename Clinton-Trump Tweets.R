#load dataset
tweets <- read.csv(".../input/tweets.csv", header = T)

#load some useful packages
library(dplyr)
library(plyr)
library(Hmisc)
library(ggplot2)
library(twitteR)
library(tm)
library(graph)
library(Rgraphviz)
library(wordcloud)
library(topicmodels)

#checking the data
str(tweets)
describe(tweets)
head(tweets)
tail(tweets)

#Getting a view on data with some plots
ggplot(tweets, aes(as.factor(handle))) + geom_bar(aes(fill = factor(handle))) + scale_fill_discrete(name = "Candidate" ,
       labels = c("Hillary Clinton" , "Donald Trump")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Candidate",y= "Frequency" , title = "Candidate Tweets")

ggplot(tweets, aes(as.factor(is_quote_status))) + geom_bar(aes(fill = factor(is_quote_status)))+ scale_fill_discrete(name = "Is Quote Status" ,
        labels = c("No" , "Yes")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Is Quote",y= "Frequency" , title = "Is Quote Status")

ggplot(tweets, aes(is_retweet)) + geom_bar(aes(fill = factor(is_retweet)))+ scale_fill_discrete(name = "Retweeted" ,
                                                                                                labels = c("No" , "Yes")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Is Retweeted?",y= "Frequency" , title = "Is Retweeted?")

ggplot(tweets, aes(lang)) + geom_bar(aes(fill = factor(lang))) + scale_fill_discrete(name = "Tweet Language") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Tweet Language",y= "Frequency" , title = "Tweet Language")


ggplot(tweets, aes(place_country)) + geom_bar(aes(fill = factor(place_country)))+ scale_fill_discrete(name = "Country" ,
           labels = c("N/A" , "United Kingdom" , "United States")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Country",y= "Number of Tweets" , title = "Country")

ggplot(tweets, aes(place_type)) + geom_bar(aes(fill = factor(place_type)))+ scale_fill_discrete(name = "Place Type" ,
 labels = c("N/A" , "Admin" , "City", "Country" , "Neighborhood" , "Poi")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Place",y= "Number of Tweets" , title = "Place of tweet Sender")


#Convert factor text from tweets to a dataframe
tweet <- tweets$text
tweets.df <- do.call("rbind" , lapply(tweet, as.data.frame))

#build a corpus
myCorpus <- Corpus(VectorSource(tweets.df))
#cleaning and converting corpus
myCorpus <- tm_map(myCorpus, content_transformer(tolower)) #make everything lowercase

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) #remove URLs

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) #remove anything but english letters and punctuation

myCorpus <- tm_map(myCorpus, removePunctuation) #remove Punctuation
myCorpus <- tm_map(myCorpus, removeNumbers) #remove numbers

myCorpus <- tm_map(myCorpus, removeWords, stopwords("en")) #remove stopwords
myCorpus <- tm_map(myCorpus, stripWhitespace) #remove whitespace


tdm <- TermDocumentMatrix(myCorpus)
tdm

#find frequent words
freq.terms <- findFreqTerms(tdm, lowfreq = 150)
freq.terms

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 150)
df <- data.frame(term = names(term.freq), freq = term.freq)

#word frequency plot
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#create a wordcloud
m <- as.matrix(tdm)

word.freq <- sort(rowSums(m), decreasing = T) #calculate the frequency of words and sort it by frequency
pal <- brewer.pal(9, "BuGn")[-(1:4)] #set colors

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 50,
          random.order = F, colors = pal)


