library(twitteR)
library(tm)
library(wordcloud)

key <- "SOXZUOxa4gnoxqRdzFrGVaycu"
  
secret<- "kea5IyOeafzBIxWjtITRnUvsqysOkOjDqCo0uqemLPE5cNB0hz"

setwd("~/Workspace/textAnalysisR/")

setup_twitter_oauth(consumer_key=key,
                                 consumer_secret=secret,
                                 #requestURL='https://api.twitter.com/oauth/request_token',
                                 access_token = '293529388-hi8m7j2rV29Xfx6enZGysRVX4uYpxRXTbXNl7160',
                                 access_secret ='CZSaBEchRX2z468REazAt0PgFwUfO8BIu4Hs25KtmTNLo')




udemytweet<-searchTwitteR('#Udemy', n=1000)

removeStopwords <- function(x){
  removeWords(x, stopwords())
}

udemylist <- sapply(udemytweet, FUN = function(x) x$getText())
udemycorpus <- Corpus(VectorSource(udemylist))
udemycorpus <- tm_map(udemycorpus, tolower)
udemycorpus <- tm_map(udemycorpus, removePunctuation)
udemycorpus <- tm_map(udemycorpus, removeStopwords)

udemycorpus2 <- tm_map(udemycorpus, PlainTextDocument)



wordcloud(udemycorpus2, min.freq = 4, scale = c(3.5,1), random.order = F, random.color = F, max.words = 45)



udemytdm<- TermDocumentMatrix(udemycorpus2)
udemytdm
findFreqTerms(udemytdm, lowfreq=11)
findAssocs(udemytdm, "android", 0.6)


udemy2tdm <- removeSparseTerms(udemytdm, 0.9)
udemy2tdmscale <- scale(udemy2tdm)
udemydist<-dist(udemy2tdmscale, method = "euclidean")
udemyfit<-hclust(udemydist)
plot(udemyfit)
cutree(udemyfit, k=6)
rect.hclust(udemyfit, k=6)
