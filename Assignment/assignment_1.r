library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)

#Sys.setlocale("LC_ALL", "C")

setwd("~/Workspace/textAnalysisR/")



twits<- searchTwitter("#starcraft2", n=2000)
twitlist <- sapply(twits, FUN = function(x) x$getText())

textcorpus <- Corpus(VectorSource(twitlist))

textcorpus <- tm_map(textcorpus,
                    content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                    mc.cores=1
)

textcorpus<- tm_map(textcorpus, content_transformer(tolower), mc.cores=1)
textcorpus<- tm_map(textcorpus, removeNumbers, mc.cores=1)
textcorpus<- tm_map(textcorpus, removePunctuation, mc.cores=1)
textcorpus<- tm_map(textcorpus, function(x) removeWords(x, stopwords()), mc.cores=1)

textcorpus2 <- tm_map(textcorpus, PlainTextDocument)

mypalette<-brewer.pal(5,"Dark2")

wordcloud(textcorpus2, min.freq = 3, scale = c(4,1), rot.per = 0.5,
          random.order = F, random.color = T, max.words = 45, colors = mypalette)


starctdm<- TermDocumentMatrix(textcorpus2)
starctdm
findFreqTerms(starctdm, lowfreq=40)
findAssocs(starctdm, "terran", 0.15)


starctdm2 <- removeSparseTerms(starctdm, 0.98)
starctdm2scale <- scale(starctdm2)
starcdist<-dist(starctdm2scale, method = "euclidean")
starcfit<-hclust(starcdist)
plot(starcfit)
cutree(starcfit, k=4)
rect.hclust(starcfit, k=4)


