library(stringr)
library(plyr)
library(twitteR)
library(lattice)

setwd("~/Workspace/textAnalysisR/")
pos <- readLines("positive-words.txt")
neg <- readLines("negative-words.txt")

score.sentiment <- function(sentences, posWords, negWords, .progress="none"){
  
  scores <- laply(sentences, function(sentence, posWords, negWords){
    sentence <- gsub("[[:punct:]]", "", sentence)
    sentence <- gsub("[[:cntrl:]]", "", sentence)
    sentence <- gsub("\\d+", "", sentence)
    
    tryTolower <- function(x){
      y <- NA
      try_error <- tryCatch(tolower(x), error=function(e) e)
      
      if (!inherits(try_error, "error")){
        y <- tolower(x)
        }
      return(y)
    }
    
    sentence <- sapply(sentence, tryTolower)
    
    word.list <- str_split(sentence, "\\s+")
    words <- unlist(word.list)
    
    posMatch <- match(words, posWords)
    negMatch <- match(words, negWords)
    
    posMatch = !is.na(posMatch)
    negMatch = !is.na(negMatch)
    
    score <- sum(posMatch) - sum(negMatch)
    
    return(score)
    
  }, posWords, negWords, .progress=.progress )
  
  scores.df <- data.frame(text = sentences, scores = scores)
  return(scores.df)
  
}

usatweet <- searchTwitter("usa", n=900, lang="en")
chinatweet <- searchTwitter("china", n=900, lang="en")
russiatweet <- searchTwitter("russia", n=900, lang="en")
indiatweet <- searchTwitter("india", n=900, lang="en")

usa_text <- sapply(usatweet, function(x) x$getText())
china_text <- sapply(chinatweet, function(x) x$getText())
russia_text <- sapply(russiatweet, function(x) x$getText())
india_text <- sapply(indiatweet, function(x) x$getText())

nd <- c(length(usa_text), length(china_text), length(russia_text), length(india_text))
country <- c(usa_text, china_text, russia_text, india_text)
country <- iconv(country, to="utf-8", sub="byte")

scores <- score.sentiment(country, pos, neg, .progress = "text")
scores$country <- factor(rep(c("usa", "china", "russia", "india"), nd))
scores$very.pos <- as.numeric(scores$score>=2)
scores$very.neg <- as.numeric(scores$score<=-2)

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

global_score <- round(100 *numpos / (numpos+numneg))


boxplot(scores~country, data=scores)


histogram(data=scores, ~scores|country, main="Sentiment Analysis of 4 Countries", xlab="", sub="Sentiment Score")
