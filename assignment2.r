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

bayertweet <- searchTwitter("Bayer", n=900, lang="en")
pfizertweet <- searchTwitter("Pfizer", n=900, lang="en")
rochetweet <- searchTwitter("Roche", n=900, lang="en")
novartistweet <- searchTwitter("Novartis", n=900, lang="en")

bayer_text <- sapply(bayertweet, function(x) x$getText())
pfizer_text <- sapply(pfizertweet, function(x) x$getText())
roche_text <- sapply(rochetweet, function(x) x$getText())
novartis_text <- sapply(novartistweet, function(x) x$getText())

nd <- c(length(bayer_text), length(pfizer_text), length(roche_text), length(novartis_text))
companies <- c(bayer_text, pfizer_text, roche_text, novartis_text)
companies <- iconv(companies, to="utf-8", sub="byte")

scores <- score.sentiment(companies, pos, neg, .progress = "text")
scores$company <- factor(rep(c("Bayer", "Pfizer", "Roche", "Novartis"), nd))
scores$very.pos <- as.numeric(scores$score>=2)
scores$very.neg <- as.numeric(scores$score<=-2)

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

global_score <- round(100 *numpos / (numpos+numneg))


boxplot(scores~company, data=scores)


histogram(data=scores, ~scores|company, main="Sentiment Analysis of 4 Companies", xlab="", sub="Sentiment Score")
