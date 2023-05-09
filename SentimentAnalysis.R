# Sentiment Analysis Code

# Install these libraries before using -> install.packages("-")
library(plyr)
library(stringr)

setwd("C:/R_wd") 

text <- readLines("file_name.txt", encoding = "UTF-8")

# Filtering the pos/neg words
# Download the pos/neg_word list file
positive <- readLines("pol_pos_word.txt", encoding = "UTF-8")
positive=positive[-1]

negative <- readLines("pol_neg_word.txt", encoding = "UTF-8")
negative=negative[-1]

# Sentiment Analysis
sentimental = function(sentences, positive, negative){
  scores = laply(sentences, function(sentence, positive, negative) {
    sentence = gsub('[[:punct:]]', '', sentence) 
    sentence = gsub('[[:cntrl:]]', '', sentence) 
    sentence = gsub('\\d+', '', sentence)        
    word.list = str_split(sentence, '\\s+')      
    words = unlist(word.list)       
    pos.matches = match(words, positive) 
    neg.matches = match(words, negative)
    pos.matches = !is.na(pos.matches)            
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)    
    return(score)
  }, positive, negative)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

result = sentimental(text, positive, negative)
result$color[result$score >=1] = "blue"       
result$color[result$score ==0] = "green"       
result$color[result$score < 0] = "red"         
table(result$color)

result$remark[result$score >=1] = "positive"
result$remark[result$score ==0] = "neutrality"
result$remark[result$score < 0] = "negative"

circle <-table(result$remark)
circle

sentiment = table(result$remark)
pie(sentiment, main="table_name",
    col=c("blue","red","green"), radius=1.0)

label <- paste(names(circle), "\n", circle)
label
pie(circle, labels=label, radius=1.0)

label <- paste(names(circle), "\n", circle/sum(circle)*100)
label
pie(circle, labels=label)

pct <- round(circle/sum(circle)*100,2)
label <- paste(names(circle), "\n", pct, "%")
pie(circle, labels=label, main="table_name",
    col=c("blue","red","green"), radius=1.0)

library(plotrix)

pie3D(circle, labels=label)
pie3D(circle, labels=label,  main="table_name",
      col=c("blue","red","green"), labelcex=1.5, explode=0.2)
pie3D
