# High frequency word Analysis & Making Word cloud Code

# Install these libraries before using  -> install.packages("--")
library(KoNLP)
library(memoise)
library(dplyr)
library(plyr)
library(stringr)

# Using Korean dictionary
buildDictionary(ext_dic = "woorimalsam")
useNIADic()

setwd("C:/R_wd")

txt <- readLines("file_name.txt", encoding = "UTF-8")

txt <- str_replace_all(txt, "\\W", " ")

nouns <- extractNoun(txt)
wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, c("Var1"="word", "Freq"="freq"))

# High frequency words Extraction
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20 

# Making graph
library(ggplot2)
order <- arrange(top_20, freq)$word               
ggplot(data = top_20, aes(x = word, y = freq)) +
  ylim(0, 60) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +              
  geom_text(aes(label = freq), hjust = -0.3)     

# Making word cloud
library(wordcloud2)
wordcloud2(data=df_word,fontFamily = '--your font--')
