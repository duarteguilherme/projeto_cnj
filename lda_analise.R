library(readxl)

dados_excel <- readRDS('dados_excel.rds')

library(tm)
library(topicmodels)
library(tidyverse)


dados_excel <- dados_excel %>%
  mutate(ementa = gsub("\r", "", ementa)) %>%
  mutate(ementa = gsub("\t", "", ementa)) %>%
  mutate(ementa = gsub("\n", "", ementa)) %>%
  mutate(ementa = gsub(" +", " ", ementa)) %>%
  mutate(ementa = iconv(to="ascii//translit", ementa))%>%
  mutate(ementa = toupper(ementa))

docs <- Corpus(VectorSource(dados_excel$ementa))



#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, "\\.")
docs <- tm_map(docs, toSpace, "º")
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, toSpace, 'ª')

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords

docs <- tm_map(docs, removeWords, stopwords(“english”))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then


dtm <- DocumentTermMatrix(docs)

as.character(docs[[1]])