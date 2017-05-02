library(readxl)

dados_excel <- readRDS('~/Documents/projeto_cnj/dados_excel.rds')

library(tm)
library(topicmodels)
library(tidyverse)


dados_excel <- dados_excel %>%
  mutate(ementa = gsub("\r", "", ementa)) %>%
  mutate(ementa = gsub("\t", "", ementa)) %>%
  mutate(ementa = gsub("\n", "", ementa)) %>%
  mutate(ementa = gsub(" +", " ", ementa)) %>%
  mutate(ementa = iconv(to="ascii//translit", ementa))%>%
  mutate(ementa = tolower(ementa))

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

docs <- tm_map(docs, removeWords, stopwords("portuguese"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then

# Testing
as.character(docs[[2]])

dtm <- DocumentTermMatrix(docs)



# Running the topic model

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5




#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, 
                          best=best, burnin = burnin, iter = iter, thin=thin))


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))


