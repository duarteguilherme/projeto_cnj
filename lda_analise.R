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


# Inspecting line 30
as.character(docs[[2]])