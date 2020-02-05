library(tm)
library(NLP)
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(wordcloud)
library(dplyr)
library(stringr)


rm(list = ls())
ls()
getwd()
setwd("/Users/andrecarvalho/Desktop/TM")


rm(list = ls())
ls()
getwd()
setwd("D:/TM5")


cps <- Corpus(DirSource("/Users/andrecarvalho/Desktop/TM2",
                        encoding = "UTF-8"),
              readerControl = list(language = "pt"))


cps <- Corpus(DirSource('D:/TM5',
                        encoding = "UTF-8"),
              readerControl = list(language = "pt"))


cps <- tm_map(cps, stripWhitespace)
cps <- tm_map(cps, content_transformer(tolower))
cps <- tm_map(cps, removeWords, stopwords("portuguese"))
#cps <- tm_map(cps, stemDocument)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeWords, c("falo", "né", "galeno", "senhor","proqu","a","A","agora","ainda","pra","tÃ¡","dÃ¡","lÃ¡","tÃ´","nÃ©",
                                  "algum","alguma","algumas","alguns","antes","senhora","quatro","aqui","aÃ","conta", "faz", "hoje",   
                                  "ao","Ao","aos","aqui","as","se","As","assim","Assim","bem","zero", "vinte", "dia", "coisa", "coisas",     
                                  "das","dava","de","dela","dele","dentro","dizer", "dizia", "do","muita", "venha", "entrar", "opinião",  
                                  "dos", "dous","e","E","ela","ele","eles","isso","isto","la","em","Em","ter", "ano","liga","longo","lógico",
                                  "fora","fosse","gente","lhe","lo","logo","mais","mas","Mas","me","como","muitas","quais","tantas","tatiana",  
                                  "meus","mim","na","pelo", "Pois", "por", "este","diz","então",  "expndexpndtwkerning", "vagas","vezes","lado",
                                  "nas","nem","no","No","nos","nossa","o","com", "é", "cielo" , "expandedcolortblcssrgbccccssrgbccc","têm","nisso",
                                  "O", "os", "Os", "ou","para", "pela", "D", "aí", "né", "vou", "cento","robfs","servieo","toda","todo","todos","tudo",
                                  "que","Que","queria","se","Se","sem","da", "r", "tá", "colortblredgreenblueredgreenblueredgreenblue","deftab","meo","neo",   
                                  "senhor","senhora","seu","seus","si","sua","tal", "helveticaneue","ffs", "helveticaneue" , "cocoatextscalingcocoaplatformfonttblffnilfcharset",
                                  "paperwpaperhmarglmargrviewwviewhviewkind","pardpardeftabslpartightenfactor","rtfansiansicpgcocoartf","enteo","cartfes","inteligeancia",
                                  "obstrued","substitued",
                                  "ve","sf","cb","cf",
                                  "inteligeancia","robf","entanto","cor",
                                  "tanto", "tarde","doi","veio", "vi", "vai"))

################################################
###############################################
##    DTM


dtm <- DocumentTermMatrix(cps)
dim(dtm)


findFreqTerms(dtm, lowfreq = 10)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

length(freq)
head(freq, 10)
tail(freq, 10)

ord <- order(freq)

dtms <- removeSparseTerms(dtm, 0.7)
dim(dtms)
inspect(dtms)

write.csv(freq, file = "frequencies.csv")

findAssocs(dtm,terms = "artificial",0.41)
findAssocs(dtm, c("artificial", "humano", "trabalho"), c(0.7, 0.7, 0.7))

# remove sparse terms
dtm2 <- removeSparseTerms(dtm, sparse = 0.70)
m2 <- as.matrix(dtm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")


plot(fit)
rect.hclust(fit, k = 6) # modelo com 6 clusters

  
  
################################################
################################################

tdm <- TermDocumentMatrix(cps,
                          control = list(wordLengths = c(1, Inf)))



(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)

findAssocs(tdm, "não", 0.1)

tdm2 <- removeSparseTerms(tdm, sparse = 0.60)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2")

plot(fit)
rect.hclust(fit, k = 5)

