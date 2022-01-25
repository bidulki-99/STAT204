setwd("C:/Users/윤민서/OneDrive/Desktop/통계계산프로그래밍")
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)

#1.
buildDictionary(ext_dic = "woorimalsam")
sj01 <- readLines('sj01.txt', encoding = 'UTF-8')
nouns <- extractNoun(sj01)
mnous <- unlist(nouns)

mnous <- removeNumbers(mnous)
mnous <- removePunctuation(mnous)
mnous <- stripWhitespace(mnous)
mnous <- Filter(function(x) {nchar(x) > 1}, mnous)
mnous <- gsub('그것', '', mnous)

mnous_freq <- table(mnous)
v <- sort(mnous_freq, decreasing = TRUE)
v <- v[-2]
pal = brewer.pal(11, "Spectral")
wordcloud(words = names(v), freq = v, min.freq = 1, max.words = length(v), random.order = F,
          rot.per = 0.1, scale = c(3, 0.5), colors = pal, random.color = T)

#2.
t = readLines("https://ko.wikipedia.org/wiki/%EB%8F%99%ED%95%99_%EB%86%8D%EB%AF%BC_%ED%98%81%EB%AA%85")
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p", xmlValue)

doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)

inspect(dtm)

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
d1 = d[1:100, ]
wordcloud2(d1)

useSejongDic()

nouns = extractNoun(clean_doc)
mnous = unlist(nouns)
mnous_freq = table(mnous)
v = sort(mnous_freq, decreasing = TRUE)

wordcloud2(v)
v1 = v[1:100]
wordcloud2(v1)

#
mnous <- removeNumbers(mnous)
mnous <- removePunctuation(mnous)
mnous <- stripWhitespace(mnous)
mnous <- Filter(function(x) {nchar(x) > 1}, mnous)
mnous <- gsub('그것', '', mnous)

mnous_freq <- table(mnous)
v <- sort(mnous_freq, decreasing = TRUE)

wordcloud2(v)
v1 = v[1:100]
wordcloud2(v1)