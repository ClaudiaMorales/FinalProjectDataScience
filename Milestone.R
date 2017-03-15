library(tm)
library(stringr)
library(R.utils)
library(wordcloud)
library(RWeka)
library(ggplot2)
library(readr)
library(stringi)

## Directory
setwd("C:/Users/vgw52064/Desktop/Coursera/10_Capstone/")
path <- getwd()

fileb = "./Data/en_US.blogs.txt"
filen = "./Data/en_US.news.txt"
filet = "./Data/en_US.twitter.txt"
profanity <- file.path("./Data/","profanity.txt")

## count lines in each file
linesb <- as.numeric(countLines(fileb))
linesb
linesn <- as.numeric(countLines(filen))
linesn
linest <- as.numeric(countLines(filet))
linest

## connect with files and choose sample
conblog = file(fileb, "r")
Datab <-readLines(conblog, n=10000, encoding = "UTF-8", skipNul = TRUE)
close(conblog)

connews = file(filen, "r")
Datan <-readLines(connews, n=10000, encoding = "UTF-8", skipNul = TRUE)
close(connews)

contwit = file(filet, "r")
Datat <-readLines(contwit, n=10000, encoding = "UTF-8", skipNul = TRUE)
close(contwit)

## combine and create one sample file
sample <- sample(paste(Datan, Datab, Datat), size = 10000, replace=T)

     ## sample stats
     stri_stats_latex(sample)
     ## total number of words in sample
     stri_stats_latex(sample)[4]
     ## total number of letters in sample
     stri_stats_latex(sample)[1]

## set up source and create Corpus
source <- VectorSource(sample)
corpus <- Corpus(source)

## clean the text
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, read_lines(profanity))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)

## graph
wordcloud(corpus, max.words = 250, rot.per=0.25, random.order = F, use.r.layout = F, colors = brewer.pal(4, "PRGn"))

## the most frequent words used in the corpus
text = TermDocumentMatrix(corpus, control = list(minWordLength = 5))
freq = findFreqTerms(text, lowfreq = 300)
freq

## change corpus into data frame
df <- data.frame(text = unlist(sapply(corpus, '[', "content")), stringsAsFactors = F)

## create tokens for n frequencies
token = function(data, size){
     ngram = NGramTokenizer(data, Weka_control(min=size, max=size, delimiters = " \\t\\r\\n.!?,;\"()"))
     word = data.frame(table(ngram))
     sorted = word[order(word$Freq, decreasing = T),]
     colnames(sorted) = c("Word", "Count")
     sorted
}

one   = token(df,1)
two   = token(df,2)
three = token(df,3)

## plot the n grams
par(mfrow = c(1, 1))
plot = function(data, count = 15){
     barplot(data[1:count,2], names.arg = data[1:count,1], cex.names = .5, 
             col = "lavender", 
             main = paste("Frequency of Word(s)"), las = 2)
}
     
plot(one)
plot(two)
plot(three)

## analyze how many words will cover for 50% and 90% of all instances

one = cbind(one, one$Count/sum(one$Count))
colnames(one) = c("Word", "Count", "%")
two = cbind(two, two$Count/sum(two$Count))
colnames(two) = c("Word", "Count", "%")
three = cbind(three, three$Count/sum(three$Count))
colnames(three) = c("Word", "Count", "%")

uniq = function(x,percent){
     feq   = 0
     count = 1
     
     while(feq < percent){
          feq = feq + x[count,3]
          count = count +1
     }
     count
}

uniq(one,.50)
uniq(one,.90)

uniq(two,.50)
uniq(two,.90)

uniq(three,.50)
uniq(three,.90)




























