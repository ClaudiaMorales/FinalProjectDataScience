library(tm)
library(stringr)
library(R.utils)
library(wordcloud)
library(RWeka)
library(ggplot2)
library(readr)
library(stringi)
library(SnowballC)

## Directory
setwd("C:/Users/vgw52064/Desktop/Coursera/10_Capstone/")
path <- getwd()

fileb = "./Week 2/Data/en_US.blogs.txt"
filen = "./Week 2/Data/en_US.news.txt"
filet = "./Week 2/Data/en_US.twitter.txt"
profanity <- file.path("./Week 2/Data/profanity.txt")

## count lines in each file
linesb <- as.numeric(countLines(fileb))
linesb
linesn <- as.numeric(countLines(filen))
linesn
linest <- as.numeric(countLines(filet))
linest

## connect with files and choose sample
conblog = file(fileb, "r")
Datab <-readLines(conblog, n=20000, encoding = "UTF-8", skipNul = TRUE)
close(conblog)

connews = file(filen, "r")
Datan <-readLines(connews, n=20000, encoding = "UTF-8", skipNul = TRUE)
close(connews)

contwit = file(filet, "r")
Datat <-readLines(contwit, n=20000, encoding = "UTF-8", skipNul = TRUE)
close(contwit)

## combine and create one sample file
sample <- sample(paste(Datan, Datab, Datat), size = 40000, replace=T)

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
text = TermDocumentMatrix(corpus, control = list(minWordLength = 3))
freq = findFreqTerms(text, lowfreq = 200)
freq

## change corpus into data frame
library(tm)
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
four  = token(df,4)

save.image(file='Env.RData')



######################
##     PREDICT     ##
######################



require(stringr); require(data.table)

predictNgrams <- function(input){
     ## clean input text
     # remove numbers, punctuations
     word <- gsub("[^a-zA-Z\n\']", " ", input)
     # convert all words to lowercase
     word <- tolower(word)
     # remove extra spaces
     trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))
     word <-trim(word)      
     
     str <- unlist(str_split(word," "))
     len <- length(str)
     
     predict <- c()
     
     if (len>=3){
          ##trigram 
          W1 <- str[len-2]; W2 <- str[len-1]; W3 <- str[len]
          ngram <- four[list(W1, W2, W3)]
          predict <- head(ngram[order(ngram$freq, decreasing=T),]$pred)
          
          if(length(predict)<6){
               ##bigram
               ngram <- three[list(W2, W3)]
               predict <- c(predict,head(ngram[order(ngram$freq, decreasing=T),]$pred))    
          }
          
          if(length(predict)<6){
               ##unigram
               ngram <- two[list(W3)]
               predict <- c(predict,head(ngram[order(ngram$freq, decreasing=T),]$pred))    
          }
          
     }else if(len==2){
          W1 <- str[len-1]; W2 <- str[len]
          ngram <- three[list(W1, W2)]
          predict <- head(ngram[order(ngram$freq, decreasing=T),]$pred)
          
          if(length(predict)<6){
               ##unigram
               ngram <- two[list(W2)]
               predict <- c(predict,head(ngram[order(ngram$freq, decreasing=T),]$pred))    
          }
          
     }else if(len==1){
          W1 <- str[len]
          ngram <- two[list(W1)]
          predict <- head(ngram[order(ngram$freq, decreasing=T),]$pred)
          
     }    
     
     predict <- predict[!is.na(predict)]
     
     if(length(predict)<5){
          predict <- c(predict, one$pred)
     }
     return(predict[1:5])
}

input   <- 's'
predict <- grep(input, one[,1])
one[predict[1],]

input   <- 'she'
predict <- grep(input, two[,1])
two[predict[1:2],]

input   <- 'she said'
predict <- grep(input, three[,1])
three[predict[1:2],]

input <- "me about his"
system.time(result <- predictNgrams(input))
result
four[list('me', 'about', 'his')]









######################
##     NGRAMIFY     ##
######################

require(tm); require(SnowballC); require(data.table); require(stringr)
require(ggplot2); require(RWeka); require(qdap);
require(scales); require(gridExtra); require(wordcloud)

## ngramify function ##

gram <- function(split_num, ngram_df, grams){
     require(RWeka)
     cat(paste('Input data frame (rows:',nrow(ngram_df), '| size:',round(object.size(ngram_df)/1024/1024,0),
               'mb) \n are going to split into', split_num, 'and', grams, 'grams prediction chunks...'))
     cat(paste('\n (Step 1 of 5) Start to create chunks...'))
     
     chunks <- list()
     for (i in 1:split_num){
          chunks[[i]] <- ngram_df[(ceiling(nrow(ngram_df)/split_num)*(i-1)+1):(ceiling(nrow(ngram_df)/split_num)*i),1]
     } 
     rm(ngram_df); gc()
     
     cat(paste('\n (Step 2 of 5) Start to convert chunks into n-grams matrix...'))
     ngram_chunks <- list()
     for (j in 1:split_num){
          ngram_chunks[[j]] <- NGramTokenizer(chunks[[j]], Weka_control(min=grams,max=grams))    
     }
     rm(chunks); gc()
     
     cat(paste('\n (Step 3 of 5) Start to integrate chunks into one matrix...'))
     ngram_chunks_all <- c()
     for (z in 1:split_num){
          ngram_chunks_all <- c(ngram_chunks_all, ngram_chunks[[z]])
     }
     rm(ngram_chunks); gc()
     
     cat(paste('\n (Step 4 of 5) Start to calculate the frequency of each term...'))
     ngram_freq_tb <- sort(table(ngram_chunks_all), decreasing=T)
     rm(ngram_chunks_all); gc()
     
     cat(paste('\n (Step 5 of 5) Finishing the process...'))
     ngram_pred <- data.frame(terms = names(ngram_freq_tb), freq = ngram_freq_tb, row.names = NULL, stringsAsFactors = F)
     rm(ngram_freq_tb); gc()
     
     return(ngram_pred)
}


ngram_pred <- gram(split_num=200, df, grams=1)
dim(ngram_pred)
round(object.size(ngram_pred),0)
save(ngram_pred, file="./Final Project/Unigrams.RData")

ngram_pred <- gram(split_num=200, df, grams=2)
dim(ngram_pred)
round(object.size(ngram_pred),0)
save(ngram_pred, file="./Final Project/Bigrams.RData")

ngram_pred <- gram(split_num=200, df, grams=3)
dim(ngram_pred)
round(object.size(ngram_pred),0)
save(ngram_pred, file="./Final Project/Trigrams.RData")

ngram_pred <- gram(split_num=200, df, grams=4)
dim(ngram_pred)
round(object.size(ngram_pred),0)
save(ngram_pred, file="./Final Project/Qgrams.RData")

rm(ngram_pred)













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




























