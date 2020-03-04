download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "data.zip", mode = "wb")

library(stringr)

con <- file("en_US/en_US.twitter.txt")
twitter <- readLines(con)
close(con)

con <- file("en_US/en_US.blogs.txt")
blogs <- readLines(con)
close(con)

con <- file("en_US/en_US.news.txt")
news <- readLines(con)
close(con)

mean(nchar(twitter))
mean(nchar(blogs))
mean(nchar(news))

length(twitter)
length(blogs)
length(news)

set.seed(147)
twitter.intrain <- as.logical(rbinom(n = length(twitter), size = 1, prob = .3))
twitter.train <- twitter[twitter.intrain]
twitter.test <- twitter[-twitter.intrain]

blogs.intrain <- as.logical(rbinom(n = length(blogs), size = 1, prob = .3))
blogs.train <- blogs[blogs.intrain]
blogs.test <- blogs[-blogs.intrain]

news.intrain <- as.logical(rbinom(n = length(news), size = 1, prob = .3))
news.train <- news[news.intrain]
news.test <- news[-news.intrain]

training <- c(twitter.train, blogs.train, news.train)
testing <- c(twitter.test, blogs.test, news.test)



# quiz1 -------------------------------------------------------------------


number.chr <- list(sapply(twitter,str_length),sapply(news,str_length), sapply(blogs, str_length))
number.chr[[1]][sapply(number.chr, which.max)][1]
number.chr[[2]][sapply(number.chr, which.max)][2]
number.chr[[3]][sapply(number.chr, which.max)][3]

sum(grepl("love", twitter))/sum(grepl("hate", twitter))
twitter[grep("[Bb]iostats", twitter)]

twitter[grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter)]

# week2 -------------------------------------------------------------------

library(tm)
library(tau)
library(ngram)

help(package = tm)

tokenizer <- scan_tokenizer(news)
tokenizerMC <- MC_tokenizer(news)

head(tokenizer)

count <- textcnt(twitter,2, method = "string")
barplot(head(count[order(count, decreasing = T)],100), main="2-gram Frequency Distribution")
head(count[order(count, decreasing = T)],100)


# simple prediction algo --------------------------------------------------
library(ngram)
library(ggplot2)
library(tau)
library(wordcloud2)


phrase.table1 <- read.csv("phrase 1gram.csv", as.is=T)[[1]]
phrase.table2 <- read.csv("phrase 2gram trim.csv", as.is=T)
phrase.table3 <- read.csv("phrase 3gram trim.csv", as.is=T)
phrase.table4 <- read.csv("phrase 4gram trim.csv", as.is=T)

phrase.table2$X <- NULL
phrase.table3$X <- NULL
phrase.table4$X <- NULL

####
training <- read.csv("training.csv", as.is = T)
training <- training[[2]]

gram2 <- ngram(concatenate(training), 2)
gram3 <- ngram(concatenate(training), 3)
gram4 <- ngram(concatenate(training), 4)

phrase.table1 <- textcnt(training,1, method = "string")
phrase.table1 <- names(phrase.table1[order(phrase.table1, decreasing = T)])
phrase.table2 <- get.phrasetable(gram2)
phrase.table3 <- get.phrasetable(gram3)
phrase.table4 <- get.phrasetable(gram4)

cum.sum <- cumsum(phrase.table4[2]/sapply(phrase.table2[4], sum))
cum.sum$names <- row.names(cum.sum)
qplot(y = cum.sum[1000000:1050000,1], x=row.names(cum.sum)[1000000:1050000])
head(cum.sum[cum.sum>=.6,])
head(phrase.table4[phrase.table4$freq==1,])
cum.sum[741817,]

phrase.table2.trim <- phrase.table2[phrase.table2$freq>5,] ##0.591036
phrase.table3.trim <- phrase.table3[phrase.table3$freq>5,]
phrase.table4.trim <- phrase.table4[phrase.table4$freq>1,] ##0.6625572

####
split.phrase2 <- strsplit(phrase.table2[[1]],split = " ")
split.phrase3 <- strsplit(phrase.table3[[1]],split = " ")
split.phrase4 <- strsplit(phrase.table4[[1]],split = " ")

input2 <- paste(sapply(split.phrase2,FUN = "[[",1))
input3 <- paste(sapply(split.phrase3,FUN = "[[",1),sapply(split.phrase3,FUN = "[[",2))
input4 <- paste(sapply(split.phrase4,FUN = "[[",1),sapply(split.phrase4,FUN = "[[",2),sapply(split.phrase4,FUN = "[[",3))

output2 <- sapply(split.phrase2,FUN = "[[",2)
output3 <- sapply(split.phrase3,FUN = "[[",3)
output4 <- sapply(split.phrase4,FUN = "[[",4)

system.time({
phrase <- "I"
phrase <- paste("", phrase)
phrase.word <- unlist(strsplit(phrase, " "))
phrase.char <- unlist(strsplit(phrase, ""))
length.word <- length(phrase.word)
length.char <- length(phrase.char)


prediction <- ifelse(phrase.char[length.char]!=c(" "," "," "), phrase.table1[startsWith(phrase.table1, tolower(phrase.word[length.word]))][1:3],
       ifelse(!is.na(output4[input4%in%paste(phrase.word[length.word-2], phrase.word[length.word-1], phrase.word[length.word])][1:3]),
              output4[input4%in%paste(phrase.word[length.word-2], phrase.word[length.word-1],phrase.word[length.word])][1:3],
              ifelse(!is.na(output3[input3%in%paste(phrase.word[length.word-1], phrase.word[length.word])][1:3]),
                     output3[input3%in%paste(phrase.word[length.word-1], phrase.word[length.word])][1:3],
                     ifelse(!is.na(output2[input2%in%paste(phrase.word[length.word])][1:3]),
                            output2[input2%in%paste(phrase.word[length.word])][1:3],
                            ""))))
    
    
wordcloud2(data.frame(word=prediction, freq=c(5,3,1)), size = 1,shape = 'diamond')
})
