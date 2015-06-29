setwd("~/Desktop/ProjectRepo/group-project-242/Data")

#loading in sentiments
senti <- read.table("NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt", stringsAsFactors = FALSE )
colnames(senti) <- c("word", "emotion", "score")

sentiments <- senti 

#remove all words that have zeros 

sentiments <- subset(senti, senti$score != 0)
rownames(sentiments) <- c(1:nrow(sentiments))


#converting all negative words to -1 
anger <- subset(sentiments, sentiments$emotion=="anger" & sentiments$score == 1 )
negcolumn <- anger$score * -1
anger$score <- negcolumn
rows <- as.numeric(rownames(anger))
sentiments[rows,] <- anger
rownames(sentiments) <- c(1:nrow(sentiments))


disgust <- subset(sentiments, sentiments$emotion=="disgust" & sentiments$score == 1 )
negcolumn <- disgust$score * -1
disgust$score <- negcolumn
rows <- as.numeric(rownames(disgust))
sentiments[rows,] <- disgust
rownames(sentiments) <- c(1:nrow(sentiments))


fear <- subset(sentiments, sentiments$emotion=="fear" & sentiments$score == 1 )
negcolumn <- fear$score * -1
fear$score <- negcolumn
rows <- as.numeric(rownames(fear))
sentiments[rows,] <- fear
rownames(sentiments) <- c(1:nrow(sentiments))


negative <- subset(sentiments, sentiments$emotion=="negative" & sentiments$score == 1 )
negcolumn <- negative$score * -1
negative$score <- negcolumn
rows <- as.numeric(rownames(negative))
sentiments[rows,] <- negative
rownames(sentiments) <- c(1:nrow(sentiments))


sadness <- subset(sentiments, sentiments$emotion=="sadness" & sentiments$score == 1 )
negcolumn <- sadness$score * -1
sadness$score <- negcolumn
rows <- as.numeric(rownames(sadness))
sentiments[rows,] <- sadness
rownames(sentiments) <- c(1:nrow(sentiments))


#now we aggregate by word 

splitwords <- split(sentiments, sentiments$word)
wordagg <- sapply(splitwords, function(i) sum(i$score))
wordagg <- wordagg[wordagg!=0]

aggframe <- data.frame(X1 = names(wordagg), X2=wordagg)
rownames(aggframe) <- c(1:nrow(aggframe))
colnames(aggframe) <- c("word", "score")

#taking in first book and splitting 

computebook <- function(x){
  
book <- tolower(readLines(x))
book <- paste(book,collapse="")
booksplit <- strsplit(book, "\\.")
booksplit <- booksplit[[1]]
sentsplit <- strsplit(booksplit, " ")
}
#we wend up with a list with each element of the list representing a sentence and within each element you hae a vector of words

listlog <- lapply(sentsplit, function(i){
  aggframe$word %in% i 
  })

sentencesums <- sapply(listlog, function(i) sum(aggframe[i,2]))
return(sentencesums)
}

test <- computebook("OliverTwist.txt")
test1 <- princomp(test)
screeplot(test1, npcs = 1, type = "lines")

plot(sentencesums, type="l", main="score by sentence for Oliver Twist")

##############################trying a different method 

setwd("~/Desktop/ProjectRepo/group-project-242/Data")

#loading in sentiments
senti <- read.table("NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt", stringsAsFactors = FALSE )
colnames(senti) <- c("word", "emotion", "score")
Uword <- unique(senti$word)

#take in book 

book <- tolower(readLines("OliverTwist.txt"))
book <- paste(book,collapse="")

wordcount <- sapply(Uword, function(i) {
  log <- unlist(strsplit(book, " ")) %in% i 
  count <- length(which(log))
  return(count)}) 

########## TM code starts here h CARLOS START HERE ######################




library(stringi)
library(tm)
library(proxy)

files <- list.files()
books <- character(length(files))
for(i in 1:length(files)){
  books[i] <- stri_flatten(readLines(stri_paste(files[i])), col = " ")
}

docs <- Corpus(VectorSource(books))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, PlainTextDocument)


TDM <- TermDocumentMatrix(docs, list(dictionary=Uword))

wordmatrix <- t(as.matrix(TDM))

dissim <- dist(wordmatrix, method = "cosine")

hc.average<- hclust(dissim, method = "average")
hc.complete=hclust(dissim, method="complete")
hc.single=hclust(dissim, method="single")

plot(hc.average, labels = files)
plot(hc.complete, labels = files)
plot(hc.single, labels = files)

#single gives us our lowest misclassification rate 
# running KNN 
library(class)

files <- list.files()
books <- character(length(files))
for(i in 1:length(files)){
  books[i] <- stri_flatten(readLines(stri_paste(files[i])), col = " ")
}

docs <- Corpus(VectorSource(books))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, PlainTextDocument)

TDM <- TermDocumentMatrix(docs, list(dictionary=Uword))

training <- t(as.matrix(TDM))

#did same thing above but switched train for testing data and changed working directory

testing <- t(as.matrix(TDM))

#remember that TDM is is different from the one above. You have to rerun the code with a new set of list files

trainnames <- as.factor(c("music", "philosophy", "religion", "music", "religion", "religion", "crime", "philosophy", "philosophy", "philosophy", "religion", "religion", "music", "music", "crime", "music", "philosophy", "crime", "crime", "crime"))
testnames <- as.factor(c("crime", "religion", "philosophy", "music", "music", "crime", "crime", "philosophy", "music", "religion", "crime", "philosophy", "religion", "religion", "crime", "philosophy", "music", "music", "philosophy", "religion"))

training_names <- data.frame(trainnames, training)
testing_names <- data.frame(testnames, testing)


k.knn <- 15
miss.rate <- rep(NA, k.knn)
for(i in 1:k.knn){
  test <- knn(training_names[,-1], testing_names[,-1], training_names[,1], k = i)
  miss.rate[i] <- mean(test == testnames)
}

# so we will use 1 nearest neighbors 
# 0.80 0.70 0.70 0.75 0.65 0.75 0.70 0.65 0.55 0.60 0.40 0.35 0.40 0.30 0.30

knn.predict <- knn(training_names[,-1], testing_names[,-1], training_names[,1], k = 1)

#able(knn.predict, testnames)
#testnames
#knn.predict  crime music philosophy religion
#crime          5     0          0        0
#music          0     3          0        1
#philosophy     0     2          5        1
#religion       0     0          0        3

#now I am using randomForest to see if i can classify the books 
library(randomForest)

randomF.fit = randomForest(as.factor(training_names$trainname)~., data = training_names, mtry = round(sqrt(ncol(training)-1)), importance = TRUE); randomF.fit
imp.gini.rf = importance(randomF.fit)[,"MeanDecreaseGini"]
varImpPlot(randomF.fit)
imp.gini.rf[which(imp.gini.rf > 1)]  ## Importanc features based on mean decrease of Gini index

