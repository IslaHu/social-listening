# Uncompress the file to retrieve 12,895 text files.
# Each text file contains a tweet randomly selected from one particular hour of the past.
# Explore the hidden topics from the documents to identify an international event of major political consequences.

rm(list=ls())
library(tm)
library(wordcloud)
library(topicmodels)

# Step1. load the files, changing into document-term-matrix
docs<-Corpus(DirSource( "~/Desktop/Social Media/HW3/spritzer/15" ))
## change stopwords to get better consequences
mystop=c('just', 'hey', 'can', 'amp', 'now', 'lol','via','twitter','dont','cant','ive')
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(mystop, stopwords("english"), stopwords("spanish"))))

# Step2. munipulate the text data, deleting the documents that have no term left
dtm = removeSparseTerms(dtm,0.9999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]

# Step3. topic modelling, estimating the LDA model
lda.model = LDA(dtm, 12)  # try different topic amounts
myposterior <- posterior(lda.model) # get the posterior of the model

# Step4. post analysis, getting topic distribution of each document
topics = myposterior$topics 
dim(topics) # 9008 documents on 12 topics
barplot(topics[c(1,10),], beside=TRUE, col=c("red","blue")) # plot topic distribution of specific documents

# Step5. post analysis, getting term distribution of each topic
terms = myposterior$terms ## each row defines a topic
dim(terms) # 12 topics on 199 terms
tid <- 6  ## pick a topic
termprob <- terms[tid, ] # the probability of each term in a given topic
wordcloud(names(termprob), termprob, max.words=20, colors=brewer.pal(6,"Dark2"))

# Step6. identify an international event of major political consequences.
ix = sort( topics[,tid], decreasing = TRUE, index.return=TRUE )$ix # sort documents by topic proportion
newdocs[ ix[1:5] ]$"content" # check textual contents of documents with highe proportion of the topic

#####————————————————————————The topic---------------------
# One Malaysian Airlines's plane got shot near ukraine today.
