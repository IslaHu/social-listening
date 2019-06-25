rm(list=ls())
## train the classification model using a big data set on Kaggle
mydata <- read.csv("Tweets.csv",header = T)
mydata <- mydata[c("tweet_id","text","airline_sentiment")]
mydata <- mydata[mydata$airline_sentiment!="neutral",c(1,2,3)]

summary(mydata)
####train data
library(tm)
library(DBI)
library(RMySQL)
library(e1071)
library(maxent)
library(pROC)

docs <- Corpus(VectorSource(mydata$text))

mystopwords <- c("virginamerica","amp","ive","jetblue","southwestair","americanair")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), 
                                                        mystopwords), stripWhitespace=T, stemming=F)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm.full,0.99)
X <- as.matrix(dtm)
colnames(X)
nrow(X)

Y = as.numeric(mydata$airline_sentiment)
for(i in 1:length(Y)){
  if(Y[i]==1){
    Y[i]=0
  }else if(Y[i]==3){
    Y[i]=1
  }
}

table(Y)


set.seed(1) # fixing the seed value for the random selection guarantees the same results in repeated runs
n=length(Y)
n1=round(n*0.75)
n2=n-n1
train=sample(1:n,n1)

###########################################
#############   Evaluation   ##############
###########################################

Evaluation <- function(pred, true, class)
{
  tp <- sum( pred==class & true==class)
  fp <- sum( pred==class & true!=class)
  tn <- sum( pred!=class & true!=class)
  fn <- sum( pred!=class & true==class)
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  F1 <- 2/(1/precision + 1/recall)
  F1
}

###########################################
##########   Naive Bayesion   #############
###########################################

nb.model <- naiveBayes( X[train,], factor( Y[train]) ) # encode the response as a factor variable
pred.class <- predict( nb.model, X[-train,] )
table( pred.class, Y[-train] )
Evaluation( pred.class, Y[-train], 0 )
Evaluation( pred.class, Y[-train], 1 )

pred <- predict( nb.model, X[-train,], type = "raw" )
nb.roc <- roc( Y[-train], pred[,2] )
plot.roc( nb.roc )
auc( Y[-train], pred[,2] )

###########################################
##########   Maximum Entropy   ############
###########################################

maxent.model <- maxent( X[train,], Y[train] )
pred <- predict( maxent.model, X[-train,] )
table( pred[,1], Y[-train] )
Evaluation( pred[,1], Y[-train], 1 )
Evaluation( pred[,1], Y[-train], 0 )

maxent.roc <- roc( Y[-train], as.numeric(pred[,2]) )
plot.roc( maxent.roc )
auc( Y[-train], as.numeric(pred[,2]) )

pred <- predict( maxent.model, X )
table( pred[,1], Y )

####################################
#######predict
driver <- dbDriver("MySQL")
myhost <- "localhost"
mydb   <- "studb"
myacct <- "cis434"
mypwd  <- "LLhtFPbdwiJans8F@S207" 

conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
mydata1 <- dbGetQuery(conn, paste("SELECT id, airline, tweet FROM hw4 WHERE rtag='yX)x7n07&Tgv'"))
dbDisconnect(conn)

##DTM
docs <- Corpus(VectorSource(mydata1$tweet))

mystopwords <- c("virginamerica","amp","ive","jetblue","southwestair","americanair")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T,
                   stopwords=c(stopwords("english"), mystopwords), stripWhitespace=T, stemming=F)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm.full,0.99)
X1 <- as.matrix(dtm)

#entropy
pred.entropy <- predict( maxent.model, X1 )
table(pred.entropy[,1])
class <-pred.entropy[,1]
class <- as.numeric(class)
class <- as.logical(class)
tweet <- mydata1[class,]

#manually
docs <- Corpus(VectorSource(tweet$tweet))

mystopwords <- c()
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), 
                                                              mystopwords), stripWhitespace=T, stemming=F)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm.full,0.999)
X2 <- as.matrix(dtm)
colnames(X2)

df <- as.data.frame(X2)
tweet1 = tweet[!(df$delayed>0|df$lost>0|df$miss>0|df$ill>0|df$delay>0|df$unacceptable>0|
                   df$late>0|df$fail>0|df$missed>0|df$delays>0|df$problem>0|
                   df$cancelled>0|df$missing>0|df$bad>0|df$lax>0|df$fucking>0|df$awful>0|
                   df$issue>0|df$shame>0|df$hell>0|df$fuck>0|df$ugh>0|
                   df$broken>0|df$refund>0|df$hate>0|df$worst>0|df$horrible>0|df$changed>0|
                   df$cost>0|df$inconvenience>0|df$delayed>0|df$damaged>0|df$leave>0|df$delaying>0|
                   df$rude>0|df$lie>0|df$complaints>0|df$disappointment>0|df$cancellation>0|
                   df$poor>0|df$leaving>0|df$hell>0|df$fuck>0|df$failed>0|df$fault>0|
                   df$ashamed>0|df$stop>0|df$ruined|df$awaiting>0|df$broke>0|df$lose>0|df$lost>0|
                   df$killing>0|df$time>0|df$ruined>0|df$waiting>0|df$nothing>0|df$cancellation>0|
                   df$explanation>0|df$response>0|df$terrible>0|df$fucked>0|df$fuckers>0|df$never>0|df$negative>0|
                   df$annoying>0|df$smell|df$damaged>0|df$damage>0|df$damn>0|df$charge>0|df$charging>0|
                   df$flawed>0|df$change>0|df$dreadful>0|df$kicked>0|df$unable>0|df$unbearable>0|df$uncaring>0|
                   df$unhappy>0|df$unhappycustomer>0|df$noexcuse>0|df$nowhere>0|df$nobody>0|df$shit>0|
                   df$ruining>0|df$should>0|df$cancel>0|df$rudest>0|df$rudeness>0|df$shitty>0|
                   df$shits>0|df$shittier>0|df$stuck>0|df$brutal>0|df$uncomfortable>0|df$excuse>0|df$unpleasant>0|
                   df$screwed>0|df$destroyed>0|df$opposite>0),]
tweet1 <- tweet1[,c(1,3)]

write.csv(tweet1, file="Zhizhou_Hu.csv",sep = ",")
