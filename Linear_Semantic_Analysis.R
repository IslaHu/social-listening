
rm(list=ls())
#install.packages("NLP")
library(tm)

#Read all documents at the same time
docs<-Corpus(DirSource( "specialty3" ))

#Q1-------------------------------------------------------
#Use LSA to reduce the dimensionality
tdm.full <- TermDocumentMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
tdm = removeSparseTerms(tdm.full,0.85)
X = as.matrix(tdm)
X.svd = svd(X)
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v
X
round( U %*% D %*% t(V) )
round(D, digits=2)

#Q2-------------------------------------------------------
#Place the 300 documents in a 2-dimensional semantic space
dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]
rownames(Uk) = rownames(X)
rownames(Vk) = colnames(X)

R = Uk %*% Dk %*% t(Vk) 

round(R, digits=2)

### relations between terms and documents
rownames(R)

### relations between terms
cor(X['dentist',], X['work',])
cor(R['doctor',], R['work',])
cor(X['insurance',], X['dont',])
cor(R['room',], R['good',])

### relations between documents
cor(X)
cor(R)

#Q3-------------------------------------------------------
#Compare the document clusters with the ground truth, 
#which is embeded in the file names. You may use functions such as gsub(), svd().

# projection on semantic space
term.proj = Uk %*% Dk
doc.proj = Dk %*% t(Vk)

groups <- names(docs)
group_color <- numeric(0)
for(i in 1:length(groups)){
        group_color[i] <- strsplit(groups[i],'-')[[1]][1]
}

# draw the clusters grouped by file names
require(ggplot2)
colnames(doc.proj) = c('c1', 'c2', 'c3', 'c4', 'c5', 'm1', 'm2', 'm3', 'm4')
doc.plot <- data.frame(x=doc.proj[1,], y=doc.proj[2,], names=colnames(doc.proj))
ggplot(doc.plot, aes(x,y,color=group_color)) + geom_point() + geom_text(aes(label=group_color), hjust=0.5, vjust=-1)


#Q4-------------------------------------------------------
#Use k-means to cluster the documents.
library(cluster) 
library(fpc)

toclust <- t(R)
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

pm1 = pamk(toclust,scaling=TRUE)
pm1$nc #this indicates the optimal number of clusters based on specific criterion
#4 groups are the best

km.out = kmeans(t(R),4,nstart=20)
km.out$tot.withinss #251.0213

km.out$cluster
plotcluster(toclust, km.out$cluster)
clusplot(toclust,km.out$cluster, color = T)
