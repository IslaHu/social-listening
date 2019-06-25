require("NLP")
require("openNLP")
library(wordcloud)
gc()
options(java.parameters = "- Xmx1024m")

#Q1---------------------------------------------
#You will find 2 files after you uncompress the zip file. 
#Each file contains online reviews of doctors of a particular specialty. 
#For each specialty, plot the wordcloud of the 20 most frequently mentioned adjectives1 in the reviews.
rm(list = ls())
#Retore all adjectives in 11.txt into a new vector JJ_of_11
JJ_of_11 <- numeric(0)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

fileName="11.txt"
con = file(fileName, "r")
while(1){
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {break}
        line = as.String(line)
        
        #annotate using POS
        ## Sentence
        a1 <- annotate(line, sent_token_annotator)
        ### Word
        a2 <- annotate(line, word_token_annotator, a1)
        ### POS Tag
        a3 <- annotate(line, pos_tag_annotator,a2)
        a3w <- subset(a3, type == "word")
        tags <- sapply(a3w$features, "[[", "POS")
        mypos = a3w[tags=="JJ"]
        JJ_of_11 <- c(JJ_of_11, line[mypos])
}
close(con)

a <- table(JJ_of_11)
b <- as.vector(a)
c <- order(b, decreasing = T)
Words_11 <- names(a)[c[1:20]]
Words_11 <- as.data.frame(Words_11)
Words_11[,2] <- as.numeric(a[c[1:20]])
#Word Cloud
wordcloud(Words_11[,1], Words_11[,2], colors = "Pink")
gc() 

# --------------------------------------------
#do the same to 22.txt
rm(list = ls())
#Retore all adjectives in 22.txt into a new vector JJ_of_22
JJ_of_22 <- numeric(0)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

fileName="22.txt"
con = file(fileName, "r")
while(1){
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {break}
        line = as.String(line)
        
        #annotate using POS
        ## Sentence
        a1 <- annotate(line, sent_token_annotator)
        ### Word
        a2 <- annotate(line, word_token_annotator, a1)
        ### POS Tag
        a3 <- annotate(line, pos_tag_annotator,a2)
        a3w <- subset(a3, type == "word")
        tags <- sapply(a3w$features, "[[", "POS")
        mypos = a3w[tags=="JJ"]
        JJ_of_22 <- c(JJ_of_22, line[mypos])
}
close(con)

a <- table(JJ_of_22)
b <- as.vector(a)
c <- order(b, decreasing = T)
Words_22 <- names(a)[c[1:20]]
Words_22 <- as.data.frame(Words_22)
Words_22[,2] <- as.numeric(a[c[1:20]])
#Word Cloud
wordcloud(Words_22[,1], Words_22[,2], colors = "Skyblue")
gc()



