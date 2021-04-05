#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE,repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN')  
#install.packages("Rcampdf")
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")  


## Search PubMed with (("2019/12/01"[Date - Publication] : "2020/01/30"[Date - Publication])) AND "Science (New York, N.Y.)"[Journal]
## Query term: https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%222019%2F12%2F01%22%5BDate+-+Publication%5D+%3A+%222020%2F01%2F30%22%5BDate+-+Publication%5D%29%29+AND+%22Nature%22%5BJournal%5D
## the file consists of 279 abstracts from Science.
## Note: Webpage downloaind is not supported any longger.
##E-Util has to be used nowadays. https://www.ncbi.nlm.nih.gov/books/NBK179288/

##Create a folder named with "Corpus"

setwd("/Users/jingboxia/BioNLPcourse/")
cname <- file.path("", "Users", "jingboxia", "BioNLPcourse", "corpus")

#################Load the R package for text mining and then load your texts into R.
library(NLP)
library(tm)   
docs <- Corpus(DirSource(cname))   
summary(docs) 

#Removing punctuation:
docs <- tm_map(docs, removePunctuation)   
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}   

#################Removing numbers:
docs <- tm_map(docs, removeNumbers)   

###################Converting to lowercase:
docs <- tm_map(docs, tolower)   

############Removing “stopwords” (common words) that usually have no analytic value.
docs <- tm_map(docs, removeWords, stopwords("english"))   

######Removing particular words:
docs <- tm_map(docs, removeWords, c("department", "email", "doi", "center", "sciences", "pubmed", "nature", "university", "pmid", "author", "school", "research"))

####Tell R to treat your preprocessed documents as text documents.
docs <- tm_map(docs, PlainTextDocument)  

#########To proceed, create a document term matrix.
dtm <- DocumentTermMatrix(docs)   

##########You’ll also need a transpose of this matrix. Create it using:
tdm <- TermDocumentMatrix(docs)   

#Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
freq
names(freq)

ord <- order(freq)  



###############Word Frequency
###There are lots of terms, just check some of the most and least frequently occurring words.
freq[head(ord, 10)]   
freq[tail(ord, 50)]  


###If you prefer to export the matrix to Excel:   
m <- as.matrix(dtm)   
write.csv(m, file="dtm.csv")  




wf <- data.frame(word = names(freq), freq=freq)
head(wf)


############Plot words that appear at least 50 times.
library(ggplot2)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#########word cloud
set.seed(142)   
library(wordcloud)
wordcloud(names(freq), freq, min.freq=25) 


