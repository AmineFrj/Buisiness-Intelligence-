# ------------------------------ Import Data ---------------------------------

data <- read.csv("/Users/amine/Desktop/Buisiness-Intelligence--master/DBLP_Subset.txt",header = F, stringsAsFactors=FALSE,quote = "")

# ------------------------------ Create Table  ---------------------------------
table <- data.frame()
line <- c("","","","","~~","")

for(i in data[,1]){
  if(startsWith(i,"#*")){
    table <- rbind(table, data.frame("article_id" = line[1],"Title" = line[2],"Author" = line[3],"Year" = line[4],"Review" = line[5],"Citations" = line[6]))
    line = c("","","","","~~","")
    line[2] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#@")){
    line[3] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#t")){
    line[4] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#c")){
    line[5] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#i")){
    line[1] = substr(i,7,nchar(i))
  }
  else if(startsWith(i,"#%")){
    if(line[6]=="")
      line[6] <- substr(i,3,nchar(i))
    else
      line[6] = paste(line[6],",",substr(i,3,nchar(i)))
  }
  else if(startsWith(i,"#!")){
    line[5] = substr(i,3,nchar(i))
  }
  else {
    if(startsWith(line[5],"~~")){
      line[3] = paste(line[3],",",i)
    }
    else 
      line[5] = paste(line[5],"",i)
  }
}

table <- table[-1,]
table <- table[!(table$Review == ""), ]
table$Title <- levels(table$Title)[as.numeric(table$Title)]
table$Review <- levels(table$Review)[as.numeric(table$Review)]
table$Author <- levels(table$Author)[as.numeric(table$Author)]
table$Citations <- levels(table$Citations)[as.numeric(table$Citations)]
table$article_id <- levels(table$article_id)[as.numeric(table$article_id)]
#Remove dot at the end of the title
table$Title = gsub("[.]","",table$Title)

# ------ Store #Citations 
citations <- unlist(strsplit(table$Citations," , "))
citation_table <- as.data.frame(table(citations))
colnames(citation_table) <- c("Citations","Citations_count")
table = merge(table, citation_table, by.x = "Citations", by.y = "Citations",all.x = TRUE)
table[is.na(table)] <- 0

# ------------------------- Create Document/Tern Matrix ------------------------------
library("tm")
corpus <- Corpus(VectorSource(table$Title))
dtm <- removeSparseTerms(DocumentTermMatrix(corpus),0.99)
docTerm <- as.data.frame(as.matrix(dtm))
table$id <- 1:nrow(table)
docTerm$id <- 1:nrow(docTerm)
docTerm = merge(docTerm,table, by = "id")
remove <- c("id","Title","Author","Year","Review","Citations")
docTerm <- docTerm[, -which(names(docTerm) %in% remove)]

table <- table[, -which(names(table) %in% "id")]


# Create Tern/document Matrix ------------------------------
tdm <- removeSparseTerms(TermDocumentMatrix(corpus),0.99)
termDoc <- as.data.frame(as.matrix(tdm))

#echantillon 1.5K
# train = table[1:1500,]
# corpus <- Corpus(VectorSource(train$Title))
# tdm <- TermDocumentMatrix(corpus)
# docTerm_train <- as.data.frame(t(as.matrix(tdm)))
# docTerm_train$id <- 1:nrow(docTerm_train)
  
# ------------------------- Create Authors Matrix ------------------------------

authors <- c()
for (i in table$Author)
  for (j in strsplit(i, " , "))
    authors <- append(authors,j)
  
authors <- unique(authors)
authors <- data.frame("author_name" = authors)
authors$id = seq.int(nrow(authors))

#Merge with #Publications
authors_ <- unlist(strsplit(table$Author," , "))
authors_pub <- as.data.frame(table(authors_))
colnames(authors_pub) <- c("author_name","Count")
authors = merge(authors,authors_pub, by = "author_name")
# ------------------------- Export Tables ------------------------------

termFreq <- as.data.frame(rowSums(as.matrix(tdm)))
colnames(termFreq) <- c("Frequence")


write.csv(authors,"tables/Athors.csv",row.names = F)
write.csv(table,"tables/Table.csv",row.names = F)
write.csv(docTerm,"tables/DocumentTerm.csv",row.names = F)
write.csv(termFreq,"tables/TermFrequency.csv",row.names = T)

termDoc

#############

# write.csv(authors,"train/Athors.csv",row.names = F)
# write.csv(train,"train/Table.csv",row.names = F)
# write.csv(docTerm_train,"train/DocumentTerm.csv",row.names = F)

# ------------------------- Clustering ------------------------------

clustering <- as.data.frame(kmeans(docTerm, 10)$cluster)
names(clustering) <- "Cluster"

library(igraph)
fastgreedy.community(graph(as.matrix(docTerm)))
graph(as.matrix(docTerm))
