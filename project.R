# ------------------------------ Import Data ---------------------------------

data <- read.delim("/Users/amine/Desktop/Buisiness-Intelligence--master/DBLP_Subset.txt",header = F, stringsAsFactors=FALSE,quote = "")

# ------------------------------ Create Table  ---------------------------------
table <- data.frame()
line <- c("","","","","~~","","")

for(i in data[,1]){
  if(startsWith(i,"#*")){
    table <- rbind(table, data.frame("Article_id" = line[1],"Title" = line[2],"Author" = line[3],"Year" = line[4],"Venue" = line[5],"Citations" = line[6], "Abstract" = line[7]))
    line = c("","","","","","","~~")
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
    line[7] = substr(i,3,nchar(i))
  }
  else {
    if(startsWith(line[7],"~~")){
      line[3] = paste(line[3],",",i)
    }
    else 
      line[7] = paste(line[7],"",i)
  }
}

copie = data.frame(table)

table <- table[-1,]
table <- table[!(table$Abstract == "~~"), ]
table$Title <- levels(table$Title)[as.numeric(table$Title)]
table$Venue <- levels(table$Venue)[as.numeric(table$Venue)]
table$Abstract <- levels(table$Abstract)[as.numeric(table$Abstract)]
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
remove <- c("id","Title","Author","Year","Venue","Citations","Abstract")
docTerm <- docTerm[, -which(names(docTerm) %in% remove)]

table <- table[, -which(names(table) %in% "id")]
docTerm$Article_id <- levels(docTerm$Article_id)[as.numeric(docTerm$Article_id)]

# Create Term/document Matrix ------------------------------
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
  for (j in strsplit(i, ","))
    authors <- append(authors,j)

authors <- unique(trimws(authors))
authors <- data.frame("author_name" = (authors))
authors$author_name <- levels(authors$author_name)[as.numeric(authors$author_name)]
authors$id = seq.int(nrow(authors))

#Merge with #Publications
authors_ <- unlist(strsplit(table$Author,","))
authors_pub <- as.data.frame(table(trimws(authors_)))
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

# ------------------------- Select Venues ------------------------------

SIGIR <- table[startsWith(table$Venue,"SIGIR"), ]
SIGMOD <- table[startsWith(table$Venue,"SIGMOD"), ]
STOC <- table[startsWith(table$Venue,"STOC"), ]

# ------------------------- Clustering ------------------------------

clustering <- as.data.frame(kmeans(docTerm, 10)$cluster)
names(clustering) <- "Cluster"

library(igraph)

graph <- graph(as.matrix(docTerm),directed=F)
fastgreedy.community(as.undirected(graph))

community <- fastgreedy.community(g)
community$membership



g1 <- graph( edges=c(1,3, 2,3, 3, 1), n=3, directed=F ) 
plot(g1,vertex.label=NA)



termDocMatrix <- termDoc
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- as.matrix(termDocMatrix) %*% t(as.matrix(termDocMatrix))
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)

g


# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


table[(table$Abstract == "~~"),"Venue"]
