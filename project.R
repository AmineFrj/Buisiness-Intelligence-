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
rm(citation_table,citations)

# ------------------------- Create Document/Term Matrix ------------------------------
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
rm(authors_pub)

# ------------------------- Select Venues ------------------------------

SIGIR <- table[startsWith(table$Venue,"SIGIR"), ]
SIGMOD <- table[startsWith(table$Venue,"SIGMOD"), ]
STOC <- table[startsWith(table$Venue,"STOC"), ]

SIG_STOC <- rbind(SIGIR,SIGMOD)
SIG_STOC <- rbind(SIG_STOC,STOC)
rm(SIGIR,SIGMOD,STOC)

# ------------------------- Clustering of the THREE VENUES ------------------------------
corpus <- Corpus(VectorSource(SIG_STOC$Title))
dtm <- removeSparseTerms(DocumentTermMatrix(corpus),0.99)
SIG_STOC_docTerm <- as.data.frame(as.matrix(dtm))

SIG_STOC$id <- 1:nrow(SIG_STOC)
SIG_STOC_docTerm$id <- 1:nrow(SIG_STOC_docTerm)
SIG_STOC_docTerm = merge(SIG_STOC_docTerm,SIG_STOC, by = "id")
remove <- c("id","Title","Author","Year","Venue","Citations","Abstract")
SIG_STOC_docTerm <- SIG_STOC_docTerm[, -which(names(SIG_STOC_docTerm) %in% remove)]
##### Remove ids for clustering 
SIG_STOC_docTerm_NoID <- SIG_STOC_docTerm[,-which(names(SIG_STOC_docTerm) %in% "Article_id")] 

#kmeans
fviz_nbclust::fviz_nbclust(SIG_STOC_docTerm_NoID, stats::kmeans, method='silhouette',k.max = 30)
fviz_nbclust::fviz_nbclust(SIG_STOC_docTerm_NoID, cluster::pam, method='silhouette',k.max = 30)

#kmedoids - PAM
pam <- cluster::pam(SIG_STOC_docTerm_NoID, 16)
clustering <- as.data.frame(pam$clustering)
clustering <- cbind(clustering,SIG_STOC$Article_id)
names(clustering) <- c("Cluster","Article_id")

############### store centers#################################################################################
# medoids <- pam$medoids
# medoids <- as.data.frame(seq.int(nrow(medoids)))
# medoids$k <- pam$id.med
# names(medoids) <- c("k","Article_id")
# 
# medoids = merge(medoids, SIG_STOC[,c("id","Article_id")], by.x = "Article_id", by.y = "id",all.x = TRUE)
# medoids <- medoids[, -which(names(medoids) %in% "Article_id")]
# names(medoids) <- c("Cluster","Article_id")
##############################################################################################################

##### Store Article_Term
Article_Term <- data.frame(matrix(ncol = 2, nrow = 0))
names(Article_Term) <- c("Article_id","Term")
SIG_STOC_docTerm[,"Article_id"] <- levels(SIG_STOC_docTerm[,"Article_id"])[as.numeric(SIG_STOC_docTerm[,"Article_id"])]

for (row in rownames(SIG_STOC_docTerm)) {
  for (col in colnames(SIG_STOC_docTerm)) {
    if (SIG_STOC_docTerm[row,col] != 0){
      Article_Term_tmp <- data.frame(matrix(ncol = 2, nrow = 1))
      names(Article_Term_tmp) <- c("Article_id","Term")
      Article_Term_tmp[1,1] <- SIG_STOC_docTerm[row,"Article_id"]
      Article_Term_tmp[1,2] <- col
      Article_Term <- rbind(Article_Term,Article_Term_tmp)
    }
  }
}

row.names.remove <- c("and", "for","the","from","with","using","Article_id")
Article_Term <- Article_Term[!(Article_Term$Term %in% row.names.remove), ]

# ------------------------- Clustering of ALL DATA ------------------------------
cpy_docTerm <- data.frame(docTerm)

##### Remove ids for clustering 
docTerm_NoID <- docTerm[,-which(names(docTerm) %in% "Article_id")] 

#kmedoids - PAM
factoextra::fviz_nbclust(docTerm_NoID, cluster::pam, method='silhouette',k.max = 30)

pam <- cluster::pam(docTerm_NoID, 16)
clustering <- as.data.frame(pam$clustering)
clustering <- cbind(clustering,SIG_STOC$Article_id)
names(clustering) <- c("Cluster","Article_id")

##### Store Article_Term
Article_Term <- data.frame(matrix(ncol = 2, nrow = 0))
names(Article_Term) <- c("Article_id","Term")
docTerm[,"Article_id"] <- levels(docTerm[,"Article_id"])[as.numeric(docTerm[,"Article_id"])]

for (row in rownames(docTerm)) {
  for (col in colnames(docTerm)) {
    if (docTerm[row,col] != 0){
      Article_Term_tmp <- data.frame(matrix(ncol = 2, nrow = 1))
      names(Article_Term_tmp) <- c("Article_id","Term")
      Article_Term_tmp[1,1] <- docTerm[row,"Article_id"]
      Article_Term_tmp[1,2] <- col
      Article_Term <- rbind(Article_Term,Article_Term_tmp)
    }
  }
}

row.names.remove <- c("and", "for","the","from","with","using","Article_id")
Article_Term <- Article_Term[!(Article_Term$Term %in% row.names.remove), ]

# ------------------------- Word Cloud ------------------------------
library(data.table)
tdm <- removeSparseTerms(TermDocumentMatrix(corpus),0.99)
termDoc <- as.data.frame(as.matrix(tdm))
termFreq <- as.data.frame(rowSums(as.matrix(tdm)))
setDT(termFreq, keep.rownames = TRUE)[]
colnames(termFreq) <- c("Term","Frequence")
#FILTER WORDS
row.names.remove <- c("and", "for","the","from","with","using")
new_termFreq <- termFreq[!(termFreq$Term %in% row.names.remove), ]

library("wordcloud")
set.seed(1234)
wordcloud(words = new_termFreq$Term, freq = new_termFreq$Frequence,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# ------------------------- Citations Community detection ------------------------------

# documents <- unique(unlist(strsplit(SIG_STOC[,"Citations"], " , ")))
# citations = data.frame(matrix(ncol = length(documents), nrow = 0))
# colnames(citations) = documents
# 
# for (row in rownames(SIG_STOC)) {
#   article_tmp <- data.frame(matrix(ncol = length(documents), nrow = 1, data = 0),row.names = SIG_STOC[row,"Article_id"])
#   colnames(article_tmp) = documents
#   if (SIG_STOC[row,"Citations"] != "")
#     for (i in unlist(strsplit(SIG_STOC[row,"Citations"], " , ")))
#       if (i %in% documents)
#         article_tmp[,i] = 1
#   citations <- rbind(citations,article_tmp)
# }
SIG_STOC[,"Article_id"] <- levels(SIG_STOC[,"Article_id"])[as.numeric(SIG_STOC[,"Article_id"])]

documents <- unique(unlist(strsplit(SIG_STOC[,"Citations"], " , ")))
documents <- unique(c(documents,SIG_STOC[,"Article_id"]))
citations = data.frame(matrix(ncol = length(documents), nrow = length(documents), data = 0))
colnames(citations) = documents
row.names(citations) = documents

for (row in rownames(SIG_STOC)) {
  if (SIG_STOC[row,"Citations"] != "")
    for (i in unlist(strsplit(SIG_STOC[row,"Citations"], " , "))){
      citations[i,SIG_STOC[row,"Article_id"]] = 1
      citations[i,SIG_STOC["Article_id",row]] = 1
    }
}

library(igraph)
# build a graph from citations matrix
graph_citations <- graph.adjacency(as.matrix(citations), mode = "undirected")
# remove loops
graph_citations <- simplify(graph_citations)
community_citations <- fastgreedy.community(graph_citations)

# plot(g,community$membership,)
plot(community_citations, graph_citations,vertex.label=NA,vertex.size=2)

# ------------------------- Co-autheur Community detection ------------------------------

SIG_STOC_AUTH <- c(SIG_STOC$Author)
SIG_STOC_AUTH <- unlist(strsplit(SIG_STOC_AUTH,","))
SIG_STOC_AUTH <- as.data.frame(table(trimws(SIG_STOC_AUTH)))
SIG_STOC_AUTH <- levels(SIG_STOC_AUTH$Var1)[as.numeric(SIG_STOC_AUTH$Var1)]

documents <- SIG_STOC[,"Article_id"]
Auth_Pub = data.frame(matrix(ncol = length(documents), nrow = length(SIG_STOC_AUTH), data = 0))
colnames(Auth_Pub) = documents
row.names(Auth_Pub) = SIG_STOC_AUTH

for (row in rownames(SIG_STOC)) {
  if (SIG_STOC[row,"Author"] != "")
    for (i in trimws(unlist(strsplit(SIG_STOC[row,"Author"], ","))))
      Auth_Pub[i,SIG_STOC[row,"Article_id"]] = 1
}

Co_auth <- as.matrix(Auth_Pub) %*% t(as.matrix(Auth_Pub))
# Co_auth[5:10,5:10]

graph_auth <- graph.adjacency(Co_auth, mode = "undirected")
# remove loops
graph_auth <- simplify(graph_auth)
community_auth <- fastgreedy.community(graph_auth)
plot(community_auth, graph_auth,vertex.label=NA,vertex.size=2)

# ------------------------- Export Tables ------------------------------

write.csv(authors,"tables/Athors.csv",row.names = F)
write.csv(table,"tables/Articles.csv",row.names = F)
write.csv(docTerm,"tables/DocumentTerm.csv",row.names = F)
write.csv(new_termFreq,"tables/TermFrequency.csv",row.names = T)
write.csv(clustering,"tables/Clustering.csv",row.names = F)
write.csv(Article_Term,"tables/Article_Term.csv",row.names = F)

################################################################################
# ------------------------- Community detection ----------------------------- /
#############################################################################
# library(igraph)
# 
# graph <- graph(as.matrix(docTerm),directed=F)
# fastgreedy.community(as.undirected(graph))
# 
# community <- fastgreedy.community(g)
# community$membership
#############################################################################

# g1 <- graph( edges=c(1,3, 2,3, 3, 1), n=3, directed=F ) 
# plot(g1,vertex.label=NA)
# 
# 
# termDocMatrix <- termDoc
# # change it to a Boolean matrix
# termDocMatrix[termDocMatrix>=1] <- 1
# # transform into a term-term adjacency matrix
# termMatrix <- as.matrix(termDocMatrix) %*% t(as.matrix(termDocMatrix))
# termMatrix[5:10,5:10]
# 
# library(igraph)
# # build a graph from the above matrix
# g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# # remove loops
# g <- simplify(g)
# 
# # set labels and degrees of vertices
# V(g)$label <- V(g)$name
# V(g)$degree <- degree(g)
# 
# # set seed to make the layout reproducible
# set.seed(1234)
# layout1 <- layout.fruchterman.reingold(g)
# plot(g, layout=layout1)

#############################################################################
#############################################################################


