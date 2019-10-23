# ------------------------------ Import Data ---------------------------------

data <- read.csv("/Users/amine/Desktop/Buisiness-Intelligence--master/DBLP_Subset.txt",header = F, stringsAsFactors=FALSE,quote = "")
# train = data.frame(data[1:1000,1])
# ------------------------------ Create Table  ---------------------------------
#data <- data.frame(data[1:20000,])
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
    line[5] = substr(i,7,nchar(i))
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
table$Title <- levels(table$Title)[as.numeric(table$Title)]
table$Review <- levels(table$Review)[as.numeric(table$Review)]
table$Author <- levels(table$Author)[as.numeric(table$Author)]
table$Citations <- levels(table$Citations)[as.numeric(table$Citations)]
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
tdm <- TermDocumentMatrix(corpus)
docTerm <- as.data.frame(t(as.matrix(tdm)))
docTerm$id <- 1:nrow(docTerm)

#echantillon 1.5K
train = table[1:1500,]
corpus <- Corpus(VectorSource(train$Title))
tdm <- TermDocumentMatrix(corpus)
docTerm_train <- as.data.frame(t(as.matrix(tdm)))
docTerm_train$id <- 1:nrow(docTerm_train)
  
# ------------------------- Create Authors Matrix ------------------------------

authors <- c()
for (i in table$Author)
  for (j in strsplit(i, " , "))
    authors <- append(authors,j)
  
authors <- unique(authors)
authors <- data.frame("author_name" = authors)
authors$id = seq.int(nrow(authors))

#Merge with #Publications
authors <- unlist(strsplit(train$Author," , "))
authors_pub <- as.data.frame(table(authors))
colnames(aut) <- c("author_name","Count")
dd = merge(train,citation_table,by = "author_name")
# ------------------------- Export Tables ------------------------------

write.csv(authors,"Athors.csv",row.names = F)
write.csv(table,"Table.csv",row.names = F)
write.csv(docTerm,"DocumentTerm.csv",row.names = F)

#############

write.csv(authors,"train/Athors.csv",row.names = F)
write.csv(train,"train/Table.csv",row.names = F)
write.csv(docTerm_train,"train/DocumentTerm.csv",row.names = F)





