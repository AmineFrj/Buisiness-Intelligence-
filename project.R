data = read.csv("/Users/ferdjaouiamine/Downloads/Buisiness-Intelligence--master/DBLP_Subset.txt",header = F, stringsAsFactors=FALSE)
train = data.frame(data[1:1000,1])

table = data.frame()
line = c("","","","~~","")

for(i in data[,1]){
  if(startsWith(i,"#*")){
    table <- rbind(table, data.frame("Title" = line[1],"Author" = line[2],"Year" = line[3],"Review" = line[4],"Citations" = line[5]))
    line = c("","","","~~","")
    line[1] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#@")){
    line[2] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#t")){
    line[3] = substr(i,3,nchar(i))
  }
  else if(startsWith(i,"#c")){
    line[4] = substr(i,7,nchar(i))
  }
  else if(startsWith(i,"#i")){
    line[5] = substr(i,7,nchar(i))
  }
  else if(startsWith(i,"#%")){
    line[5] = paste(line[5],",",substr(i,3,nchar(i)))
  }
  else if(startsWith(i,"#!")){
    line[4] = substr(i,3,nchar(i))
  }
  else {
    if(startsWith(line[4],"~~")){
      line[2] = paste(line[2],",",i)
    }
    else 
      line[4] = paste(line[4],"",i)
  }
}

str(table)

