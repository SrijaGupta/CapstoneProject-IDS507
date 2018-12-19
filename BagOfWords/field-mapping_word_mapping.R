library(readxl)

schema<-read_xlsx(file.choose()) # 
data1<-read_xlsx(file.choose()) # 
#data2<-read.csv(file.choose(),header = T,stringsAsFactors = F) # 

col.name<-schema[schema$TableName=='Name',"ColumnName"]# extract colummn name of schema of a particular table 
num.col.name<-nrow(col.name) # number of column of table particular schema

col.data1<-colnames(data1) # 1st data column names
num.data1<-ncol(data1) # number of column in 1st data

#col.data2<-colnames(data2) # 2nd date column names
#num.data2<-ncol(data2) # number of column in 2nd data

temp.result<-NULL
temp.result <- data.frame(
  SchemaColumn=character(),
  DataColumn=character(),
  DstanceScore=numeric(),
  stringsAsFactors=FALSE)
result <- data.frame(
  SchemaColumn=character(),
  DataColumn=character(),
  DstanceScore=numeric(),
  stringsAsFactors=FALSE)


for(i in 1:num.data1){
  for (j in 1:num.col.name){
    a<-col.data2[i]
    b<-as.character(col.name[j,])
    dist.score<-adist(a,b,ignore.case = T)[1]
    temp.result[j,c(1,2,3)] <- c(b,a,dist.score)
  }
  temp<-temp.result[temp.result$DstanceScore==min(as.numeric(temp.result$DstanceScore)),]
  result<-rbind(result,temp)
}


write.csv(result,file = '/comapared_column.csv' )