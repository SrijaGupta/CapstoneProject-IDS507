install.packages("plyr")
library(plyr)
install.packages("stringr")
library(stringr)
install.packages("plyr")
require(plyr)
?laply


column_compare <- as.data.frame(column_names_delta$Columns)
column_compare$v2 <- toupper(column_names_delta$Columns)
column_compare$v2 <- str_replace_all(column_compare$v2, "_", " ")
column_compare$v2 <- str_replace_all(column_compare$v2, "-", " ")
column_compare$v2 <- str_replace_all(column_compare$v2, ",", " ")

word.match <- function(sentences,list.words){
  
  scores<-laply(sentences,function(sentence,list.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,list.words)
    
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },list.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

#First Name*********************************************
First_Name_Column <- c("FIRST","FIRSTNAME","FNAME","F","NAME")
test.score<- word.match(column_compare$v2,First_Name_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])

column_order <- as.data.frame(cbind("First_Name",column_compare[column_compare$v2 == name$text,2]))
colnames(column_order)[1]<-'standard'
colnames(column_order)[2]<-'actual'

#Last Name*********************************************
Last_Name_Column <- c("LAST","NAME","LASTNAME","LNAME","L")
test.score<- word.match(column_compare$v2,Last_Name_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("Last_Name",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)

#Email ID*********************************************
Email_Id_Column <- c("EMAIL","MAIL","E","ID","EMAILID","ADDRESS","EMAILADDRESS")
test.score<- word.match(column_compare$v2,Email_Id_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("Email_ID",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)

#City *********************************************
City_Column <- c("CITY", "HOME", "HOMECITY")
test.score<- word.match(column_compare$v2,City_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("City",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)

#State *********************************************
State_Column <- c("STATE","PROVINCE","HOME","HOMESTATE")
test.score<- word.match(column_compare$v2,State_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("State",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)

#Middle Name *********************************************
Middle_Name_Column <- c("MIDDLE","NAME","MI","MIDDLENAME")
test.score<- word.match(column_compare$v2,Middle_Name_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("MiddleName",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)

#Middle Name *********************************************
Functional_Title_Column <- c("FUNCTIONAL","TITLE","POSITION","JOB","JOBTITLE","WORKTITLE","WORK")
test.score<- word.match(column_compare$v2,Functional_Title_Column)
#View(test.score)

name <- as.data.frame(test.score[test.score$score==max(test.score$score),])
temp <- as.data.frame(cbind("Functional_Title",column_compare[column_compare$v2 == name$text,2]))
colnames(temp)[1]<-'standard'
colnames(temp)[2]<-'actual'
column_order <- rbind.data.frame(column_order,temp)


column_order

