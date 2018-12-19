data <- Delta_Delta_Alumni_Database_8_18
data$Year <- as.numeric(data$Year)
year <- data$Year

year[is.na(year)] <- 3111

l <- length(year)

for (row in 1:l)
{
  if(year[row] < 18)
    {
      year[row] <- year[row] + 2000
  }
  else 
  {
    year[row] <- year[row] + 1900
  }
}

data$Year <- year
View(data)

year[year==3011] <- NA

as.factor(data$State)

year_D <- year

for (row in 1:l)
{
  if(year[row] <= 1943)
  {
    year_D[row] <- '1918-43'
  }
  else if(year[row] > 1943 && year[row] <= 1968)
  {
    year_D[row] <- '1944-68'
  }
  else if(year[row] > 1968 && year[row] <= 1993)
  {
    year_D[row] <- '1967-93'
  }
  else if(year[row] > 1993 && year[row] <= 2018)
  {
    year_D[row] <- '1994-2018'
  }
  else
  {
    year_D[row] <- 'NA'
  }
}


View(year_D)

data$year_D <- year_D

write.csv(data, "C:/Study/Fall 2018/Capstone/Data/Delta.csv")


hist(data$Year, main = "Distribution of year", xlab = "VARNAME", col = "steelblue", las = 2, freq = F)
rug(jitter(data$Year), col = "red")
lines(density(data$Year), col = "green", lwd = 2)

data$State <- as.factor(data$State)
prpTbl <- prop.table(table(data$State))
barplot(prpTbl * 100, main = "VARNAME Categories", xlab = "State Category (Levels)", 
        col = c("darkgrey", "darkgoldenrod1"), las = 1)

data$Status <- as.factor(data$Status)
prpTbl <- prop.table(table(data$Status))
barplot(prpTbl * 100, main = "VARNAME Categories", xlab = "State Category (Levels)", 
        col = c("darkgrey", "darkgoldenrod1"), las = 1)














##################---Database2---###########

db2 <- Second_Database

state <- db2$`Business State`
state

tweetsCorpus <- Corpus(VectorSource(state)) 

docs <-tm_map(tweetsCorpus,content_transformer(toupper))

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "#")
docs <- tm_map(docs, toSpace, "<")
docs <- tm_map(docs, toSpace, ">")
docs <- tm_map(docs, toSpace, "@\\w+ *")
docs <- tm_map(docs, toSpace, "&\\w+ *")
docs <- tm_map(docs, toSpace, "[^\x01-\x7F]")
docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
docs <- tm_map(docs, toSpace, ":")

docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)

docs1 <- docs
docs <- docs1

for (row in 1:length(state))
{
  state[row] <- as.character(docs[[row]])
}
state[is.na(state)] <- 'NA'
View(state)

state_map <- state_mappings


for (j in 1:length(state_map$STATE))
{
  pos <- grep(state_map[j,1], state)
  state[pos] <- state_map[j,2]
  
  pos <- grep(state_map[j,2], state)
  state[pos] <- state_map[j,2]
}

status <- state

for (i in 1:length(state))
{
  status[i] <- 0
  for (j in 1:length(state_map$STATE))
 {
    if((state[i] == state_map[j,2]) == TRUE)
    {
    status[i]<- 1
    }
  }
}

View(status)
View(state)

View(state[status==0])

db2$`Home State` <- as.character(state)
db2$`Business State` <- as.character(db2$`Business State`)

write.csv(db2, "C:/Study/Fall 2018/Capstone/Data/db2.csv")
str(db2)


#####----extras----#########
#for (i in 1:length(state))
#{
#  for (j in 1:length(state_map$STATE))
# {
#  if((state[i] == state_map[j,1]) == TRUE)
#  {
#    state[i] <- state_map[j,2]
#  }
#}
#}





