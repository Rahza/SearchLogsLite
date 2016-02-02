queries = read.csv("C:\\SearchLogs\\output\\output.csv",header=TRUE,sep=",")

epoc = ISOdatetime(1970,1,1,0,0,0);
epoc = epoc +60 *60;

queries$rdate = (queries$epoc/1000)+epoc

############
# Week 2.1 #
############
char.vector = as.vector(as.matrix(queries["query"]))
string.count = sapply(gregexpr("\\S+", char.vector), length)
char.count = nchar(char.vector, type = "chars")

# Statistics
summary(char.count)   # char length summary
summary(string.count) # word length summary

hist(char.count, breaks=100, freq=F)
hist(char.count[which(char.count<=80)], breaks=seq(0,80,5), freq=F) # Only show values smaller than X

hist(string.count, breaks=100, freq=F)
hist(string.count[which(string.count<11)], breaks=seq(0,10,1), freq=F)

# Which queries do have more than X characters/strings?
char.vector[which(char.count>490)]
char.vector[which(string.count>50)]

############
# Week 2.2 #
############
query.table = table(queries$query)
query.freq = as.data.frame(query.table)$Freq

hist(query.freq, breaks=100, freq=F)
hist(query.freq[which(query.freq<21)], breaks=seq(0,20,1), freq=F)

sort(query.table,decreasing=TRUE)[1:20] # Top X queries

############
# Week 2.3 #
############
sessions = as.numeric(table(queries$sessionId))
hist(sessions, freq=F)
hist(sessions[which(sessions<16)], breaks=seq(0,15,1), freq=F)
summary(sessions)

# MISSING: Time of sessions?
sessions.time = data.frame(as.numeric(unique(queries$sessionId)))
sessions.time$duration = tapply(queries$epoc, queries$sessionId, function(x) (max(x)-min(x))/1000)

summary(sessions.time$duration)
summary(sessions.time$duration[which(sessions.time$duration>0)])

hist(sessions.time$duration, freq=F)
hist(sessions.time$duration[which(sessions.time$duration<5000)], freq=F)


############
# Week 2.4 #
############
position.data = queries$position[which(queries$position<=500)] # data corrupted
position.data.clicks = position.data[which(position.data>0)]

sum(position.data>0)/length(position.data) # how many queries click on a result

summary(position.data)

hist(position.data.clicks, breaks=100, freq=F)
hist(position.data.clicks[which(position.data.clicks<30)], breaks=seq(0,30,1), freq=F)


############
# Week 2.5 #
############


############
# Week 3.1 #
############

#### TODO: Queries mit exakt gleicher Zeit nur einmal 
#### Letztes query immer matchen mit zufälligem zwischen min und max


library(dplyr)

all.queries = queries %>% arrange(userId, query, epoc) # sort all queries by userId, query and epoc
all.queries$id = as.numeric(rownames(all.queries)) # create id column
all.queries = all.queries %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) # calculate the difference in time between the same queries

# duplicates = mutate(duplicates, diff = epoc-lag(epoc)) # difference between queries
# duplicates = mutate(duplicates, diff = epoc-min(epoc))  # difference to first query

all.queries = all.queries %>% group_by(userId, query, epoc) %>% mutate(id = min(id)) # unique ID for queries with multiple result clicks (same query by the same user at the same time)

querydata = all.queries[,c("id", "userId", "query", "epoc", "diff")] # new dataframe with only the relevant columns 
duplicates = querydata %>% filter(diff>0 | is.na(diff)) # remove multiple occurences of queries with multiple result clicks
duplicates = duplicates %>% group_by(userId, query) %>% filter(n()>1) # new dataframe with only duplicate queries
duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id))

result = data.frame(left = duplicates$id, right = c(tail(duplicates$id, -1), 0), diff = c(tail(duplicates$diff, -1), 0), min = duplicates$min) # match each duplicate query with the subsequent query
result$right = apply(result, 1, function(x) if(is.na(x["diff"])) get_match(x["min"], x["left"]) else return (x["right"]))

unmachted = result[which(is.na(result$diff)),]

unmatched$right = apply(unmatched, 1, function(x) get_match(x["min"], x["left"]))

get_match = function(min, max) {
  x = min:(max-1)
  return (x[sample(length(x), 1)])
}

multiple.click.ids = (duplicates %>% filter(diff == 0))$id
duplicates = subset(duplicates, ! id %in% multiple.click.ids)

max_values = duplicates %>% group_by(userId, query) %>% filter(id == max(id))
remove_values = c(max_values$id, multiple.click.ids)

# maxid = max(duplicates$id)
maxid = as.numeric(count(duplicates))

duplicates$diff = c(tail(duplicates$diff, -1), 0)
duplicates = subset(duplicates, ! id %in% max_values)



result = subset(result, ! left %in% remove_values)
result = mutate(result, diff = tail(duplicates$diff, -1))
result = mutate(result, days = round(diff / (1000*60*60*24)))

result.table = table(result$days[which(result$days>0 | result$days == NA)])
plot(result.table, type="p")

############
# Week 3.2 #
############
all.queries = queries[,c("userId", "query", "url", "epoc")]
all.queries$id = as.numeric(rownames(all.queries))

all.queries = mutate(all.queries, diff = epoc-lag(epoc))
## don't delete false positives
all.queries = all.queries %>% filter(diff > 0) # remove queries with diff 0

duplicates = all.queries %>% group_by(userId, query) %>% filter(n()>1)
duplicates.count = count(duplicates)

uniques = uniques %>% mutate(pair = sample(querydata$id[which(querydata$userId == userId)], 1))
uniques = querydata %>% group_by(userId, query) %>% filter(n()==1)


single_values = uniques$id

##################
# Week 3.1 (OLD) #
##################
library(dplyr)

querydata = queries[,c("userId", "query", "epoc")]

duplicates = querydata[querydata$query %in% querydata$ID[duplicated(querydata$ID)],]

duplicates = querydata %>% group_by(userId, query) %>% filter(n()>1)
duplicates = arrange(duplicates, userId, query, epoc)
duplicates$id = as.numeric(rownames(duplicates))

max_values = duplicates %>% group_by(userId, query) %>% filter(id == max(id))
min_values = duplicates %>% group_by(userId, query) %>% filter(id == min(id))

querypairs = data.frame(min_values[,c(1,2,4)], max_values$id)

colnames(querypairs)[3:4] = c("min", "max")

duplicates = mutate(duplicates, diff = epoc-lag(epoc))

shift = function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

duplicates$diff = shift(duplicates$diff, 1)

maxid = max(duplicates$id)

result = data.frame(
  left = 1:(maxid-1),
  right = 2:maxid,
  diff = head(duplicates$diff, -1)
)

result = subset(result, ! left %in% querypairs$max)

result = do.call(rbind, apply(querypairs, 1, function(x) get_pairs(as.numeric(x["min"]), as.numeric(x["max"]))))
rownames(result) = NULL

get_pairs = function(min, max) {
  left = min:(max-1)
  right = (min+1):max
  result = data.frame(left, right)
  return (result)
}



# MATRIX WORK
queries$rday = format(queries$rdate,"%d %b %Y")
dates=seq(min(queries$rdate),length.out=max(queries$rdate)-min(queries$rdate), by = "1 day")
days = format(dates,"%d %b %Y")

matrix = read.csv("C:\\SearchLogs\\output\\matrix.csv",header=TRUE,sep=",")
dayCounts <- matrix[,-1]
rownames(dayCounts) <- matrix[,1]

max <- apply(dayCounts, 1, max)
mean <- apply(dayCounts, 1, mean)
median <- apply(dayCounts, 1, median)
sd <- apply(dayCounts, 1, sd)
extremeDays <- apply(dayCounts, 1, function(x) length(x[x>=50]))

results <- data.frame(max, mean, median, sd, extremeDays)

interactions = read.csv("C:\\SearchLogs\\output\\interactions.csv",header=TRUE,sep=",")

