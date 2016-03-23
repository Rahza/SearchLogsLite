test = data.frame(A = sample(1:5, 10, replace = T)) %>% arrange(A)
test %>% mutate(id=group_indices_(test, .dots="A"))
help(group_indices)
test = test %>% mutate(id = as.numeric(rownames(test))) %>% group_by(A) %>% mutate(id = min(id))
test

library(dplyr)
library(ggplot2)
library(entropy)

"entropy"############
# Week 1.0 #
############
# Read the CSV file
# arguments:
# --- header = TRUE     // The CSV file containts a header
# --- quote = ""        // Disable quoting to prevent errors when a query contains quotes
# --- sep = ","         // The column separator
# --- na = "na"         // The string that should be interpretated as NA (in addition to empty columns)
# --- comment.char = "" // The hash icon # is interpretated as a comment, which results in errors if the query/URL contains it - since we don't use comments in the csv file, disable the comment char
queries = read.table("C:\\SearchLogs\\output\\outputx.csv",header=TRUE,quote="",sep=",",na="na",comment.char="")

epoc = ISOdatetime(1970,1,1,0,0,0);
epoc = epoc +60 *60;

queries$rdate = (queries$epoc/1000)+epoc

############
# Week 1.2 #
############
min.outlier = 0.98 # quantile; values equal or greater are considered outliers

bot.queries = queries[,c("userId", "query", "url", "lastInteraction")]
bot.queries.searches = bot.queries[bot.queries$lastInteraction != 0,] # For every query, only keep the first click
bot.queries.searches.count = bot.queries.searches %>% group_by(userId, query) %>% summarise(count = n()) # for each user, count how often they have searched for the same query

ranking = data.frame(count = bot.queries.searches.count$count)
ranking = ranking %>% arrange(desc(count))
ranking$rank = seq(1:1679194)

plot(ranking[1:10000,]$rank, ranking[1:10000,]$count)

bot.queries.outliers.q = quantile(bot.queries.searches.count$count, min.outlier)
suspicious = bot.queries.searches.count %>% filter(count >= bot.queries.outliers.q) %>% group_by(userId) %>% summarise(queryCount = n()) # create column queryCount with amount of suspicious queries

suspicious = bot.queries.searches.count %>% group_by(userId) %>% summarise(sum = sum(count)) # total number of queries per user
quantile(suspicious$sum, 0.98)
min.outlier = min(boxplot(suspicious$sum)$out) # identify the smallest outlier
suspicious = suspicious %>% filter(sum >= min.outlier) # all the users that are suspicious because of a high query count


suspicious$userId = bot.queries.searches.count[which(bot.queries.searches.count$count>100),]$userId
suspicious$maxQuery = bot.queries.searches.count[which(bot.queries.searches.count$count>100),]$count
suspicious = 


test1 = c(120, 18, 12)
test2 = c(20, 18, 12, 6, 4)
KL.empirical(test1, test2)

############
# Week 2.0 #
############
nrow(queries) # How many entries are in the file?
length(unique(queries$userId)) # How many unique users were recorded?
queries.filtered = queries[queries$lastInteraction != 0,] # For every request, only keep the entry of the first click
nrow(queries.filtered) # How many queries are in the file?

queries.filtered = queries.filtered[queries.filtered$query != "-",]
nrow(queries.filtered)

############
# Week 2.1 #
############
char.vector = as.vector(as.matrix(queries.filtered["query"]))
string.count = sapply(gregexpr("\\S+", char.vector), length)
queries.filtered$count = string.count
char.count = nchar(char.vector, type = "chars")

# Statistics
summary(string.count) # word length summary
summary(char.count)   # char length summary

hist(char.count, breaks=100, freq=F)
hist(char.count[which(char.count<=80)], breaks=seq(0,80,5), freq=F) # Only show values smaller than X

hist(string.count, breaks=100, freq=F)
hist(string.count[which(string.count<11)], breaks=seq(0,10,1), freq=F)

round(prop.table(table(string.count))*100, 1) # http://stackoverflow.com/questions/9623763/in-r-how-can-i-compute-percentage-statistics-on-a-column-in-a-dataframe-tabl

# Which queries do have more than X characters/strings?
char.vector[which(char.count>490)]
char.vector[which(string.count>50)]

############
# Week 2.2 #
############
query.table = table(queries.filtered$query)
query.freq = as.data.frame(query.table)$Freq

hist(query.freq, breaks=100, freq=F)
hist(query.freq[which(query.freq<21)], breaks=seq(0,20,1), freq=T)

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
duplicates = queries %>% arrange(userId, query, epoc) %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc))
duplicates = duplicates %>% filter((diff>0 | is.na(diff)) & position >= 0) %>% group_by(userId) %>% filter(n()>1)
duplicates = duplicates[,c("userId", "query", "epoc", "diff")]
duplicates = duplicates %>% group_by(userId, query) %>% filter(n()>1) # only duplicate queries
duplicates$id = 1:nrow(duplicates)
duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id))
duplicates = duplicates %>% group_by(userId, query) %>% mutate(max = max(id))



duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id)) # save the min ID of each query to later identify the sequence of IDs for each query
rownames(duplicates) = duplicates$id

all.queries = queries %>% arrange(userId, query, epoc) %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) # sort all queries by userId, query and epoc and calculate the difference in time between the same queries

get_group_number = function(start){
  i = start
  function(){
    i <<- i+1
    i
  }
}

group_number = get_group_number(0)
all.queries = all.queries %>% group_by(userId, query, epoc) %>% mutate(id = group_number()) # http://stackoverflow.com/questions/23026145/dplyr-how-to-number-label-data-table-by-group-number-from-group-by

filtered = all.queries %>% filter((diff>0 | is.na(diff)) & position >= 0) %>% group_by(userId) %>% filter(n()>1) # merge queries with multiple clicks into a single query, remove queries with no click and remove users with only one query

querydata = filtered[,c("id", "userId", "query", "epoc", "diff")] # new dataframe with only the relevant columns 

duplicates = querydata %>% group_by(userId, query) %>% filter(n()>1) # new dataframe with only duplicate queries
duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id)) # save the min ID of each query to later identify the sequence of IDs for each query
rownames(duplicates) = duplicates$id

result = data.frame(left = duplicates$id, right = c(tail(duplicates$id, -1), 0), diff = c(tail(duplicates$diff, -1), 0), min = duplicates$min) # match each duplicate query with the subsequent query

get_match = function(min, max) {
  x = min:(max-1)
  return (x[sample(length(x), 1)])
}

result$right = apply(result, 1, function(x) if(is.na(x["diff"])) get_match(x["min"], x["left"]) else return (x["right"]))





duplicates = as.data.frame(duplicates)

calc_diff = function(left, right) {
  return (abs(duplicates[right,]$epoc - duplicates[left,]$epoc))
}

result$diff = apply(result, 1, function(x) if (is.na(x["diff"])) calc_diff(x["left"], x["right"]) else return (x["diff"]))

result = mutate(result, days = round(diff / (1000*60*60*24)))

result.table = table(result$days[which(result$days>0 | is.na(result$days))])
plot(result.table, type="p")

############
# Week 3.2 #
############
uniques = querydata %>% group_by(userId, query) %>% filter(n()==1) # new dataframe with only duplicate queries
uniques = uniques %>% group_by(userId) %>% mutate(min = min(id))
uniques = uniques %>% group_by(userId) %>% mutate(max = max(id))

rownames(uniques) = uniques$id

result.uniques = data.frame(left = uniques$id, right = 0, min = uniques$min, max = uniques$max) # match each duplicate query with the subsequent query

get_match_uniques = function(id, min, max) {
  x = min:max
  x= x[x!=id]
  return (x[sample(length(x), 1)])
}

result.uniques$right = apply(result.uniques, 1, function(x) get_match_uniques(x["left"], x["min"], x["max"]))


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


##########
# Week 4 #
##########
topics = read.table("C:\\SearchLogs\\topics\\topics.csv",header=TRUE,quote="",sep="\t",na="na",comment.char="") # read the csv with the data
topics = unique(topics) # remove duplicate entries
topic.data = na.omit(queries[,c("url", "position", "epoc")]) # remove entries with no url (no click)
topic.data = merge(topic.data, topics, by.x = "url", by.y = "url") # merge both dataframes (to each url in topic.data, assign the corresponding topic from topics)
topic.data$class = gsub("Top/([^/]+)\\/.*", "\\1", topic.data$class) # Get the first subcategory from the topic string by using regex
topic.data$class = gsub("Top/", "", topic.data$class) # Remove the Top (won't be removed for topics with only one subcategory)


############
# Week 4.1 #
############


############
# Week 4.2 #
############
pie(table(topic.data$class)) #pie chart of topic distribution

############
# Week 4.2 #
############
topic.data = topic.data %>% mutate(hour = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%H")) # add an hour column (convert epoch time to date first, then extract the hour component)
topic.data = topic.data %>% mutate(weekday = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%a")) # ad a weekday column 

topic.hours = data.frame(table(topic.data$hour, topic.data$class)) # count topics grouped by hour and topic
colnames(topic.hours) = c("hour", "topic", "freq") # rename the columns

ggplot(topic.hours, aes(hour, freq, colour=topic, group=topic)) + geom_line() # plot (distribution of topics based on time)

topic.weekdays = data.frame(table(topic.data$weekday, topic.data$class)) # count topics grouped by hour and topic
colnames(topic.weekdays) = c("weekday", "topic", "freq") # rename the columns

ggplot(topic.weekdays, aes(weekday, freq, colour=topic, group=topic)) + geom_line() # plot (distribution of topics based on time)


##########
# Week 5 #
##########



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

