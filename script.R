#### SOMMERZEIT: lastInteraction erstes nach umstellung + 1h!!!
#### QUERIES RICHTIG FILTERN!!!! AUCH IN DOC ÄNDERN!!!

library(dplyr)
library(ggplot2)
library(entropy)
library(stringr)

############
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
bot.queries = queries[,c("userId", "query", "url", "lastInteraction")]
bot.queries.searches = bot.queries[bot.queries$lastInteraction != 0,] # For every query, only keep the first click
bot.queries.searches = bot.queries.searches %>% group_by(userId) %>% filter(n() >= 100)
bot.queries.searches.count = bot.queries.searches %>% group_by(userId, query) %>% summarise(count = n()) # for each user, count how often they have searched for the same query

table(queries$userId, queries$query)

dist = as.data.frame(table(bot.queries.searches.count[1,]$query))
dist = dist %>% mutate(p = Freq/n())

bot.queries.searches.quantiles = bot.queries.searches.count %>% group_by(userId) %>% summarise(q10 = quantile(count, 0.1), q20 = quantile(count, 0.2), q30 = quantile(count, 0.3), q40 = quantile(count, 0.4), q50 = quantile(count, 0.5), q60 = quantile(count, 0.6), q70 = quantile(count, 0.7), q80 = quantile(count, 0.8), q90 = quantile(count, 0.9))
average.quantiles = bot.queries.searches.count %>% ungroup %>% summarise(q10 = quantile(count, 0.1), q20 = quantile(count, 0.2), q30 = quantile(count, 0.3), q40 = quantile(count, 0.4), q50 = quantile(count, 0.5), q60 = quantile(count, 0.6), q70 = quantile(count, 0.7), q80 = quantile(count, 0.8), q90 = quantile(count, 0.9))

bot.queries.searches.quantiles$KL = apply(bot.queries.searches.quantiles, 1, function(x) KL.empirical(average.quantiles[1,], x[-1]))

summary(bot.queries.searches.quantiles$KL)
boxplot(bot.queries.searches.quantiles$KL)

top.whisker = boxplot(bot.queries.searches.quantiles$KL)$stats[5,]

suspicious = bot.queries.searches.quantiles %>% filter(KL > top.whisker)


### LAST INTERACTION
bot.queries.lastInteraction = bot.queries.searches %>% group_by(userId) %>% summarise(q10 = quantile(lastInteraction, 0.1), q20 = quantile(lastInteraction, 0.2), q30 = quantile(lastInteraction, 0.3), q40 = quantile(lastInteraction, 0.4), q50 = quantile(lastInteraction, 0.5), q60 = quantile(lastInteraction, 0.6), q70 = quantile(lastInteraction, 0.7), q80 = quantile(lastInteraction, 0.8), q90 = quantile(lastInteraction, 0.9))
average.lastInteraction.quantiles = bot.queries.searches %>% ungroup %>% summarise(q10 = quantile(lastInteraction, 0.1), q20 = quantile(lastInteraction, 0.2), q30 = quantile(lastInteraction, 0.3), q40 = quantile(lastInteraction, 0.4), q50 = quantile(lastInteraction, 0.5), q60 = quantile(lastInteraction, 0.6), q70 = quantile(lastInteraction, 0.7), q80 = quantile(lastInteraction, 0.8), q90 = quantile(lastInteraction, 0.9))
bot.queries.lastInteraction$KL = apply(bot.queries.lastInteraction, 1, function(x) KL.empirical(average.lastInteraction.quantiles[1,], x[-1]))

summary(bot.queries.lastInteraction$KL)
boxplot(bot.queries.lastInteraction$KL)

top.whisker.lastInteraction = boxplot(bot.queries.lastInteraction$KL)$stats[5,]
suspicious.lastInteraction = bot.queries.lastInteraction %>% filter(KL > top.whisker.lastInteraction)


#####################

min.outlier = 0.98 # quantile; values equal or greater are considered outliers

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
query.count = queries.filtered %>% group_by(query) %>% summarise(count = n())
count.vector = query.count$count

summary(count.vector)

hist(count.vector, breaks=100, freq=F)
hist(count.vector[count.vector<21], breaks=seq(0,20,1), freq=F)

length(count.vector[count.vector == 1])/length(count.vector) # how many queries were searched only once?
length(count.vector[count.vector == 2])/length(count.vector) # how many queries were searched twice?
length(count.vector[count.vector == 3])/length(count.vector) # how many queries were searched three times?
length(count.vector[count.vector > 3])/length(count.vector) # how many queries were searched more than three times?

############
# Week 2.3 #
############


#### 10 MIN THRESHOLD!!!


sessions = queries.filtered %>% group_by(sessionId) %>% summarise(session_length = (max(epoc)-min(epoc))/1000, queries = n())
sessions = sessions %>% mutate(minutes = session_length/60)

summary(sessions$session_length) # statistics regarding session length (time)

summary(sessions$queries) # statistics regarding session length (number of queries)
length(sessions$queries[which(sessions$queries == 1)])/length(sessions$queries) # how many percent of sessions consist of only one query

hist(sessions$session_length[which(sessions$session_length < 7500)], freq=F)
hist(sessions$minutes[which(sessions$minutes < 40)], breaks=seq(0, 40, 1), freq=F)

hist(sessions$queries[which(sessions$queries < 20)], breaks=seq(0, 20, 1), freq=F)

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
position.data = queries$position
position.data.filtered = queries$position[which(queries$lastInteraction != 0)] # remove multiple clicks for the same query

position.data.clicks = position.data[position.data > 0]
position.data.clicks.filtered = position.data.filtered[position.data.filtered > 0]

length(position.data.clicks.filtered)/length(position.data.filtered) # how many queries click on a result

summary(position.data.clicks)

length(position.data.clicks[position.data.clicks == 1])/length(position.data.clicks) # of all clicks, how much percent click on the very first result
ecdf(position.data.clicks)(10) # percentile of "10" in the distribution of position.data.clicks (https://stat.ethz.ch/pipermail/r-help/2012-March/305368.html)

summary(position.data.clicks)

# hist(position.data.clicks, breaks=100, freq=F)
hist(position.data.clicks[which(position.data.clicks<20)], breaks=seq(0,20,1), freq=F) # page 1 and 2
hist(position.data.clicks[which(position.data.clicks>0 & position.data.clicks<=10)], breaks=seq(1, 10,1), freq=F) # page 1
hist(position.data.clicks[which(position.data.clicks>10 & position.data.clicks<=20)], breaks=seq(11, 20,1), freq=F) # page 2
hist(position.data.clicks[which(position.data.clicks>20 & position.data.clicks<=30)], breaks=seq(21, 30,1), freq=F) # page 3


############
# Week 2.5 #
############
test = queries %>% group_by(userId, query, url) %>% filter(n() > 1 & position != -1)


#################
# BOT DETECTION #
#################
bot.queries = read.table("C:\\SearchLogs\\output\\output_session10.csv",header=TRUE,quote="",sep=",",na="na",comment.char="")

epoc = ISOdatetime(1970,1,1,0,0,0);
epoc = epoc +60 *60;

bot.queries$rdate = (bot.queries$epoc/1000)+epoc

bot.queries = bot.queries %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) %>% ungroup() %>% filter(is.na(diff) | diff > 0)
bot.queries$day = format(bot.queries$rdate,"%d-%m")
bot.queries$minute = format(bot.queries$rdate,"%d-%m %H:%M")

getRLE = function(x) {
  return (max(rle(x)$lengths))
}

bot.daycount = bot.queries %>% filter(query != "-") %>% group_by(userId, day) %>% summarise(count = n()) %>% ungroup() %>% group_by(userId) %>% summarise(max_day_count = max(count))
bot.minutecount = bot.queries %>% group_by(userId, minute) %>% summarise(count = n()) %>% ungroup() %>% group_by(userId) %>% summarise(max_minute_count = max(count))
bot.interval = bot.queries %>% filter(lastInteraction >= 0) %>% group_by(userId) %>% summarise(min_interval = min(lastInteraction))
bot.querycount = bot.queries %>% group_by(userId,query) %>% summarise(count = n()) %>% ungroup() %>% group_by(userId) %>% summarise(max_query_count = max(count)) # maximum amount of repetitions of the same query for each user
bot.periodicity = bot.queries %>% filter(query != "-") %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) %>% ungroup() %>% group_by(userId, query) %>% summarise(count = getRLE(diff)) %>% ungroup() %>% group_by(userId) %>% summarise(max_periodicity_count = max(count))
bot.sessions = bot.queries %>% group_by(userId, sessionId) %>% summarise(session_length = (max(epoc)-min(epoc))/60000) %>% ungroup() %>% group_by(userId) %>% summarise(max_session_length = max(session_length))

library(plyr)
bot.detection = join_all(list(bot.daycount, bot.minutecount, bot.interval, bot.querycount, bot.periodicity, bot.sessions), by="userId", type="full")
detach("package:plyr", unload=TRUE) 

get_bot_tag = function(x, strong_bot, bot, human) {
  if (is.na(x)) return ("UNKNOWN")
  else if (x > strong_bot) return ("STRONG_BOT")
  else if (x > bot) return ("NORMAL_BOT")
  else if (x < human) return ("HUMAN")
  else return ("UNKNOWN")
}

get_bot_tag_min = function(x, strong_bot, bot, human) {
  if (is.na(x)) return ("UNKNOWN")
  else if (x < bot) return ("NORMAL_BOT")
  else if (x > human) return ("HUMAN")
  else return ("UNKNOWN")
}

bots = data.frame(
  userId = bot.detection$userId,
  max_day_count = apply(bot.detection, 1, function(x) get_bot_tag(x["max_day_count"], 200, 50, 25)),
  max_minute_count = apply(bot.detection, 1, function(x) get_bot_tag(x["max_minute_count"], 15, 10, 6)),
  min_interval = apply(bot.detection, 1, function(x) get_bot_tag_min(x["min_interval"], 0, 1000, 9000)),
  max_query_count = apply(bot.detection, 1, function(x) get_bot_tag(x["max_query_count"], 150, 30, 10)),
  max_periodicity_count = apply(bot.detection, 1, function(x) get_bot_tag(x["max_periodicity_count"], 6, 3, 2)),
  max_session_length = apply(bot.detection, 1, function(x) get_bot_tag(x["max_session_length"], 600, 35, 20))
)

bots.count = data.frame(userId = bots$userId, human = apply(bots, 1, function(x) return (sum(str_count(x, pattern="HUMAN")))), strong_bot = apply(bots, 1, function(x) return (sum(str_count(x, pattern="STRONG_BOT")))), bot = apply(bots, 1, function(x) return (sum(str_count(x, pattern="NORMAL_BOT")))), unknown = apply(bots, 1, function(x) return (sum(str_count(x, pattern="UNKNOWN")))))

get_bot_class = function(human, strong_bot, bot) {
  if (strong_bot >= 1) return ("BOT")
  else if (human > 0 && bot == 0) return ("HUMAN")
  else if (bot > 0 && human == 0) return ("BOT")
  else return ("UNKNOWN")
}

bots.count$class = apply(bots.count, 1, function(x) get_bot_class(x["human"], x["strong_bot"], x["bot"]))
View(bot.queries %>% filter(userId == 30011) %>% group_by(userId, minute) %>% summarise(count = n()))
View(as.data.frame(bots.count[which(bots.count$class == "BOT"),]$userId))

table(bots.count$class)
(435)/59105
(802+33)/65515

View(bot.queries[which(bot.queries$userId == 30011),])

############
# Week 3.1 #
############
filtered = queries %>% arrange(userId, query, epoc) %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) # sort all queries by userId, query and epoc and calculate the difference in time between the same queries
filtered$id_overall = 1:nrow(filtered)

lookup = filtered %>% group_by(userId, query, epoc) %>% mutate(min = min(id_overall))
lookup = data.frame(id = lookup$min, url = lookup$url)
lookup = na.omit(lookup)
lookup$url = as.character(lookup$url)
lookup.list = split(lookup$url, lookup$id, drop=FALSE)
lookup.env = list2env(lookup.list)

filtered = filtered %>% filter((diff>0 | is.na(diff)) & position >= 0) %>% group_by(userId) %>% filter(n()>1) # merge queries with multiple clicks into a single query, remove queries with no click and remove users with only one query

querydata = filtered[,c("id_overall", "userId", "query", "url", "epoc", "diff")] # new dataframe with only the relevant columns

duplicates = querydata %>% group_by(userId, query) %>% filter(n()>1) # only duplicate queries
duplicates$id = 1:nrow(duplicates)
duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id))

duplicates$pair = c(tail(duplicates$id, -1), NA)
duplicates$diff = c(tail(duplicates$diff, -1), NA)

get_match = function(min, max) {
  x = min:(max-1)
  return (as.numeric(x[sample(length(x), 1)])) # http://stackoverflow.com/questions/13990125/sampling-in-r-from-vector-of-varying-length
}

duplicates$pair = apply(duplicates, 1, function(x) if(is.na(x["diff"])) get_match(x["min"], as.numeric(x["id"])) else return (x["pair"]))
duplicates$pair = as.numeric(duplicates$pair)

calc_diff = function(left, right) {
  return (abs(duplicates[right,]$epoc - duplicates[left,]$epoc))
}

duplicates$diff = apply(duplicates, 1, function(x) if (is.na(x["diff"])) calc_diff(as.numeric(x["id"]), as.numeric(x["pair"])) else return (x["diff"]))

duplicates = mutate(duplicates, days = round(as.numeric(diff) / (1000*60*60*24)))

plot(table(duplicates$days), type="p")
plot(table(duplicates$days[which(duplicates$days>1)]), type="p")
plot(table(duplicates$days[which(duplicates$days>8 & duplicates$days<49)]), type="p")

get_overall_id = function(id) {
  return (duplicates[id,]$id_overall)
}

result = data.frame(left = duplicates$id_overall, right = get_overall_id(duplicates$pair))
result$left = as.character(result$left)
result$right = as.character(result$right)

get_click_result = function(a, b) {
  a = unique(get(a, envir = lookup.env))
  b = unique(get(b, envir = lookup.env))
  intersect = length(intersect(a, b))
  if (intersect == 0) return ("no_common")
  else if (length(a) == 1 && length(b) == 1 && intersect == 1) return ("single_identical")
  else if (length(a) == length(b) && intersect == length(a)) return ("multiple_identical")
  else return ("some_common")
}

result$class = apply(result, 1, function(x) get_click_result(x["left"], x["right"]))

table(result$class)


############
# Week 3.1 #
############
library(dplyr)
filtered = queries %>% arrange(userId, query, epoc) %>% group_by(userId, query) %>% mutate(diff = epoc-lag(epoc)) # sort all queries by userId, query and epoc and calculate the difference in time between the same queries
filtered = filtered %>% ungroup() %>% group_by(userId, query, epoc) %>% summarise(clicks = n(), unique_clicks = length(unique(url)))

filtered = filtered %>% filter((diff>0 | is.na(diff)) & position >= 0) %>% group_by(userId) %>% filter(n()>1) # merge queries with multiple clicks into a single query, remove queries with no click and remove users with only one query

querydata = filtered[,c("userId", "query", "url", "epoc", "diff")] # new dataframe with only the relevant columns

duplicates = querydata %>% group_by(userId, query) %>% filter(n()>1) # only duplicate queries
duplicates$id = 1:nrow(duplicates)
duplicates = duplicates %>% group_by(userId, query) %>% mutate(min = min(id))

duplicates$pair = c(tail(duplicates$id, -1), NA)
duplicates$diff = c(tail(duplicates$diff, -1), NA)

get_match = function(min, max) {
  x = min:(max-1)
  return (as.numeric(x[sample(length(x), 1)])) # http://stackoverflow.com/questions/13990125/sampling-in-r-from-vector-of-varying-length
}

duplicates$pair = apply(duplicates, 1, function(x) if(is.na(x["diff"])) get_match(x["min"], as.numeric(x["id"])) else return (x["pair"]))
duplicates$pair = as.numeric(duplicates$pair)

calc_diff = function(left, right) {
  return (abs(duplicates[right,]$epoc - duplicates[left,]$epoc))
}

duplicates$diff = apply(duplicates, 1, function(x) if (is.na(x["diff"])) calc_diff(as.numeric(x["id"]), as.numeric(x["pair"])) else return (x["diff"]))

duplicates = mutate(duplicates, days = round(as.numeric(diff) / (1000*60*60*24)))

plot(table(duplicates$days), type="p")
plot(table(duplicates$days[which(duplicates$days>1)]), type="p")
plot(table(duplicates$days[which(duplicates$days>14 & duplicates$days<80)]), type="p")

duplicates$pair_url = as.character(duplicates[duplicates$pair,]$url)
duplicates$url = as.character(duplicates$url)

result = duplicates %>% group_by(userId, query, url) %>% summarise(clicks = n())
result = result %>% group_by(userId, query) %>% summarise(count = n(), clicksum = sum(clicks), min = min(clicks))

get_click_result = function(sum, count, min) {
  if (count == 1) return ("single_identical")
  else if (min > 1) return ("multiple_identical")
  else if (count < sum) return ("some_common")
  else if (count == sum) return ("no_common")
  else return("error")
}

result$click_category = apply(result, 1, function(x) get_click_result(as.numeric(x["clicksum"]), as.numeric(x["count"]), as.numeric(x["min"])))

table(result$click_category)
nrow(result)




#duplicates = duplicates %>% mutate(match = (url == pair_url))
#result = duplicates %>% group_by(userId, query) %>% summarise(sum = sum(match), count = n())

#result = data.frame(userId = duplicates$userId, query = duplicates$query, left = duplicates$id, right = c(tail(duplicates$id, -1), 0), diff = c(tail(duplicates$diff, -1), 0), min = duplicates$min) # match each duplicate query with the subsequent query

#get_match = function(min, max) {
#  x = min:(max-1)
  #return (x[sample(length(x), 1)]) # http://stackoverflow.com/questions/13990125/sampling-in-r-from-vector-of-varying-length
#  
#}
#
#result$right = apply(result, 1, function(x) if(is.na(x["diff"]) || x["diff"] == 0) get_match(x["min"], x["left"]) else return (x["right"]))
#
#calc_diff = function(left, right) {
  #return (abs(duplicates[right,]$epoc - duplicates[left,]$epoc))
#}
#
#result$diff = apply(result, 1, function(x) if (is.na(x["diff"])) calc_diff(x["left"], x["right"]) else return (x["diff"]))
#
#result = mutate(result, days = round(diff / (1000*60*60*24)))
#
#plot(table(result$days), type="p")
#plot(table(result$days[which(result$days>1)]), type="p")
#plot(table(result$days[which(result$days>14 & result$days<80)]), type="p")

############
# Week 3.2 #
############
uniques = querydata %>% group_by(userId, query) %>% filter(n()==1) # new dataframe with only unique queries
uniques = uniques %>% group_by(userId) %>% filter(n()>1) # remove users with only one unique query
uniques$id = 1:nrow(uniques)
uniques = uniques %>% group_by(userId) %>% mutate(min = min(id))
uniques = uniques %>% group_by(userId) %>% mutate(max = max(id))



# result.uniques = data.frame(left = uniques$id, right = 0, min = uniques$min, max = uniques$max)

get_match_uniques = function(id, min, max) {
  x = min:max
  x = x[x!=id]
  return (x[sample(length(x), 1)]) # http://stackoverflow.com/questions/13990125/sampling-in-r-from-vector-of-varying-length
}

uniques$pair = apply(result.uniques, 1, function(x) get_match_uniques(x["left"], x["min"], x["max"]))

# result.uniques$right = apply(result.uniques, 1, function(x) get_match_uniques(x["left"], x["min"], x["max"]))
result.uniques$right = as.numeric(result.uniques$right)

result.uniques$url_left = as.character(uniques[result.uniques$left,]$url)
result.uniques$url_right = as.character(uniques[result.uniques$right,]$url)

result.uniques = result.uniques %>% mutate(match = (url_left == url_right))

count.all.queries = nrow(querydata)

count.equal.queries = nrow(duplicates)
pct.equal.queries = count.equal.queries / count.all.queries

count.different.queries = nrow(uniques)
pct.different.queries = count.different.queries / count.all.queries



##########
# Week 4 #
##########
topics = read.table("C:\\SearchLogs\\topics\\topics.csv",header=TRUE,quote="",sep="\t",na="na",comment.char="") # read the csv with the data
topics = unique(topics) # remove duplicate entries
topic.data = na.omit(queries[,c("url", "position", "epoc")]) # remove entries with no url (no click)
topic.data = merge(topic.data, topics, by.x = "url", by.y = "url") # merge both dataframes (to each url in topic.data, assign the corresponding topic from topics)
topic.data$class2 = gsub("Top/([^/]+)\\/([^/]+)\\/.*", "\\2", topic.data$class)
topic.data$class2 = gsub("Top/([^/]+)\\/", "", topic.data$class2)
topic.data$class = gsub("Top/([^/]+)\\/.*", "\\1", topic.data$class) # Get the first subcategory from the topic string by using regex
topic.data$class = gsub("Top/", "", topic.data$class) # Remove the Top (won't be removed for topics with only one subcategory)

############
# Week 4.1 #
############
table(topic.data$class)
prop.table(table(topic.data$class))

############
# Week 4.2 #
############
pie(table(topic.data$class), col=rainbow(20, s=.6)) #pie chart of topic distribution
barplot(prop.table(table(topic.data$class)), col=rainbow(20, s=.6), las=2)
dotchart(prop.table(table(topic.data$class)))

get_sub_frame = function(topic, n) {
  result = topic.data %>% filter(class == topic) %>% group_by(class2) %>% summarise(count = n()) %>% ungroup() %>% mutate(pct = count/sum(count))
  other.count = sum(result[which(result$pct <= n),]$count)
  result = result %>% filter(pct > n)
  result = rbind(result[,-3], c("Other", other.count))
  result$count = as.numeric(result$count)
  
  return (result)
}

topics.arts = get_sub_frame("Arts", 0.03)
topics.business = get_sub_frame("Business", 0.03)
topics.computers = get_sub_frame("Computers", 0.03)
topics.games = get_sub_frame("Games", 0.02)
topics.health = get_sub_frame("Health", 0.03)
topics.home = get_sub_frame("Home", 0.03)
topics.news = get_sub_frame("News", 0.03)
topics.recreation = get_sub_frame("Recreation", 0.03)
topics.reference = get_sub_frame("Reference", 0.03)
topics.regional = get_sub_frame("Regional", 0.02)
topics.science = get_sub_frame("Science", 0.03)
topics.shopping = get_sub_frame("Shopping", 0.03)
topics.society = get_sub_frame("Society", 0.03)
topics.sports = get_sub_frame("Sports", 0.03)
topics.world = get_sub_frame("World", 0.03)

barplot(prop.table(topics.arts$count), names.arg=topics.arts$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.business$count), names.arg=topics.business$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.computers$count), names.arg=topics.computers$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.games$count), names.arg=topics.games$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.health$count), names.arg=topics.health$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.home$count), names.arg=topics.home$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.news$count), names.arg=topics.news$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.recreation$count), names.arg=topics.recreation$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.reference$count), names.arg=topics.reference$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.regional$count), names.arg=topics.regional$class2, col=rainbow(20, s=.6))
barplot(prop.table(topics.science$count), names.arg=topics.science$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.shopping$count), names.arg=topics.shopping$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.society$count), names.arg=topics.society$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.sports$count), names.arg=topics.sports$class2, col=rainbow(20, s=.6), las=2)
barplot(prop.table(topics.world$count), names.arg=topics.world$class2, col=rainbow(20, s=.6), las=2)


############
# Week 4.3 #
############
topic.data = topic.data %>% mutate(hour = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%H")) # add an hour column (convert epoch time to date first, then extract the hour component)
topic.data = topic.data %>% mutate(weekday = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%a")) # ad a weekday column 

topic.hours = data.frame(table(topic.data$hour, topic.data$class)) # count topics grouped by hour and topic
colnames(topic.hours) = c("hour", "topic", "freq") # rename the columns
topic.hours = topic.hours %>% mutate(density = freq/sum(freq))
topic.hours = topic.hours %>% group_by(hour) %>% mutate(hour_density = freq/sum(freq))

ggplot(topic.hours, aes(hour, density, colour=topic, group=topic)) + geom_line(size=1) # plot (distribution of topics based on time)
ggplot(topic.hours, aes(hour, hour_density, colour=topic, group=topic)) + geom_line(size=1) # plot (percentage of each topic at each hour)
ggplot(topic.hours %>% filter(topic != "Arts" & topic != "Business" & topic != "Reference"), aes(hour, hour_density, colour=topic, group=topic)) + geom_line(size=1) # plot (percentage of each topic at each hour)

topic.weekdays = data.frame(table(topic.data$weekday, topic.data$class)) # count topics grouped by hour and topic
colnames(topic.weekdays) = c("weekday", "topic", "freq") # rename the columns
topic.weekdays = topic.weekdays %>% mutate(density = freq/sum(freq))
topic.weekdays = topic.weekdays %>% group_by(weekday) %>% mutate(weekday_density = freq/sum(freq))

ggplot(topic.weekdays, aes(weekday, density, colour=topic, group=topic)) + geom_line() + scale_x_discrete(limits=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")) # plot (distribution of topics based on time) http://stackoverflow.com/questions/3253641/how-to-change-the-order-of-a-discrete-x-scale-in-ggplot
ggplot(topic.weekdays, aes(weekday, weekday_density, colour=topic, group=topic)) + geom_line() + scale_x_discrete(limits=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")) # plot (distribution of topics based on time)


##########
# Week 5 #
##########
music = read.table("C:\\SearchLogs\\output\\music_medium.csv",header=TRUE,quote="",sep=",",na="na",comment.char="") # read the csv with the data
music = music %>% mutate (rdate = as.Date(as.POSIXct(epoc/1000, origin="1970-01-01")))
music = music %>% mutate (week = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%W"))

music.d4l = music %>% filter(match == "D4L" | match == "Laffy Taffy" | match == "D4L Laffy Taffy")

plot(table(music.d4l$rdate))
plot(table(music.d4l$week), type="l")


music.neyo = music %>% filter(match == "Ne-Yo" | match == "So Sick" | match == "Ne-Yo So Sick")

plot(table(music.neyo$rdate))
plot(table(music.neyo$week), type="l")


music.powter = music %>% filter(match == "Daniel Powter" | match == "Bad Day" | match == "Daniel Powter Bad Day" | match == "Powter Bad Day")
music.powter = music.powter %>% filter(query != "may day" & query != "daniel j porter" & query != "daniel genter" & query != "daniel porter" & query != "daniel hunter" & query != "daniel obler" & query != "bad mayo" & query != "daniel winter" & query != "rad daly")

plot(table(music.powter$rdate))
plot(table(music.powter$week), type="l")


music.rihanna = music %>% filter(match == "Rihanna" | match == "SOS" | match == "Rihanna SOS")
music.rihanna.sos = music %>% filter(match == "Rihanna SOS" | match == "SOS")
music.rihanna = music.rihanna %>% filter(query != "asos" & query != "sosa" & query != "sons" & query != "rihanna pics" & query != "pirhanna" & query != "wsos" & query != "slos" & query != "sobs" & query != "sous" & query != "sols" & query != "sogs" & query != "soso" & query != "isos")
music.rihanna.sos = music.rihanna.sos %>% filter(query != "asos" & query != "sosa" & query != "sons" & query != "rihanna pics" & query != "pirhanna" & query != "wsos" & query != "slos" & query != "sobs" & query != "sous" & query != "sols" & query != "sogs" & query != "soso" & query != "isos")

plot(table(music.rihanna$rdate))
plot(table(music.rihanna$week), type="l")
lines(table(music.rihanna.sos$week), type="l")


music.cha = music %>% filter(match == "Chamillionaire" | match ==  "Krayzie Bone" | match ==  "Chamillionaire ft Krayzie Bone" | match == "Chamillionaire featuring Krayzie Bone" | match == "Ridin'" | match == "Chamillionaire Ridin'" | match == "Krayzie Bone Ridin'" | match == "Chamillionaire ft Krayzie Bone Ridin'" | match == "Chamillionaire featuring Krayzie Bone Ridin'")
music.cha = music.cha %>% filter(query != "be a millionaire" & query != "abc millionaire" & query != "math millionaire" & query != "play millionaire" & query != "sex millionaire")

plot(table(music.cha$rdate), type="l")
plot(table(music.cha$week), type="l")


music.shakira = music %>% filter(match == "Shakira" | match == "Wyclef Jean" | match == "Shakira featuring Wyclef Jean" | match == "Shakira ft Wyclef Jean" | match == "Hips don't lie" | match == "Shakira Hips don't lie" | match == "Wyclef Jean Hips don't lie" | match == "Shakira featuring Wyclef Jean Hips don't lie" | match == "Shakira ft Wyclef Jean Hips don't lie")

plot(table(music.shakira$rdate), type="l")
plot(table(music.shakira$week), type="l")


music.hicks = music %>% filter(match == "Taylor Hicks" | match == "Do I Make You Proud" | match == "Taylor Hicks Do I Make You Proud" | match == "Hicks Do I Make You Proud")

plot(table(music.hicks$rdate), type="l")
plot(table(music.hicks$week), type="l")

View(as.data.frame(table(music.hicks$query)))

############
# Week 5.2 #
############

#### http://www.billboard.com/charts/hot-100/2006-05-27
march = read.table("C:\\SearchLogs\\output\\music_march.csv",header=TRUE,quote="",sep=",",na="na",comment.char="") # read the csv with the data
march = march %>% mutate (rdate = as.Date(as.POSIXct(epoc/1000, origin="1970-01-01")))
march = march %>% mutate (week = strftime((as.POSIXct(epoc/1000, origin="1970-01-01")), format="%W"))
march$week = as.numeric(march$week)

music.blunt = march %>% filter(match == "James Blunt" | match == "You're Beautiful" | match == "James Blunt You're Beautiful" | match == "Blunt You're Beautiful" | match == "James Blunt Beautiful" | match == "Blunt Beautiful")
music.blunt = music.blunt %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.blunt$position = c(1, 3, 3, 4, 4, 3, 4, 7, 8, 10, 12, 14)
plot(music.blunt$week, music.blunt$position, type="l")
plot(music.blunt$week, music.blunt$count, type="l")
cor(music.blunt$count, music.blunt$position)

music.beyonce = march %>% filter(match == "Check On It" | match == "Beyonce" | match == "Slim Thug" | match == "Beyonce ft Slim Thug" | match == "Beyonce featuring Slim Thug" | match == "Beyonce Check On It" | match == "Slim Thug Check On It" | match == "Beyonce ft Slim Thug Check On It" | match == "Beyonce featuring Slim Thug Check On It")
music.beyonce = music.beyonce %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.beyonce$position = c(2, 5, 8, 10, 12, 15, 19, 27, 35, 36, 43, 48)
plot(music.beyonce$week, music.beyonce$position, type="l")
plot(music.beyonce$week, music.beyonce$count, type="l")
cor(music.beyonce$count, music.beyonce$position)

music.sean = march %>% filter(match == "Sean Paul" | match == "Temperature" | match == "Sean Paul Temperature")
music.sean = music.sean %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.sean$position = c(3, 2, 2, 1, 2, 2, 2, 2, 2, 3, 3, 4)
plot(music.sean$week, music.sean$position, type="l")
plot(music.sean$week, music.sean$count, type="l")
cor(music.sean$count, music.sean$position)

music.nelly = march %>% filter(match == "Grillz" | match == "Nelly featuring Paul Wall | match == Ali & Gipp" | match == "Nelly ft Paul Wall | match == Ali & Gipp" | match == "Nelly Grillz")
music.nelly = music.nelly %>% filter(query != "gorillaz")
music.nelly = music.nelly %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.nelly$position = c(4, 7, 11, 12, 14, 19, 22, 31, 36, 40, 47, 50)
plot(music.nelly$week, music.nelly$position, type="l")
plot(music.nelly$week, music.nelly$count, type="l")
cor(music.nelly$count, music.nelly$position)

music.blige = march %>% filter(match == "Mary J. Blige" | match == "Be Without You" | match == "Mary J. Blige Be Without You")
music.blige = music.blige %>% filter(query != "gorillaz")
music.blige = music.blige %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.blige$position = c(5, 4, 4, 5, 5, 5, 5, 8, 11, 13, 14, 18)
plot(music.blige$week, music.blige$position, type="l")
plot(music.blige$week, music.blige$count, type="l")
cor(music.blige$count, music.blige$position)

music.tpain = march %>% filter(match == "T-Pain" | match == "Mike Jones" | match == "T-Pain ft Mike Jones" | match == "T-Pain featuring Mike Jones" | match == "I'm N Luv" | match == "I'm N Luv (Wit A Stripper)" | match == "T-Pain I'm N Luv" | match == "Mike Jones I'm N Luv" | match == "T-Pain ft Mike Jones I'm N Luv")
music.tpain = music.tpain %>% filter(query != "miss jones")
music.tpain = music.tpain %>% filter(week > 9 & week < 20) %>% group_by(week) %>% summarise(count = n())
music.tpain$position = c(6, 6, 6, 8, 8, 10, 18, 23, 31, 37)
plot(music.tpain$week, music.tpain$position, type="l")
plot(music.tpain$week, music.tpain$count, type="l")
cor(music.tpain$count, music.tpain$position)

music.brown = march %>% filter(match == "Chris Brown" | match == "Chris Brown Yo" | match == "Excuse Me Miss" | match == "Yo Excuse Me Miss" | match == "Chris Brown Yo Excuse Me Miss")
music.brown = music.brown %>% filter(week > 9 & week < 21) %>% group_by(week) %>% summarise(count = n())
music.brown$position = c(7, 8, 10, 9, 10, 11, 11, 19, 23, 32, 41)
plot(music.brown$week, music.brown$position, type="l")
plot(music.brown$week, music.brown$count, type="l")
cor(music.brown$count, music.brown$position)

music.bedingfield = march %>% filter(match == "Natasha Bedingfield" | match == "Unwritten" | match == "Natasha Bedingfield Unwritten" | match == "Bedingfield Unwritten" | match == "Natasha Unwritten")
music.bedingfield = music.bedingfield %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.bedingfield$position = c(8, 10, 7, 6, 6, 6, 6, 5, 5, 8, 11, 11)
plot(music.bedingfield$week, music.bedingfield$position, type="l")
plot(music.bedingfield$week, music.bedingfield$count, type="l")
cor(music.bedingfield$count, music.bedingfield$position)

music.neyo = march %>% filter(match == "Ne-Yo" | match == "So Sick" | match == "Ne-Yo So Sick")
music.neyo = music.neyo %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.neyo$position = c(9, 1, 1, 3, 3, 7, 10, 13, 17, 28, 30, 41)
plot(music.neyo$week, music.neyo$position, type="l")
plot(music.neyo$week, music.neyo$count, type="l")
cor(music.neyo$count, music.neyo$position)

music.cascada = march %>% filter(match == "Cascada" | match == "Everytime We Touch" | match == "Cascada Everytime We Touch")
music.cascada = music.cascada %>% filter(week > 9 & week < 22) %>% group_by(week) %>% summarise(count = n())
music.cascada$position = c(10, 11, 12, 11, 11, 12, 16, 16, 16, 20, 25, 25)
plot(music.cascada$week, music.cascada$position, type="l")
plot(music.cascada$week, music.cascada$count, type="l")
cor(music.cascada$count, music.cascada$position)


View(as.data.frame(table(music.cascada$query)))



#########################################


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

