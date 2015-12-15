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

sort(query.table,decreasing=TRUE)[1:10] # Top X queries

############
# Week 2.3 #
############
sessions = as.numeric(table(queries$sessionId))
hist(sessions, freq=F)
hist(sessions[which(sessions<16)], breaks=seq(0,15,1), freq=F)
summary(sessions)

# MISSING: Time of sessions?
sessionIds = as.numeric(unique(queries$sessionId))
sessions.time = data.frame(as.numeric(unique(queries$sessionId)))
sessions.duration = apply(sessions.time, 1, get_session_duration)

get_session_duration = function(id) {
  print(id)
  maxTime = max(queries$epoc[which(queries$sessionId == id)])
  minTime = min(queries$epoc[which(queries$sessionId == id)])
  return ((maxTime - minTime) / 1000) # difference in seconds
}

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

