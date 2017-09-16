#clears everything
cat("\014")
rm(list = ls())

#Loads the CSV data
setwd("~/StabilityProject")
cleantweet= read.csv("~/StabilityProject/cleandtweet.csv")

attach(cleantweet)
head(score, 50)

verified = cleantweet$verified

min(as.numeric(cleantweet$tweet_created_at))



kenya_data= cleantweet[cleantweet$score<6,]

canada_data= cleantweet[which(cleantweet$score<11 & cleantweet$score>5),]

joint_data= cleantweet[cleantweet$score>10,]

#Kenya_data= cleantweet[ which(cleantweet$score=1 & cleantweet$score<6),]


summary(cleantweet$score)

library(DBI)
library(RSQLite)

clean = dbConnect(SQLite(), dbname="clean.db")

colnames(clean)







