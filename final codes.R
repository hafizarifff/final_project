# Project: To answer the following research questions:
# 1. Which genre of anime show was preferred overall (highest rated)?
# 2. Which genre of anime show was popular by genre (more users)?
# 3. Which genre of anime show was popular by age (more users)?
# data can be downloaded from Kaggle: "https://www.kaggle.com/azathoth42/myanimelist"

library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
anime <- fread("anime_cleaned.csv")
users <- fread("users_cleaned.csv")
lists <- fread("animelists_cleaned.csv")

# Q1: find overall genre preference
animesm <- anime[,c("anime_id","title", "genre")]
listsagg <- dcast(lists, anime_id~., length, value.var = c("username"))
listsagg <- data.table(listsagg)
setnames(listsagg, ".", "N_users")
totscore <- dcast(lists, anime_id~., sum, value.var = c("my_score"))
totscore <- data.table(totscore)
setnames(totscore, ".", "total_score")
totscore <- merge(totscore, listsagg, all = T)
totscore$avg_rating <- totscore$total_score / totscore$N_users
pref <- merge(animesm, totscore, by="anime_id", all = T)
genpref <- separate_rows(pref, genre, sep = ", ")
genpref <- genpref[!genpref$genre == "",]
bygenre <- dcast(genpref, genre~., sum, value.var = "N_users")
setnames(bygenre, ".", "non_unique_N")
bygenre <- data.table(bygenre)
bygenre$genre <- factor(bygenre$genre, levels = bygenre$genre[order(-bygenre$non_unique_N)])
general <- dcast(genpref, genre~., mean, value.var = "avg_rating")
general <- data.table(general)
bygenre <- merge(bygenre, general, all = T)
setnames(bygenre, ".", "avg_rating")

# plot 1
ggplot(bygenre, aes(x = reorder(genre, -avg_rating), y = avg_rating)) + geom_bar(stat = "identity", aes(fill = ..x..)) + scale_fill_distiller(palette = "Spectral") + labs(x = "Genre", y = "Average Rating", title = "Overall Genre Preference", subtitle = "Each user score a show out of 10, and average for each genre is calculated. Each user can appear more than once.") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 35)) + guides(fill=FALSE)



# Q2: find genre popularity by gender
# find total number of users by each genre by gender
userssmm <- users[,c("username", "gender", "birth_date")]
listssmm <- lists[,c("username", "anime_id", "my_score")]
glist <- merge(listssmm, userssmm, all.x = T)
glist[,.(count = length(username)), by= "gender"]
glist <- glist[!glist$gender == "Non-Binary",]
glistgender <- glist[,c("username", "anime_id", "my_score", "gender")]
animegenre <- anime[,c("anime_id", "genre")]
castgender <- dcast(glist, anime_id+gender~., length, value.var = c("username"))
castgender <- merge(castgender, animegenre, all.y = T)
setnames(castgender, ".", "count")

castgender <- separate_rows(castgender, genre, sep = ", ")
castgender <- castgender[!castgender$genre == "",]
totalgender <- dcast(castgender, genre+gender~., sum, value.var = c("count"))
setnames(totalgender, ".", "total_users")
totalgender <- data.table(totalgender)
sumbygenre <- totalgender[,.(N=sum(total_users)), by = genre]
sumbygenre <- data.table(sumbygenre)
totalgender <- merge(totalgender, sumbygenre, all = T)
setnames(totalgender, "N", "tot_by_gender")
totalgender$percentage <- (totalgender$total_users / totalgender$tot_by_gender) *100

# for labeling purpose
totalgender <- ddply(totalgender, .(genre), transform, pos = cumsum(percentage) - (0.5 * percentage))

# plot 2
ggplot(totalgender, aes(x = genre, y = percentage, fill=gender)) + geom_bar(position = "stack", stat = "identity") + geom_text(aes(x=genre, y=-pos+100, label=format(round(percentage,1), nsmall = 1)), size=3.5) + labs(x = "Genre", y = "Percentage of Users", title = "Popularity of Genre by Gender", subtitle = "Each users can appear more than once in each genre. Popularity calculated by counting watched shows in each category.") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 35))


# Q3: find genre popularity by age group
# first group users by their age
birthusers <- users[, c("username", "birth_date")]
birthusers$birth_date <- as_datetime(birthusers$birth_date)
birthusers$interval <- interval(birthusers$birth_date, ymd(20181201))
birthusers$age <- birthusers$interval / dyears(x=1)
birthusers$rage <- round(birthusers$age, digits = 0)
g1vec <- birthusers$rage %in% c(10:14)
birthusers$group[g1vec] <- 1
g2vec <- birthusers$rage %in% c(15:17)
birthusers$group[g2vec] <- 2
g3vec <- birthusers$rage %in% c(18:20)
birthusers$group[g3vec] <- 3
g4vec <- birthusers$rage %in% c(21:23)
birthusers$group[g4vec] <- 4
g5vec <- birthusers$rage %in% c(24:26)
birthusers$group[g5vec] <- 5
g6vec <- birthusers$rage %in% c(27:30)
birthusers$group[g6vec] <- 6
g7vec <- birthusers$rage %in% c(31:35)
birthusers$group[g7vec] <- 7
g8vec <- birthusers$rage %in% c(36:40)
birthusers$group[g8vec] <- 8
g9vec <- birthusers$rage %in% c(41:50)
birthusers$group[g9vec] <- 9

# then find total users of each genre by age group 
agegroup <- birthusers[,c("username", "group")]
glistag <- merge(glist[,c("username", "anime_id")], agegroup, all.x = T)
castage <- dcast(glistag, anime_id + group~., length, value.var = c("username"))
castage <- data.table(castage)
setnames(castage, ".", "count")
animecat <- anime[, c("anime_id", "genre")]
animecat <- separate_rows(animecat, genre, sep = ", ")
castage <- merge(castage, animecat, all.x = T)
castage <- castage[!castage$genre == "",]
totalage <- dcast(castage, group + genre~., sum, value.var = c("count"))
totalage <- data.table(totalage)
setnames(totalage, ".", "total_users")

sumbyage <- totalage[,.(N=sum(total_users)), by = group]
totalage <- merge(totalage, sumbyage, by = "group", all.x = T)
setnames(totalage, "N", "tot_by_age")
totalage$percentage_by_age <- (totalage$total_users / totalage$N) *100

#taking top 5 most popular genre from each group
top5 <- setorder(setDT(totalage), -percentage_by_age)[, head(.SD, 5), keyby = group]

# to reorder bar graphs
top5 = top5 %>% group_by(group) %>% mutate(position = rank(-percentage_by_age))
groupno <- 1:9

# plot 3
ggplot(top5, aes(x=group, y=percentage_by_age, col=genre, group=position)) + geom_bar(aes(fill=genre, y=percentage_by_age), stat = "identity", position = "dodge") + geom_text(aes(y=percentage_by_age, label=format(round(percentage_by_age,1), nsmall = 1, label = sprintf("%2.1f", percentage_by_age))), position = position_dodge(width = 1), vjust= -0.5) + labs(x = "Age Group", y = "Percentage of Users (%)", title = "Popularity of Genre by Age", subtitle = "Users age ranged from 9 to 50 years old; higher group number means older user group. Popularity calculated by counting number of watched shows in each genre, and top 5 genres in each age group are graphed.", caption= "Age group = G1: <14; G2: 14-17; G3: 17-20; G4: 20-23; G5: 24-26; G6: 27-30; G7: 31-35; G8: 36-40; G9: 40") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text()) + scale_x_continuous(labels = as.character(groupno), breaks = groupno)


