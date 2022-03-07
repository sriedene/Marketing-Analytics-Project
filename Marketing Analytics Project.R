library(fastDummies)
library(stats)
library(anchors)

airport <- read.csv("C:/Users/User/Desktop/Syracuse/airport.csv")
str(airport)
airport$HOWLONG <- as.numeric(airport$HOWLONG)
airport2 <- data.frame(airport$DAY, airport$BAREA, airport$STRATA,
                       airport$HOWLONG, airport$Q4STORE, airport$Q4FOOD,
                       airport$Q4WIFI, airport$Q5TIMESFLOWN, airport$Q5FIRSTTIME,
                       airport$Q6LONGUSE, airport$Q7ART, airport$Q7FOOD, airport$Q7STORE,
                       airport$Q7SIGN, airport$Q7SCREENS, airport$Q7INFODOWN, airport$Q7INFOUP,
                       airport$Q7WIFI, airport$Q7ROADS, airport$Q7ALL, airport$Q9Food,
                       airport$Q9All, airport$Q14FIND, airport$NETPRO, airport$Q18STATE,
                       airport$Q18COUNTRY, airport$Q20Age, airport$Q21Gender, airport$Q22Income)
length(which(is.na(airport2)))

table(airport$DAY)
barplot(table(airport2$airport.DAY))
barplot(table(airport$BAREA))
barplot(table(airport$STRATA))
hist(airport$HOWLONG)
# Q4 - did you use this thing?
barplot(table(airport$Q4STORE))
barplot(table(airport$Q4FOOD))
barplot(table(airport$Q4WIFI))

# Q5/6 - how often customer is here
barplot(table(airport$Q5TIMESFLOWN))
barplot(table(airport$Q5FIRSTTIME))
barplot(table(airport$Q6LONGUSE))

# Q7 - Quality of things
barplot(table(airport$Q7ART))
barplot(table(airport$Q7FOOD))
barplot(table(airport$Q7STORE))
barplot(table(airport$Q7SIGN))
barplot(table(airport$Q7SCREENS))
barplot(table(airport$Q7INFODOWN))
barplot(table(airport$Q7INFOUP))
barplot(table(airport$Q7WIFI))
barplot(table(airport$Q7ROADS))
barplot(table(airport$Q7ALL))

# Q8 - look for specific comments
table(airport$Q8.1)

# Q9 - cleanliness
barplot(table(airport$Q9Food))
barplot(table(airport$Q9All))
table(airport$Q9.1)

# Q14 - ease of finding way around airport
barplot(table(airport$Q14FIND))

# Q15 - comments on signage
table(airport3$airport.Q15.1)

barplot(table(airport$NETPRO))

barplot(table(airport$Q18STATE))
table(airport$Q18COUNTRY)
barplot(table(airport$Q20Age))
barplot(table(airport$Q21Gender))
barplot(table(airport$Q22Income))

airport3 <- data.frame(airport$Q8.1, airport$Q9.1, airport$Q15.1)
length(which(is.na(airport3)))
airport3[is.na(airport3)] <- 0
str(airport3)

write.csv(airport2, "C:/Users/User/Desktop/Syracuse/airport_clean.csv")

airport2$airport.DAY <- as.factor(airport2$airport.DAY)
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="1"] <- "Sunday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="2"] <- "Monday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="3"] <- "Tuesday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="4"] <- "Wednesday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="5"] <- "Thursday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="6"] <- "Friday"
levels(airport2$airport.DAY)[levels(airport2$airport.DAY)=="7"] <- "Saturday"

airport2$airport.STRATA <- as.factor(airport2$airport.STRATA)
levels(airport2$airport.STRATA)[levels(airport2$airport.STRATA)=="1"] <- "AM"
levels(airport2$airport.STRATA)[levels(airport2$airport.STRATA)=="2"] <- "MID"
levels(airport2$airport.STRATA)[levels(airport2$airport.STRATA)=="3"] <- "PM"

airport2$airport.HOWLONG <- as.numeric(airport2$airport.HOWLONG)
str(airport2)
airport3.sub <- airport2[airport2$airport.Q4STORE %in% c(1,2),]
airport.sub$airport.Q4FOOD <- as.factor(airport2.sub$airport.Q4FOOD)
levels(airport2.sub$airport.Q4FOOD)[levels(airport2.sub$airport.Q4FOOD)=="2"] <- "0"
airport2.sub$airport.Q4FOOD <- as.numeric(airport2.sub$airport.Q4FOOD)
airport3.sub$airport.Q4STORE[airport3.sub$airport.Q4STORE == 2] <- 0
str(airport3.sub)
airport3.sub <- airport3.sub[airport3.sub$airport.Q7STORE %in% c(1,2,3,4,5),]
airport2.sub <- airport2.sub[airport2.sub$airport.Q9Food %in% c(1,2,3,4,5),]
airport3.sub <- airport3.sub[airport3.sub$airport.Q20Age %in% c(1,2,3,4,5,6,7),]
airport3.sub <- airport3.sub[airport3.sub$airport.Q21Gender %in% c(1,2),]
airport3.sub <- airport3.sub[airport3.sub$airport.Q22Income %in% c(1,2,3,4),]

airport3.sub$airport.Q21Gender <- as.factor(airport3.sub$airport.Q21Gender)
levels(airport3.sub$airport.Q21Gender)[levels(airport3.sub$airport.Q21Gender)=="1"] <- "Male"
levels(airport3.sub$airport.Q21Gender)[levels(airport3.sub$airport.Q21Gender)=="2"] <- "Female"

airportdummies3 <- dummy_cols(airport3.sub, select_columns = c('airport.DAY', 'airport.STRATA', 'airport.Q21Gender'))
str(airportdummies)
airportdummies3$airport.DAY_Monday <- as.factor(airportdummies3$airport.DAY_Monday)
airportdummies3$airport.DAY_Tuesday <- as.factor(airportdummies3$airport.DAY_Tuesday)
airportdummies3$airport.DAY_Wednesday <- as.factor(airportdummies3$airport.DAY_Wednesday)
airportdummies3$airport.DAY_Thursday <- as.factor(airportdummies3$airport.DAY_Thursday)
airportdummies3$airport.DAY_Friday <- as.factor(airportdummies3$airport.DAY_Friday)
airportdummies3$airport.DAY_Saturday <- as.factor(airportdummies3$airport.DAY_Saturday)
airportdummies3$airport.DAY_Sunday <- as.factor(airportdummies3$airport.DAY_Sunday)
airportdummies3$airport.STRATA_AM <- as.factor(airportdummies3$airport.STRATA_AM)
airportdummies3$airport.STRATA_MID <- as.factor(airportdummies3$airport.STRATA_MID)
airportdummies3$airport.STRATA_PM <- as.factor(airportdummies3$airport.STRATA_PM)
airportdummies3$airport.Q21Gender_Female <- as.factor(airportdummies3$airport.Q21Gender_Female)
airportdummies3$airport.Q21Gender_Male <- as.factor(airportdummies3$airport.Q21Gender_Male)


logit1 <- glm(airport.Q4FOOD ~ airport.DAY_Monday + airport.DAY_Tuesday + airport.DAY_Wednesday + airport.DAY_Thursday + airport.DAY_Friday 
              + airport.DAY_Saturday + airport.STRATA_AM + airport.STRATA_MID + airport.Q21Gender_Male + airport.HOWLONG + airport.Q20Age
              + airport.Q22Income, data=airportdummies, family=binomial(link='logit'))
summary(logit1)

logit2 <- glm(airport.Q4FOOD ~ airport.HOWLONG + airport.Q7FOOD + airport.Q9Food + airport.Q20Age + airport.Q22Income + airport.Q21Gender_Male
              ,data=airportdummies, family=binomial(link='logit'))
summary(logit2)

logit3 <- glm(airport.Q4FOOD ~ airport.DAY_Monday + airport.DAY_Tuesday + airport.DAY_Wednesday + airport.DAY_Thursday + airport.DAY_Friday 
              + airport.DAY_Saturday + airport.STRATA_AM + airport.STRATA_MID + airport.Q21Gender_Male + airport.HOWLONG + airport.Q20Age
              + airport.Q22Income + airport.Q7FOOD + airport.Q9Food, data=airportdummies2, family=binomial(link='logit'))
summary(logit3)

logit4 <- glm(airport.Q4FOOD ~ airport.HOWLONG, data=airport2.sub, family=binomial(link='logit'))
summary(logit4)

logit6 <- glm(airport.Q4STORE ~ airport.DAY_Monday + airport.DAY_Tuesday + airport.DAY_Wednesday + airport.DAY_Thursday + airport.DAY_Friday 
              + airport.DAY_Saturday + airport.STRATA_AM + airport.STRATA_MID + airport.Q21Gender_Male + airport.HOWLONG + airport.Q20Age
              + airport.Q22Income + airport.Q7STORE, data=airportdummies3, family=binomial(link='logit'))
summary(logit6)

logit7 <- glm(airport.Q4STORE ~ airport.HOWLONG + airport.STRATA_AM, data=airportdummies3, family=binomial(link='logit'))
summary(logit7)

airportdummies2 <- airportdummies[(airportdummies$airport.HOWLONG > 10),]
summary(airportdummies2)
summary(hist(airportdummies$airport.HOWLONG))
str(airportdummies)

predicted <- predict(logit6, airportdummies3, type='response')
optCutOff <- optimalCutoff(airportdummies3$airport.Q4STORE, predicted)
confusionMatrix(airportdummies3$airport.Q4STORE, predicted, threshold = optCutOff)
