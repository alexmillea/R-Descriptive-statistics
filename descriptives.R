#descriptives & exploratory analysis
descript <- BikeHour

str(descript)
summary(descript)

#gives deeper insights into data
library(Hmisc)
describe(descript)

#descriptives packages
library(psych)
describe(descript)

#4 histograms 2x2
par(mfrow=c(1,2))

#users 
hist(descript$registered, col = "blue", main = "Histogram of Registered")
hist(descript$casual, col = "red", main = "Histogram of Casual")
hist(descript$count)
hist(descript$temp)
hist(descript$atemp)
hist(descript$windspeed)
hist(descript$humidity)

par(mfrow = c(1,1))
boxplot(descript$casual)
boxplot(descript$registered)
boxplot(descript$count)

#boxplots comparing count with different factors 
boxplot(descript$count ~ descript$weekday, 
        horizontal = FALSE,
        main = "Weekdays VS Bike User Count",
        col = rainbow(6))

boxplot(descript$count ~ descript$season, 
        horizontal = FALSE,
        main = "Season VS Count",
        col = rainbow(4))

boxplot(descript$count ~ descript$weather, 
        horizontal = FALSE,
        main = "Weather VS Count",
        col = rainbow(4))

#compare count, reg and casual with hour - identfy most active hour of usage
par(mfrow = c(1,1))
plot(descript$hour, xlab="hour",descript$count, ylab="user count", col = "red")
plot(descript$hour, xlab="hour",descript$registered, ylab="user count - registered", col = "blue")
plot(descript$hour, xlab="hour",descript$casual, ylab="user count - casual", col = "green")

#subset date into year only - used to compare bike usage in 2011 & 2012
descript$year=substr(descript$date,1,4)
descript$year=as.factor(descript$year)
boxplot(descript$count~descript$year, xlab="Year", ylab="Yearly User Count")


#plots - same as above nicer visual
library(ggplot2)
ggplot(data = descript, aes(x = hour, y = casual)) +
  geom_point()

ggplot(data=descript, aes(x=hour, y= registered, fill=hour)) +
  geom_point()

#correlation table - continous variables 
corDF = data.frame(descript$registered,descript$casual,descript$count,descript$temp,descript$humidity,descript$atemp,descript$windspeed)
cor(corDF)

library("corrplot")
corVis <- cor(corDF)
corrplot(corVis, method = "number")
corrplot(corVis, method = "color")


#test for normality - take sample of 500
library(tidyverse)
sample <- descript
sample <- sample %>% slice(1:500)
sample$season <- as.numeric(sample$season)

shapiro.test(sample$temp)
shapiro.test(sample$atemp)
shapiro.test(sample$humidity)
shapiro.test(sample$windspeed)
shapiro.test(sample$casual)
shapiro.test(sample$registered)
shapiro.test(sample$count)


#############################################

#self organising maps (SOM)
library(kohonen)
library(RColorBrewer)

som <- as.data.frame(descript)
som$count <- unclass(som$count)
som$count <- as.numeric(som$count)
colnames(som)
som$season <- as.numeric(som$season)
som$weekday <- as.numeric(som$weekday)
som$weather <- as.numeric(som$weather)
som$hour <- as.numeric(som$hour)
som.measures1 <- c("count", "hour", "season", "weekday")
som1 <- som(scale(som[som.measures1]), grid = somgrid(10, 10, "rectangular"))
plot(som1, main = "Clusters Within 4 Variables")

som.measures2 <- c("count", "weather", "temp", "humidity")
som2 <- som(scale(som[som.measures2]), grid = somgrid(10, 10, "rectangular"))
plot(som2, main = "Clusters Within 4 Variables")

#heatmap - red = higher
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}
plot(som1, type = "count", palette.name = colors, heatkey = TRUE, main = "Heat Maps")

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
plot(som1, type = "count", 
     count = getCodes(som1)[,4], 
     main=colnames(getCodes(som1))[4], 
     palette.name=coolBlueHotRed)

################################################

#kmeans 
#load libraries needed
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

df <- BikeHour
df <- df[, -1]
df <- df[, -1]
df <- df[, -2]
df <- df[, -4]
df <- df[, -5]
df <- df[, -10]
df <- df[, -9]
#remove all factors from data 
#tested with converting factors to numerics but clusters were meaningless.

#change factors to numerics
#df$month <- as.character(df$month)
#df$month <- as.numeric(factor(df$month))
df$hour <- as.character(df$hour)
df$hour <- as.numeric(factor(df$hour))
#df$weekday <- as.character(df$weekday)
#df$weekday <- as.numeric(factor(df$weekday))
#df$weather <- as.character(df$weather)
#df$weather <- as.numeric(factor(df$weather))
#df$season <- as.character(df$season)
#df$season <- as.numeric(factor(df$season))

#scale data
df <- scale(df)
head(df)

#compute distance (euclidean)
distance <- get_dist(df)

#visulise -
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering k = 2, cluster sizes 26 & 64
k2 <- kmeans(df, center = 2, nstart = 25)
k2

fviz_cluster(k2, data = df)

#pairwise scatter plots 
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(BikeHour)) %>%
  ggplot(aes(count, hour, color = factor(cluster), label = state)) +
  geom_text()

#trying different values for K
k3 <- kmeans(df, center = 3, nstart = 25)
k3
k4 <- kmeans(df, center = 4, nstart = 25)
k4
k5 <- kmeans(df, center = 5, nstart = 25)
k5
k6 <- kmeans(df, center = 6, nstart = 25)
k6

#plots
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#elbow curve to get optimal k value 
set.seed(7)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#nbclust to compute elbow curve using wss function
set.seed(7)
fviz_nbclust(df, kmeans, method = "wss")

#no clear k value distinguished  
#kmeans with k = 7 - taken from elbow curve
set.seed(7)
final <- kmeans(df, 7, nstart = 25)
print(final)

#plots 
fviz_cluster(final, data = df)









