
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(plotly)
library(arules)
library(arulesViz)



times_ranking <- read.csv("./dataset/timesData.csv", stringsAsFactors = FALSE,na.strings = c("", "-"))
cwurData <- read.csv("./dataset/cwurData.csv")
shangai <- read.csv("./dataset/shanghaiData.csv")

#desc(cwurData)
View(cwurData)
View(times_ranking)
View(shangai)

########################## Preprocessing ###########################

#handling missing values
na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

barplot(na_count(cwurData), main="CWUR Dataset Missing Values")
#there is only the broad_impact with missing values of years before 2014, but this will not cause
#any problem as we will not use this attribute that much

barplot(na_count(times_ranking), main="Times Ranking Dataset Missing Values")
#this dataset has a lot of missing values for total_score attribute which is a very important feature
#so we might not use this dataset for this reason.

barplot(na_count(shangai), main="Shanghai Dataset Missing Values")
#also has a lot of missing value for the most important feature, score, but fortunately it has 
#around 4 thousands rows meaning we can delete these missing values and we still have an important 
#number of rows to work with

###################################  Visualization ############################################

cwurData_year <- subset(cwurData, cwurData$year == 2015)
height <- sort(table(cwurData_year$country), decreasing = TRUE)
barplot(height[1:10], las = 3, main = "Top Countries in University Rankings 2015")



cwurPlotYear <- function(nYear) {
  cwurData %>% filter(year==nYear) %>% top_n(10,-world_rank) %>% 
    ggplot(aes(x=reorder(institution,-world_rank), y=world_rank)) + geom_bar(stat="identity", aes(fill=reorder(institution,-world_rank)), colour="black") +
    theme_bw() + coord_flip() +  scale_fill_manual(values=c(rep("lightgreen",7), "#CD7F32", "grey", "gold")) + guides(fill=FALSE) +
    labs(x="Institution", y="World Rank", 
         title=paste("Rank in ",nYear), subtitle="(smaller value is better)") 
}

cwurPlotYear(2012) -> d1
cwurPlotYear(2013) -> d2
cwurPlotYear(2014) -> d3
cwurPlotYear(2015) -> d4
grid.arrange(d1,d2,d3,d4, ncol=2)


cwurData %>% group_by(country,year) %>% 
  summarise(nr = length(world_rank), minw=min(world_rank), maxw=max(world_rank), avgw=round(mean(world_rank),0)) %>%
  select(country, year, nr, minw, maxw, avgw) %>% ungroup() -> ccwur
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)
ccwur$hover <- with(ccwur, 
                    paste("Country: ", country, '<br>', 
                          "Year: ",year, "<br>",
                          "Universities in top: ", nr, "<br>",
                          "Min rank in top: ", minw, "<br>",
                          "Max rank in top: ", maxw, "<br>",
                          "Mean rank in top: ", avgw,"<br>"
                    ))
# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'orthogonal')
)
plot_geo(ccwur, locationmode = 'country names') %>%
  add_trace(
    z = ~nr, color = ~nr, colors = 'Spectral', frame = ~year,
    text = ~hover, locations=~country, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of\nuniversities in top', tickprefix = '') %>%
  layout(title = with(ccwur),geo = g)



#France universities ans schools correlation between their attributes

fr_univs <- subset(cwurData, cwurData$country == "France")

cor_cols = c("national_rank", "quality_of_education", "alumni_employment", "quality_of_faculty", "publications", "influence", "citations")

corrplot(cor(fr_univs[cor_cols]))

#regression examples

plot(fr_univs$publications, fr_univs$national_rank, xlab ="Publications", ylab = "National Rank", main = "Publications vs National Rank")
c <- lm(national_rank ~ publications, data = fr_univs)
abline(c)

plot(fr_univs$citations, fr_univs$national_rank, xlab ="Citations", ylab = "National Rank", main = "Citations vs National Rank")
c <- lm(national_rank ~ citations, data = fr_univs)
abline(c)



######################### modeling ########################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Kmeans %%%%%%%%%%%%%%%%%%%%%%%
########## CWUR dataset  ############

#clustering of univs based on their rank and score
library(cluster)
library(ggplot2)
library(factoextra)
library(NbClust)

cwurX <- cwurData[c('world_rank','score')]

kmeans_elb <- fviz_nbclust(cwurX, kmeans, method = "wss") +
  ggtitle("(A) Elbow method")

kmeans_silh <- fviz_nbclust(cwurX, kmeans, method = "silhouette") +
  ggtitle("(B) Silhouette method")

gridExtra::grid.arrange(kmeans_elb, kmeans_silh, nrow = 1)

#we will investigate two values of number of clusters, k=2 and k=3


n = nrow(cwurX)
######################## number of clusters is 2
k = 2
kmeans.result <- kmeans(cwurX, k)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # WSS= 23.57%
print(paste("BSS=",round(b,2),"%")) # BSS= 76.43%



BH_index <- w / k
print(paste("BH index=",round(BH_index,2))) #11.79
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2))) #7126.95

#visualization
plot(cwurX, col=kmeans.result$cluster, main='Kmeans visualized with k=2 for CWUR Dataset')


######################## number of clusters is 3
k = 3
kmeans.result <- kmeans(cwurX, k)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # WSS= 10.9%
print(paste("BSS=",round(b,2),"%")) # BSS= 89.91%

BH_index <- w / k
print(paste("BH index=",round(BH_index,2))) #3.36
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2))) #9792.21

#visualization
plot(cwurX, col=kmeans.result$cluster, main='Kmeans visualized with k=3 for CWUR Dataset')


########## SHANGAI dataset  ############

#clustering of univs based on their rank and score

shangaiX <- shangai[c('world_rank','total_score')]
#drop nan value
shangaiX <- shangaiX[complete.cases(shangaiX),]

kmeans_elb <- fviz_nbclust(shangaiX, kmeans, method = "wss") +
  ggtitle("(A) Elbow method")

kmeans_silh <- fviz_nbclust(shangaiX, kmeans, method = "silhouette") +
  ggtitle("(B) Silhouette method")

gridExtra::grid.arrange(kmeans_elb, kmeans_silh, nrow = 1)

#we will investigate two values of number of clusters, k=2 and k=3 as in previous dataset


n = nrow(shangaiX)
######################## number of clusters is 2
k = 2
kmeans.result <- kmeans(shangaiX, k)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # WSS= 30.15%
print(paste("BSS=",round(b,2),"%")) # BSS= 69.85%

BH_index <- w / k
print(paste("BH index=",round(BH_index,2))) #15.07
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2))) #2546.61
#visualization
plot(cwurX, col=kmeans.result$cluster, main='Kmeans based on score and rank visualized')

#visualization
plot(shangaiX, col=kmeans.result$cluster, main='Kmeans visualized with k=2 for SHANGAI Dataset')

######################## number of clusters is 3
k = 3
kmeans.result <- kmeans(shangaiX, k)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # WSS= 14.77%
print(paste("BSS=",round(b,2),"%")) # BSS= 85.23%


BH_index <- w / k
print(paste("BH index=",round(BH_index,2))) #4.92
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2))) #3166.1

#visualization
plot(shangaiX, col=kmeans.result$cluster, main='Kmeans visualized with k=3 for SHANGAI Dataset')

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Regression %%%%%%%%%%%%%%%%%%%%%%%
library(mice)
library(VIM)
library(corrplot)



## Change some columns to numeric
times_ranking$world_rank = as.numeric(as.character(gsub('-','',times_ranking$international_students)))
times_ranking$num_students = gsub(',','',times_ranking$num_students)
times_ranking$num_students = as.numeric(as.character(times_ranking$num_students))
times_ranking$international_students = as.numeric(as.character(gsub('%','',times_ranking$international_students)))

#for this dataset we will replace missing values by 
miss_fill <- function(x){sum(is.na(x))/length(x)*100}
apply(times_ranking,1,miss_fill)
apply(times_ranking,2,miss_fill)

## image of missing data
md.pattern(times_ranking)

## check missing values again
sapply(times_ranking, function(x) sum(is.na(x)))
aggr_plot <- aggr(times_ranking, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(times_ranking), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## Data imputation
tempData <- mice(times_ranking,m=5,meth='cart')

train_complete <- complete(tempData,"long")
#13,015 rows

#data split
training_set = train_complete[(1:9110),]
testing_set = train_complete[(9110:13015),]


#check missing values in training_set and test_set
sapply(training_set, function(x) sum(is.na(x)))
sapply(testing_set, function(x) sum(is.na(x)))
#only in 'female_male_ration' and 'international_student' variables left some missing vals but to reduce data any more
#we will ignore these variables

#correlation in training set
q = as.matrix(training_set[,c(3,6:13)])
corrplot(cor(q), method = "number", number.cex=0.75, is.corr = FALSE)
#Correlation Matrix for Times Ranking Dataset


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Build different models using training set

#%%%%%%%%%%%%%%%% World Rank %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg_model = lm(world_rank~research+citations+international+teaching+total_score+student_staff_ratio+num_students+income, 
               data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #33.15

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.7

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #32.68

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #0.71

#%%%%%%%%%%%

reg_model = lm(world_rank~research+citations+international+income, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #35.32

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.68

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #34.65

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #0.69

#%%%%%%%%%%

reg_model = lm(world_rank~international, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #37.89

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.66

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #36.85

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #0.67

#### plotting model 
par(mfrow=c(2,2))
plot(reg_model, 
     main="Regression of world_rank~international in Time Ranking Dataset")


plot(training_set$world_rank, training_set$international, xlab ="World Rank", ylab = "International", 
     main = "World Rank vs International")
abline(reg_model)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Total score

reg_model = lm(total_score~research+international+student_staff_ratio, data=training_set)

model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #24.91

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.83

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #1384.16

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #-11.22

#%%%%%%%%%%%%%%%%%%%% Total score

reg_model = lm(total_score~research+citations+international+income, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #20.31

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.86

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #1386.9

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #-11.24


#%%%%%%%%%%%%%%%%%%%%%%%%%% Total score

reg_model = lm(total_score~research+citations+international+income+student_staff_ratio, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #19.87

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.86

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #1385.44

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #-11.23

#%%%%%%%%%%%%%
reg_model = lm(total_score~research+teaching, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #21.88

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.85

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #1395.64

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #-11.32

#%%%%%%%%%%%%%

reg_model = lm(total_score~research, data=training_set)
model_summary = summary(reg_model)
## Predictions for data
train_mse <- mean(model_summary$residuals^2)
print(paste("train_mse =",round(train_mse,2))) #25.93

train_rsq <- model_summary$r.squared
print(paste("train R^2 =",round(train_rsq,2))) #0.82

predictions <- predict(reg_model, testing_set)
actual <- as.numeric(testing_set$world_rank)
test_mse <- mean((actual - predictions)^2)
print(paste("test_mse =",round(test_mse,2))) #1393.91

rss <- sum((predictions - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
test_rsq <- 1 - rss/tss
print(paste("test R^2 =",round(test_rsq,2))) #-11.3


par(mfrow=c(2,2))
plot(reg_model, 
     main="Regression of total_score~research in Time Ranking Dataset")

plot(training_set$total_score, training_set$research, xlab ="Total Score", ylab = "Research", 
     main = "Total Score vs Research")
abline(reg_model)


















































