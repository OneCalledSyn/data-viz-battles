library(data.table)
library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)
library(Hmisc)
library(caret)
library(MLmetrics)
library(e1071)
library(party)

set.seed(2345)

#Read in the data set
jumpies <- fread("C:/Users/OneCalledSyn/Desktop/jumpscare.csv")

#Examine the data
str(jumpies)
head(jumpies)
summary(jumpies)

#Remove problem entries
new_jumpies <- na.omit(jumpies)

#Cast Year and Jump Count as numerics instead of strings
new_jumpies$Year <- as.numeric(new_jumpies$Year)
new_jumpies$`Jump Count` <- as.numeric(new_jumpies$`Jump Count`)

#Extract the numeric ratings from the jumbled mess of characters in Jump Scare Rating
new_jumpies$`Jump Scare Rating` <- unlist(regmatches(new_jumpies$`Jump Scare Rating`, gregexpr('\\(?[0-9,.]+', new_jumpies$`Jump Scare Rating`)))
new_jumpies$`Jump Scare Rating` <- as.numeric(gsub('\\(', '-', gsub(',', '', new_jumpies$`Jump Scare Rating`)))

#Cast Netflix(US) as a two level factor instead of a character
new_jumpies$`Netflix(US)` <- as.factor(new_jumpies$`Netflix(US)`)

#Rename the columns with spaces and parentheses in the variable name so my random forest doesn't catch on fire
names(new_jumpies)[1] <- "Movie_Name"
names(new_jumpies)[4] <- "Jump_Count"
names(new_jumpies)[5] <- "Jump_Scare_Rating"
names(new_jumpies)[6] <- "Netflix_US"

str(new_jumpies)

#Ideas: Jump scare rating vs jump count, rating vs imbd score

train <- new_jumpies[1:378]
test <- new_jumpies[379:540]

train %>%
  group_by(Netflix_US) %>%
  summarise(count = n()) %>%
  glimpse
  
test %>%
    group_by(Netflix_US) %>%
    summarise(count = n()) %>%
    glimpse

#Model 1
rf_model <- randomForest(Netflix_US ~ Year, data=train, proximity = TRUE)
rf_model

test$predicted <- predict(rf_model, test)

confusionMatrix(test$Netflix_US, test$predicted)

#Model 2
rf_model2 <- randomForest(Netflix_US ~ Year + Imdb, data=train, proximity = TRUE)
rf_model2

test$predicted2 <- predict(rf_model2, test)

confusionMatrix(test$Netflix_US, test$predicted2)

#Model 3
rf_model3 <- randomForest(Netflix_US ~ Year + Imdb + Jump_Count + Jump_Scare_Rating, data=train, proximity = TRUE)
rf_model3

test$predicted3 <- predict(rf_model3, test)

confusionMatrix(test$Netflix_US, test$predicted3)

varImpPlot(rf_model3, sort = TRUE, n.var = 4, main = "How important are the variables?")
varImpPlot(rf_model2, sort = TRUE, n.var = 4, main = "How important are the variables?")
varImpPlot(rf_model, sort = TRUE, n.var = 4, main = "How important are the variables?")

#Model 4
rf_model4 <- randomForest(Netflix_US ~ Imdb, data=train, proximity = TRUE)
rf_model4

test$predicted4 <- predict(rf_model4, test)

confusionMatrix(test$Netflix_US, test$predicted4)

new_jumpies$Year_by_category <- cut(x = new_jumpies$Year, 
                                  breaks = c(-Inf, 1959, 1969, 1979, 1989, 1999, 2009, Inf), 
                                  labels = c("Earlier than 1959", "1960 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2019"), 
                                  right = TRUE)

ggplot(data = new_jumpies, mapping = aes(x = Jump_Count, fill = Year_by_category))+
  geom_histogram(color = "white", bins = 10) +
  facet_wrap(~Year_by_category, nrow = 2) +
  labs(title = "Histogram of Jump Count by Decade",
       x = "Jump Count",
       y = "Number of Movies")
