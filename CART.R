setwd("D:/Udemy/Machine Learning A-Z Template Folder/Part 3 - Classification/Section 19 - Decision Tree Classification/Decision_Tree_Classification")

dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[3:5]

str(dataset)
dataset$Purchased <- as.factor(dataset$Purchased)

library(caTools)
set.seed(190)
split <- sample.split(dataset$Purchased,SplitRatio = 0.75)         
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# install.packages("rpart")
library(rpart)

classifier <- rpart(formula = Purchased ~ .,
                    data = training_set)

y_pred <- predict(classifier, newdata = test_set[-3], type = 'class')

cm<- table(y_pred,test_set[,3])
cm

plot(classifier)
text(classifier)
