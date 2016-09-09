catchers <- read.csv("catchers15.csv")
hitters <- read.csv("hitters15.csv")
library(caret)
library(class)
library(gbm)
library(cluster)
install.packages("NbClust")
library(NbClust)
library(rpart)

catchers.1 <- catchers[,3:95]
hitters.1 <- hitters[,3:94]
hitters.1$Age.Rng <- NULL
str(catchers.1)
str(hitters.1)

hitter.scale <- as.data.frame(scale(hitters.1))
catch.scale <- as.data.frame(scale(catchers.1[,c(-93,-94)]))
str(catch.scale)
catch.scale$HV <- as.factor(ifelse(catchers.1$Dol >= 5.5, 1, 0))
hitter.scale$HV <- as.factor(ifelse(hitters.1$Dol >= 5.5, 1, 0))
catch.scale$phLI <- NULL

set.seed(123)
ind <- sample(2, nrow(hitter.scale), replace = TRUE, prob = c(0.7,0.3))
train <- hitter.scale[ind==1,]
test <- hitter.scale[ind==2,]
str(train)


grid <- expand.grid(.k=seq(2, 20, by = 1))
control <- trainControl(method = "cv")
set.seed(123)
knn.train <- train(HV ~ ., data = train, method = "knn", trControl = control, tuneGrid = grid)
knn.train


knn.test <- knn(train[,-92], test[,-92], train[,92], k = 17)
acc <- table(knn.test, test$HV)
table(knn.test, test$HV)
round((sum(diag(acc)) / sum(acc) * 100), 1)


fit <- lm(as.numeric(HV) ~ OPS, data = train)
summary(fit)
pred <- predict(fit)
plot(pred)
summary(pred)




