#Into to datascience part 1

train <- read.csv("C:/Users/Nathan/Documents/IntroToDataScience-master/train.csv", header = TRUE)
test <- read.csv("C:/Users/Nathan/Documents/IntroToDataScience-master/test.csv", header = TRUE) 

#add survived variable to combined data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])
data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)


#take a look at gross survival rates
table(data.combined$survived)

#distribution across classes
table(data.combined$pclass)

library(ggplot2)

#Hypothesis - Ricj folks survived at a higher rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "survived")

#Examine the first few names
head(as.character(train$name))

#how many unique fmaes are there across both train and test sets
length(unique(as.character(data.combined$name)))

#Two duplicate names, take a closer look
#first get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

#next take a look at the recors in the combined data sets
data.combined[which(data.combined$name %in% dup.names),]

#what is up with the miss and mr
library(stringr)

#any correlation with other variables
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")), ]
misses[1:5, ]

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

#check males for pattern
males <- data.combined[which(train$sex == "male"),]
males[1:5, ]



#Expand upon the relationship between survived and p class by adding the new title variable
#to the datset and explore a potential 3- dimensional relationship
extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  }else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  }else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "name"]))
}

data.combined$title <- as.factor(titles)

#since we have only survived lables for the train set, only  use first 891 rows
ggplot(data.combined[1:891, ], aes(x = title, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#distirbtion of males to femalse
table(data.combined$sex)
summary(data.combined[1:891, "age"])

#visualize the 3 way relationsjpp of sex, pclass, and surgival
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
#agee and sex are important
summary(data.combined$age)
summary
#just to be thorough, take a look at surivval rates
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  stat_count(width = 10) +
  xlab("Age") +
  ylab("Total Count")

#validatae that master isa  good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

#miss
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) + 
  stat_count(width = 5) +
  ggtitle("Age for Miss. by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

#sibsp
summary(data.combined$sibsp)

length(unique(data.combined$sibsp))

data.combined$sibsp <- as.factor(data.combined$sibsp)

ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#feature engineering - family size feature
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#ticket variable

str(data.combined$ticket)

data.combined$ticket <- as.character(data.combined$ticket)  
data.combined$ticket[1:20]

#first character for each
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)

#ok, we can make a factor
data.combined$ticket.first.char <- as.factor(ticket.first.char)

#high level plot of data

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char ") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

#fares
summary(data.combined$fare)
length(unique(data.combined$fare))

#can't make fare a factor, treat as numeric and visualize with histogram
ggplot(data.combined, aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined fare Distribution") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  stat_count(width = 5) +
  facet_wrap(~pclass + title) +
  ggtitle("Combined fare Distribution") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

#analysis of the cabin variable
str(data.combined$cabin)

#make cabin into string
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

#replace empty area with U
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]

#take a look at jujst the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#multiplie cabins
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#does location matter
str(data.combined$embarked)
levels(data.combined$embarked)

ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


#Exploratory modeling

library(randomForest)

#train a random forest withthe default parameters using pclass and title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]
#train a rf using pclass, title, and sibsp
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

#pclass, title, parch
set.seed(1234)
rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#all of them
rf.train.4 <- data.combined[1:891, c("pclass", "title", "parch", "sibsp")]
set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#train a random using pclass, title and family size
rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]
set.seed(1234)

rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#sibsp, family size, pclass, title
rf.train.6 <- data.combined[1:891, c("pclass", "title", "family.size", "sibsp")]
set.seed(1234)

rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


#rf.5 + parch
rf.train.7 <- data.combined[1:891, c("pclass", "title", "family.size", "parch")]
set.seed(1234)

rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)




#video 5

#subset our test records and features
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

#make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

#write submission for kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160215.csv", row.names = FALSE)

library(caret)
library(doSNOW)


#leverage caret to create 100 total folds, but ensure that the ratio of those that
#survived and perished in each fold mat ches the overall straining set
#stratified cross validation

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

#check stratification
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308 / 494

#st up caret's traincontro object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats =  10, index = cv.10.folds)

#set up doSNOW package for multi-core training. This is helpful as we're going
#to be training a lot of trees
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)

#set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   mtree = 1000, trControl = ctrl.1)
#shutdown cluster
stopCluster(c1)

#check out results
rf.5.cv.1

#the above is only slightly more pessimistic than the rf.5 008 prediction, but
#not pessimist enough, lets try 5 fold cv repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#shutdown cluster
stopCluster(c1)

#results
rf.5.cv.2

#3 fold
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, 
                       index = cv.3.folds)
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)
set.seed(94622)

rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

stopCluster(c1)
rf.5.cv.3


#==============================================================================
#
# Video #6 - Exploratory Modeling 2
#
#==============================================================================

# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.


library(rpart)
library(rpart.plot)


#3 fold cv 10 times

#utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  c1 <- makeCluster(6, type = "SOCK")
  registerDoSNOW(c1)
  
  set.seed(seed)
  #leverage formula interace for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  #shutdown cluster
  stopCluster(c1)
  
  return (rpart.cv)
}

#grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

#Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.2 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 94.9%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 59.6% accuracy.

#bot rpart and rf confirm that title s important
table(data.combined$title)

#parse out last name and title
data.combined[1:25, "name"]

name.splits <- str_split(data.combined$name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

#add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

#now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

#the?
data.combined[which(titles == "the"), ]

#re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mile.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

#make title a factor
data.combined$new.title <- as.factor(titles)

#visualize new version of title

ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survival Rates for new.title by pclass")

#collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

#visualize
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survival Rates for Collapsed new.title by pclass")

#Grab features
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]


#run cv
rpart.2.cv.1 <- rpart.cv(94662, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#one female?
first.mr.df[first.mr.df$sex == "female",]

#update new.title feature
indexes <- which(data.combined$new.title == "Mr." &
                   data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#other gender mistakes

length(which(data.combined$sex == "female" &
               (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))

#refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

#surviving first class "Mr."
summary(first.mr.df[first.mr.df$survived == "1",])
View(first.mr.df[first.mr.df$survived == "1",])

#take a look at fares
indexes <- which(data.combined$ticket == "PC 17755" |
                   data.combined$ticket == "PC 17611" |
                   data.combined$ticket == "113760")
View(data.combined[indexes,])

#visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


#Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket) 
  current.avg.fare <- data.combined[party.indexes[1], "fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

#refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


#visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")

#hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

#one missing value
data.combined[is.na(data.combined$avg.fare),]

#get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == 1 & ticket != "3701"))
similar.na.passengers <- data.combined[indexes, ]
summary(similar.na.passengers$avg.fare)

#use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840


#leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

#Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

#just 1st class all-up
indexes <- which(data.combined$pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])

#hypothesis refuted again

#see if feature engineering made a difference
features <- c("pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

#run cv and check out results
rpart.3.cv.1 <- rpart.cv(94662, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#==============================================================================
#
# Video #7 - Submitting, scoring, and some analysis.
#
#==============================================================================

#
# Rpart scores 0.80383
#
# Subset our test records and features

test.submit.df <- data.combined[892:1309, features]

#Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

#write out a csv file for submission to kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20160619_1.csv", row.names = FALSE)



#rf score 0.80861

features <- c("pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

#Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

#write out csv file for submission to kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160619_1.csv", row.names = FALSE)

#
# If we want to improve our model, a good place to start is focusing on where it
# gets things wrong!
#


# First, let's explore our collection of features using mutual information to
# gain some additional insight. Our intuition is that the plot of our tree
# should align well to the definition of mutual information.

library(infotheo)

mutinformation(rf.label, data.combined$pclass[1:891])
mutinformation(rf.label, data.combined$sex[1:891])
mutinformation(rf.label, data.combined$sibsp[1:891])
mutinformation(rf.label, data.combined$parch[1:891])
mutinformation(rf.label, discretize(data.combined$fare[1:891]))
mutinformation(rf.label, data.combined$embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than 'Mr."
#install.packages("Rtsne")

library(Rtsne)
most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$survived != "None")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")

# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$survived[indexes], discretize(tsne.1$Y[indexes]))

# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(most.correct$survived[indexes], discretize(tsne.1$Y[indexes,]))


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2],
                 color = misters$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")

#Now conditional mutual information for tsne features for adult males
condinformation(misters$survived[indexes], discretize(tsne.2$Y[indexes, ]))

#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#

tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]
