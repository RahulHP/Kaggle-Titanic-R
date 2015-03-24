# Set Working Directory
setwd("~/Kaggle/Personal")

# Read Data
train <- read.csv("~/Kaggle/Data/train.csv")
test <- read.csv("~/Kaggle/Data/test.csv")

test$Name <- as.character(test$Name)
train$Name <- as.character(train$Name)

# Combine both into one for data cleaning
test$Survived <- NA
combi <- rbind(train, test)

# Surname
combi$Surname <- sapply(combi$Name,FUN=function(x){strsplit(x, split='[,.]')[[1]][1]})
combi$Surname <- as.factor(combi$Surname)

# Title
combi$Title <- sapply(combi$Name,FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- as.character(combi$Title)
combi$Title <- sub(' ', '', combi$Title) # Strip spaces from titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title[combi$Title %in% 'Jonkheer'] <- 'Master'
combi$Title <- as.factor(combi$Title)
# Deck 
combi$deck <- sapply(combi$Cabin,FUN=function(x){substring(x,1,1)})
combi$deck <- as.factor(combi$deck)


# Assigning decks based on price
aggregate(Fare~deck,data=combi,FUN=mean)
table(combi$Pclass,combi$deck)

# Remove T
which(combi$deck=='T')
combi[340,]$deck='A'
combi$deck<-as.character(combi$deck)
combi$deck<-as.factor(combi$deck)

xdeck<-combi[1,]$deck
# Combine decks into A,BC,DE,FG
combi[259,]$deck='B'



combi$deckgroup[combi$deck=='B' | combi$deck=='C'] <- 'BC'
combi$deckgroup[combi$deck=='D' | combi$deck=='E'] <- 'DE'
combi$deckgroup[combi$deck=='F' | combi$deck=='G'] <- 'FG'
combi$deckgroup[combi$deck=='A'] <- 'A'
combi$deckgroup[combi$deck==xdeck] <- 'Q'
combi$deckgroup<-as.factor(combi$deckgroup)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$deckgroup[combi$Pclass=='1' & combi$Fare > (116.32706+53.74124)/2] <- 'BC'
combi$deckgroup[combi$Pclass=='1' & combi$Fare < (116.32706+53.74124)/2] <- 'DE'

combi$deckgroup[combi$Pclass=='2' & combi$Fare > (17.33430+53.74124)/2] <- 'DE'
combi$deckgroup[combi$Pclass=='2' & combi$Fare < (17.33430+53.74124)/2] <- 'FG'

combi$deckgroup[combi$Pclass=='3' & combi$Fare > (17.33430+53.74124)/2] <- 'DE'
combi$deckgroup[combi$Pclass=='3' & combi$Fare < (17.33430+53.74124)/2] <- 'FG'



combi$deckgroup<-as.character(combi$deckgroup)
combi$deckgroup<-as.factor(combi$deckgroup)


train <- combi[1:891,]
test <- combi[892:1309,]




# Family Size
combi$FamilySize <- combi$SibSp + combi$Parch + 1


# Fixing Age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title +deckgroup + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])



fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + deckgroup + FamilySize,data=train,controls=cforest_unbiased(ntree=2000,mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)