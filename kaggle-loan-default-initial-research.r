######################################################
#  Kaggle Loan Default Predictor Challenge
#  Jason Green
#  March, 4th 2014
#  https://github.com/Convalytics/kaggle-loan-default-prediction
#  Last Updated: 3/4/2014
######################################################

# Load Packages
library(plyr)
library(ggplot2)
library(gridExtra)
library(psych)

# Set Working Directory
setwd("~/GitHub/kaggle-loan-default-prediction")

# Import Data
submission <- read.csv("~/GitHub/kaggle-loan-default-prediction/sampleSubmission.csv")
#test <- read.csv("~/GitHub/giantfiles/kaggle-loan-default-prediction/test_v2.csv")   # CSV is 1 GB
train <- read.csv("~/GitHub/giantfiles/kaggle-loan-default-prediction/train_v2.csv") # CSV is .5 GB

train$hadloss[train$loss == 0] <- "N"
train$hadloss[train$loss > 0] <- "Y"
train$hadloss <- as.factor(train$hadloss)

train.noloss <- subset(train,hadloss == "N")
train.losses <- subset(train,hadloss == "Y")
train.superlosers <- subset(train, loss > 20)
# head(sampleSubmission, n=5)
# head(test, n=5)
head(train.losses, n=5)

summary(train$f3)


fit <- lm(f729~loss, data=train.losses)

plot(fit)


hist(train.losses$f311, length(train.losses$loss))
#boxplot(train$f2,train$hadloss)
qplot(hadloss,f733,data=train, geom="boxplot", na.rm=T)


####
ggplot(train,aes(x=hadloss,y=f400)) + geom_boxplot()
####
ggplot(train,aes(x=f400,y=loss)) + geom_jitter()



names(train)
plot(train.losses$f311, train.losses$loss)
lines(lowess(train$loss, train$f729))
describe.by(train$f5,train$hadloss)

lm(train$loss, train$f729)
# f1 bimodal : not very telling
# f3 no help

# loss = f2 * 10 (cap at 100)
# loss = (200-f729)/2
##############################################################################
test$loss <- round((((200-test$f729) + (test$f2 * 10)) / 7),0)
summary(test$loss)

# Format and export the prediction
convalytics.prediction <- test[,c("id","loss")]
# Write out the submission file for kaggle:
write.csv(convalytics.prediction, file = "convalytics_prediction-xx.csv", row.names=F)
########################################################################################

# summary(train$hadloss)
# help(memory.size)
# Aplot <- qplot(A,A,data=train.selection, geom="violin", na.rm=T)
# Bplot <- qplot(B,B,data=train.selection, geom="violin", na.rm=T)
# Cplot <- qplot(C,C,data=train.selection, geom="violin", na.rm=T)
# Dplot <- qplot(D,D,data=train.selection, geom="violin", na.rm=T)
# Eplot <- qplot(E,E,data=train.selection, geom="violin", na.rm=T)
# Fplot <- qplot(F,F,data=train.selection, geom="violin", na.rm=T)
# Gplot <- qplot(G,G,data=train.selection, geom="violin", na.rm=T)


Aplot <- qplot(A,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Bplot <- qplot(B,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Cplot <- qplot(C,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Dplot <- qplot(D,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Eplot <- qplot(E,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Fplot <- qplot(F,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Gplot <- qplot(G,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)

grid.arrange(Aplot, Bplot,Cplot,Dplot,Eplot,Fplot,Gplot,ncol=4)
#ggplot(train.selection, aes(x=A, na.rm=T)) + geom_histogram(binwidth=1) 


# Look at distributions
qplot(state, data=train.selection, geom="histogram")
qplot(group_size, data=train.selection, geom="histogram")
qplot(homeowner, data=train.selection, geom="histogram")
qplot(car_age, data=train.selection, binwidth = 10, geom="histogram")   # Car Age > 15 ... "A" from 2 to 0 ... E from 1 to 0. ...F could be changed from 3 down to 0.
boxplot(train.selection$A ~ train.selection$car_age)
qplot(car_value, data=train.selection, geom="histogram")
qplot(risk_factor, data=train.selection, geom="histogram")
qplot(married_couple, data=train.selection, geom="histogram")
qplot(c_previous, data=train.selection, geom="histogram")
qplot(duration_previous, data=train.selection, geom="histogram")
qplot(group_size, data=train.selection, geom="histogram")


training <- subset(train.selection, state == "IN")
training <- subset(train.selection, car_age  > 15)
Aplot <- qplot(A,data=training, binwidth = 1, geom="histogram", na.rm=T)
Bplot <- qplot(B,data=training, binwidth = 1, geom="histogram", na.rm=T)
Cplot <- qplot(C,data=training, binwidth = 1, geom="histogram", na.rm=T)
Dplot <- qplot(D,data=training, binwidth = 1, geom="histogram", na.rm=T)
Eplot <- qplot(E,data=training, binwidth = 1, geom="histogram", na.rm=T)
Fplot <- qplot(F,data=training, binwidth = 1, geom="histogram", na.rm=T)
Gplot <- qplot(G,data=training, binwidth = 1, geom="histogram", na.rm=T)

grid.arrange(Aplot, Bplot,Cplot,Dplot,Eplot,Fplot,Gplot,ncol=4)