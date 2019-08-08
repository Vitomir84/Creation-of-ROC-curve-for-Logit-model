# Logit-model---ROC-curve
Logit model - creation of ROC curve
#ROC curve

getwd()
binary <- read.csv("C:/Users/vjovanovic/Desktop/R Udemy/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")

#logit model
library(nnet)
head(binary)
mymodel <- multinom(income~., data = binary)
p <- predict(mymodel, binary)
head(p)
tab <- table(p, binary$income)
tab
#accuracy rate
acc <- sum(diag(tab))/sum(tab)
acc
#mislasification rate
mfc <- 1-acc
mfc

#Model performance evaluation
library(ROCR)

#predviđamo verovatnoću za svakog ispitanika
pred <- predict(mymodel, binary, type = 'prob')
head(pred)

#na osnovu cut-off tačke (najčešće 0.5) gledamo da li je predikcija tačna ili ne
#na osnovu funkcije tačnosti u zavisnosti od različitih cut-off tačaka gledamo koji cutoff da izaberemo
pred <- prediction (pred, binary$income)
pred

eval <- performance(pred, "acc")
plot(eval)
abline(h=0.85, v=0.4)

#Računanje true positive rate
tpr <- performance (pred, "tpr")
plot(tpr)
abline(a=0, b=1)

#crtanje roc curve
roc <- performance(pred, "tpr", "fpr")

plot(roc, colorize = TRUE)

#Auc - area under curve

auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc
