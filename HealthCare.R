library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(infotheo)
library(stringr)
library(RODBC)

dbhandle <- odbcDriverConnect('driver={SQL Server};
server=Akshay-PC\\SQLEXPRESS;database=Akshay;trusted_connection=true')


# 20% sample Massachusetts Medicare Covered Beneficiaries' demographic information
patient  <- sqlQuery(dbhandle, 'select * from Patient')
#patient <-read_csv("C:\\data\\Patient.csv")
patient <- patient %>%
  mutate(RACE=ifelse(race=="White",1, ifelse(race=="Black",2,ifelse(race=="Hispanic",3,4))))


# Medical spending in 2013, some people don't have record in this dataset simply because they didn't have any medical events/spending, so we need to replace NA with 0 later
spending2013 <-sqlQuery(dbhandle, 'select * from Spending2013')
#spending2013 <-read_csv("C:\\data\\Spending2013.csv")
spending2013 <- spending2013%>% 
select(BENE_ID, Spending2013)

# Medical spending in 2012, some people don't have record in this dataset simply because they didn't have any medical events/spending, so we need to replace NA with 0 later
spending2012 <-sqlQuery(dbhandle, 'select * from Spending2012')
#spending2012 <-read_csv("C:\\data\\Spending2012.csv")
spending2012 <- spending2012%>% 
  replace_na(list(IP=0, OP=0, Carrier=0, HHA=0, Hospice=0, SNF=0, DME=0)) %>% 
  mutate(Spending2012=IP+OP+Carrier+HHA+as.numeric(Hospice)+SNF+DME)%>%
  select(BENE_ID, Spending2012)

# HCC indicators
hcc <-sqlQuery(dbhandle, 'select * from HCC')
#hcc <-read_csv("C:\\data\\HCC.csv")
hcc<-hcc%>%rename(BENE_ID=bene_id)

HCC.label <- sqlQuery(dbhandle, 'select * from HCC_variable_list')
#HCC.label <-read_csv("C:\\data\\HCC_variable_list.csv")
HCC.label$"HCC No."<-  word(HCC.label$"HCC No",1)
names(hcc)[match(HCC.label$"HCC No.", names(hcc))]<-  HCC.label$'HCC Name' 
names(hcc)[-1]<-paste("Chronic Condition:", names(hcc)[-1], sep='')

# Diagnosis Clinical Classification: This is clinically meaningful grouping for 14,000 diagnosis codes  into 200+ groups. This dataset contains one row per patient, with columns as number of diagnosis detected during the entire year for individual CCS, for example, if CCS1=10 for patient 1, it means that patient was diagnosed for 10 times by doctors having a condition broadly described as CCS1. The intuition here is that more diagnoses, more often that patient had a medical event and thus more often cost would occur. CCS is related to HCC but not identical, because HCC only captures the chronic part of disease, but CCS captures both chronic and acute disease. For example, cancer is indicated by both HCC and CCS, but a hip/knee fracture maybe only captured by CCS. So you can imagine HCC and CCS related but not identical, we could explore how to incorporate them in model.
dxccs <-sqlQuery(dbhandle, 'select * from DxCCS')
#dxccs <-read_csv("C:\\data\\dxCCS.csv")

# de-duplicate
dxccs.label<-sqlQuery(dbhandle, 'select * from dxCCS_variable_list')
#dxccs.label <-read_csv("C:\\data\\dxCCS_variable_list.csv")
dxccs.label<- dxccs.label %>% filter(!duplicated(dxccs.label$"CCS Category" )) 
# clean dxccs names
dxccs.label$"CCS Category Description"<-str_replace_all(dxccs.label$"CCS Category Description",pattern = "'", replacement="")
# clean CCS Category
dxccs.label$"CCS Category" <- paste("CCS",dxccs.label$"CCS Category",sep='')
dxccs.label$"CCS Category" <- str_replace_all(dxccs.label$"CCS Category",pattern = "'", replacement="")
dxccs.label$"CCS Category" <- str_replace_all(dxccs.label$"CCS Category",pattern = " ", replacement="")
names(dxccs)[match(dxccs.label$"CCS Category", names(dxccs))]<-  dxccs.label$'CCS Category Description'   
names(dxccs)[-1]<-paste("Diagnosis:", names(dxccs)[-1], sep='')  


# Procedure: This is clinically meaningful grouping for procedures codes. This dataset contains one row per patient, with columns as number of procedures performed during the entire year. For example, a patient could have 1 Anesthesia. The file is too large, I split into two.

#pr<-read.csv("C:\\data\\Procedure.csv")
pr <-sqlQuery(dbhandle,'select * from Procedure')
pr<-pr[,-ncol(pr)] # drop last column

#pr.label<-read.csv("C:\\data\\Procedure_variable_list.csv")
pr.label<-sqlQuery(dbhandle,'select * from Procedure_variable_list')
match.index<-match(pr.label$"betos", names(pr))
pr.label<-pr.label[!is.na(match.index),]
names(pr)[match.index[!is.na(match.index)]]<-  pr.label$"description"
names(pr)[-1]<-paste("Procedure:", names(pr)[-1], sep='') 

# Merge with patient file, transform data frame into matrix, with each row as patient  
patient.level <-select(patient, BENE_ID, AGE, SEX, RACE) %>%
  left_join(spending2013, by="BENE_ID")%>%  
  left_join(spending2012, by="BENE_ID")%>%
  left_join(hcc, by="BENE_ID")%>%
  left_join(dxccs, by="BENE_ID") %>%
  left_join(pr, by="BENE_ID")

# from patient.level, create a data matrix contains all predictors, including demographic information such as age, sex, race, but drop spending 2012
X<- patient.level%>%
  select(-BENE_ID,-Spending2013,-Spending2012, -AGE, -SEX,-RACE)%>%
  data.matrix()


# Original Diagnosis Group and Procedure Group are counts, now create binary indicators 0-1 
X[X>=1]<-1
X[X==0|is.na(X)]<-0


# from patient.level, create a vector contains spending 2013, and a vector contains binary indicator for top 10% high cost patient
Y<-patient.level%>%
  select(Spending2013) %>%
  replace_na(list(Spending2013=0))%>%
  data.matrix()%>%
  as.vector() 


Y <-rank(Y)/length(Y)
Y<-as.numeric(Y>=0.9)
prop.table(table(Y))

#Part4

# Distribution of Age
#patient<-as.matrix(sapply(patient, as.numeric))  
#patient.level<-as.matrix(sapply(patient.level, as.numeric))  

patient.level %>% 
  ggplot(aes(AGE)) +
  geom_histogram(fill="pink",binwidth=5)+
  ggtitle("Age")+
  xlab("Age") +
  geom_vline(xintercept=mean(patient.level$AGE), colour='red') + 
  scale_x_continuous(breaks = seq(0 , max(patient.level$AGE) , 5 ) )+
  annotate("text", x = mean(patient.level$AGE), y = 1000, label = "Mean")


# Distribution of Gender
patient.level %>% 
  mutate(gender=ifelse(SEX==1,"Male", "Female"))%>%
  ggplot(aes(x=factor(1),  fill=gender)) +
  geom_bar(width=1 )+ coord_polar("y" )+
  ggtitle("Gender")+
  xlab("Gender") +
  scale_fill_brewer("Blues")+ 
  theme(axis.text.x=element_blank()) 


# Distribution of Race
patient.level %>% mutate(race=ifelse(RACE==1,"White", ifelse(RACE==2,"Black","Others")))%>%
  ggplot(aes(x=factor(1),  fill=race)) +
  geom_bar(width=1 )+ coord_polar("y" )+
  ggtitle("Race")+
  xlab("Race") +
  scale_fill_brewer("Blues")+ 
  theme(axis.text.x=element_blank()) 


patient.level%>%
  replace_na(list(Spending2012=0,Spending2013=0))%>%
  ggplot(aes(x=Spending2012, y=Spending2013))+
  geom_point(aes(color=Spending2013,alpha=Spending2013)) +
  scale_colour_gradient(low="blue",high="red") +
  theme(legend.position='none')+
  ggtitle("Medical Cost at 2013 versus Medical Cost at 2012")


#Part 5
mutual.info <-rep(0,ncol(X) ) # place-holder for info gain

for(i in 1:length(mutual.info)){
  mutual.info[i]<-mutinformation(X[,i],Y,method="emp")
} 

# standardize information gain by the maximum, the resulting number would be the percentage of the maximum information gain
mutual.info<-mutual.info/mutual.info[which.max(mutual.info)]

# create a dataframe contains mutual.info and feature names corresponding to the order of X
mi <- data.frame(mutual.info, var.name=colnames(X)) %>%
  mutate(order= rank(desc(mutual.info),  ties.method = c("random"))) %>%
  arrange(order)  

# create feature.class 
mi$feature.class <-rep(NA, length(mi$var.name))
mi$feature.class[grep(pattern="Chronic Condition", x=mi$var.name)] <- "Chronic Condition"
mi$feature.class[grep(pattern="Diagnosis", x=mi$var.name)] <- "Diagnosis"
mi$feature.class[grep(pattern="Procedure", x=mi$var.name)] <- "Procedure"


mi%>% 
    filter(order<=50)%>%  
    ggplot(aes(x=order, y=mutual.info, fill=factor(feature.class))) +
    geom_point( stat="identity") + 
  geom_text(aes(label=var.name,check_overlap=TRUE, size=100, angle=45, colour =factor(feature.class)))+
  xlab("Order of Feature Importance") + ylab("%Mutual Information")+
    ggtitle("Top Ranked Features by mutual information with High Cost Status")+
  theme(legend.position='none')+
  xlim(c(-5,55))+ylim(c(-0.5,1.5))


mi%>% 
  filter(order<=100)%>%select(var.name)



#Part 6
library("glmnet")
library("dplyr")
library("caret")
library("stringr")


set.seed(02138)
Xi<-X[,mi$var.name[mi$order<=100]]
TrainIndex  <- createDataPartition(y=seq(1,nrow(X),1), p=0.8, times=1)
trainX <-Xi[TrainIndex$Resample1,] 
trainY <-Y[TrainIndex$Resample1] 

testX <-Xi[-TrainIndex$Resample1,] 
testY <-Y[-TrainIndex$Resample1]

# Choose lambda via cross-validation
CV = cv.glmnet(x=trainX,y=trainY,family="binomial",type.measure = "class")

plot(CV)

L=CV$lambda.1se
Lmin = CV$lambda.min
L

Lmin

fit = glmnet(x=trainX,y=trainY,family="binomial",alpha=1,lambda=Lmin)

# Coefficients
b0 = fit$beta[,1] 

# Non-zero coefficients
b1 = b0[b0!=0]
predictors<- data.frame(var.name=names(b1)[order(abs(b1), decreasing = TRUE)],Coef=b1[order(abs(b1), decreasing = TRUE)] )


# Visualize it
predictors$feature.class <-rep(NA, length(predictors$var.name))
predictors$feature.class[grep(pattern="Chronic.Condition", x=predictors$var.name)] <- "Chronic Condition"
predictors$feature.class[grep(pattern="Diagnosis", x=predictors$var.name)] <- "Diagnosis"
predictors$feature.class[grep(pattern="Procedure", x=predictors$var.name)] <- "Procedure"

# features selected by LASSO
predictors$var.name


predictors%>%
  mutate(order=row_number())%>%
  filter(order<=50)%>%
  ggplot(aes(x=order, y=Coef, fill=factor(feature.class))) +
  geom_point( stat="identity") + 
  geom_text(aes(label=var.name,check_overlap=TRUE, size=100, angle=45, colour =factor(feature.class)))+
  xlab("Order of Feature Importance") + ylab("LASSO Coefficients")+
  ggtitle("Feature Importance")+
  theme(legend.position='none')+
  xlim(c(-5,55))


# LASSO model validation
LASSOprediction <- predict(fit, testX,type="class")

# performance measure

## create a place-holder for all model performance measures
perf <-data.frame(model=rep(NA,3*5), Measure=rep(c("Sensitivity", "Specificity", "Accuracy"), 5), value=rep(NA, 3*5))

perf$value[3]<-prop.table(table(LASSOprediction, testY))[1,1]+prop.table(table(LASSOprediction, testY))[2,2]

perf$value[1]<-prop.table(table(LASSOprediction, testY))[2,2]/(prop.table(table(LASSOprediction, testY))[1,2]+prop.table(table(LASSOprediction, testY))[2,2])

perf$value[2]<-prop.table(table(LASSOprediction, testY))[1,1]/(prop.table(table(LASSOprediction, testY))[1,1]+prop.table(table(LASSOprediction, testY))[2,1])

perf$model[1:3]<-"LASSO Logistic Regression"  



# Visualize it
perf%>% na.omit()%>%
  ggplot(aes(x=model, y=value,color=factor(model), label=value))+
  geom_point() +
  geom_text() +ggtitle("Compare Performance Measure")+
  facet_grid(Measure~.,scales = "free_y")+
  theme(legend.position="bottom")


#Part2

library(e1071)
library(caret)

set.seed(02138)

# create a data frame place-holder for 2-CV sensitivity, specificity and accuracy
perf.model1 <-data.frame(par=seq(10,100,10), #tuning parameter
                         sensitivity=rep(NA,length(seq(10,100,10))),
                         specificity=rep(NA,length(seq(10,100,10))),
                         accuracy=rep(NA,length(seq(10,100,10))))

for(i in 1:length(perf.model1$par)){
  
  Xi<- X[,mi$var.name[mi$order<=perf.model1$par[i]]]# select top i features
  mydata<-data.frame(x=Xi,y=as.factor(Y))
  TrainIndex  <- createDataPartition(y=seq(1,nrow(mydata),1), p=0.8, times=1)
  train_set <-mydata[TrainIndex$Resample1,]
  test_set <-mydata[-TrainIndex$Resample1,]
  fit<- naiveBayes(x=train_set [,-ncol(train_set )], y=train_set [,ncol(train_set )])
  pred <-predict(fit, test_set[,-ncol(mydata)]) 
  
  # performance measure
  perf.model1$accuracy[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]
  
  perf.model1$sensitivity[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,2]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2])
  
  perf.model1$specificity[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,1])
  
  
  
}

perf.model1%>% 
  gather(key=measure,value, sensitivity:accuracy)%>%
  ggplot(aes(x=par, y=value, colour=measure, label=value))+
  geom_line( )


perf.model1$par[which.max(perf.model1$sensitivity)]

perf$value[6]<-perf.model1$accuracy[which.max(perf.model1$sensitivity)]

perf$value[4]<-perf.model1$sensitivity[which.max(perf.model1$sensitivity)]

perf$value[5]<-perf.model1$specificity[which.max(perf.model1$sensitivity)]

perf$model[4:6]<-"Naive Bayes Classifier"  

# Visualize it
perf%>% na.omit()%>%
  ggplot(aes(x=model, y=value,color=factor(model), label=value))+
  geom_point() +
  geom_text() +ggtitle("Compare Performance Measure")+
  facet_grid(Measure~.,scales = "free_y")+
  theme(legend.position="bottom")


#Model 3 Decision Tree

library(rpart)
library(caret)
library(rpart.plot)
library(RColorBrewer)
set.seed(02138)

Xi<-X[,mi$var.name[mi$order<=100]] 
mydata<-data.frame(x=Xi,y=as.factor(Y))
TrainIndex  <- createDataPartition(y=seq(1,nrow(mydata),1), p=0.8, times=1)
train_set <-mydata[TrainIndex$Resample1,]
test_set <-mydata[-TrainIndex$Resample1,]


# use all features to build a full tree
fit<-rpart(y~.,method="class", y=TRUE,control=rpart.control(cp=0,xval=2), parms=list(split="information"),data=train_set)

# CP table
plotcp(fit,upper="size")

# Prune the tree
num.split<-fit$cptable[,"nsplit"] #possible number of splits

# create a data frame place-holder for 2-CV sensitivity, specificity and accuracy
perf.model2 <-data.frame(par=num.split, # tuning parameter
                         sensitivity=rep(NA,length(num.split)),
                         specificity=rep(NA,length(num.split)),
                         accuracy=rep(NA,length(num.split)))

for(i in 1:length(num.split)){
  fit.prune <- prune(fit,cp=fit$cptable[i,"CP"])
  pred<- predict(fit.prune,test_set[,-ncol(test_set)],type="class")
  
  # performance measure
  perf.model2$accuracy[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]
  
  perf.model2$sensitivity[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,2]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2])
  
  perf.model2$specificity[i]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,1])
  
}

# Visualize 
perf.model2%>% 
  gather(key=measure,value, sensitivity:accuracy)%>%
  ggplot(aes(x=par, y=value, colour=measure, label=value))+
  geom_line( ) 


prune.fit<-prune(fit, cp=fit$cptable[which.max(perf.model2$sensitivity),"CP"])
pred<- predict(prune.fit,test_set[,-ncol(test_set)],type="class")


predictors<- data.frame(var.name=names(prune.fit$variable.importance)[order(abs(prune.fit$variable.importance), decreasing = TRUE)],entropy=prune.fit$variable.importance[order(abs(prune.fit$variable.importance), decreasing = TRUE)] ) 
predictors$var.name<-gsub(pattern="x.", replacement="", x=predictors$var.name)

# standardize information gain by the maximum, the resulting number would be the percentage of the maximum information gain
predictors$entropy<-predictors$entropy/predictors$entropy[which.max(predictors$entropy)]


predictors$feature.class <-rep(NA, length(predictors$var.name))
predictors$feature.class[grep(pattern="Chronic.Condition", x=predictors$var.name)] <- "Chronic Condition"
predictors$feature.class[grep(pattern="Diagnosis", x=predictors$var.name)] <- "Diagnosis"
predictors$feature.class[grep(pattern="Procedure", x=predictors$var.name)] <- "Procedure"

# Visualize it
predictors%>%
  mutate(order=row_number())%>%
  filter(order<=50)%>%
  ggplot(aes(x=order, y=entropy, fill=factor(feature.class))) +
  geom_point( stat="identity") + 
  geom_text(aes(label=var.name,check_overlap=TRUE, size=100, angle=45, colour =factor(feature.class)))+
  xlab("Order of Feature Importance") + ylab("Entropy")+
  ggtitle("Feature Importance")+
  theme(legend.position='none')+
  xlim(c(-5,55)) 


# performance measure
accuracy<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]

sensitivity<-prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,2]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2])

specificity<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,1])


perf$value[9]<-accuracy

perf$value[7]<-sensitivity

perf$value[8]<-specificity

perf$model[7:9]<-"Decision Tree"  


# Visualize it
perf%>% na.omit()%>%
  ggplot(aes(x=model, y=value,color=factor(model), label=value))+
  geom_point() +
  geom_text() +ggtitle("Compare Performance Measure")+
  facet_grid(Measure~.,scales = "free_y")+
  theme(legend.position="bottom")

plot(fit, uniform=TRUE, main="Classification Tree")
text(prune.fit, use.n=TRUE, cex=.8, col="red")


#Part4 Random Forest
library(randomForest)
set.seed(02138)

mydata<-data.frame(x=X[,mi$var.name[mi$order<=100]],y=as.factor(Y))
TrainIndex  <- createDataPartition(y=seq(1,nrow(mydata),1), p=0.8, times=1)
train_set <-mydata[TrainIndex$Resample1,]
test_set <-mydata[-TrainIndex$Resample1,]

fit<-randomForest(y~., data=train_set, ntree=50, keep.forest=TRUE, importance=TRUE)
pred <- predict(fit, type='class',newdata=test_set[,-ncol(test_set)] )


# visualize it
dat<-data.frame(imp=importance(fit, type=1)[order(-importance(fit, type=1))], var.name=rownames(importance(fit, type=1))[order(-importance(fit, type=1))])%>%
  mutate(order=row_number()) 

dat$feature.class <-rep(NA, length(dat$var.name))
dat$feature.class[grep(pattern="Chronic.Condition", x=dat$var.name)] <- "Chronic Condition"
dat$feature.class[grep(pattern="Diagnosis", x=dat$var.name)] <- "Diagnosis"
dat$feature.class[grep(pattern="Procedure", x=dat$var.name)] <- "Procedure"

# Visualize it
dat%>%
  mutate(order=row_number())%>%
  filter(order<=50)%>%
  ggplot(aes(x=order, y=imp, fill=factor(feature.class))) +
  geom_point( stat="identity") + 
  geom_text(aes(label=var.name,check_overlap=TRUE, size=100, angle=45, colour =factor(feature.class)))+
  xlab("Order of Feature Importance") + ylab("mean decrease in node impurity")+
  ggtitle("Feature Importance")+
  theme(legend.position='none')+
  xlim(c(-5,55)) 


perf$value[12]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]

perf$value[10]<-prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,2]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2])

perf$value[11]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,1])

perf$model[10:12]<-"Random Forest"  



# Visualize it
perf%>% na.omit()%>%
  ggplot(aes(x=model, y=value,color=factor(model), label=value))+
  geom_point() +
  geom_text() +ggtitle("Compare Performance Measure")+
  facet_grid(Measure~.,scales = "free_y")+
  theme(legend.position="bottom")


#Model5 Boosting
library(ada)
library(caret)
set.seed(02138)

mydata<-data.frame(x=X[,mi$var.name[mi$order<=100]],y=as.factor(Y))
TrainIndex  <- createDataPartition(y=seq(1,nrow(mydata),1), p=0.8, times=1)
train_set <-mydata[TrainIndex$Resample1,]
test_set <-mydata[-TrainIndex$Resample1,]

fit <- ada(y~.,data=train_set,iter = 50, loss = "e", type = "discrete")
pred <-predict(fit,test_set [,-ncol(test_set )])    

varplot(fit)

perf$value[15]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]

perf$value[13]<-prop.table(table(pred, test_set[,ncol(test_set)]))[2,2]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,2]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,2])

perf$value[14]<-prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]/(prop.table(table(pred, test_set[,ncol(test_set)]))[1,1]+prop.table(table(pred, test_set[,ncol(test_set)]))[2,1])

perf$model[13:15]<-"Ada Boosting"  

# Visualize it
perf%>% na.omit()%>%
  ggplot(aes(x=model, y=value,color=factor(model), label=value))+
  geom_point() +
  geom_text() +ggtitle("Compare Performance Measure")+
  facet_grid(Measure~.,scales = "free_y")+
  theme(legend.position="bottom")