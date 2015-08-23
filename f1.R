library('parallel')
library('RCurl')
#library('doParallel')
training_file<-'pml-training.csv'
test_file<-'pml-testing.csv'

files_url<-'https://d396qusza40orc.cloudfront.net/predmachlearn/'
training_url<-paste(files_url,training_file,sep="")
testing_url<-paste(files_url,test_file,sep="")
print(training_url)
print(testing_url)

if(!exists('xURL_training')) {
  xURL_training<-getURL(training_url,ssl.verifypeer=FALSE)  
}

if(!exists('xURL_testing')) {
  xURL_testing<-getURL(testing_url,ssl.verifypeer=FALSE)
}

#xURL_testing<-getURL(testing_url,ssl.verifypeer=FALSE)
# 
# if(! file.exists(training_file)) {
#   
#   download.file(paste(files_url,training_file,sep=""),training_file,method='curl')
# }
# 
# if(! file.exists(test_file)) {
#   download.file(paste(files_url,test_file,sep=""),test_file,method='curl')
# }


library(caret);library(kernlab);#data(spam)
if (!exists('maindf')) {
  maindf<-read.csv(textConnection(xURL_training))
}

if (!exists('testdf')) {
  testdf<-read.csv(textConnection(xURL_testing))
}
traindf<-maindf
#remove ID
traindf<-traindf[,-1]
set.seed(1234)
inTrain<-createDataPartition(y=traindf$classe,p=0.8,list=FALSE)


#nearzerovariancecols<-nearZeroVar(training,foreach=TRUE,allowParallel=TRUE)

#colnames(traindf2)
#preProc <- preProcess(trdata,method="pca",thres=.8)
#modelFit<-train(classe~.,data=trdata)
#colnames(trdata)
summary(traindf)
summary(testdf)
names<-colnames(traindf)
mycols<-grep('arm',names)
mycols<-c(mycols,grep('forearm',names))
mycols<-c(mycols,grep('belt',names))
mycols<-c(mycols,grep('user_name',names))
mycols<-c(mycols,grep('class',names))
mycols<-sort(unique(mycols))



traindf<-traindf[,mycols]

training<-traindf[inTrain,]
testing<-traindf[-inTrain,]


numeric_columns<-sapply(training,is.numeric)

traindf<-training[,numeric_columns]
testdf<-testing[,numeric_columns]
if(!exists('nearzerovariancecols')) {
  nearzerovariancecols<-nearZeroVar(traindf)
  
}

traindf2<-traindf[,-nearzerovariancecols]
testdf2<-testdf[,-nearzerovariancecols]


traindf2$classe<-training$classe
testdf2$classe<-testing$classe

control <- trainControl(method="repeatedcv", number=40, repeats=3)


if(!exists('modfit1')) {
  modfit1<-train(classe~.,data=traindf2,preProcess=c("pca","knnImpute"),trControl=control)
  
}



if(!exists('modfit2')) {
  modfit2<-train(classe~.,data=traindf2,preProcess=c("ica","knnImpute"),method='gbm',trControl=control)
  
}

if(!exists('modfit3')) {
  modfit3<-train(classe~.,data=traindf2,preProcess=c("ica","knnImpute"),method='rf',trControl=control)
  
}

if(!exists('modfit4')) {
  modfit4<-train(classe~.,data=traindf2,preProcess=c("ica","knnImpute"),method='svmLinear',trControl=control)
  
}

if(!exists('modfit5')) {
  modfit5<-train(classe~.,data=traindf2,preProcess=c("knnImpute"),trControl=control)
  
}



if(!exists('modfit6')) {
  modfit6<-train(classe~.,data=traindf2,preProcess=c("knnImpute"),method='gbm',trControl=control)
  
}

if(!exists('modfit7')) {
  modfit7<-train(classe~.,data=traindf2,preProcess=c("knnImpute"),method='rf',trControl=control)
  
}

if(!exists('modfit8')) {
  modfit8<-train(classe~.,data=traindf2,preProcess=c("knnImpute"),method='svmLinear',trControl=control)
  
}


if(!exists('modfit9')) {
  modfit9<-train(classe~.,data=traindf2,preProcess=c("knnImpute"),method='C5.0',trControl=control)
  
}


predictions1<-predict(modfit1,testdf2,na.action=na.pass)
predictions2<-predict(modfit2,testdf2,na.action=na.pass)
predictions3<-predict(modfit2,testdf2,na.action=na.pass)
predictions4<-predict(modfit2,testdf2,na.action=na.pass)
predictions5<-predict(modfit5,testdf2,na.action=na.pass)
predictions6<-predict(modfit6,testdf2,na.action=na.pass)
predictions7<-predict(modfit7,testdf2,na.action=na.pass)
predictions8<-predict(modfit8,testdf2,na.action=na.pass)
predictions9<-predict(modfit9,testdf2,na.action=na.pass)

<<<<<<< HEAD
predictions3<-predict(modfit1,traindf2)
predictions4<-predict(modfit1,traindf2)

length(predictions1)
length(predictions2)
length(predictions3)
length(predictions4)

=======
cm1<-confusionMatrix(predictions1,testdf2$classe)
cm2<-confusionMatrix(predictions2,testdf2$classe)
cm3<-confusionMatrix(predictions3,testdf2$classe)
cm4<-confusionMatrix(predictions4,testdf2$classe)
>>>>>>> 478f1e15fb32cd70d5391c0b88790aebc37395a4

cm5<-confusionMatrix(predictions5,testdf2$classe)
cm6<-confusionMatrix(predictions6,testdf2$classe)
cm7<-confusionMatrix(predictions7,testdf2$classe)
cm8<-confusionMatrix(predictions8,testdf2$classe)
cm9<-confusionMatrix(predictions9,testdf2$classe)

trellis.par.set(caretTheme())

#kappas<-c()
#kappas<-c(kappas,cm1$overall$)

#preProc <- preProcess(classe~.,traindf,method="pca",thres=.8)
#numeric_columns<-sapply(traindf,is.numeric)
#traindf2<-traindf[,numeric_columns]

#newtraining<-model.matrix(classe~.,data=training)

#corrMatrix<-cor(traindf)
#highlyCorrelated<-findCorrelation(corrMatrix,cutoff=0.5)
#training_final<-traindf[,mycols]
#modfit<-train(classe~.,method="gbm",data=training,preProcess="pca")
#preProc <- preProcess(classe~.,training_final,method="pca",thres=.8)
#names_of_interest<-sort(unique(c(names1,names2,names3)))