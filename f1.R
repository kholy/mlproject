library('parallel')
#library('doParallel')
training_file<-'pml-training.csv'
test_file<-'pml-testing.csv'

files_url<-'https://d396qusza40orc.cloudfront.net/predmachlearn/'
training_url<-paste(files_url,training_file,sep="")
testing_url<-paste(files_url,test_file,sep="")
print(training_url)
print(testing_url)
if(! file.exists(training_file)) {
  
  download.file(paste(files_url,training_file,sep=""),training_file,method='curl')
}

if(! file.exists(test_file)) {
  download.file(paste(files_url,test_file,sep=""),test_file,method='curl')
}


library(caret);library(kernlab);#data(spam)
if (!exists('maindf')) {
  maindf<-read.csv(training_file)
}

if (!exists('testdf')) {
  testdf<-read.csv(test_file)
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

nearzerovariancecols<-nearZeroVar(traindf)
traindf2<-traindf[,-nearzerovariancecols]
testdf2<-testdf[,-nearzerovariancecols]


traindf2$classe<-training$classe
testdf2$classe<-testing$classe

modfit1<-train(classe~.,data=traindf2,preProcess=c("pca","knnImpute"))
modfit2<-train(classe~.,data=traindf2,preProcess=c("ica","knnImpute"))

predictions1<-predict(modfit1,traindf2,na.action=na.pass)
predictions2<-predict(modfit2,traindf2,na.action=na.pass)

predictions3<-predict(modfit1,traindf2)
predictions4<-predict(modfit1,traindf2)

length(predictions1)
length(predictions2)
length(predictions3)
length(predictions4)


confusionMatrix(predictions1,traindf2$classe)
confusionMatrix(predictions2,traindf2$classe)


predictions1<-predict(modfit1,testdf2)
predictions2<-predict(modfit2,testdf2)

confusionMatrix(predictions1,testdf2$classe)
confusionMatrix(predictions2,testdf2$classe)
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