library(caret);library(kernlab);#data(spam)
trfile<-'pml-training.csv'
tefile<-'pml-testing.csv'
if (!exists('trdata')) {
  trdata<-read.csv(trfile)
}
if (!exists('tedata')) {
  tedata<-read.csv(tefile)
}
set.seed(1234)

#preProc <- preProcess(trdata,method="pca",thres=.8)
#modelFit<-train(classe~.,data=trdata)
#colnames(trdata)
summary(tedata)
summary(trdata)
names<-colnames(trdata)
mycols<-grep('arm',names)
mycols<-c(mycols,grep('forearm',names))
mycols<-c(mycols,grep('belt',names))
mycols<-c(mycols,grep('user_name',names))
mycols<-c(mycols,grep('class',names))
mycols<-sort(unique(mycols))

trdata1<-trdata[,mycols]
preProc <- preProcess(trdata1[,1:159],method="pca",thres=.8)
#names_of_interest<-sort(unique(c(names1,names2,names3)))