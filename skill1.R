train_data1<-read.csv("train.csv")
dim(train_data1)
train_data<-train_data1
data.frame() 
dim(train_data) 
summary(train_data$SalePrice) 
names(train_data)

#Partition and clean the data 
colnames(train_data)[colSums(is.na(train_data))>nrow(train_data)/2]
train_data<-train_data[,colSums(is.na(train_data))<nrow(train_data)/2]
table(colSums(is.na(train_data)))
ind<-sample(2,nrow(train_data),replace=T,prob = c(0.8,0.2))
train_data<-train_data[ind==1,]
validation_data<-train_data[ind==2,]
dim(train_data)
summary(train_data$SalePrice)

#Build random Forest model
set.seed(222)
library(randomForest)
attach(train_data)
model<-randomForest(SalePrice~GrLivArea+Neighborhood+OverallQual+TotalBsmtSF+X1stFlrSF+CentralAir+BsmtFinSF1+ExterQual+GarageArea+X2ndFlrSF, data = train_data, ntree=200 , importance=TRUE)
plot(model)
attributes(model)
model
varImpPlot(model,n.var = 10)


#validate the model
dim(validation_data)
validation<-predict(model,validation_data,na.action=na.roughfix)
write.csv(validation)

#apply model to test data
#Choose test.csv file here
test_data=read.csv(file.choose())
dim(test_data)
colnames(test_data)[colSums(is.na(test_data))>nrow(test_data)/2]
test_data<-test_data[,colSums(is.na(test_data))<nrow(test_data)/2]
table(colSums(is.na(test_data)))

attach(test_data)
#sapply(test_data, function(x) if("factor" %in% class(x) ) { })
colnames(validation_data)
colnames(test_data)
test<-predict(model,test_data)
write.csv(test,file = "house_price_prediction_test.csv", col.names = c(Id,SalePrice))
summary(test)
