# 1) Load data ----
library(readxl)
cc15 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2015")
cc16 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2016")
names(cc15) <- make.names(names(cc15))
names(cc16) <- make.names(names(cc16))

# 2) Data Preparation ----
# Start with cc 2016 result form Souce_Code REJ
library(dplyr)

cc1516 <- cc16 %>%
  filter(Branch_Code == "REJ") %>%
  left_join(cc15, by = c("ID" = "ID")) %>%
  filter(distinct())




# 3) Model createion
#Randomly shuffle the data
yourData<-yourData[sample(nrow(yourData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}