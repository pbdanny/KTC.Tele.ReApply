## 1) Load data ----

library(readxl)

lead <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/List Reapply_Jan-May'16_78577 11.43.10 PM.xlsx", 
                   sheet = 1)
lead <- lead[!is.na(lead$ID), ]  # remove Lead without ID

cc15 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2015")
cc16 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2016")
pl15 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/MIS_PL_2015-2016_Income.xlsx", 
                   sheet = "MIS_PL_2015")
pl16 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/MIS_PL_2015-2016_Income.xlsx", 
                   sheet = "MIS_PL_2016")
occupation <- read_excel("/Users/Danny/Share Win7/Occupation_Code_Frontend.xlsx", sheet = 1)
province <- read_excel("/Users/Danny/Share Win7/province.xlsx", sheet = "Region_KTC")

names(lead) <- make.names(names(lead))
names(cc15) <- make.names(names(cc15))
names(cc16) <- make.names(names(cc16))
names(pl15) <- make.names(names(pl15))
names(pl16) <- make.names(names(pl16))

## 2) Explanatory Data Analysis ----

# 2.1) Lead analysis

library(dplyr)
print(paste0("Total Lead : ", nrow(lead)))
print(paste0("Distinct Lead :", nrow(distinct(lead, ID))))
# Total Reject Lead 201506-201510 78,577, Distinct lead 74,990
# Duplicate obs 3587, why?

# Select only the duplicate obs
leadDup <- lead %>%
  arrange(ID, Data.Mth) %>%
  filter(duplicated(ID))

# Select only the first duplicated record 
leadDupFirstLast <- lead %>%
  arrange(ID, Data.Mth) %>%
  filter(duplicated(ID) | duplicated(ID, fromLast = TRUE)) %>%  # Filter all the duplicated (include first obs)
  filter(!duplicated(ID)) %>%  #  Filter only first obs
  left_join(leadDup, by = c("ID", "ID"))  # join with the duplicated 

# Check if Duplication in many months : All are truely 
leadDupFirstLast %>%
  count(Data.Mth.x == Data.Mth.y)

# How mush duplicated by source code? : Dup by same Source Code 2560, Different Source Code 1027
leadDupFirstLast %>%
  count(AGENT.SOURCE.CODE.x == AGENT.SOURCE.CODE.y)

# How mush duplicated by Product : same products 2507; diff product = 1080
leadDupFirstLast %>%
  count(Product.CC.PL.RL..x == Product.CC.PL.RL..y)

# Cross tabulation duplication by source code - products
leadDupFirstLast %>%
  mutate(sameSource = (AGENT.SOURCE.CODE.x == AGENT.SOURCE.CODE.y)) %>%
  mutate(sameProduct = (Product.CC.PL.RL..x == Product.CC.PL.RL..y)) %>%
  count(sameSource, sameProduct)

# Same source code & same product 1897
# Same source code ; diff product 663
# Diff source code ; same product 610
# Diff source code ; diff product 417

# 2.2) Reject CC lead and CC mapping analysis

library(dplyr)
lead %>%
  filter(!grepl("^[A-Z]{2}L", Product.CC.PL.RL.)) %>% # select not PL
  filter(!duplicated(ID)) %>%
  summarise(n = n())
# Unique ID from Reject CC 14316

cc15 %>%
  filter(!duplicated(ID)) %>%
  summarise(n = n())
# Also unique ID 14316

# X check lead - cc15
l <- lead %>%
  filter(!grepl("^[A-Z]{2}L", Product.CC.PL.RL.)) %>% # select not PL
  filter(!duplicated(ID)) %>%
  left_join(cc15, by = c("ID" = "ID")) %>%

## 3) Data preparation for Learning model ----

library(dplyr)

# 3.1) Select only combineable varibles from cc and pl
# List column names & find common column names
n.cc <- names(cc15)
n.pl <- names(pl15)
join.col <- n.cc[n.cc %in% n.pl]

# Rename cc and pl column for data mearge
pl15 <- pl15 %>%
  rename(SubType = LoanType) %>%
  rename(Monthly_Salary = Income) %>%
  rename(firstUsedAmt = DrawDown)

pl16 <- pl16 %>%
  rename(SubType = LoanType) %>%
  rename(Monthly_Salary = Income) %>%
  rename(firstUsedAmt = DrawDown)

cc15 <- cc15 %>%
  rename(firstUsedAmt = spending)

cc16 <- cc16 %>%
  rename(firstUsedAmt = spending)

# List renames column names for common column
n.cc <- names(cc15)
n.pl <- names(pl15)
join.col <- n.cc[n.cc %in% n.pl]

# Cross check same column name for cc and pl
# col.join <- n.pl[n.pl %in% n.cc]
# sum(!(join.col %in% col.join))

# Select only same names
cc15 <- cc15[ ,join.col]
cc16 <- cc16[ ,join.col]
pl15 <- pl15[ ,join.col]
pl16 <- pl16[ ,join.col]

rm(list = c("join.col", "n.cc", "n.pl"))

# 3.2) Combine CC & PL data 15-16 filter only Branch = REJ
dat <- cc15 %>%
  bind_rows(pl15) %>%
  bind_rows(cc16) %>%
  bind_rows(pl16) %>%
  filter(Branch_Code == "REJ")  # only in Reject project

# Check duplication ID
dup <- dat %>%
  arrange(ID, desc(ApproveDate)) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Test the duplication data
dat.dup <- dat %>%
  filter(ID %in% dup$ID) %>%
  arrange(ID)

# Accept duplication assume different obs
rm(list = c("cc15", "cc16", "dat.dup", "pl15", "pl16", "dup", "lead"))


# 3.3) Prepare data for Machine Learnin Model
dat.rej <- dat %>%
  left_join(occupation, by = c("Occupation" = "Desc")) %>%
  rename(OccupationCode = Code) %>%
  mutate(Left2_Zipcode = substr(Zipcode, 1, 2)) %>%
  left_join(province) %>%
  select(-c(1:9), -c(11:12), -Channel, -Channel_Group, -Left2_Zipcode, -Province_Tha, -Region_Tha,
         -RefCode, -City_Type, -Province_Eng, -Region_Eng, -Criteria_Code, -Branch_Code,
         -SubType, -Branch_Code, -Occupation, -Work_Place, -Doc_Waive, -Flag_Test, -OccupationCode,
         -Zipcode, -DOB, -Income_Range, -New.Exist, -Income15_per_Head_per_Year, -firstUsedAmt, 
         -LineLimit) %>%
  filter(Product != "FIX") %>%
  filter(Monthly_Salary > 10000 | is.na(Monthly_Salary)) %>%
  filter(Age < 1 | !is.na(Age)) %>%
  # Create target varible Result in D, R = 0 , Result in A & Product = CC -> 1, 
  # Result in A & Product = PL -> 1
  mutate(Target = if_else(Result == "A" & Product == "CC", 1, 
         if_else(Result == "A" & Product == "PROUD", 1, 0))) %>%
  # Flag decline reason realted to Cash & Debt 
  mutate(Decline.Cash = if_else(Result_Description %in% 
                                 c("D01", "D02", "D09", "D10", "D11", "D21", 
                                   "D22", "D23", "D24"), 1, 0)) %>%
  select(-Product, -Result, -Result_Description)

## 4) Machine Learning with Generalized Linear Model methods creation ----

# 4.1) Select features

# 4.2) Split train and test
set.seed(1)
train.idx <- sample(1:nrow(df), size = floor(0.8*nrow(df)))  # Create training size 80%
df.train <- df[train.idx, ]
df.test <- df[-train.idx, ]

# 4.3) Create 5 fold data validation

# Randomly shuffle the data
set.seed(2)
df.train <- df.train[sample(nrow(df.train)), ]

# Create 5 equally size folds idx
n_folds <- 10
folds <- cut(seq(1, nrow(df.train)), breaks = n_folds, labels = FALSE)

# 4.4) Perform ML & 5 fold cross validation

model.perf <- data.frame()  # Create model.perf for store auc in each validation

for(i in 1:n_folds){
  
  # Segement your data by fold using the which() function 
  df.train.folds.idx <- which(folds == i, arr.ind = TRUE)
  df.train.folds <- df.train[df.train.folds.idx, ]
  df.cv.folds <- df.train[-df.train.folds.idx, ]
  
  # train GLM model
  glm_fit <- glm(Target ~ ., data = df.train.folds, family = "binomial"(link = "logit"),
                 na.action = na.omit)
  
  # measure model performanc with ROC & AUC
  library(ROCR)
  cv.pred <- predict(glm_fit, newdata = na.omit(df.cv.folds), type = "response")
  
  # Create ROC Curve
  pred <- prediction(cv.pred, na.omit(df.cv.folds)$Target)  # Create ROCR::prediction object
  
  # Create ROCR:performance object tpr = true positive, fpr = false positive
  pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(filename = paste0(n_folds, "foldsCV_", i, ".jpg"))
  plot(pred.perf, main = paste0(n_folds, " folds cv #", i))
  dev.off()
  
  # Calculate Area Under Curve (auc) and precision
  auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
  auc <- auc@y.values[[1]]
  model.perf <- rbind(model.perf, auc)  # Store each cv auc in model
}
# Average each CV model auc (area under curve)
mean(model.perf[, 1])

# Anova test
anova(glm_fit, test = "Chisq")

# Use precision : Accept high FP

# Precision {TP(TP+FP)} model = 1/(1+4) = 0.2
# Precision all recurrence (all 1) precision = 55/(55+375) = 0.12
# Presision all no recurrence (all 0) precision = 0/(0+0) = 0

## 5) ML model wtih caret package ----

# 5.1) Select features

df <- dat.rej

# 5.2) Balance data with SMOTE methods

library(DMwR)
table(df$Target)
df <- as.data.frame(na.omit(df))  # SMOTE apply only to dataframe
df$Target <- as.factor(df$Target)  # SMOTE apply only to factor data (since it boot straping with k nearest neighbours)
df$City_Type_Code <- as.factor(df$City_Type_Code)
df$Decline.Cash <- as.factor(df$Decline.Cash)
ndf <- SMOTE(Target ~ ., df, proc.over = 100, proc.under = 200) # Over sampling minority group (Target = 1) 2 times
table(ndf$Target)

# 5.2) Create index for training data
# Evenly distributed with result data use createDataPartition fn

library(caret)

result <- data.frame(ndf)[,"Target"]
train_idx <- createDataPartition(result, p = 0.8, list = FALSE)
rm(result)

# 5.3) Training model
df <- ndf %>%
  mutate(Target = factor(Target))

train <- as.data.frame(df[train_idx, ])
test <- as.data.frame(df[-train_idx, ])

# Create tuning parameter grid with all combination of possible value with expand.grid fn
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))

glmnet_ctrl <- trainControl(method = "cv", number = 10)

# Training model with train() fn 
glmnet_fit <- train(Target ~ ., data = train,
                    method = "glmnet",
                    # family = binomial,
                    na.action = na.omit,  # allow na in model
                    # preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)

glmnet_fit

# 5.4) Predict from model
# Predict the binary output (TRUE, FALSE)
pred_result <- predict(glmnet_fit, newdata = na.omit(test))
table(pred_result)

# Predict the binary output (TRUE, FALSE)
pred_prob <- predict(glmnet_fit, newdata = na.omit(test), type = "prob")
head(pred_prob)

# 5.5) Use ROCR pagckage to print out the ROC and AUC

library(ROCR)

# Create ROC Curve
pred <- prediction(as.numeric(as.character(pred_result)), 
                   as.numeric(as.character(test$Target)))  # Create ROCR::prediction object

# Create ROCR:performance object tpr = true positive, fpr = false positive
pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(pred.perf)

# Calculate Area Under Curve (auc) and precision
auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
auc <- auc@y.values[[1]]
auc
