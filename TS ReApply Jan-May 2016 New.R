## 1) Load data ----
library(readxl)
lead <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/List Reapply_Jan-May'16_78577 11.43.10 PM.xlsx", 
                   sheet = 1)
lead <- lead[!is.na(lead$ID), ]
cc15 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2015")
cc16 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2016")
occupation <- read_excel("/Users/Danny/Share Win7/Occupation_Code_Frontend.xlsx", sheet = 1)
province <- read_excel("/Users/Danny/Share Win7/province.xlsx", sheet = "Region_KTC")
names(lead) <- make.names(names(lead))
names(cc15) <- make.names(names(cc15))
names(cc16) <- make.names(names(cc16))

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

# Combine CC data 15-16 filter only Branch = REJ , latest result
cc1516Rej <- cc15 %>%
  bind_rows(cc16) %>%
  filter(Branch_Code == "REJ") %>%
  arrange(ID, desc(ApproveDate)) %>%  # incase of duplicate REJ choose last one on top
  filter(!duplicated(ID)) %>% # Select the first obs 
  left_join(occupation, by = c("Occupation" = "Desc")) %>%
  rename(OccupationCode = Code) %>%
  mutate(Left2_Zipcode = substr(Zipcode, 1, 2)) %>%
  left_join(province) %>%
  select(-c(3:9), -Work_Place, -Spending_Range, -Spending60, -Occupation,
         -Income_Range, -QUEUE, -Channel, -Channel_Group, -Left2_Zipcode, -Province_Tha, -Region_Tha,
         -RefCode, -City_Type, -Province_Eng, -Region_Eng)

# Create pre Reject project data
preRej <- cc15 %>%
  arrange(ID, ApproveDate) %>%
  filter(!duplicated(ID)) %>%
  left_join(occupation, by = c("Occupation" = "Desc")) %>%
  rename(OccupationCode = Code) %>%
  mutate(Left2_Zipcode = substr(Zipcode, 1, 2)) %>%
  left_join(province) %>%
  select(-c(3:9), -Work_Place, -Spending_Range, -Spending60, -Occupation,
         -Income_Range, -QUEUE, -Channel, -Channel_Group, -Left2_Zipcode, -Province_Tha, -Region_Tha,
         -RefCode, -City_Type, -Province_Eng, -Region_Eng)

# Join after Reject project <- Pre Project
cc1516RejPre <- cc1516Rej %>%
  left_join(preRej, by = c("ID" =  "ID")) %>%
  mutate(Appl_In_Date.x = as.Date(Appl_In_Date.x, format("%d/%m/%Y"))) %>%
  mutate(Appl_In_Date.y = as.Date(Appl_In_Date.y, format("%d/%m/%Y"))) %>%
  mutate_each(funs(as.Date), ApproveDate.x, ApproveDate.y, DOB.x, DOB.y) %>%
  # Remove Occupation Code = n/a
  filter(!is.na(OccupationCode.x) & !is.na(OccupationCode.y)) %>%
  # Create features for analysis
  mutate(Monthly.Salary.diff = Monthly_Salary.x - Monthly_Salary.y) %>%
  mutate(OccupationCode.diff.flag = ifelse(OccupationCode.x == OccupationCode.y, 0, 1)) %>%
  mutate(ZipCode.diff.flag = ifelse(Zipcode.x == Zipcode.y, 0, 1)) %>%
  mutate(Doc_Waive.y.flag = if_else(is.na(Doc_Waive.y), 0, 1))

rm(list = c("cc1516Rej", "lead", "preRej", "province"))

## 4) Machine Learning with Generalized Linear Model methods creation ----

# 4.1) Select features
df <- cc1516RejPre %>%
  select(Age.y, Monthly.Salary.diff, 
          Result.x) %>%
  mutate(Appr.flag = ifelse(Result.x == "A", 1, 0)) %>%
  select(-Result.x)

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

model.perf <- data.frame()  # Create model.perf 

for(i in 1:n_folds){
  
  # Segement your data by fold using the which() function 
  df.train.folds.idx <- which(folds == i, arr.ind = TRUE)
  df.train.folds <- df.train[df.train.folds.idx, ]
  df.cv.folds <- df.train[-df.train.folds.idx, ]
  
  # train GLM model
  glm_fit <- glm(Appr.flag ~ ., data = df.train.folds, family = "binomial"(link = "logit"),
                 na.action = na.omit)
  
  # measure model performanc with ROC & AUC
  library(ROCR)
  cv.pred <- predict(glm_fit, newdata = df.cv.folds, type = "response")
  
  # Create ROC Curve
  pred <- prediction(cv.pred, df.cv.folds$Appr.flag)  # Create ROCR::prediction object
  
  # Create ROCR:performance object tpr = true positive, fpr = false positive
  pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(filename = paste0(n_folds, "foldsCV_", i, ".jpg"))
  plot(pred.perf, main = paste0(n_folds, " folds cv #", i))
  dev.off()
  
  # Calculate AUC
  auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
  auc <- auc@y.values[[1]]
  model.perf <- rbind(model.perf, auc)  # Store each cv accuracy in model
}
# Average model accuracy
mean(model.perf[, 1])

# Anova test
anova(glm_fit, test = "Chisq")

## 5) ML model wtih caret package ----

# 5.1) Select features

library(dplyr)

df <- cc1516RejPre %>%
  select(Monthly.Salary.diff, Age.y,
         Result.x) %>%
  mutate(Appr.flag = ifelse(Result.x == "A", 1, 0)) %>%
  select(-Result.x)

# 5.2) Create index for training data
# Evenly distributed with result data use createDataPartition fn

library(caret)

result <- data.frame(df)[,"Appr.flag"]
train_idx <- createDataPartition(result, p = 0.8, list = FALSE)
rm(result)

# 5.3) Training model
df <- df %>%
  mutate(Appr.flag = factor(Appr.flag))

train <- as.data.frame(df[train_idx, ])
test <- as.data.frame(df[-train_idx, ])

# Create tuning parameter grid with all combination of possible value with expand.grid fn
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))

glmnet_ctrl <- trainControl(method = "cv", number = 10)

# Training model with train() fn 
glmnet_fit <- train(Appr.flag ~ ., data = train,
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

# Convert to numeric data for ROC Plot
pred_result_logi <- as.numeric((pred_result))
test_Appr_log <- as.numeric(as.logical(na.omit(test)[,"Appr.flag"]))
pr <- prediction(pred_result_logi, test_Appr_log)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
