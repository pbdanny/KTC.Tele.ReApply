# 1) Load data ----
library(readxl)
cc15 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2015")
cc16 <- read_excel("/Users/Danny/Documents/R Project/KTC.Tele.ReApply/Mapping_CC_2015-2016.xlsx",
                   sheet = "CC_2016")
occupation <- read_excel("/Users/Danny/Share Win7/Occupation_Code_Frontend.xlsx", sheet = 1)
names(cc15) <- make.names(names(cc15))
names(cc16) <- make.names(names(cc16))

# 2) Data Preparation ----

# Start with cc 2016 result form Souce_Code REJ

library(dplyr)

cc1516 <- cc16 %>%
  filter(Branch_Code == "REJ") %>%  # Choose only REJ
  left_join(cc15, by = c("ID" = "ID")) %>%  # left_join with cc15 key = ID
  left_join(occupation, by = c("Occupation.x" = "Desc")) %>%
  left_join(occupation, by = c("Occupation.y" = "Desc")) %>%
  rename(OccupationCode.x = Code.x) %>%
  rename(OccupationCode.y = Code.y) %>%
  filter(!is.na(OccupationCode.x) & !is.na(OccupationCode.y)) %>%
  mutate(ApproveDate.x = as.Date(ApproveDate.x)) %>%  # Covnert AppovedDate to Data
  arrange(ApproveDate.x) %>%  # Sort ascending by ApprovedDate 
  distinct(ID, .keep_all = TRUE) %>%  # Distinct by ID (keep the first one) , keep all varibles
  mutate(ApproveDate.y = as.Date(ApproveDate.y)) %>%
  mutate(Appl_In_Date.x = as.Date(Appl_In_Date.x, format("%d/%m/%Y"))) %>%
  mutate(Appl_In_Date.y = as.Date(Appl_In_Date.y, format("%d/%m/%Y"))) %>%
  # Create diff flag for analysis
  mutate(Monthly.Salary.diff = Monthly_Salary.x - Monthly_Salary.y) %>%
  mutate(OccupationCode.diff.flag = ifelse(OccupationCode.x == OccupationCode.y, 0, 1)) %>%
  mutate(ZipCode.diff.flag = ifelse(Zipcode.x == Zipcode.y, 0, 1))

# 3) Explanatory Data Analysis ----


# 4) Predictive model creation ----

# 4.1) Select features
df <- cc1516 %>%
  select(Monthly_Salary.y, Monthly.Salary.diff, Age.y, 
         OccupationCode.diff.flag, ZipCode.diff.flag, Result.x) %>%
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
n_folds <- 5
folds <- cut(seq(1, nrow(df.train)), breaks = n_folds, labels = FALSE)

# 4.4) Perform ML & 5 fold cross validation

model.perf <- data.frame()  # Create model.perf 

for(i in 1:n_folds){
  
  # Segement your data by fold using the which() function 
  df.train.folds.idx <- which(folds == i, arr.ind = TRUE)
  df.train.folds <- df.train[df.train.folds.idx, ]
  df.cv.folds <- df.train[-df.train.folds.idx, ]
  
  # train GLM model
  glm_fit <- glm(Appr.flag ~ ., data = df.train.folds, family = "binomial",
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

mean(model.perf[, 1])
