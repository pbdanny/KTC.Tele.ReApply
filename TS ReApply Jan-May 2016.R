# Data Loading ----
library(readr)
dat <- read_tsv(file = "/Users/Danny/Share Win7/qry_TSReApply_Result.txt",
                   col_types = cols(.default = "c"), na = c("", " ", "NA"))
detach(package:readr, unload = TRUE)
names(dat) <- make.names(names(dat))

# Data Preparation ----
library(dplyr)
reapp <- dat %>%
  mutate(Old_Date = as.Date(paste0("2015-", Data.Mth, "-01"))) %>% 
  mutate(Old_Product = ifelse(grepl("VRL", Product.CC.PL.RL.), "RL", "CC")) %>%
  mutate(New_Date = as.Date(paste0(Month, "01"), format = "%Y%m%d")) %>%
  mutate(Diff_Month = 12*(as.numeric(format(New_Date, "%Y")) - as.numeric(format(Old_Date, "%Y"))) +
                          (as.numeric(format(New_Date, "%m")) - as.numeric(format(Old_Date, "%m")))) %>%
  mutate(AGE = as.integer(AGE)) %>%
  mutate(Monthly_Salary = as.numeric(Monthly_Salary)) %>%
  filter(Product == "CC")  # Select Product CC for different criteria

# De-duplication ---- 
# Check dupication part
# reapp %>%
#  group_by(THAI.NAME) %>%
#  summarise(n = n()) %>%
#  arrange(-n) %>%
#  head()

# View(reapp[reapp$THAI.NAME == "ไตรรงค์ กินไธสง",])

# Create row index having no. of THAI.NAME  = 1
dedup <- reapp %>%
  mutate(idx = row.names(reapp)) %>%  # store original rowname for reference index
  group_by(THAI.NAME) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(idx, THAI.NAME, n)

# Select distinct THAI.NAME, predictor ----
reg <- reapp[as.vector(as.integer(dedup$idx)),] %>%  # Use idx in dedup to filter out the duplicate
  select(Old_Reason_Desc, Old_Product,
         Occupation_Code, Doc_Waive, AGE,
         Appr, Diff_Month, Monthly_Salary) %>%
  filter(Monthly_Salary < 125000 & Monthly_Salary > 1000) %>%  # Remove outliner
  mutate(Doc_Waive = ifelse(is.na(Doc_Waive), "N", Doc_Waive))  # Clear all NA, could not yield model

rm(dedup)

# save(dat, reapp, reg, file = "CCReApply.RData")
load("CCReApply.RData")

# Exploratory Data Analysis ----
library(ggplot2)

# Bivariate plot with ggpairs
# library(GGally)
# ggpairs(data = reg)

# Bivariate EDA predictor vs predictor

h <- ggplot(data = reg, aes(x = AGE))
h + geom_density(aes(col = as.factor(Appr))) + labs(title = "Age group by Appr - Density")
# ggsave("age by Appr densityPlot.png", plot = last_plot()) 
h + geom_freqpoly(binwidth = 1, aes(col = as.factor(Appr))) + labs(title = "Age group by Appr - Frequency")
# ggsave("age by Appr histogramPlot.png", plot = last_plot())

i <- ggplot(data = reg, aes(x = Monthly_Salary))
i + geom_density(aes(col = as.factor(Appr))) + labs(title = "Monthly Salary group by Appr - Density")
# ggsave("salary by Appr densityPlot.png", plot = last_plot())

i + geom_freqpoly(aes(binwidth = 1, col = as.factor(Appr))) + labs(title = "Monthly Salary group by Appr - Frequency")
# ggsave("salary by Appr freqpolyPlot.png", plot = last_plot())

# Bivariate class vs predictors
g <- ggplot(data = reg, aes(x = AGE, y = Monthly_Salary, col = as.factor(Appr)))
g + geom_point()
# ggsave("salary - AGE group by Appr - pointPlot.png", plot = last_plot())

# saveRDS(reg, file = "ReApply regression data.RDS")
# reg <- readRDS("ReApply regression data.RDS")

# Split data into train and test with train_idx function ----

train_idx <- function (x, smp_size = 0.75) {
  smp <- floor(smp_size * nrow(x))  # How many obs need in sample
  set.seed(123)  # For reproducible purpose
  idx <- sample(seq_len(nrow(x)), size = smp)  #  Sample from vector of 1..nrow(x) with sample size defined
  return(idx)
}

# Split data to train & test
reg$Appr <- as.integer(reg$Appr)
idx <- train_idx(reg, smp_size = 0.8)
reg_train <- reg[idx, ]
reg_train <- rbind(reg_train, reg[reg$Occupation_Code == "05", ])
reg_test <- reg[-idx,]

# Regression analysis with glm ----
glm_fit <- glm(Appr ~ ., data = reg_train, family = binomial(link = "logit"))
summary(glm_fit)
train <- predict(glm_fit, type = "response")

test <- predict(glm_fit, reg_test, type = "response")

# Create confusion table for model accuracy
test <- ifelse(test >= 0.5, 1, 0)
table(test, reg_test$Appr)

# Plot ROC & AUC
library(ROCR)
test <- predict(glm_fit, newdata = reg_test, type = "response")
# Creat ROC Curve
pred <- prediction(test, reg_test$Appr)  # Create ROCR:prediction object
# Create ROCR:performance object tpr = true positive, fpr = false positive
pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(pred.perf)

# Calculate AUC
auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
auc <- auc@y.values[[1]]
auc   # Value closet to .5 -> mean not good model-

# Select predictor for better model
summary(glm_fit)

reg2 <- reg %>%
  select(AGE, Monthly_Salary, Doc_Waive, Old_Product, Appr)

idx <- train_idx(reg2, smp_size = 0.8)
reg_train <- reg2[idx, ]
reg_test <- reg2[-idx,]

glm_fit2 <- glm(Appr ~ . , reg_train, family = binomial)

summary(glm_fit2)
train <- predict(glm_fit2, type = "response")
test <- predict(glm_fit2, reg_test, type = "response")

test <- ifelse(test >= 0.5, 1, 0)
table(test, reg_test$Appr)

test <- predict(glm_fit2, newdata = reg_test, type = "response")
pred <- prediction(test, reg_test$Appr) 
pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(pred.perf)

auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
auc <- auc@y.values[[1]]
auc   # Value closet to .5 -> mean not good model-

# Classification analysis with LDA and QDA ----

# LDA Part ----
library(MASS)
lda_fit <- lda(Appr ~ ., data = reg_train)
lda_fit 

plot(lda_fit)
lda_pred <- predict(lda_fit, newdata = reg_test)
names(lad_pred)

lda_pred_class <- lda_pred$class
table(lda_pred_class, reg_test$Appr)

# Plot ROC & AUC
library(ROCR)
test <- predict(lda_fit, newdata = reg_test)
# Creat ROC Curve
lda_pred_class <- as.integer(test$class)
pred <- prediction(lda_pred_class, reg_test$Appr)  # Create ROCR:prediction object
# Create ROCR:performance object tpr = true positive, fpr = false positive
pred.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(pred.perf)

# Calculate AUC
auc <- performance(pred, measure = "auc")  # Create ROCR:performance object auc = accuracy
auc <- auc@y.values[[1]]
auc   # Value closet to .5 -> mean not good model-

# QDA part ----
library(MASS)
qda_fit <- qda(Appr ~ ., data = reg_train)
# Problems with rank deficiency

# Classification with KNN ----
library(class)
train_X <- as.matrix(reg_train[, -c(1,2,3,4,6)])  # Remove categorical data & turn to matrix
test_X <- as.matrix(reg_test[, -c(1,2,3,4,6)])  # Remove categorical data & turn to matrix
train_Direction <- as.matrix(reg_train["Appr"])

# Finding best k
best_k <- function(k) {
  out <- data.frame()
  set.seed(1)
  for(i in 1:k) {
    knn_pred <- knn(train_X, test_X, train_Direction, i)
    AUC <- mean(reg_test$Appr == knn_pred)
    out <- rbind(out, c(i, AUC))
  }
  colnames(out) <- c("k","AUC")
  return(out)
}

t <- best_k(40)
plot(t)
# best k >= 16 yield best result


# Regression analysis with library caret ----
library(caret)

# Create data partition break by balance output class 
result <- data.frame(reg2)[,"Appr"]
train_idx <- createDataPartition(result, p = 0.8, list = FALSE)  # Split train:test = 80:20
rm(result)

# Convert output class to factor () Create train & test df
reg2$Appr <- factor(reg2$Appr)
reg2$Doc_Waive <- factor(reg2$Doc_Waive)
reg2$Old_Product <- factor(reg2$Old_Product)
reg_train <- reg2[train_idx,]
reg_test <- reg2[-train_idx,]

# Training data with caret package
glm_fit3 <- train(Appr ~ ., data = reg_train, 
                  method = "glm",
                  family = "binomial",
                  preProcess = c("center", "scale"),  # preprocessing center, scales
                  trControl = trainControl(method = "cv",  # Crossvalidation
                                           number = 10,  # 10-fold CV
                                           summaryFunction = twoClassSummary, 
                                           classProbs = TRUE,  # for ROC, AUC
                                           savePredictions = TRUE),  # for ROC, AUC
                  na.action = na.pass)

glm_fit3

library(pROC)
# Select a parameter setting
selectedIndices <- glm_fit3$pred$mtry == 2
# Plot:
plot.roc(glm_fit3$pred$obs[selectedIndices],
         glm_fit3$pred$M[selectedIndices])

