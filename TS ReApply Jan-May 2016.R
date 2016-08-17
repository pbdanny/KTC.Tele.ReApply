# Data Loading ----
library(readr)
dat <- read_tsv(file = "/Users/Danny/Share Win7/qry_TSReApply_Result.txt",
                   col_types = cols(.default = "c"), na = c("", " ", "NA"))
detach(package:readr, unload = TRUE)
names(dat) <- make.names(names(dat))

# Data Preparation ----
library(dplyr)
reapp <- dat %>%
  filter(Branch_Code == "REJ") %>%
  mutate(Old_Date = as.Date(paste0("2015-", Data.Mth, "-01"))) %>% 
  mutate(Old_Product = ifelse(grepl("VRL", Product.CC.PL.RL.), "RL", "CC")) %>%
  mutate(New_Date = as.Date(paste0(Month, "01"), format = "%Y%m%d")) %>%
  mutate(Diff_Month = 12*(as.numeric(format(New_Date, "%Y")) - as.numeric(format(Old_Date, "%Y"))) +
                          (as.numeric(format(New_Date, "%m")) - as.numeric(format(Old_Date, "%m")))) %>%
  mutate(AGE = as.integer(AGE))
  
# Select predictor ----
reg <- reapp %>%
  select(Old_Date, Old_Reason, Old_Reason_Desc, Old_Product, AGENT.SOURCE.ID,
         Result, Result_Description, New_Date, Occupation_Code, Doc_Waive, AGE,
         Appr, Diff_Month)

# Exploratory Data Analysis ----
library(ggplot2)
ggplot(data = reg, aes(x = AGE, col = Appr)) +
  geom_bar()

saveRDS(reg, file = "ReApplyMap.RDS")

