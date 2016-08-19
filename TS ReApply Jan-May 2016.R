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

## De-duplication ---- 
# Check dupication part ----
#reapp %>%
#  group_by(THAI.NAME) %>%
#  summarise(n = n()) %>%
#  arrange(-n) %>%
#  head()

#View(reapp[reapp$THAI.NAME == "ไตรรงค์ กินไธสง",])

# Create row index having no. of THAI.NAME  = 1 ----
dedup <- reapp %>%
  mutate(idx = row.names(reapp)) %>%  # store original rowname for reference index
  group_by(THAI.NAME) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(idx, THAI.NAME, n)

# Select distinct THAI.NAME, predictor ----
reg <- reapp[as.vector(as.integer(dedup$idx)),] %>%  # Use idx in dedup to filter out the duplicate
  select(Old_Date, Old_Reason, Old_Reason_Desc, Old_Product, AGENT.SOURCE.ID,
         Result, Result_Description, New_Date, Occupation_Code, Doc_Waive, AGE,
         Appr, Diff_Month, Monthly_Salary) %>%
  filter(Monthly_Salary < 125000)

rm(dedup)

# Exploratory Data Analysis ----
library(ggplot2)

# Bivariate plot with ggpairs
# library(GGally)
# ggpairs(data = reg)

# Bivariate EDA predictor vs predictor
h <- ggplot(data = reg, aes(x = AGE))
h + geom_density(aes(col = as.factor(Appr))) + labs(title = "Age group by Appr - Density")
ggsave("age by Appr densityPlot.png", plot = last_plot()) 
h + geom_freqpoly(binwidth = 1, aes(col = as.factor(Appr))) + labs(title = "Age group by Appr - Frequency")
ggsave("age by Appr histogramPlot.png", plot = last_plot())

i <- ggplot(data = reg, aes(x = Monthly_Salary))
i + geom_density(aes(col = as.factor(Appr))) + labs(title = "Monthly Salary group by Appr - Density")
ggsave("salary by Appr densityPlot.png", plot = last_plot())

i + geom_freqpoly(aes(binwidth = 1, col = as.factor(Appr))) + labs(title = "Monthly Salary group by Appr - Frequency")
ggsave("salary by Appr freqpolyPlot.png", plot = last_plot())

# Bivariate class vs predictors
g <- ggplot(data = reg, aes(x = AGE, y = Monthly_Salary, col = as.factor(Appr)))
g + geom_point()
ggsave("salary - AGE group by Appr - pointPlot.png", plot = last_plot())

saveRDS(reg, file = "ReApply regression data.RDS")