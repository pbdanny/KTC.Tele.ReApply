# Lead Re-Apply Data Loading ----
library(readxl)
dat <- read_excel(path = "/Users/Danny/Downloads/List Reapply_Jan-May'16_78577.xlsx", 
                   sheet = 1, col_names = TRUE)
detach(package:readxl, unload = TRUE)

# Lead Re-Apply Data Cleansing ----
names(dat) <- c("Data.Mth", "ID", "THAI.NAME", "ENG.NAME", "ENG.LAST.NAME", 
                "AGENT.SOURCE.CODE", "AGENT.SOURCE.ID", "Old.REASON", "Old.Product")  # Rename colnames

library(dplyr)
reapp <- dat %>%
  filter(!is.na(ID)) %>%  # Remove blank row
  mutate(Date = as.Date(paste0("2016-", Data.Mth, "-01"))) %>% # Create date
  mutate(Old_Product = ifelse(grepl("VRL", Old.Product), "RL", "CC")) %>%
  mutate(Old_Reason = substr(Old.REASON, 2, 2)) %>% # Extract Old Reason
  mutate(Old_ReasonDESC = substr(Old.REASON, 4, 6))  # Extract Old Reason Code

# Result data Loading ----
library(readr)
result <- read_tsv(file = "/Users/Danny/Share Win7/qry_ALLProduct_KPI_Export.txt",
                   col_types = cols(.default = "c"), na = c("", " ", "NA"))

library(ggplot2)
ggplot(data = data, aes(x = date)) +
  geom_bar(aes(fill = Old_Product))