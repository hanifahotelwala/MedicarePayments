# Medicare payments data analysis. 
library(readr)
library(readxl)
library(haven)
library(tidyverse)
setwd("/Users/hanifahotelwala/Documents/CodeBase/Rstudio/msda/dataviz/MedicarePayments/")
getwd()
md = read_tsv("data/Medicare_Provider_Util_Payment_PUF_CY2017.txt")
View(md)
dim(md)
class(md)
summary(md)
head(md)
md_data <- as_tibble(md) #conversion into a tibble data frame for easier data analysis
md_data
md_distinct <- md_data %>% distinct() #no repeated data -  same observations 
md_distinct

#dataframes based on provider types + Medicare average columns
typesOfProviders <- md_data$provider_type %>% distinct()
typesOfProviders 
summary(typesOfProviders)

df_providerType_by_averagesOfMedicare <- md_data %>% 
  group_by(provider_type) %>% 
  summarize(mean_average_Medicare_allowed_amt = mean(average_Medicare_allowed_amt, na.rm=TRUE),
            mean_average_submitted_chart_amt= mean(average_submitted_chrg_amt, na.rm=TRUE), 
            mean_average_Medicare_payment_amt = mean(average_Medicare_payment_amt, na.rm=TRUE),
            mean_average_Medicare_standard_amt = mean(average_Medicare_standard_amt, na.rm=TRUE), 
            .groups="drop")
View(df_providerType_by_averagesOfMedicare)
summary(df_providerType_by_averagesOfMedicare)
sapply(df_providerType_by_averagesOfMedicare, class) # view variables of dataframe

ggplot(data = df_providerType_by_averagesOfMedicare,  ## too many providers listed in the x axis. 
      mapping = aes(x = provider_type, y = mean_average_Medicare_allowed_amt)) + 
      geom_point() +  
      geom_line()
