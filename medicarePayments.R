# Medicare payments data analysis. 
library(readr)
library(readxl)
library(haven)
library(tidyverse)
library(zipcode)
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


#### October 10 Analysis

## Dataframe with only Texas. 
df_TexasOnly <- filter(md_data, nppes_provider_state=="TX") ##filtering by state.
View(df_TexasOnly)

#df consisting of the columns of interest only
df_selectedColumns <- df_TexasOnly %>% select(nppes_provider_gender, nppes_credentials, nppes_provider_city,nppes_provider_state,nppes_provider_zip, provider_type,place_of_service, medicare_participation_indicator,hcpcs_code, hcpcs_description, line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt, average_submitted_chrg_amt, average_Medicare_payment_amt)
View(df_selectedColumns)

##### October 13 analysis. 
# grouping by hcpcs codes with letters in front of them 
df_hcpcs_alphaNumerica <- df_selectedColumns %>%  filter(str_detect(hcpcs_code, "[:alpha:]"))
View(df_hcpcs_alphaNumerica)

##Occurances of hcpcs codes + cpt codes mixed in 
HCPCSandCPT_occurances<- table(unlist(df_selectedColumns$hcpcs_code))
View(HCPCSandCPT_occurances) ##click on frequency to assess what is found the most. 

##Occurances of hcpcs codes only 
HCPCS_occurances<- table(unlist(df_hcpcs_alphaNumerica$hcpcs_code))
View(HCPCS_occurances) ##click on frequency to assess what is found the most. 

##data frame with most coded hcpcs code: G0008 - flu
df_g0008 <- df_hcpcs_alphaNumerica %>% filter(hcpcs_code == "G0008") 
df_columnsOfInterest <- df_g0008 %>% select(nppes_provider_gender, average_submitted_chrg_amt, nppes_provider_city)
df_genderAndChargeByCity_forFlu <- df_columnsOfInterest %>% 
                            group_by(nppes_provider_gender)  %>% 
                            summarize(mean_average_submitted_chart_amt= mean(average_submitted_chrg_amt, na.rm=TRUE), 
                            .groups="drop")
  
View(df_genderAndChargeByCity_forFlu)

ggplot(df_genderAndChargeByCity_forFlu, aes(nppes_provider_gender)) +
  geom_bar(stat="count", aes(fill=nppes_provider_gender, color="yellow")) +
  theme(legend.position="none") ##not working the way i want it to. 


#oct 18 
#find unique nppes_credentials 
distinct_credentials<- distinct(df_selectedColumns,nppes_credentials)

new_nppes_credentials_addedToDataframe<- df_selectedColumns %>% mutate(new_nppes_credentials = str_replace_all(df_selectedColumns$nppes_credentials, "[.]","") )
class(new_nppes_credentials_addedToDataframe) #output : "tbl_df"     "tbl"        "data.frame"
df_new_NC <- as.data.frame(new_nppes_credentials_addedToDataframe) #converting to data.frame 
View(df_new_NC)
class(df_new_NC) #converterd  to dataframe
revised_selectedColumns <- df_new_NC %>% subset(select=-nppes_credentials)
revised_selectedColumns <- revised_selectedColumns %>% subset(select=c(nppes_provider_gender,new_nppes_credentials,nppes_provider_city:average_Medicare_payment_amt))
View(revised_selectedColumns) ##HAS NEW NPPES_CREDENTIALS CLEANED UP

######### oct 19 ##########
# grouping by hcpcs codes with letters in front of them only
df_hcpcs_alphaNumerica <- revised_selectedColumns %>%  filter(str_detect(hcpcs_code, "[:alpha:]"))
View(df_hcpcs_alphaNumerica)

##distinct tests
seeDistinctProviderType<- distinct(revised_selectedColumns,provider_type)
View(seeDistinctProviderType)
viewCredentialFrequency <- table(unlist(revised_selectedColumns$new_nppes_credentials)) ##Most credential used is MD: #473688 (2) DO- #34520 
View(viewCredentialFrequency)

##### oct 21 ###############
filter_byOnly_MD <- revised_selectedColumns %>%  ##filtering by only nppes_credentials == MD after cleaning. 
  filter(new_nppes_credentials=="MD") 

write.csv(filter_byOnly_MD, "/Users/hanifahotelwala/Desktop/filter_byOnly_MD.csv")

View(filter_byOnly_MD)
     

x <- read.csv("/Users/hanifahotelwala/Desktop/filter_byOnly_MD.csv")
View(x)

x <- drop_na(x)
View(x)
class(x)
test <- table(unlist(x$nppes_provider_gender))
View(test)
x <- x %>% subset(select=-X)
View(x)
write.csv(x,"/Users/hanifahotelwala/Desktop/TXMedicarePayments_ByMD.csv" )
########updated medicare payments ################
ump = read_csv("/Users/hanifahotelwala/Desktop/TXMedicarePayments_ByMD/TXMedicarePayments_ByMD.csv")
head(ump)
View(ump)



#### #1 health issue in texas: heart disease
###CPT is a code set to describe medical, surgical ,and diagnostic services; 
#HCPCS are codes based on the CPT to provide standardized coding when healthcare is delivered.

#distincts
distinct_pt<- distinct(ump, provider_type)
View(distinct_pt)
#x <- distinct(cardioRecordings, nppes_provider_gender)
View(x)

cardioRecordings <- filter(ump, provider_type=="Cardiology")
View(cardioRecordings) ##cardio recordings in tx, 28k+ observaations 

cardioRecordingsByCity_count <- table(unlist(cardioRecordings$nppes_provider_city))
View(cardioRecordingsByCity_count)
#### CITIES 500 + recordings. 
#HOUSTON - 4694, DALLAS - 2195, SAN ANTONIO - 2065, LUBBOCK - 991, PLANO-977, AUSTIN - 944
#TYLER- 738 , EL PASO - 691, FORT WORTH - 689, MCALLEN- 611, THE WOODLANDS - 566
# c("HOUSTON", "DALLAS", "SAN ANTONIO", "LUBBOCK", "PLANO", "AUSTIN", "TYLER", "EL PASO", "FORT WORTH", "MCALLEN", "THE WOODLANDS")

cardioRecordingsByHouston <- filter(cardioRecordings, nppes_provider_city=="HOUSTON")
View(cardioRecordingsByHouston)

# grouping by hcpcs codes with letters in front of them 
CRBH_alphaNumericaHCPCS <- cardioRecordingsByHouston %>%  filter(str_detect(hcpcs_code, "[:alpha:]"))
View(CRBH_alphaNumericaHCPCS)

mostalphaHCPCScodes <- table(unlist(CRBH_alphaNumericaHCPCS$hcpcs_code)) #103 obs
View(mostalphaHCPCScodes) #J2785 - 103 , A9500 - 78

# filter by most coded 
J2785_only <- CRBH_alphaNumericaHCPCS %>%  filter(hcpcs_code=="J2785")


ggplot(ump, aes(x=nppes_provider_city,y=bene_unique_cnt)) +
  ggtitle("Scatterplot City + bene unique cnt  -version 1") +
  geom_point(aes(fill=nppes_provider_gender), alpha=0.3, shape=21) 



ggplot(cardioRecordingsByHouston, aes(x=hcpcs_code,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN HOUSTON -version 1") +
  geom_point(aes(fill=nppes_provider_gender), alpha=0.3, shape=21) 

ggplot(CRBH_alphaNumericaHCPCS, aes(x=hcpcs_code,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN HOUSTON -version 2") +
  geom_point(aes(fill=nppes_provider_gender), alpha=0.3, shape=21) 

ggplot(J2785_only, aes(x=nppes_provider_gender,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN HOUSTON -version 3") +
  geom_point(aes(fill=hcpcs_code), alpha=0.3, shape=21) 

##cont for dallas, san , austin
cardioRecByHcpcs <- cardioRecordings %>% filter(hcpcs_code=="J2785")
View(cardioRecByHcpcs)

#Dallas 
dallas <- cardioRecByHcpcs %>% filter(nppes_provider_city=="DALLAS") ## 29  observations 
ggplot(dallas, aes(x=nppes_provider_gender,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN Dallas -version 4") +
  geom_point(aes(fill=hcpcs_code), alpha=0.3, shape=21) 

#san antonio 
sa <-  cardioRecByHcpcs %>% filter(nppes_provider_city=="SAN ANTONIO")
ggplot(sa, aes(x=nppes_provider_gender,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN SA -version 4") +
  geom_point(aes(fill=hcpcs_code), alpha=0.3, shape=21) 

#Austin 
austin <-  cardioRecByHcpcs %>% filter(nppes_provider_city=="AUSTIN") ## 8 obs
ggplot(austin, aes(x=nppes_provider_gender,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt IN AUSTIN -version 4") +
  geom_point(aes(fill=hcpcs_code), alpha=0.3, shape=21) 

### attempt to put all of them together withot the filter of J2785
byMostCities <- cardioRecordings %>% filter(nppes_provider_city==c("HOUSTON", "SAN ANTONIO", "AUSTIN", "DALLAS"))
View(byMostCities)
ggplot(byMostCities, aes(x=nppes_provider_city,y=bene_unique_cnt)) +
  ggtitle("Scatterplot hcpcs Codes + bene unique cnt in Most coded cities -version 4") +
  geom_point(aes(fill=nppes_provider_gender), alpha=0.3, shape=21) 

