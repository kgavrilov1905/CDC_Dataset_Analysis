
cdc = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

### Seeing the distribution of the Data_Values
hist(cdc$Data_Value)

### Trying to reduce the dataset to the see meaning of variables
cdc_Alabama = cdc %>% filter(LocationDesc == "Alabama") # Reducing the dataset to only Alabama
cdc_Alabama_red  = cdc_Alabama %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Data_Value, Data_Value_Alt, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Age.years.,
         Education, Gender, Income, Race.Ethnicity, StratificationCategory1, Stratification1, YearStart, YearEnd)

### Repeating the same "study" of finding the percent of adults ages 18 years and older 
### who have obesity for 6 years from 2011-2016. First 4 columns represent that exact percentage with
### an appropriate CI. Other variables are indicating different categories. 
