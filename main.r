
cdc = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# Seeing the distribution of the Data_Values
hist(cdc$Data_Value)

# Trying to reduce the dataset to the see meaning of variables
cdc_Alabama = cdc %>% filter(LocationDesc == "Alabama") # Reducing the dataset to only Alabama
cdc_Alabama_red  = cdc_Alabama %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Data_Value, Data_Value_Alt, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Age.years.,
         Education, Gender, Income, Race.Ethnicity, StratificationCategory1, Stratification1, YearStart, YearEnd)

# Reducing further...
cdc_Alabama_final = cdc_Alabama_red %>% select(Data_Value, Data_Value_Alt, Low_Confidence_Limit, High_Confidence_Limit,
                                                 Sample_Size, StratificationCategory1, Stratification1, YearStart, YearEnd)

### Repeating the same "study" of finding the percent of adults ages 18 years and older 
### who have obesity for 6 years from 2011-2016. First 4 columns represent that exact percentage with
### an appropriate CI. Other variables are indicating different categories. 

# Now looking at the original dataset, trying to locate any useless variables and remove them.
colnames(cdc)
unique(cdc$YearStart) # Going 1 by 1 and checking each column...

# Need to see if each instance of YearStart == YearEnd, then can remove YearEnd & rename the column
sum(cdc$YearStart != cdc$YearEnd) # The sum is zero, hence remove & rename 
cdc_adjusted = cdc %>% select(-YearEnd) %>% rename(Year = YearStart)

# Removing LocationAbbr, LocationID and renaming LocationDesc
cdc_adjusted = cdc_adjusted %>% select(-LocationAbbr, -LocationID) %>% rename(Location = LocationDesc)

# Removing DataSource, Topic, TopicID, ClassID, QuestionID, GeoLocation, Data_Value_Unit, 
# Data_Value_Type, DataValueTypeID
cdc_adjusted = cdc_adjusted %>% select(-Datasource, -Topic, -TopicID, -ClassID, -QuestionID, -GeoLocation,
                                       -Data_Value_Unit, -Data_Value_Type, - DataValueTypeID)

# Removing more variables... Data_Value_Footnote_Symbol, StratificationCategoryId1, StratificationID1
cdc_adjusted = cdc_adjusted %>% select(-Data_Value_Footnote_Symbol, -StratificationCategoryId1, -StratificationID1)

# Checking whether or not Data_Value and Data_Value_Alt are different...
sum(cdc$Data_Value != cdc$Data_Value_Alt, na.rm = TRUE) # Two columns are identical, hence removing Data_Value_Alt 
cdc_adjusted = cdc_adjusted %>% select(-Data_Value_Alt)

# First 26 cases repeat twice?
