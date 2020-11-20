
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

# Checking whether or not Data_Value and Data_Value_Alt are different... Also going to remove Data_Value_Foot_Note 
# If we do need it later, can join it back in
sum(cdc$Data_Value != cdc$Data_Value_Alt, na.rm = TRUE) # Two columns are identical, hence removing Data_Value_Alt 
cdc_adjusted = cdc_adjusted %>% select(-Data_Value_Alt, -Data_Value_Footnote)

# First 26 cases repeat twice? Going to sort it by Question
cdc_adjusted = cdc_adjusted %>% arrange(Question, Location, Year)

####################################################################################

# Coming back to Alabama & the 1st question, but now only with relevant variables. Will try to fit different linear models to check dependence
cdc_Alabama = cdc_adjusted %>% filter(Location == "Maine", Question == "Percent of adults aged 18 years and older who have obesity")

# Focusing on gender ("Total" = Male + Female) & removing na rows
cdc_Alabama_gender = cdc_Alabama %>% select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit,
                                            Sample_Size, Total)
cdc_Alabama_gender[cdc_Alabama_gender == ""] = NA
cdc_Alabama_gender = cdc_Alabama_gender %>% drop_na()
ggplot(cdc_Alabama_gender, aes(x = Year, y = Data_Value)) + geom_point() #As seen in the graph there's an evident linear pattern
model_alabama_gender = lm(Data_Value ~ Year, data = cdc_Alabama_gender)

# Now let's analyze "Total" for all the states
cdc_all_states_total = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Total)
cdc_all_states_total[cdc_all_states_total == ""] = NA
cdc_all_states_total = cdc_all_states_total %>% drop_na()
ggplot(cdc_all_states_total, aes(x = Year, y = Data_Value, colour = Location)) + geom_line()

cdc_all_states_total = cdc_all_states_total %>% select(-Class, -Question, -High_Confidence_Limit, -Low_Confidence_Limit, - Sample_Size, -Total)
cdc_all_states_total = cdc_all_states_total %>% spread(key = Year, value = Data_Value)

ggplot(cdc_all_states_total, aes(x = Year, y = Data_Value, colour = Location)) + geom_point()
model_all_states = lm(Data_Value ~ Location , data = cdc_all_states_total)
plot(model_all_states)
plot()


ggplot(cdc_all_states_total, aes(x = Year, y = Data_Value, colour = Location)) + geom_line()
cdc_Alabama = cdc_all_states_total[1:6,]

plot(cdc_Alabama$Year, cdc_Alabama$Data_Value)
model_Alabama = lm(Data_Value ~ Year + Location, data = cdc_Alabama)
abline(model_Alabama)

cdc_Alabama_Alaska = cdc_all_states_total[1:12,]
plot(cdc_Alabama_Alaska$Year, cdc_Alabama_Alaska$Data_Value)
model_Alabama_Alaska = lm(Data_Value ~ Year + Location, data = cdc_Alabama_Alaska)
abline(model_Alabama_Alaska)

progress = cdc_all_states_total %>% group_by(Location) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])













# Figuring out if the difference is statistically significant 
model_all_states_total = lm(Data_Value ~ Year + Location, data = cdc_all_states_total)
plot(model_all_states_total)

### All accept few states show significant growth in the percent of adults who have obesity. Therefore we can say that
### the Location of the person in the country does not impact the obesity rates. There are a few outliers, however nothing
### out of the ordinary. Now we will investigate all the subsections and possible targets as to why the obesity rates are rising

cdc_gender = cdc_adjusted %>% select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit,
                                     Sample_Size, Gender)
cdc_gender[cdc_gender == ""] = NA
cdc_gender = cdc_gender %>% drop_na()
ggplot(cdc_gender, aes(x = Gender, y = Data_Value)) + geom_point()
model_gender = lm(Data_Value ~ Gender, cdc_gender) # Apparently significant too

cdc_gender_male = cdc_gender %>% filter(Gender == "Male")
ggplot(cdc_gender_male, aes(x = Year, y = Data_Value)) + geom_point()


### Test which states have obesity percentage increase over the years
