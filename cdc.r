cdc = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# Seeing the distribution of the Data_Values
hist(cdc$Data_Value)

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


# Now let's analyze "Total" for all the states
cdc_all_states_total = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Total)
cdc_all_states_total[cdc_all_states_total == ""] = NA
cdc_all_states_total = cdc_all_states_total %>% drop_na()
ggplot(cdc_all_states_total, aes(x = Year, y = Data_Value, colour = Location)) + geom_line()

reg_coef_all_states = cdc_all_states_total %>% group_by(Location) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"]) 
# Only 2 states show negative slope & National is showing 0.43 regression coefficient which is strong

# Plotting to see which states are particularly affected

reg_coef_all_states %>% mutate(Location = reorder(Location, slope)) %>%
  ggplot(aes(x = Location, y = slope, fill = Location)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Now will check for gender
cdc_all_states_gender = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Gender)
cdc_all_states_gender[cdc_all_states_gender == ""] = NA
cdc_all_states_gender = cdc_all_states_gender %>% drop_na() %>% arrange(Gender)

reg_coef_gender = cdc_all_states_gender %>% group_by(Gender) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])
reg_coef_gender %>% mutate(Gender = reorder(Gender, slope)) %>%
  ggplot(aes(x = Gender, y = slope, fill = Gender)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Now checking for Age
cdc_all_states_age = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Age.years.) %>%
  rename(Age = Age.years.)
cdc_all_states_age[cdc_all_states_age == ""] = NA
cdc_all_states_age = cdc_all_states_age %>% drop_na() %>% arrange(Age)

reg_coef_age = cdc_all_states_age %>% group_by(Age) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])
reg_coef_age %>% mutate(Age = reorder(Age, slope)) %>%
  ggplot(aes(x = Age, y = slope, fill = Age)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Checking for Education
cdc_all_states_education = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Education)
cdc_all_states_education[cdc_all_states_education == ""] = NA
cdc_all_states_education = cdc_all_states_education %>% drop_na() %>% arrange(Education)

reg_coef_education = cdc_all_states_education %>% group_by(Education) %>% 
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])
reg_coef_education %>% mutate(Education = reorder(Education, slope)) %>%
  ggplot(aes(x = Education, y = slope, fill = Education)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Checking for Income
cdc_all_states_income = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Income)
cdc_all_states_income[cdc_all_states_income == ""] = NA
cdc_all_states_income = cdc_all_states_income %>% drop_na() %>% arrange(Income)

reg_coef_income = cdc_all_states_income %>% group_by(Income) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])
reg_coef_income %>% mutate(Income = reorder(Income, slope)) %>%
  ggplot(aes(x = Income, y = slope, fill = Income)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Checking for Race
cdc_all_states_race = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Race.Ethnicity) %>%
  rename(Race = Race.Ethnicity)
cdc_all_states_race[cdc_all_states_race == ""] = NA
cdc_all_states_race = cdc_all_states_race%>% drop_na() %>% arrange(Race)

reg_coef_race = cdc_all_states_race %>% group_by(Race) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"])
reg_coef_race %>% mutate(Race = reorder(Race, slope)) %>%
  ggplot(aes(x = Race, y = slope, fill = Race)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))



