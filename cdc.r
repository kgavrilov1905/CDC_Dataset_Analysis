cdc = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# Seeing the distribution of the Data_Values
hist(cdc$Data_Value)

# Now looking at the original dataset, trying to locate any useless variables and remove them.
colnames(cdc)
unique(cdc$YearStart) # Going 1 by 1 and checking each column...

# Need to see if each instance of YearStart == YearEnd, then can remove YearEnd & rename the column
sum(cdc$YearStart != cdc$YearEnd) # The sum is zero, hence remove & rename 
cdc_adjusted = cdc %>% select(-YearEnd) %>% rename(Year = YearStart, Age = Age.years.)

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
#Removing Virigin Islands because they have observations for only 2016
cdc_adjusted = cdc_adjusted[!(cdc_adjusted$Location == "Virgin Islands"),]


#### Now let's analyze "Total" for all the states ####
cdc_all_states_total = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Total)
cdc_all_states_total[cdc_all_states_total == ""] = NA
cdc_all_states_total = cdc_all_states_total %>% drop_na()
ggplot(cdc_all_states_total, aes(x = Year, y = Data_Value, colour = Location)) + geom_line()

reg_coef_all_states = cdc_all_states_total %>% group_by(Location) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4]) 
cdc_reduced = reg_coef_all_states %>% filter(pvalue < 0.05) %>% select(Location) %>% left_join(cdc_adjusted)

# Verifying the model
cdc_reduced_states = reg_coef_all_states %>% filter(pvalue < 0.05) %>% select(Location) %>% left_join(cdc_all_states_total)
model_temp = lm(Data_Value ~ Year + Location, data = cdc_reduced_states)

# Plotting to see which states are particularly affected

reg_coef_all_states %>% mutate(Location = reorder(Location, slope)) %>%
  ggplot(aes(x = Location, y = slope, fill = Location)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Now will check for gender
cdc_all_states_gender = cdc_reduced %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Gender)
cdc_all_states_gender[cdc_all_states_gender == ""] = NA
cdc_all_states_gender = cdc_all_states_gender %>% drop_na() %>% arrange(Gender)

reg_coef_gender = cdc_all_states_gender %>% group_by(Gender) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4])
reg_coef_gender %>% mutate(Gender = reorder(Gender, slope)) %>%
  ggplot(aes(x = Gender, y = slope, fill = Gender)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


# Now checking for Age
cdc_all_states_age = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Age)
cdc_all_states_age[cdc_all_states_age == ""] = NA
cdc_all_states_age = cdc_all_states_age %>% drop_na() %>% arrange(Age)
cdc_all_states_age %>% filter(Age == "55 - 64") %>% ggplot(aes(x = Year, y = Data_Value, colour = Location)) +
  geom_line()

reg_coef_age = cdc_all_states_age %>% group_by(Age) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4])
reg_coef_age %>% mutate(Age = reorder(Age, slope)) %>%
  ggplot(aes(x = Age, y = slope, fill = Age)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

cdc_reduced = cdc_reduced[!(cdc_reduced$Age == pull(reg_coef_age %>% filter(pvalue >= 0.05) %>% select(Age))),]

# Checking for Education
cdc_all_states_education = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Education)
cdc_all_states_education[cdc_all_states_education == ""] = NA
cdc_all_states_education = cdc_all_states_education %>% drop_na() %>% arrange(Education)

reg_coef_education = cdc_all_states_education %>% group_by(Education) %>% 
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4])
reg_coef_education %>% mutate(Education = reorder(Education, slope)) %>%
  ggplot(aes(x = Education, y = slope, fill = Education)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

# Checking for Income
cdc_all_states_income = cdc_adjusted %>% filter(Question == "Percent of adults aged 18 years and older who have obesity") %>%
  select(Year, Location, Class, Question, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, Income)
cdc_all_states_income[cdc_all_states_income == ""] = NA
cdc_all_states_income = cdc_all_states_income %>% drop_na() %>% arrange(Income)

reg_coef_income = cdc_all_states_income %>% group_by(Income) %>%
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4])
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
  summarize(slope = lm(Data_Value ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value ~ Year)))[2,4])
reg_coef_race %>% mutate(Race = reorder(Race, slope)) %>%
  ggplot(aes(x = Race, y = slope, fill = Race)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

reg_coef_race %>% filter(pvalue < 0.05) %>% select(Race)
cdc_reduced = cdc_reduced[!(cdc_reduced$Race.Ethnicity == "2 or more races"),]
cdc_reduced = cdc_reduced[!(cdc_reduced$Race.Ethnicity == "American Indian/Alaska Native"),]
cdc_reduced = cdc_reduced[!(cdc_reduced$Race.Ethnicity == "Hawaiian/Pacific Islander"),]
cdc_reduced = cdc_reduced[!(cdc_reduced$Race.Ethnicity == "Non-Hispanic Black"),]
cdc_reduced = cdc_reduced[!(cdc_reduced$Race.Ethnicity == "Other"),]

# Cleaning the environment
rm(cdc_all_states_age, cdc_all_states_education, cdc_all_states_gender, cdc_all_states_income,
   cdc_all_states_total, cdc_all_states_race, reg_coef_age, reg_coef_all_states, reg_coef_education,
   reg_coef_gender, reg_coef_income, reg_coef_race)

#######################################################################################

# Want to see if there's any relationship between increasing obesity rates and veggie intake

#### For instance if we take Alabama ####

cdc_Alabama_obese = cdc_reduced %>% filter(Location == "Alabama") %>% 
  filter(Question == "Percent of adults aged 18 years and older who have obesity") %>% 
  spread(key = Question, value = Data_Value) %>% rename("Obesity" = "Percent of adults aged 18 years and older who have obesity")

cdc_Alabama_veggie = cdc_reduced %>% filter(Location == "Alabama" & Question == "Percent of adults who report consuming vegetables less than one time daily") %>%
  spread(key = Question, value = Data_Value) %>% rename("Veggie" = "Percent of adults who report consuming vegetables less than one time daily")
temp_join = cdc_Alabama_obese %>% right_join(cdc_Alabama_veggie, by = c("Year", "Total", "Age", "Education", "Gender", "Income", "Race.Ethnicity")) %>%
  select(Year, Obesity, Veggie)

plot(temp_join$Veggie, temp_join$Obesity)
abline(lm(Obesity ~ Veggie, data = temp_join))


#Percent of adults who report consuming fruit less than one time daily
Q18 <- cdc_adjusted %>% filter(QuestionID == 'Q018' )%>%select(-QuestionID) %>%
  rename(Class18=Class,Question18=Question, Data_Value18=Data_Value, Sample_Size18=Sample_Size)

#Percent of adults who report consuming vegetables less than one time daily
Q19 <- cdc_adjusted %>% filter(QuestionID == 'Q019' )%>%select(-QuestionID) %>%
  rename(Class19=Class,Question19=Question, Data_Value19=Data_Value, Sample_Size19=Sample_Size)

#Percent of adults aged 18 years and older who have obesity
Q36 <- cdc_adjusted %>% filter(QuestionID == 'Q036' )%>%select(-QuestionID) %>%
  rename(Class36=Class,Question36=Question, Data_Value36=Data_Value, Sample_Size36=Sample_Size)

#Percent of adults aged 18 years and older who have an overweight classification
Q37 <- cdc_adjusted %>% filter(QuestionID == 'Q037' )%>%select(-QuestionID)

#Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)
Q43 <- cdc_adjusted %>% filter(QuestionID == 'Q043' )%>%select(-QuestionID)

#Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week
Q44 <- cdc_adjusted %>% filter(QuestionID == 'Q044' )%>%select(-QuestionID)

#Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)
Q45 <- cdc_adjusted %>% filter(QuestionID == 'Q045' )%>%select(-QuestionID)

#Percent of adults who engage in muscle-strengthening activities on 2 or more days a week
Q46 <- cdc_adjusted %>% filter(QuestionID == 'Q046' )%>%select(-QuestionID)

#Percent of adults who engage in no leisure-time physical activity
Q47 <- cdc_adjusted %>% filter(QuestionID == 'Q047' )%>%select(-QuestionID) %>%
  rename(Class47=Class,Question47=Question, Data_Value47=Data_Value, Sample_Size47=Sample_Size)


total <- inner_join(Q18,Q19,by=c("Year","Location","Total", "Age.years.", "Education", "Gender", "Income", "Race.Ethnicity","LocationID","StratificationCategory1","Stratification1"))
total <-inner_join(total,Q36,by=c("Year","Location","Total", "Age.years.", "Education", "Gender", "Income", "Race.Ethnicity","LocationID","StratificationCategory1","Stratification1"))
total <-inner_join(total,Q47,by=c("Year","Location","Total", "Age.years.", "Education", "Gender", "Income", "Race.Ethnicity","LocationID","StratificationCategory1","Stratification1"))
total <- total%>% select(-Class18,-Class19,-Class36,-Class47, -Question18,-Question19,-Question36,-Question47)%>%drop_na()%>%rename(Age=Age.years.)

## Analysis of obesity rates, fruit/vegetable intake and physical activity
model <- lm(Data_Value36 ~ Data_Value18 + Data_Value19 + Data_Value47, data=total)
summary(model)


#Using Backward Elimination method, remove the variable Data_Value19
model <- lm(Data_Value36 ~ Data_Value18 + Data_Value47, data=total)
summary(model)

vif(model)

par(mar = c(4, 4, 0.1, 0.1))
plot(model, which = c(1,2))
plot(model, which = c(3,4))


library(MASS)
model_robust = rlm(Data_Value36 ~ Data_Value18 + Data_Value47, data=total)
summary(model_robust)$coefficient

detach("package:MASS")

set.seed(123)

nsamp = ceiling(0.75*nrow(total)) # number of samples                             
training_samps = sample(c(1:nrow(total)),nsamp)   # sampled cases

train_data <- total[training_samps, ]   
test_data  <- total[-training_samps, ]

cross_val_model<- lm(Data_Value36 ~ Data_Value18 + Data_Value19, data=train_data)
summary(cross_val_model)

preds <- predict(cross_val_model,test_data)
error_percent<-100*abs(preds-test_data$Data_Value36)/test_data$Data_Value36
mean(error_percent) # prediction error is around 20%



model_age <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Age, data=total)
summary(model_age)


model_race <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Race.Ethnicity, data=total)
temp = as.data.frame(summary(model_race)$coef[,4])
colnames(temp) = c("Pr(>|t|)")
temp



ggplot(total, aes(x = Data_Value47, y = Data_Value36, colour = Age)) + geom_point(data = subset(total, Age != "")) + 
  ggtitle ("Obesity Rates vs. Physical Activity for Different Age Groups") + xlab("% of adults with no physical activity") + 
  ylab("% of adults who have obesity ")



ggplot(total, aes(x = Data_Value18, y = Data_Value36, colour = Age)) + geom_point(data = subset(total, Age != "")) + 
  ggtitle("Obesity Rates vs Fruit Intake for Different Age Groups") + xlab("% of adults with consume fruit less than one time a day") + ylab("% of adults who have obesity")



model_gender <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Gender, data=total)
summary(model_gender) 

model_income <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Income, data=total)
summary(model_income)

model_education <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Education, data=total)
summary(model_education)

model_race <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Race.Ethnicity, data=total)
summary(model_race)


ggplot(total, aes(x = Data_Value47, y = Data_Value36, colour = Race.Ethnicity)) + geom_point(data = subset(total, Race.Ethnicity != "")) + 
  ggtitle("Obesity Rates vs. Physical Activity for Different Ethnicities") + xlab("% of adults with no physical activity") + 
  ylab("% of adults who have obesity ")


ggplot(total, aes(x = Data_Value18, y = Data_Value36, colour = Race.Ethnicity)) + geom_point(data = subset(total, Race.Ethnicity != "")) + 
  ggtitle("Obesity Rates vs Fruit Intake for Different Age Groups") + xlab("% of adults with consume fruit less than one time a day") + ylab("% of adults who have obesity")



## Analysis of total obesity rates for every state

Q36 = Q36 %>% dplyr::select(Year, Location, Class36, Question36, Data_Value36, Sample_Size36, Total) %>% arrange(Location)
Q36[Q36 == ""] = NA
Q36 = Q36 %>% drop_na()
model_all_states = lm(Data_Value36 ~ Year, data = Q36)
summary(model_all_states)


ggplot(Q36, aes(x = Year, y = Data_Value36, colour = Location)) + geom_line() +
  ggtitle("2011-2016 Obesity Rates By State") + xlab("Year") + ylab("% of adults who have obesity")



par(mar = c(4, 4, 0.1, 0.1))
plot(model_all_states, which = c(2,2))
plot(model_all_states, which = c(4,4))

#ggplot(Q36, aes(x = Year, y = Data_Value36, colour = Location)) + geom_line()

reg_coef_all_states = Q36 %>% group_by(Location) %>%
  summarize(slope = lm(Data_Value36 ~ Year)$coef["Year"],
            pvalue = coef(summary(lm(Data_Value36 ~ Year)))[2,4]) 

Q36_reduced = reg_coef_all_states %>% filter(pvalue<0.05) %>% left_join(Q36)

ggplot(Q36, aes(x = Year, y = Data_Value36)) + geom_line(aes(group = Location), colour = "grey", alpha = 0.4) +
  geom_smooth(Q36_reduced, mapping = aes(x = Year, y = Data_Value36), colour = "red") +
  geom_line(Q36_reduced, mapping = aes(x = Year, y = Data_Value36, colour = Location), alpha = 0.35) + 
  geom_smooth(colour = "black") +
  ggtitle("2011-2016 National Obesity Rates") + xlab("Year") + ylab("Obesity %")

model_updated = lm(Data_Value36 ~ Year, data = Q36_reduced)
summary(model_updated)

par(mar = c(4, 4, 0.1, 0.1))
plot(model_updated, which = c(2,2))
plot(model_updated, which = c(4,4))


# Test if the points form a plane in a 3D space.
library(plot3D)
library(rgl)
scatter3D(total$Data_Value18, total$Data_Value47, total$Data_Value36, 
          theta = 45, phi = 0, xlab='Fruit', ylab='Physical Activity', zlab='Obesity')
rgb.palette <- colorRampPalette(c("green", "red","black"), space = "rgb")
par3d(windowRect = c(20,30,600,600))
plot3d(total$Data_Value18, total$Data_Value47, total$Data_Value36, xlab='Fruit', ylab='Physical Activity', zlab='Obesity',
       theta = 45, phi = 0, col = rgb.palette(100)[as.numeric(cut(total$Data_Value36, breaks = 50))])
play3d(spin3d(axis = c(0, 0, 1), rpm = 2), duration = 10)
movie3d(movie = "AnimatedGraph", spin3d(axis = c(0, 0, 1), rpm = 2), duration = 10, dir = "~/Desktop", type = "gif", clean = TRUE)




















