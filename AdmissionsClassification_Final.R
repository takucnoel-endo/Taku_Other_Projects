##############################
##############################

# Project 2: Admissions Classification
# Name: Taku Charles-Noel Endo

##############################
##############################



###########
#Functions#
###########
GetAllNA <- function (df) {
  i=1
  #Description: Function to print out percentage of missing values in a data frame.
  #Use this function on console when I need reference throughout data cleaning.
  for (col in colnames(df)) {
    print(paste('[',paste(paste(as.character(i), ']'),col)))
    print((sum(is.na(df[col]))/nrow(data))*100)
    i <- i+1
  }
}


normalize <- function(x, na.rm = TRUE) {
  #Description:*External Code Source* Scales selected vector values using Min-Max scaling technique
  #Reference: Original code from https://stackoverflow.com/questions/44050028/min-max-scaling-normalization-in-r-for-train-and-test-data
  return((x- min(x)) /(max(x)-min(x)))
}



##Load dataset
data_original <- read.csv('Admissions.csv')
#Create a copy of the original dataset.
data <- data_original


###############################
###DataClean/EDA/FeatureEng####
###############################
#Import libraries for data cleaning
library(ggplot2) #For various plotting
library(dplyr) #For easier data manipulation
library(tidyr) #For string manipulation.


#Make ID vactor of its own and Drop ID feature
ID_vector <- data$ID
data <- subset(data, select=-ID)

library(naniar)
#Visualize all missing values within the dataset.
vis_miss(data[, c('Entry.Term..Application.','Admit.Type', 'Permanent.Postal', 'Permanent.Country', 'Sex', 'Ethnicity', 'Race', 'Religion', 'First_Source.Origin.First.Source.Date','Inquiry.Date', 'Submitted', 'Application.Source', 'Decision.Plan', 'Staff.Assigned.Name','Legacy', 'Athlete')])
vis_miss(data[, c('Sport.1.Sport', 'Sport.1.Rating', 'Sport.2.Sport', 'Sport.2.Rating', 'Sport.3.Sport', 'Sport.3.Rating', 'Academic.Interest.1', 'Academic.Interest.2')])
vis_miss(data[, c('First_Source.Origin.First.Source.Summary', 'Total.Event.Participation', 'Count.of.Campus.Visits', 'School..1.Organization.Category', 'School.1.Code', 'School.1.Class.Rank..Numeric.', 'School.1.Class.Size..Numeric.')])
vis_miss(data[, c('School.1.GPA', 'School.1.GPA.Scale', 'School.1.GPA.Recalculated', 'School.2.Class.Rank..Numeric.', 'School.2.Class.Size..Numeric.', 'School.2.GPA', 'School.2.GPA.Scale','School.2.GPA.Recalculated')])
vis_miss(data[, c('School.3.Class.Rank..Numeric.', 'School.3.Class.Size..Numeric.', 'School.3.GPA', 'School.3.GPA.Scale','School.3.GPA.Recalculated', 'ACT.Composite', 'ACT.English', 'ACT.Math')])
vis_miss(data[, c('SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section','ACT.Science.Reasoning', 'ACT.Writing', 'SAT.I.CR...M', 'Permanent.Geomarket','Citizenship.Status', 'Academic.Index','Test.Optional')])
vis_miss(data[, c('Intend.to.Apply.for.Financial.Aid.','SAT.Concordance.Score..of.SAT.R.','ACT.Concordance.Score..of.SAT.R.','ACT.Concordance.Score..of.SAT.','Merit.Award','Test.Optional')])
vis_miss(data[, c('SAT.R.Evidence.Based.Reading.and.Writing.Section','SAT.I.Critical.Reading','SAT.I.Math','SAT.I.Writing','SAT.R.Math.Section')])


###1.Entry.Term..Application### No missing value
#Show barplot and frequency table to count classes.
ggplot(data,aes(Entry.Term..Application.)) + geom_bar() 
table(data$Entry.Term..Application.)
#Separate the string to only include year.
data <- data %>% separate(col='Entry.Term..Application.',sep=' ',into=c('Sem','Year'),remove=TRUE)
#Drop sem feature
data <- subset(data, select=-Sem)
#Course Year into integer.
data$Year <- as.numeric(data$Year)



###2.Admit.Type### No missing value
#Show barplot and frequency table to count classes.
ggplot(data,aes(Admit.Type)) + geom_bar() 
table(data$Admit.Type) #Only on class level exist in this feature.
#There are no statistical significance, thus drop this feature.
data <- subset(data, select=-Admit.Type)



###3.Permanent.Postal### No missing value
#For this one, it will be helpful recoding into only the National Area Code which is between 0-9, as well as I for international students
#Note: Zip code from other countries do not include hyphen. 
#Using 'Permanent.Country' feature, make all zip code from non-US students 0:string leaving only the zip from US residents.
data[data$Permanent.Country != 'United States', 'Permanent.Postal'] <- '0'
#Split the sting values by hyphen"-" and get the first 5 digits into separate columns.
data <- data %>% separate(col='Permanent.Postal',sep='-',into=c('NationalArea.Code'),remove=FALSE)
#Recode by each national area code. For the values with 0, replace with 'Int"
#Other values will be replaced by the first digit of the 5digit combination.
data[data$NationalArea.Code == '0', 'NationalArea.Code'] <- 'I'
data$NationalArea.Code <- substring(data$NationalArea.Code, 1, 1)
#Show barplot and frequency table to count classes.
ggplot(data,aes(NationalArea.Code)) + geom_bar() 
table(data$NationalArea.Code)
#Drop the 'Permanent.Postal' feature.
data <- subset(data, select=-Permanent.Postal)
#Factorize
data$NationalArea.Code <- factor(data$NationalArea.Code)



###4.Permanent.Country### No missing value
#Show frequency table to count classes.
table(data$Permanent.Country)
#Take care of the empty string first.
#Where does this occur?
data[data$Permanent.Country == '',] #The student is has a US citizenship. This student is likely to be from US.
#Impute with United States.
data[data$Permanent.Country == '','Permanent.Country'] <- 'United States'
table(data$Permanent.Country)
#Create additional feature 'GDPperCapita' depending on the country name.
#Import an external dataset that contains GDP per Capita for years since 1960.
#Dataset source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
data_gdp <- read.csv('GDP per Capita.csv')
#Before matching with country name and year, it is important that all the country name match.
#Loop through the country names to run a check if conuntry name exist (or match) in both dataset.
for (country in data$Permanent.Country) {
  #if the country name does not match, return the country name.
  if (country %in% data_gdp$Country.Name == FALSE) {
    print(country)
  }
} #For this purpose, renamed country names.
#Venezuela --- Venezuela, RB
#Hong Kong S.A.R --- Hong Kong SAR, China 
#Taiwan does not exist in the GDP dataset. Manual input needed.
#South Korea --- Korea, Rep.
#Cote D'Ivoire --- Cote d'Ivoire
#Palestine does not exist in the GDP dataset. Manual input needed.
#Russia --- Russian Federation
#The Bahamas --- Bahamas, The
#Iran --- Iran, Islamic Rep.
#Egypt --- Egypt, Arab Rep.
data_gdp[data_gdp$Country.Name == 'Venezuela, RB', 'Country.Name'] <- 'Venezuela'
data_gdp[data_gdp$Country.Name == 'Hong Kong SAR, China', 'Country.Name'] <- 'Hong Kong S.A.R.'
data_gdp[data_gdp$Country.Name == 'Korea, Rep.', 'Country.Name'] <- 'South Korea'
data_gdp[data_gdp$Country.Name == "Cote D'Ivoire", 'Country.Name'] <- "Cote D'Ivoire"
data_gdp[data_gdp$Country.Name == 'Russian Federation', 'Country.Name'] <- 'Russia'
data_gdp[data_gdp$Country.Name == "Iran, Islamic Rep.", 'Country.Name'] <- "Iran"
data_gdp[data_gdp$Country.Name == "Egypt, Arab Rep.", 'Country.Name'] <- "Egypt"

#Fill in the available GDP per Capita values for each data points.
#Initialze GDPperCapita feature in the main dataset.
data$GDPperCapita <- NA
#Go through all the rows in the dataset, get both year and country name.
for (i in (1:nrow(data))) {
  #Print current index number.
  print(i)
  #Retrieve GDP value from GDP dataset from where the country and year corresponds, and assign to GDP per Capita for the current index(i)
  #Use try() function to get past an exception that occurs when country name is not present in the GDP dataset. These are to be filled manually.
  #"silent" argument determines whether to output error message or not if there is an error. 
  try(data[i, 'GDPperCapita'] <- data_gdp[data_gdp$Country.Name == 
                                            data[i,'Permanent.Country'], 
                                          paste('X',as.character(data[i,'Year']),sep='')], silent=TRUE)
} 
#Fill in the rest of the GDP per Capita that was not available in the external dataset manually.
#It will take such a long time to input year by year, so I will just impute for the most recent GDP per Capita, 
#instead of GDP per Capita when the student was admitted.
data[data$Permanent.Country == 'Taiwan', 'GDPperCapita'] <- 46323.86
data[data$Permanent.Country == 'Palestine', 'GDPperCapita'] <- 3239.73
data[is.na(data$GDPperCapita), 'Permanent.Country'] #There are also some values that were not imputed.
#This is because the original dataset did not have the GDP value for specific year.
#Manually impute GDP values.
data[(data$Permanent.Country == 'United Arab Emirates' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 43103.34
data[(data$Permanent.Country == 'Venezuela' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 16055.65
data[(data$Permanent.Country == "Cote D'Ivoire" & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 2325.72
data[(data$Permanent.Country == 'Japan' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 40113.06
data[(data$Permanent.Country == 'The Bahamas' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 28607.90
data[(data$Permanent.Country == 'Cayman Islands' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 91392.64
data[(data$Permanent.Country == 'Palestine' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 3239.73
data[(data$Permanent.Country == 'Kuwait' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 32373.25
data[(data$Permanent.Country == 'United States' & is.na(data$GDPperCapita)), 'GDPperCapita'] <- 63543.58


ggplot(data, aes(x=GDPperCapita)) + geom_density()

#Drop the original feature at the end.
data <- subset(data, select=-Permanent.Country)


###5. Sex### No missing value
#Show frequency table to count classes.
table(data$Sex)
#Look at the barplot
ggplot(data,aes(Sex)) + geom_bar()
#Factorize
data$Sex <- factor(data$Sex)



###6. Ethnicity### No missing value
#Show frequency table to count classes.
table(data$Ethnicity) #There are 227 occurence of empty string.
#The empty values are the ones who chose not to answer the question.
data[data$Ethnicity == '', 'Ethnicity'] <- 'No Answer'
data$Ethnicity <- factor(data$Ethnicity)



###7. Race### No missing value
#Show frequency table to count classes.
table(data$Race) #There are many values where there are two or more races separated by comma.
#Perform a string manipulation to separate each races by comma and create more columns
data <- data %>% separate(col='Race',sep=',',into=c('Race1','Race2', 'Race3'))
table(data$Race1) #There are empty string values to be imputed.
#Impute with the most frequent value.
data[data$Race1 == '', 'Race1'] <- 'White'
table(data$Race2) #Previous string manipulation has created some missing values for the rows that doesn't have two or more race
#Impute the missing values with string value indicating that the observation only had one race.
data[is.na(data$Race2), 'Race2'] <- 'One Race'
table(data$Race3) 
#Impute the missing values with string value indicating that the observation only had one race.
data[is.na(data$Race3), 'Race3'] <- 'One Race'
#Frequency Distribution
for (col in c('Race1', 'Race2', 'Race3')) {
  print(col)
  print(table(data[,col]))
}
#Since the frequency distribution is very imbalanced, it is going to cause issues when train test split.
#Change the Race2 and Race3 features into feature that indicates whether student also indicated more than two race.
data$Morethan.TwoRace <- ifelse(data$Race2 == 'One Race', 0, 1)
table(data$Morethan.TwoRace)
#Factorize
data$Race1 <- factor(data$Race1)
data$Morethan.TwoRace <- factor(data$Morethan.TwoRace)
#Subset Race 2 and Race3
data <- subset(data, select=-Race2)
data <- subset(data, select=-Race3)



###8. Religion### No missing value
#Show barplot and frequency table to count classes.
ggplot(data,aes(Religion)) + geom_bar() 
table(data$Religion)
#Trinity is affliliated with Presbyterian. 
#There are many Catholic students in Trinity.
#Merge classes that have frequency of less than 100.
#In this case many subgroup of protestant can be merged, and subgroup of orthodox christianity can be merged.
data[data$Religion == '', 'Religion'] <- 'None'
data[(data$Religion == 'Methodist') | 
       (data$Religion == 'Anglican') |
       (data$Religion == 'Lutheran') |
       (data$Religion == 'Evangelical') |
       (data$Religion == 'Unitarian') |
       (data$Religion == 'Church of the Nazarene') |
       (data$Religion == 'Christian Reformed') |
       (data$Religion == 'Church of Christ') |
       (data$Religion == 'Pentecostal') |
       (data$Religion == 'Bible Churches') |
       (data$Religion == 'Episcopal') |
       (data$Religion == 'Lutheran-Missouri Synod') |
       (data$Religion == 'Society of Friends (Quaker)') |
       (data$Religion == 'Unitarian') |
       (data$Religion == 'United Church of Christ') |
       (data$Religion == 'United Methodist') |
       (data$Religion == 'Assembly of God'), 'Religion'] <- 'Protestant'

data[(data$Religion == 'Mormon-Latter Day Saints') | 
       (data$Religion == 'Non-Denominational') |
       (data$Religion == 'Christian Scientist') |
       (data$Religion == 'Church of God') |
       (data$Religion == 'Coptic Church (Egypt)') |
       (data$Religion == 'Eastern Orthodox') |
       (data$Religion == 'Independent') |
       (data$Religion == 'Mennonite') |
       (data$Religion == "Jehovah's Witnesses"), 'Religion'] <- 'Christian'

data[(data$Religion == 'Jewish Messianic'), 'Religion'] <- 'Jewish'
data[(data$Religion == 'Southern Baptist'), 'Religion'] <- 'Baptist'
data[(data$Religion == 'Presbyterian Church of America'), 'Religion'] <- 'Presbyterian'
data[(data$Religion == "Baha'I") | 
       (data$Religion == 'Jain') |
       (data$Religion == 'Sikh') |
       (data$Religion == 'Zoroastrian'), 'Religion'] <- 'Other'

table(data$Religion)
ggplot(data,aes(Religion)) + geom_bar() 
#Factorize
data$Religion <- factor(data$Religion)
#Protestant - Methodist, Anglican, Lutheran, Evangelical, Unitarian, Church of the Nazarene, 
#             Christian Reformed, Church of Christ, Pentecostal, Bible Churches, Episcopal, Lutheran-Missouri Synod,
#             Society of Friends (Quaker), Unitarian, United Church of Christ, United Methodist
#Christian - Mormon-Latter Day Saints, Non-Denomination, Christian Scientist, Church of God, Coptic Church (Egypt), 
#             Eastern Orthodox, Independent, Mennonite, Jehovah's Witnesses, 
#Jewish - Jewish Messianic
#Baptist - Southern Baptist
#Presbyterian - Presbyterian Church of America
#Other - Baha'I, Jain, Sikh, Zoroastrian



###9. First_Source.Origin.First.Source.Date### No missing value
#First change the feture name to more interpretative and short name.
data$First_Source.Origin.First.Source.Date
data <- data %>% rename(First.Notice = First_Source.Origin.First.Source.Date)
#Some of the feauture value also include the time of day when Trinity was noticed that a student was intersted.
#Time does not really affect whether student will accept an offer or not.
#Split the string values by empty string and do not keep the time value.
#Day of the month also does not really affect the decision either. 
#Split the resulting string value further into Month, Day, Year, and drop the day feature.
data <- data %>% 
  separate(col='First.Notice',sep=' ',into=c('FirstNotice.Date')) %>%
  separate(col='FirstNotice.Date',sep='/',into=c('FirstNotice.Month','FirstNotice.Day','FirstNotice.Year')) 
data <- subset(data, select=-FirstNotice.Day) #Now we have columns FirstNotice.Month, FirstNotice.Year instead.
table(data$FirstNotice.Year)
#2012 and 2013 has too little frequency. Merge them to 2014
data[(data$FirstNotice.Year == '2012') | 
       (data$FirstNotice.Year == '2013'), 'FirstNotice.Year'] <- '2014'
#Factorize
data$FirstNotice.Month <- factor(data$FirstNotice.Month)
data$FirstNotice.Year <- factor(data$FirstNotice.Year)



###10. Inquiry.Date### No missing value
data$Inquiry.Date #Also date information but have some missing values.
#The missing value indicate that the person did not inquire (ask about) the admission to Trinity.
#It would be more informationally valuable if, we change this feature into a binary feature that indicates whether a student have sent and inquiry to Trinity.
data$Sent.Inquery <- ifelse(data$Inquiry.Date == '', 'N','Y')
#Drop the inquiry date feauture since it is useless now.
data <- subset(data, select=-Inquiry.Date)
#Factorize
data$Sent.Inquery <- factor(data$Sent.Inquery)



###11. Submitted### No missing value
#Some of the feauture value also include the time of day when Trinity was noticed that a student was intersted.
#Time does not really affect whether student will accept an offer or not.
#Split the string values by empty string and do not keep the time value.
#Day of the month also does not really affect the decision either. 
#Split the resulting string value further into Month, Day, Year, and drop the day feature.
data <- data %>% 
  separate(col='Submitted',sep=' ',into=c('Submitted.Date')) %>%
  separate(col='Submitted.Date',sep='/',into=c('Submitted.Month','Submitted.Day','Submitted.Year')) 
data <- subset(data, select=-Submitted.Day) #Now we have columns FirstNotice.Month, FirstNotice.Year instead.
#Factorize
data$Submitted.Month <- factor(data$Submitted.Month)
data$Submitted.Year <- factor(data$Submitted.Year)
table(data$Submitted.Year) #Only 3 observations are from 2015
#Substitute 2015 with 2016
data[data$Submitted.Year == '2015', 'Submitted.Year'] <- '2016' 

###12. Application.Source### No missing value
#The frequency table
table(data$Application.Source)
#There seems to be nothing wrong other than the fact that classes are unbalanced.
#Factorize
data$Application.Source <- factor(data$Application.Source)



###13. Decision.Plan### No missing value
table(data$Decision.Plan) 
#Factorize
data$Decision.Plan <- factor(data$Decision.Plan)



###14. Staff.Assigned.Name### No missing value
table(data$Staff.Assigned.Name) #3 Empty strings
#Impute empty strings with most common value
data[data$Staff.Assigned.Name == '', 'Staff.Assigned.Name'] <- 'Gail Roberson'
#Impute staff (Christian Regan) frequecy 1 with other staff member.
data[data$Staff.Assigned.Name == 'Christine Ragan', 'Staff.Assigned.Name'] <- 'Gail Roberson'
#Factorize
data$Staff.Assigned.Name <- factor(data$Staff.Assigned.Name)



###15. Legacy### No missing value
table(data$Legacy)
#The classes are separated into 15 classes that are very similar (all indicate they are legacy) and empty string class that indicate No legacy.
#Group the classes into Legacy and No legacy
data$Legacy <- ifelse(data$Legacy == '', 'N', 'Y')
#Factorize
data$Legacy <- factor(data$Legacy)



###16. Athlete### No missing value
table(data$Athlete)
##The classes are separated into 15 classes that are very similar (all indicate they are athlete) and empty string class that indicate Not athlete.
#Group the classes into athlete and no athlete
data$Athlete <- ifelse(data$Athlete == '', 'N', 'Y')
#Factorize
data$Athlete <- factor(data$Athlete)



###17. Sport.1.Sport### No missing value
table(data$Sport.1.Sport)
#Classes are separated into multiple sport teams which also is separated with Men's and Women's team.
#However, the Sex information is already given from the Sex feature, if we keep the Sex information, it will be redundunt.
#Bundle classes from the same sport teams but different Sex into one class.
#The empty string indicates student is not in any sport teams. Assign "Not Athlete" to it.
data[data$Sport.1.Sport == '', 'Sport.1.Sport'] <- 'Not Athlete'
data[(data$Sport.1.Sport == 'Basketball Men' | 
        data$Sport.1.Sport == 'Basketball Women'), 'Sport.1.Sport'] <- 'Basketball'
data[(data$Sport.1.Sport == 'Cross Country Men' | 
        data$Sport.1.Sport == 'Cross Country Women'), 'Sport.1.Sport'] <- 'Cross Country'
data[(data$Sport.1.Sport == 'Diving Men' |
        data$Sport.1.Sport == 'Diving Women'), 'Sport.1.Sport'] <- 'Diving'
data[(data$Sport.1.Sport == 'Golf Men' | 
        data$Sport.1.Sport == 'Golf Women'), 'Sport.1.Sport'] <- 'Golf'
data[(data$Sport.1.Sport == 'Soccer Men' | 
        data$Sport.1.Sport == 'Soccer Women'), 'Sport.1.Sport'] <- 'Soccer'
data[(data$Sport.1.Sport == 'Swimming Men' | 
        data$Sport.1.Sport == 'Swimming Women'), 'Sport.1.Sport'] <- 'Swimming'
data[(data$Sport.1.Sport == 'Tennis Men' | 
        data$Sport.1.Sport == 'Tennis Women'), 'Sport.1.Sport'] <- 'Tennis'
data[(data$Sport.1.Sport == 'Track Men' | 
        data$Sport.1.Sport == 'Track Women'), 'Sport.1.Sport'] <- 'Track'
#Factorize
data$Sport.1.Sport <- factor(data$Sport.1.Sport)



###18. Sport.1.Rating### No missing value
table(data$Sport.1.Rating)
data[data$Sport.1.Rating == '', 'Sport.1.Rating'] <- 'Not Athlete'
#Factorize
data$Sport.1.Rating <- factor(data$Sport.1.Rating)



###19. Sport.2.Sport### No missing value
table(data$Sport.2.Sport)
#Classes are separated into multiple sport teams which also is separated with Men's and Women's team.
#However, the Sex information is already given from the Sex feature, if we keep the Sex information, it will be redundunt.
#Bundle classes from the same sport teams but different Sex into one class.
#The empty string indicates student is not in any sport teams or is not involved in secondary sport team. Assign "No Sport2" to it.
data[data$Sport.2.Sport == '', 'Sport.2.Sport'] <- 'No Sport2'
data[data$Sport.2.Sport == 'Basketball Men', 'Sport.2.Sport'] <- 'Basketball'
data[(data$Sport.2.Sport == 'Cross Country Men') |
       (data$Sport.2.Sport == 'Cross Country Women'), 'Sport.2.Sport'] <- 'Cross Country'
data[(data$Sport.2.Sport == 'Soccer Men') |
       (data$Sport.2.Sport == 'Soccer Women'), 'Sport.2.Sport'] <- 'Soccer'
data[(data$Sport.2.Sport == 'Tennis Men') |
       (data$Sport.2.Sport == 'Tennis Women'), 'Sport.2.Sport'] <- 'Tennis'
data[(data$Sport.2.Sport == 'Track & Field') |
       (data$Sport.2.Sport == 'Track Men') |
       (data$Sport.2.Sport == 'Track Women'), 'Sport.2.Sport'] <- 'Track'
#Factorize
data$Sport.2.Sport <- factor(data$Sport.2.Sport)



###20. Sport.2.Rating### No missing value
table(data$Sport.2.Rating)
data[data$Sport.2.Rating == '', 'Sport.2.Rating'] <- 'No Sport2'
table(data$Sport.2.Rating)
#Franchise have too little class frequecy.
#Merge into Blue Chip
data[data$Sport.2.Rating == 'Franchise', 'Sport.2.Rating'] <- 'Blue Chip'
#Factorize
data$Sport.2.Rating <- factor(data$Sport.2.Rating)



###21. Sport.3.Sport### No missing value
table(data$Sport.3.Sport)
data[data$Sport.3.Sport == '', 'Sport.3.Sport'] <- 'No Sport 3'
data[data$Sport.3.Sport == 'Cross Country Men', 'Sport.3.Sport'] <- 'Cross Country'
data[(data$Sport.3.Sport == 'Track & Field') |
       (data$Sport.3.Sport == 'Track Men'), 'Sport.3.Sport'] <- 'Track'
#Factorize
data$Sport.3.Sport <- factor(data$Sport.3.Sport)


###22. Sport.3.Rating### No missing value
table(data$Sport.3.Rating)
data[data$Sport.3.Rating == '', 'Sport.3.Rating'] <- 'No Sport3'
#Factorize
data$Sport.3.Rating <- factor(data$Sport.3.Rating)

#Both Sport2 and Sport3 has most of the values with low frequency.
#Instead of keeping the features, merge the features to create feature that indicate whether a student is involved in multiple
#sports teams.
data$Sports.Multiple <- ifelse(data$Sport.2.Sport == 'No Sport2', 0, 1)
table(data$Sports.Multiple)
#Factorize
data$Sports.Multiple <- factor(data$Sports.Multiple)
#Now, subset sport2 and sport3.
data <- subset(data, select=-Sport.2.Sport)
data <- subset(data, select=-Sport.2.Rating)
data <- subset(data, select=-Sport.3.Sport)
data <- subset(data, select=-Sport.3.Rating)
#Visualize the proportion of students who accept trinity depending on whether a student athlete is involved in multiple sports.
ggplot(data[(data$Sport.1.Sport != 'Not Athlete') & (!is.na(data$Decision)),],aes(x=Sports.Multiple, fill=Decision)) + geom_bar() 


###23. Academic.Interest.1### No missing value
table(data$Academic.Interest.1)
data[data$Academic.Interest.1 == '', 'Academic.Interest.1'] <- 'Undecided'
#Some classes have extremely low count. This will cause issues when train-test-split.
#Trinity university is most known to have a good science program. Recode into science related field, 
#business related field, and others
#Recode into Larger
data[(data$Academic.Interest.1 == 'Applied Chemistry') |
       (data$Academic.Interest.1 == 'Biochemistry') |
       (data$Academic.Interest.1 == 'Biochemistry & Molecular Biology') | 
       (data$Academic.Interest.1 == 'Biology') |
       (data$Academic.Interest.1 == 'Biomathematics') |
       (data$Academic.Interest.1 == 'Chemistry') |
       (data$Academic.Interest.1 == 'Biochemistry & Molecular Biology') |
       (data$Academic.Interest.1 == 'Neuroscience') |
       (data$Academic.Interest.1 == 'Physics') |
       (data$Academic.Interest.1 == 'Pre-Medical') |
       (data$Academic.Interest.1 == 'Pre-Dental') |
       (data$Academic.Interest.1 == 'Pre-Veterinary') |
       (data$Academic.Interest.1 == 'Computer Science') |
       (data$Academic.Interest.1 == 'Cognitive Science'), 'Academic.Interest.1'] <- 'Science Related'
data[(data$Academic.Interest.1 == 'Business') |
       (data$Academic.Interest.1 == 'Business - Accounting') |
       (data$Academic.Interest.1 == 'Business - Communication Management') | 
       (data$Academic.Interest.1 == 'Business - International Business') |
       (data$Academic.Interest.1 == 'Business - Management') |
       (data$Academic.Interest.1 == 'Business - Management Information Systems') |
       (data$Academic.Interest.1 == 'Business - Marketing') |
       (data$Academic.Interest.1 == 'Business - Sport Management') |
       (data$Academic.Interest.1 == 'Business Analytics & Technology') |
       (data$Academic.Interest.1 == 'Business Legal Studies'), 'Academic.Interest.1'] <- 'Business Related'
data[(data$Academic.Interest.1 != 'Science Related') & 
       (data$Academic.Interest.1 != 'Business Related') &
       (data$Academic.Interest.1 != 'Undecided'), 'Academic.Interest.1'] <- 'Other'
#Factorize
data$Academic.Interest.1 <- factor(data$Academic.Interest.1)

###24. Academic.Interest.1### No missing value
table(data$Academic.Interest.2)
#Recode into Larger
data[(data$Academic.Interest.2 == 'Applied Chemistry') |
       (data$Academic.Interest.2 == 'Biochemistry') |
       (data$Academic.Interest.2 == 'Biochemistry & Molecular Biology') | 
       (data$Academic.Interest.2 == 'Biology') |
       (data$Academic.Interest.2 == 'Biomathematics') |
       (data$Academic.Interest.2 == 'Chemistry') |
       (data$Academic.Interest.2 == 'Biochemistry & Molecular Biology') |
       (data$Academic.Interest.2 == 'Neuroscience') |
       (data$Academic.Interest.2 == 'Physics') |
       (data$Academic.Interest.2 == 'Pre-Medical') |
       (data$Academic.Interest.2 == 'Pre-Dental') |
       (data$Academic.Interest.2 == 'Pre-Veterinary') |
       (data$Academic.Interest.2 == 'Computer Science') |
       (data$Academic.Interest.2 == 'Cognitive Science'), 'Academic.Interest.2'] <- 'Science Related'
data[(data$Academic.Interest.2 == 'Business') |
       (data$Academic.Interest.2 == 'Business - Accounting') |
       (data$Academic.Interest.2 == 'Business - Communication Management') | 
       (data$Academic.Interest.2 == 'Business - International Business') |
       (data$Academic.Interest.2 == 'Business - Management') |
       (data$Academic.Interest.2 == 'Business - Management Information Systems') |
       (data$Academic.Interest.2 == 'Business - Marketing') |
       (data$Academic.Interest.2 == 'Business - Sport Management') |
       (data$Academic.Interest.2 == 'Business Analytics & Technology') |
       (data$Academic.Interest.2 == 'Business Legal Studies'), 'Academic.Interest.2'] <- 'Business Related'
data[(data$Academic.Interest.2 != 'Science Related') & 
       (data$Academic.Interest.2 != 'Business Related') &
       (data$Academic.Interest.2 != 'Undecided'), 'Academic.Interest.2'] <- 'Other'
#Factorize
data$Academic.Interest.2 <- factor(data$Academic.Interest.2)


###25. First_Source.Origin.First.Source.Summary### No missing value
table(data$First_Source.Origin.First.Source.Summary)




#Factorize
data$First_Source.Origin.First.Source.Summary <- factor(data$First_Source.Origin.First.Source.Summary)

###26. Total.Event.Participation### No missing value
table(data$Total.Event.Participation)
#If student participated more than 2 times, put as its own class
data$Total.Event.Participation <- as.character(data$Total.Event.Participation)
data[as.numeric(data$Total.Event.Participation) >= 2, 'Total.Event.Participation'] <- '>2'
#Factorize Ord.
data$Total.Event.Participation <- factor(data$Total.Event.Participation,
                                         ordered=TRUE,
                                         levels=c('0', '1', '>2'))

###26. Count.of.Campus.Visits### No missing values 
table(data$Count.of.Campus.Visits)
#If student visited more than 2 times, put as its own class
data$Count.of.Campus.Visits <- as.character(data$Count.of.Campus.Visits)
data[as.numeric(data$Count.of.Campus.Visits) >= 2, 'Count.of.Campus.Visits'] <- '>2'
#Factorize Ord.
data$Count.of.Campus.Visits <- factor(data$Count.of.Campus.Visits,
                                      ordered=TRUE,
                                      levels=c('0', '1', '>2'))



###27. School..1.Organization.Category### No missing value
table(data$School..1.Organization.Category)
#Recode empty string values as "Other"
data[data$School..1.Organization.Category == '', 'School..1.Organization.Category'] <- 'Other'
#Factorize
data$School..1.Organization.Category <- factor(data$School..1.Organization.Category) 

###28. School.1.Code### Missing value: 78.56% 
table(data$School.1.Code)
#Recode to indicate whether school code was available or not
data$School.1.Code <- ifelse(is.na(data$School.1.Code), 'N', 'Y')
#Factorize 
data$School.1.Code <- factor(data$School.1.Code)

###29. School.1.Class.Rank..Numeric.### Missing value: 53.72%
#Create a feature for adjusted class rank, using the class size feature.
data$School.1.Class.Rank.Adjusted <- data$School.1.Class.Rank..Numeric./data$School.1.Class.Size..Numeric.
data <- subset(data,select=-School.1.Class.Rank..Numeric.)
summary(data$School.1.Class.Rank.Adjusted)
#Impute Missing value in the School Rank Adjusted with median since the feature is skewed.
data[is.na(data$School.1.Class.Rank.Adjusted), 'School.1.Class.Rank.Adjusted'] <- median(data[!is.na(data$School.1.Class.Rank.Adjusted), 'School.1.Class.Rank.Adjusted']) 

###30. School.1.Class.Size..Numeric.### Missing value: 53.72%
#Drop this column since it contains too much missing vlaues.
data <- subset(data, select=-School.1.Class.Size..Numeric.)


###31. School.1.GPA### Missing value: 64.71%
#Since there is a recalculated GPA with no missing values, this feature would not be needed and in addition, if left out,
#it will cause some negative effect on the models. Drop this feature. 
data <- subset(data, select=-School.1.GPA)

###32. School.1.GPA.Scale### Missing value:64.71%
#All the values are 4. Drop the this feature.
data <- subset(data, select=-School.1.GPA.Scale)


###33. School.1.GPA.Recalculated### No missing value
summary(data$School.1.GPA.Recalculated)
#There is nothing wrong with this feature.


###34. School.2.Class.Rank..Numeric.### Missing value: 100%
###35. School.2.Class.Size..Numeric.### Missing value: 100%
###36. School.2.GPA### Missing value: 100%
###37. School.2.GPA.Scale### Missing value: 100%
###38. School.2.GPA.Recalculated### Missing value: 100%
data <- subset(data, select=-School.2.Class.Rank..Numeric.)
data <- subset(data, select=-School.2.Class.Size..Numeric.)
data <- subset(data, select=-School.2.GPA)
data <- subset(data, select=-School.2.GPA.Scale)
data <- subset(data, select=-School.2.GPA.Recalculated)

###39. School.3.Class.Rank..Numeric.### Missing value: 100%
###40. School.3.Class.Size..Numeric.### Missing value: 100%
###41. School.3.GPA### Missing value: 100%
###42. School.3.GPA.Scale### Missing value: 100%
###43. School.3.GPA.Recalculated### Missing value: 100%
data <- subset(data, select=-School.3.Class.Rank..Numeric.)
data <- subset(data, select=-School.3.Class.Size..Numeric.)
data <- subset(data, select=-School.3.GPA)
data <- subset(data, select=-School.3.GPA.Scale)
data <- subset(data, select=-School.3.GPA.Recalculated)



###44. ACT.Composite### Missing value: 49.54%
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$ACT.Composite), 'ACT.Composite'] <- median(data[!is.na(data$ACT.Composite), 'ACT.Composite'])
data$ACT.Composite <- as.numeric(data$ACT.Composite)


###45. ACT.English### Missing value: 52.05% 
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$ACT.English), 'ACT.English'] <- median(data[!is.na(data$ACT.English), 'ACT.English'])


###46. ACT.Reading### Missing value: 52.05%
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$ACT.Reading), 'ACT.Reading'] <- median(data[!is.na(data$ACT.Reading), 'ACT.Reading'])


###47. ACT.Math### Missing value: 52.05%
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$ACT.Math), 'ACT.Math'] <- median(data[!is.na(data$ACT.Math), 'ACT.Math'])


###48. ACT.Science.Reasoning### Missing value: 52.05%
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$ACT.Science.Reasoning), 'ACT.Science.Reasoning'] <- median(data[!is.na(data$ACT.Science.Reasoning), 'ACT.Science.Reasoning'])


###49. ACT.Writing### Missing value: 98.30%
#Since 98% of the data are not available, drop the feature.
data <- subset(data, select=-ACT.Writing)


###50. SAT.I.CR...M### Missing value: 96.20%
#Since 96% of the data are not available, drop the feature.
data <- subset(data, select=-SAT.I.CR...M)


###51. SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section### Missing value: 44.31%
#Since the this feature can have huge influence in whether student will accept the offer or not, we will substitute NA values with 0
#even though half of the values are missing. This might create large negative effect on the model. If results are not great,
#try to drop the column or predict this feature using linear regression.
data[is.na(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section), 
     'SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section'] <- median(!is.na(data[data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section, 
                                                                                              'SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section']))
data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section <- as.numeric(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)

###52. Permanent.Geomarket### No missing value
table(data$Permanent.Geomarket)
#Recode into Texas market, non-Texas market, and international Market.
data[(substr(data$Permanent.Geomarket, 1, 3) == 'INT'), 'Permanent.Geomarket'] <- 'International'
data[(substr(data$Permanent.Geomarket, 1, 2) == 'TX'), 'Permanent.Geomarket'] <- 'Texas'
data[(data$Permanent.Geomarket != 'International') &
       (data$Permanent.Geomarket != 'Texas'), 'Permanent.Geomarket'] <- 'Other'
#Factorize
data$Permanent.Geomarket <- factor(data$Permanent.Geomarket)



###53. Citizenship.Status### No missing value
table(data$Citizenship.Status) #Nothing to be done here
#Factorize
data$Citizenship.Status <- factor(data$Citizenship.Status)


###54. Academic.Index### Missing value: 5.47%
table(data$Academic.Index)
#For missing value, impute with most common value.
data[is.na(data$Academic.Index), 'Academic.Index'] <- 3
#Factorize Ord.
data$Academic.Index <- factor(data$Academic.Index, 
                              ordered=TRUE,
                              levels=c(5,4,3,2,1))


###55. Intend.to.Apply.for.Financial.Aid.### Missing value: 0.13%
table(data$Intend.to.Apply.for.Financial.Aid.)
#For missing value, impute with most common value.
data[is.na(data$Intend.to.Apply.for.Financial.Aid.), 'Intend.to.Apply.for.Financial.Aid.'] <- 1
#Factorize
data$Intend.to.Apply.for.Financial.Aid. <- factor(data$Intend.to.Apply.for.Financial.Aid.)


###56. Merit.Award### No missing value
table(data$Merit.Award)
#String parse type of merit award and amount.
#Use regular expression.
data <- data %>% separate(col='Merit.Award',
                          sep='(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])',
                          into=c('Merit.Type','Merit.Amount'),remove=TRUE)
#Change datatype of amount to numeric
data$Merit.Amount <- as.numeric(data$Merit.Amount)
table(data$Merit.Amount)
#The SEM and TTS are all full scholarships. Thus, does not have the amount of scholarship, 
#thus creating missing values to Merit.Amount. 
#Impute the missing values with the amount of full scholarship (Semester).
data[is.na(data$Merit.Amount), 'Merit.Amount'] <- 60
#There is a scholarship of 125,000 which is unrealistic. Impute with 12.5.
data[data$Merit.Amount == 125, 'Merit.Amount'] <- 12.5
#Merit.Type needs more cleaning, some classes have very low frequency, and needs to be recoded into larger categories.
#D: Dean's Scholarship
#I: International Scholarship
#M: Murchison Scholarship
#P: President's Scholarship
#SEM: Semmes Distinguished Scholars in Science Scholarship
#T: Transfer Scholarship
#TT: Trustee's Scholarship
#TTS: Trinity Tower Scholarship
#X, Y, Z does not have definite equivalent. Assume that these are typo, reassign X into D, Y into T, Z into D, since
#they are close to each other in the keyboard layout.
data[data$Merit.Type == 'Y', 'Merit.Type'] <- 'T'
data[(data$Merit.Type == 'X') |
       (data$Merit.Type == 'Z'), 'Merit.Type'] <- 'D'
table(data$Merit.Type)
#SEM and TTS are both full scholarships. But their frequencies are too low and causes problem with train test split.
#Merge them together to create bigger frequency
data[(data$Merit.Type == 'SEM') | 
       (data$Merit.Type == 'TTS'), 'Merit.Type'] <- 'M'
#Factorize
data$Merit.Type <- factor(data$Merit.Type)


###57. SAT.Concordance.Score..of.SAT.R.### Missing value: 44.31%
###58. ACT.Concordance.Score..of.SAT.R.### Missing value: 54.29%
###59. ACT.Concordance.Score..of.SAT.### Missing value: 99.84%
###60. Test.Optional### Missing value: 78.59%
###61. SAT.I.Critical.Reading### Missing value: 96.20%
###62. SAT.I.Math### Missing value: 96.20%
###63. SAT.I.Writing### Missing value: 96.23%
###64. SAT.R.Evidence.Based.Reading.and.Writing.Section### Missing value: 44.31%
###65. SAT.R.Math.Section### Missing value: 44.31%
data <- subset(data, select=-SAT.Concordance.Score..of.SAT.R.)
data <- subset(data, select=-ACT.Concordance.Score..of.SAT.R.)
data <- subset(data, select=-ACT.Concordance.Score..of.SAT.)
data <- subset(data, select=-Test.Optional)
data <- subset(data, select=-SAT.I.Critical.Reading)
data <- subset(data, select=-SAT.I.Math)
data <- subset(data, select=-SAT.I.Writing)
data <- subset(data, select=-SAT.R.Evidence.Based.Reading.and.Writing.Section)
data <- subset(data, select=-SAT.R.Math.Section)



#66. Decision###
data$Decision <- factor(data$Decision)

table(data$Decision)


#Variable drop for imbalanced data.
data <- subset(data, select=-First_Source.Origin.First.Source.Summary)
data <- subset(data, select=-ACT.English)
data <- subset(data, select=-ACT.Math)
data <- subset(data, select=-ACT.Science.Reasoning)
data <- subset(data, select=-ACT.Reading)
data <- subset(data, select=-SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
data <- subset(data, select=-School.1.GPA.Recalculated)





######################
###Train_Test_Split###
######################
set.seed(200)
#Split data into train and test
train <- data[data$train.test == 'train', ]
test <- data[data$train.test == 'test', ]
#Create ID vector of the legnth of test set.
ID_Test <- ID_vector[-(1:nrow(train))]
#Drop the train and test feature since it is not nessesary at this point.
train <- subset(train, select=-train.test)
test <- subset(test, select=-train.test)



#Split training data into pretrain and validation
split <- floor(0.75 * nrow(train))
#Split training
#First get index of all pre-training dataset.
pretrain_index <- sample(seq_len(nrow(train)), size = split)
#Next split the dataset using the index created.
pretrain <- train[pretrain_index,]
validation <- train[-pretrain_index,]
#Into x and y
x_pretrain<-pretrain[,!names(train)=='Decision']
y_pretrain<-pretrain[,names(train)=='Decision']
x_validaiton<-validation[,!names(train)=='Decision']
y_validation<-validation[,names(train)=='Decision']


####################
###One Hot Encode###
####################
#Split X and Y for train and test.
x_train<-train[,!names(train)=='Decision']
y_train<-train[,names(train)=='Decision']
x_test<-test[,!names(test)=='Decision']
y_test<-test[,names(test)=='Decision']

library(caret)#For onehot
#One hot for SVM 
#Get one_hot information
one_hot <- dummyVars('~.', data=x_train, fullRank=T)
#build nna one hot data
train_one_hot <- data.frame(predict(one_hot, newdata=x_train))
#Get one_hot information
one_hot <- dummyVars('~.', data=x_test, fullRank=T)
#build nna one hot data
test_one_hot <- data.frame(predict(one_hot, newdata=x_test))
#Concatenate y into dataframe. 
train_one_hot$Decision <- y_train
test_one_hot$Decision <- y_test
#Examine
glimpse(train_one_hot)
#Split train_one_hot into pretrain and validation
pretrain_one_hot <- train_one_hot[pretrain_index,]
validation_one_hot <- train_one_hot[-pretrain_index,]

#One Hot / Scaled data for KNN
#KNN is a distance based classification model.
#Thus all the numeric values needs to be scaled between 0-1.
#Use normalize function to scale the values using min-max scaling technique.
train_one_hot_knn <- train_one_hot
test_one_hot_knn <- test_one_hot
#Scale the training data
train_one_hot_knn$Merit.Amount <- normalize(train_one_hot_knn$Merit.Amount)
train_one_hot_knn$GDPperCapita <- normalize(train_one_hot_knn$GDPperCapita)
train_one_hot_knn$School.1.Class.Rank.Adjusted <- normalize(train_one_hot_knn$School.1.Class.Rank.Adjusted)
train_one_hot_knn$Year <- normalize(train_one_hot_knn$Year)
#Scale the test data 
test_one_hot_knn$Merit.Amount <- normalize(test_one_hot_knn$Merit.Amount)
test_one_hot_knn$GDPperCapita <- normalize(test_one_hot_knn$GDPperCapita)
test_one_hot_knn$School.1.Class.Rank.Adjusted <- normalize(test_one_hot_knn$School.1.Class.Rank.Adjusted)
test_one_hot_knn$Year <- normalize(test_one_hot_knn$Year)
#Split train_one_hot into pretrain and validation
pretrain_one_hot_knn <- train_one_hot_knn[pretrain_index,]
validation_one_hot_knn <- train_one_hot_knn[-pretrain_index,]


##############
###Modeling###
##############
library(Metrics) #For Keppa


### 1. Logistic Regression### Kappa: 0.5100652
#Family = Binomial logit to specify that it is logistic regression, within Generalized Linear Model.
logistic_model <- glm(factor(Decision)~.,data=pretrain,family=binomial(link='logit'))
#Predict the probability with the model, and binarize the result for prediction y.
fit_results <- predict(logistic_model,newdata=validation,type='response')
#Create vectors to hold values
threshhold <- c()
keppa <- c()
#Initialize index number as a place holder
index <- 1
#For loop for cross validation. 
for (thresh in seq(0.1,0.9, 0.01)) {
  #Within the sequence, find the threshhold that produces the best Kappa score. 
  logistic_results <- ifelse(fit_results > thresh,1,0)
  #holder kappa score and threshhold in the vector. 
  threshhold[index] <- thresh
  keppa[index] <- ScoreQuadraticWeightedKappa(logistic_results,validation$Decision,min.rating =0, max.rating =1)
  #Move one index up.
  index <- index + 1
}
thresh_compare <- data.frame(threshhold, keppa)
#Store the best performing threshhold
best_threshhold <- thresh_compare[thresh_compare$keppa == max(thresh_compare$keppa), 'threshhold']
thresh_compare[thresh_compare$keppa == max(thresh_compare$keppa), ]



### 2. KNN### Kappa: 0.2215413
library(DMwR2) #For KNN
k_num <- c()
keppa <- c()
#Use cross validation to find the best k
for (i in 1:10) {
  #Fit the model
  KNN_result <- kNN(Decision~., pretrain_one_hot_knn, validation_one_hot_knn, k=i)
  #The result of KNN is assigned 1 and 2. Negate 1 to make it 0 and 1
  KNN_result <-as.numeric(KNN_result)-1
  #Store number of k and kappa score
  k_num[i] <- i
  keppa[i] <- ScoreQuadraticWeightedKappa(KNN_result,as.numeric(validation$Decision)-1,min.rating =0, max.rating =1)
}
k_numcompare <- data.frame(k_num, keppa)
#Extract the number of k that is equivalent with the best kappa score. 
best_k_num <- k_numcompare[k_numcompare$keppa == max(k_numcompare$keppa), 'k_num']
k_numcompare[k_numcompare$keppa == max(k_numcompare$keppa), ]




### 3. Random Forest## Kappa: 0.4189854
library(randomForest) #For Random forest
#Fit the model
rf_model <- randomForest(factor(Decision)~., pretrain, na.action = na.exclude)
#Predict y
rf_result <- predict(rf_model,newdata=validation, type = "class")
#Keppa score
print(rf_kappa <- ScoreQuadraticWeightedKappa(rf_result,validation$Decision,min.rating =0, max.rating =1))


### 4. Bagging### Kappa: 0.4200277
#Fit the model
bag_model <- randomForest(factor(Decision)~., pretrain, mtry = 5, na.action = na.exclude)
#Predict y
bag_result <- predict(bag_model, newdata=validation, type = "class")
#Keppa score
print(bag_kappa <- ScoreQuadraticWeightedKappa(bag_result,validation$Decision, min.rating =0, max.rating =1))



### 5. Boosting### Kappa: 0.3571306
#Copy datasets for boosting
train_boost <- train
test_boost <- test
#Trun response variable into numeric
train_boost$Decision <- as.numeric(ifelse(train_boost$Decision==1,0,1))
test_boost$Decision <- as.numeric(ifelse(test_boost$Decision==1,0,1))
#Split intp pretrain and validation
pretrain_boost <- train_boost[pretrain_index,]
validation_boost <- train_boost[-pretrain_index,]

library(gbm) #For boosting
#Fit model 
boost_model <- gbm(Decision~., data=pretrain_boost, distribution="bernoulli")
#Predict with model
boost_result <- predict.gbm(boost_model,newdata=validation_boost, n.trees=100, type="response")
boost_result <- ifelse(boost_result<=0.5,0,1)
#Kappa score 
print(Kappa_boost<-ScoreQuadraticWeightedKappa(boost_result, validation_boost$Decision, min.rating =0, max.rating =1))




### 6/7/8. SVM###
library(e1071) #For SVM model
#Fit SVM model, linear, cost=10
svm_model_linear <- svm(Decision~., data=pretrain_one_hot, type="C-classification", kernel='linear', cost=10)
#Fit SVM model, radial, cost=10
svm_model_radial <- svm(Decision~., data=pretrain_one_hot, type="C-classification", kernel='radial', cost=10)
#Fit SVM model, polynomial, cost=10
svm_model_polynomial <- svm(Decision~., data=pretrain_one_hot, type="C-classification", kernel='polynomial', cost=10)


#Predict SVM linear Kappa: 0.3462984
svm_result_linear <- as.vector(predict(svm_model_linear,newdata=validation_one_hot))
#Predict SVM radial Kappa: 0.3767456
svm_result_radial <- as.vector(predict(svm_model_radial,newdata=validation_one_hot))
#Predict SVM polynomial Kappa: 0.3468361
svm_result_polynomial <- as.vector(predict(svm_model_polynomial,newdata=validation_one_hot))

#Kappa score SVM linear
print(svm_kappa_linear <- ScoreQuadraticWeightedKappa(svm_result_linear,validation_one_hot$Decision, min.rating=0, max.rating=1))
#Kappa score SVM Radial
print(svm_kappa_radial <- ScoreQuadraticWeightedKappa(svm_result_radial,validation_one_hot$Decision,min.rating=0, max.rating=1))
#Kappa score SVM Polynomial
print(svm_kappa_polynomial <- ScoreQuadraticWeightedKappa(svm_result_polynomial,validation_one_hot$Decision,min.rating=0, max.rating=1))




##################
###Test Predict###
##################
### 1. Logistic Regression###
#Family = Binomial logit to specify that it is logistic regression, within Generalized Linear Model.
logistic_model_test <- glm(factor(Decision)~.,data=train,family=binomial(link='logit'))
#Predict the probability with the model, and binarize the result for prediction y.
fit_results_test <- predict(logistic_model_test, newdata=test, type='response')
logistic_results_test <- ifelse(fit_results_test > best_threshhold,1,0)


#Export result into csv
result<- data.frame(ID_Test, logistic_results_test)
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 2\\Result Files\\Endo_5.csv") 
