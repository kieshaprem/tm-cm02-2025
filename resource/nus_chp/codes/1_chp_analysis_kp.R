
############# Understand your data ############# 

## Import data into R

# Because the file is an .xls file, you will need the read_xls function in the readxl package (which you will first need to install). 

# Install the readxl package
install.packages("readxl") # you will need internet for this 

# After successful installation, load the readxl package
library(readxl) 

# Load the data set "CHP data.xls" which is in the "data" folder in the working directory
chp = read_xls('data/CHP data.xls',	sheet = "Sheet1")

#############

## Data exploration 

# Describe your data: Quick look at the data
head(chp)
str(chp)
summary(chp)

# Describe your data: character variables
table(chp$gender, useNA = 'ifany')
table(chp$ethnicity, useNA = 'ifany')
table(chp$smoke, useNA = 'ifany')

#############

# Summarise the data: continuous variable

# LDL
summary(chp$ldl)
quantile(chp$ldl)
quantile(chp$ldl,na.rm = TRUE)
# Checking the distribution of the variable (using histogram)
hist(chp$ldl,breaks = 100,xlab="LDL Cholesterol (mmol/L)", main="Histogram of LDL Cholesterol")
# Checking for outliers (using box plot)
boxplot(chp$ldl,ylab="LDL Cholesterol (mmol/L)", main="Boxplot of LDL Cholesterol",col = 'grey')

# ++++++++ In-class exercise 1 ++++++++ #
# Summarize the data for the following continuous variables:
# (i) Age  
# (ii) BMI
# Do you observe any missing/erroneous values?
  
# (i) Age
summary(chp$age)
quantile(chp$age)
quantile(chp$age,na.rm = TRUE)
# Checking the distribution of the variable (using histogram)
hist(chp$age,breaks = 50,xlab="Age (in years)",ylab="Number of participants", main="Histogram of Age",col='#FFFFFF')
# Checking for outliers (using box plot)
boxplot(chp$age,ylab="Age (in years)", main="Boxplot of Age",col = 'grey')

# (ii) BMI

summary(chp$bmi)
# quantile(chp$bmi)
quantile(chp$bmi,na.rm = TRUE)
# Checking the distribution of the variable (using histogram)
hist(chp$bmi,breaks = 50,xlab="BMI (in kg/m2)",ylab="Number of participants", main="Histogram of Age",col='#FFFFFF')
# Checking for outliers (using box plot)
boxplot(chp$bmi,ylab="BMI (in kg/m2)", main="Boxplot of Age",col = 'grey')




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# Summarise the data: categorical variable

# Gender

# build a (one-way) contingency table of the counts at each combination of factor levels 
table(chp$gender, useNA = 'ifany')
# Use the “prop.table” function to get the proportions
prop.table(table(chp$gender, useNA = 'ifany'))


# ++++++++ In-class exercise 2 ++++++++ #
# Summarize the data for the following categorical variables:
# (i) Smoking status
# (ii) Ethnicity
# Do you observe any missing/erroneous values?
  

# (i) Smoking status

# build a (one-way) contingency table of the counts at each combination of factor levels 
table(chp$smoke, useNA = 'ifany')
# Use the “prop.table” function to get the proportions
prop.table(table(chp$smoke, useNA = 'ifany'))

# (ii) Ethnicity
# build a (one-way) contingency table of the counts at each combination of factor levels 
table(chp$ethnicity, useNA = 'ifany')
# Use the “prop.table” function to get the proportions
prop.table(table(chp$ethnicity, useNA = 'ifany'))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

##############################################################################





############# Clean and prepare your data ############# 

# Data cleaning for continuous variable: LDL Cholesterol 
chp$ldl_drop = chp$ldl
chp$ldl_drop[chp$ldl_drop<1 | chp$ldl_drop>10]
chp$ldl_drop[chp$ldl_drop<1 | chp$ldl_drop>10] = NA
summary(chp$ldl_drop)

# ++++++++ In-class exercise 3 ++++++++ #
# Data cleaning for the following continuous variables:
# (i) Age
# (ii) BMI

# (i) Data cleaning for continuous variable: Age
chp$age_drop = chp$age
chp$age_drop[chp$age_drop<40 | chp$age_drop>100]
chp$age_drop[chp$age_drop<40 | chp$age_drop>100] = NA
summary(chp$age_drop)





# (ii) Data cleaning for continuous variable: BMI

chp$bmi_drop = chp$bmi
chp$bmi_drop[chp$bmi_drop>50]
chp$bmi_drop[chp$bmi_drop>50] = NA
summary(chp$bmi_drop)
chp$bmi_drop[chp$bmi_drop>50]


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #



# Data cleaning for categorical variable: Gender
table(chp$gender, useNA = 'ifany')
chp$gender[chp$gender %in% "male"]
chp$gender[chp$gender %in% "male"] = "Male"
chp$gender[chp$gender %in% "q"]
chp$gender[chp$gender %in% "q"] = NA
table(chp$gender, useNA = 'ifany')


# ++++++++ In-class exercise 4 ++++++++ #
# Data cleaning for the following categorical variables:
# (i) Smoking status
# (ii) Ethnicity

# (i) Smoking status

table(chp$smoke, useNA = 'ifany')
chp$smoke[chp$smoke %in% "zsmoker"] = NA
table(chp$smoke, useNA = 'ifany')


# (ii) Ethnicity
table(chp$ethnicity, useNA = 'ifany')
chp$ethnicity[chp$ethnicity %in% "chinese"] = "Chinese"
table(chp$ethnicity, useNA = 'ifany')


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# Data transformation: Transforming character variable to binary variable: gender 

table(chp$gender, useNA = 'ifany')
chp$female = NA
table(chp$female, useNA = 'ifany')
chp$female[chp$gender %in% "Female"] = 1
chp$female[chp$gender %in% "Male"] = 0
table(chp$female, useNA = 'ifany')


# Data transformation: Alternative method to convert a character variable: smoking status

class(chp$smoke)
table(chp$smoke, useNA = 'ifany')
chp$smoke1 = NA
table(chp$smoke1, useNA = 'ifany')
chp$smoke1[chp$smoke %in% "Daily smoker"] = 1
chp$smoke1[chp$smoke %in% "Ex-smoker"] = 2
chp$smoke1[chp$smoke %in% "Never-Smoker"] = 3
chp$smoke1[chp$smoke %in% "Occasional smoker"] = 4
table(chp$smoke1, useNA = 'ifany')


# Data transformation: Re-categorise variable smoke into binary variable: smoker [1: Daily smoker/Occasional smoker; and 0: Ex-smoker/Never-smoker]

chp$smoker = NA
table(chp$smoker, useNA = 'ifany')
chp$smoker[chp$smoke %in% "Daily smoker"] = 1
chp$smoker[chp$smoke %in% "Occasional smoker"] = 1
chp$smoker[chp$smoke %in% "Ex-smoker"] = 0
chp$smoker[chp$smoke %in% "Never-Smoker"] = 0
table(chp$smoker, useNA = 'ifany')
table(chp$smoke,chp$smoker,useNA = 'ifany')


# Data transformation: Categorise variable BMI into binary variable [overweight (1: ≥25 kg/m2 ; 0: ≤24.9 kg/m2)]

chp$overweight = NA
table(chp$overweight, useNA = 'ifany')
chp$overweight[chp$bmi_drop < 25] = 0
chp$overweight[chp$bmi_drop >= 25] = 1
#verify the range
summary(chp$bmi_drop)
summary(chp$bmi_drop[chp$overweight %in% 0])
summary(chp$bmi_drop[chp$overweight %in% 1])


summary(chp$bmi_drop[chp$gender %in% 'Male'])
summary(chp$bmi_drop[chp$gender %in% 'Female'])

summary(chp$bmi_drop[chp$cvd %in% 1])
summary(chp$bmi_drop[chp$cvd %in% 0])

# Store this file as “chp_processed_data”
chp_processed_data = chp


# Delete the observations with missing and/or erroneous data

# Identify the rows with missing and/or erroneous data using the "which" function 
problemrows = which(is.na(chp$age_drop) | is.na(chp$gender) | 
                      is.na(chp$bmi_drop) | is.na(chp$ethnicity) |
                      is.na(chp$smoke) | is.na(chp$cvd) | is.na(chp$ldl_drop))
chp[problemrows,] 

# Delete the rows with missing and/or erroneous data
chp = chp[-problemrows,]

# delete this variable as it is no longer needed
rm(problemrows)  

# Save cleaned data: as Excel csv file (easy to open this file in MS Excel)
write.csv(chp,file ='data/chp_cleaned.csv',row.names = FALSE)

# Save cleaned data: as R data file (easy to load this file into R for further analysis)
save(chp,file = 'data/chp_cleaned.rdata')

# Save cleaned data: as RDS file (easy to load this file into R for further analysis)
saveRDS(chp,file = 'data/chp_cleaned.rds')


