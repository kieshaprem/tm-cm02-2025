############# Homework ############# 

# import the data for analysis
load('data/chp_cleaned.rdata')


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 1 ++++++++ #

# 1.	Populate the table 
table(chp$cvd)
## a) Age and CVD (Tips:  use the sd() function to calculate standard deviation, e.g., sd(chp$age[chp$cvd %in% 1]))
t.test(chp$age~chp$cvd) 
sd(chp$age[chp$cvd %in% 1])
sd(chp$age[chp$cvd %in% 0])


## b) BMI and CVD 
hist(chp$bmi)



## c) LDL cholesterol and CVD 
hist(chp$ldl)


## d) Gender and CVD 
table(chp$gender,chp$cvd)
chisq.test(table(chp$gender,chp$cvd))


## e) Ethnicity and CVD 



## f) Smoking status and CVD 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 2 ++++++++ #

# 2.	Is BMI (being overweight) associated with CVD? 
# Populate your results in the table.

table(chp$overweight,chp$cvd)
chisq.test(table(chp$overweight,chp$cvd))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 3 ++++++++ #

# 3.	Is smoking associated with BMI (being overweight)? 
# Populate your results in the table.

table(chp$smoker,chp$overweight)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 4 ++++++++ #

# 4.	Is smoking associated with CVD? 
# Populate your results in the table.





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 5 ++++++++ #

# 5. Is there a linear association between BMI and LDL cholesterol levels? 
# Report the correlation coefficient and p value of correlation. (use cor.test())
plot(chp$ldl ~ chp$bmi)
cor.test(chp$ldl, chp$bmi)






# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

