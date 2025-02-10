############# Analyse your data ############# 

# Depending on how you saved your cleaned data: import the data for analysis
load('data/chp_cleaned.rdata')

# chp = read.csv('data/chp_cleaned.csv',as.is = TRUE)
# chp = readRDS('data/chp_cleaned.rds')


## Data analysis : 2 sample t-test (to compare two population means)

# Q1. Is gender associated with levels of LDL cholesterol?
t.test(chp$ldl~chp$female) 

# Q1a. Is the LDL-gender association confounded by smoking status?

# ++++++++ In-class exercise 5 ++++++++ #
# Is there a difference in the mean LDL values between smokers and non-smokers?
t.test(chp$ldl~chp$smoker) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# Data analysis : Chi-square/Fisher’s exact test (association between two categorical variables)

# Is the gender associated with smoking status?
tab_gender_smoker = table(chp$female,chp$smoker)
dimnames(tab_gender_smoker) = list(c('male','female'),c('non-smoker','smoker'))

tab_gender_smoker
# to get % in each combinations: use the "apply" function to compute column percentages
apply(tab_gender_smoker,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))

chisq.test(tab_gender_smoker)
fisher.test(tab_gender_smoker)

# Data analysis : Investigating confounding effect

# 1a. Is the LDL-gender association confounded by smoking status?
  
t.test(chp$ldl[chp$smoker %in% 0]~chp$female[chp$smoker %in% 0]) 
t.test(chp$ldl[chp$smoker %in% 1]~chp$female[chp$smoker %in% 1]) 



# Data analysis : Correlation (linear association between two continuous variables)

# Is age associated with levels of LDL cholesterol?
cor.test(chp$ldl,chp$age)
# Scatter plot of LDL cholesterol and age
plot(chp$ldl~chp$age, 
     ylab = "LDL Cholesterol (mmol/L)",xlab = 'Age (years)')


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Appendix ++++++++ #

# Is the BMI-CVD association confounded by smoking status?
  
tab_overweight_cvd_nonsmoker = table(chp$overweight[chp$smoker %in% 0],chp$cvd[chp$smoker %in% 0]) 
dimnames(tab_overweight_cvd_nonsmoker) = list(c('Not overweight','Overweight'),c('no CVD','CVD'))
tab_overweight_cvd_nonsmoker
chisq.test(tab_overweight_cvd_nonsmoker)

tab_overweight_cvd_smoker = table(chp$overweight[chp$smoker %in% 1],chp$cvd[chp$smoker %in% 1]) 
dimnames(tab_overweight_cvd_smoker) = list(c('Not overweight','Overweight'),c('no CVD','CVD'))
tab_overweight_cvd_smoker
chisq.test(tab_overweight_cvd_smoker)

table(chp$ldl[chp$smoker %in% 1],chp$female[chp$smoker %in% 1])


# Logistic regression

m_cvd_overwight = glm(chp$cvd ~ chp$overweight,family=binomial(link="logit"))
m_cvd_overwight_summary = summary(m_cvd_overwight)
m_cvd_overwight_summary
# Estiamte of OR
exp(coefficients(m_cvd_overwight_summary)[2,1])
# 95% CI of OR (lower limit)
exp(coefficients(m_cvd_overwight_summary)[2,1]-1.96*coefficients(m_cvd_overwight_summary)[2,2])
# 95% CI of OR (upper limit)
exp(coefficients(m_cvd_overwight_summary)[2,1]+1.96*coefficients(m_cvd_overwight_summary)[2,2])

# Investigating confounding effect of smoking status on BMI-CVD association
m_cvd_overwight_nonsmoker = glm(chp$cvd[chp$smoker %in% 0] ~ chp$overweight[chp$smoker %in% 0],family=binomial(link="logit"))
summary(m_cvd_overwight_nonsmoker)
exp(coefficients(m_cvd_overwight_nonsmoker))

m_cvd_overwight_smoker = glm(chp$cvd[chp$smoker %in% 1] ~ chp$overweight[chp$smoker %in% 1],family=binomial(link="logit"))
summary(m_cvd_overwight_smoker)
exp(coefficients(m_cvd_overwight_smoker))


m_cvd_age = glm(chp$cvd ~ chp$age,family=binomial(link="logit"))
m_cvd_age_summary = summary(m_cvd_age)
m_cvd_age_summary
# Estiamte of OR
exp(coefficients(m_cvd_age_summary)[2,1])
# 95% CI of OR (lower limit)
exp(coefficients(m_cvd_age_summary)[2,1]-1.96*coefficients(m_cvd_age_summary)[2,2])
# 95% CI of OR (upper limit)
exp(coefficients(m_cvd_age_summary)[2,1]+1.96*coefficients(m_cvd_age_summary)[2,2])



# Linear regression

m_ldl_female = lm(chp$ldl ~ chp$female)
summary(m_ldl_female)

# t.test(chp$ldl ~ chp$female)

m_ldl_age = lm(chp$ldl ~ chp$age)
m_ldl_age_summary = summary(m_ldl_age)
m_ldl_age_summary
# Estimate (95% CI)
paste0('Estimate (95% CI): ',
       round(m_ldl_age_summary$coefficients[2,1],4),' (',
       round(m_ldl_age_summary$coefficients[2,1]-1.96*m_ldl_age_summary$coefficients[2,2],4),', ',
       round(m_ldl_age_summary$coefficients[2,1]+1.96*m_ldl_age_summary$coefficients[2,2],4),')')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

