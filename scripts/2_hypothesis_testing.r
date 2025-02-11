denguedata=read.csv('data/data_Dengue_Singapore.csv')

# Student's t-tests #######################################################
pop_mean = 1.7
t.test(log10(denguedata$Platelet),mu = pop_mean)

## ----ttest1--------------------------------------------------------------
p1=log10(denguedata$Platelet[denguedata$DHFever=='Yes'])
p2=log10(denguedata$Platelet[denguedata$DHFever=='No'])
t.test(p1,p2)

## ----ttest2--------------------------------------------------------------
t.test(log10(denguedata$Platelet)~denguedata$DHFever)

# if you had paired data
# t.test(plateletbefore,plateletafter,paired=TRUE)

# Non-parametric tests ####################################################

## ----wtest1--------------------------------------------------------------
p1=log10(denguedata$Platelet[denguedata$DHFever=='Yes'])
p2=log10(denguedata$Platelet[denguedata$DHFever=='No'])
wilcox.test(p1,p2)

## ----wtest2--------------------------------------------------------------
wilcox.test(log10(denguedata$Platelet)~denguedata$DHFever)

## ----wtest3,eval=FALSE---------------------------------------------------
## wilcox.test(plateletbefore,plateletafter,paired=TRUE)

## ----wtest4--------------------------------------------------------------
wilcox.test(denguedata$Platelet~denguedata$DHFever)

# Correlation tests #######################################################

## ----cortest1------------------------------------------------------------
cor.test(denguedata$Platelet,denguedata$Albumin)

## ----cortest2------------------------------------------------------------
cor.test( denguedata$Platelet,denguedata$Albumin,method='spearman')

library(lattice)
## ----cortest3------------------------------------------------------------
xyplot(denguedata$Platelet~denguedata$Albumin,
       xlab='Albumin',ylab='Platelets')


## ----cortest4------------------------------------------------------------
xyplot(rank(denguedata$Platelet)~rank(denguedata$Albumin),
       xlab='Ranked albumin',ylab='Ranked platelets')

# Chi-squared tests #######################################################

## ----chitest1------------------------------------------------------------
contable = table(denguedata$DHFever,denguedata$Sex)
print(contable)

## ----chitest2------------------------------------------------------------
chisq.test(contable)

## ----chitest3------------------------------------------------------------
fisher.test(contable)




# Create a 2x2 matrix representing the contingency table
data <- matrix(c(30, 12, 20, 25), nrow = 2,
               dimnames = list("After Method B" = c("Passed", "Failed"),
                               "After Method A" = c("Passed", "Failed")))

# Display the contingency table
print("Contingency table:")
print(data)

# Perform McNemar's Test with continuity correction
mcnemar.test(data)

# Perform McNemar's Test without continuity correction
mcnemar.test(data, correct = FALSE)
