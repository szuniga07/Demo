################################################################################
#                                 set up                                       #
################################################################################
library(ham)

################################################################################
#                               Load data                                      #
################################################################################


## Alpha function ##
a1s <- alpha(items= c("i1","i2","i3","i4","i5"), data= cas)
interpret(a1s)
#Removing the recommended item 1
a1s <- alpha(items= c("i2","i3","i4","i5"), data= cas)
a1s

################################################################################
#                     assess: survey los cost rdm30 death30                            #
################################################################################

#######################
## Linear regression ##
#######################

#or OLS Regression
m0 <- assess(formula=cost ~ month + program, data=hosprog, regression="ols")
summary(m0$model)

m1 <- assess(formula=cost ~ month * program, data=hosprog, intervention = "program",
             regression="ols", topcode=17150, propensity=c("female","age","risk"))
# top coding can help get more accurate program estimates with positively
# skewed outcomes which commonly happens with cost
hist(hosprog$cost)

# Review resuls
summary(m1$model)

# Interpret results
interpret(m1)$model

# importance #
importance(m1$model)

# plot importance #
par(mar=c(4.2, 2, 3.5, 3))  #helpful graph parameters
par(oma = c(0, 0, 0, 3))
plot(importance(m1$model))
# Default parameters
par(mar= c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 0, 0))


#########################
## Logistic regression ##
#########################

summary(assess(formula=vs~mpg+wt+hp, data=mtcars, 
       regression="logistic")$model)

interpret(assess(formula=vs~mpg+wt+hp, data=mtcars, 
                 regression="logistic"))$model


################################
## Differences-in-Differences ##
################################

# Standard pre-and-post test, binary time variable (did="two")
dm1 <- assess(formula= los ~ ., data=hosprog, intervention = "program",
              int.time="month", treatment= 5, did="two")
# Results
summary(dm1$DID)
# Interpret results
interpret(dm1)$did
# plot
plot(dm1, "DID", add.legend="bottom", ylim=c(0, 8))

# Continuous time variable with time points (did="many")
dm2 <- assess(formula= los ~ ., data=hosprog, intervention = "program",
              int.time="month", treatment= 5, did="many")

# Interpret results
interpret(dm2)$did
# plot
plot(dm2, "DID", add.legend="bottomleft", ylim=c(0, 8))

## Binary outcomes ##
dm3 <- assess(formula= rdm30 ~ ., data=hosprog, intervention = "program",
              int.time="month", treatment= 5, did="two")
# Results
summary(dm3$DID)


#############################
## Interrupted Time Series ##
#############################

## One group ##
im11 <- assess(formula=los ~ ., data=hosp1, intervention = "program",
               int.time="month", interrupt= 5, its="one")
summary(im11$ITS)
interpret(im11)$its
plot(im11, "ITS", add.legend="top", ylim=c(2, 8))

## One group, two periods ##
im12 <- assess(formula=los ~ ., data=hosp1, intervention = "program",
               int.time="month", interrupt= c(5, 9), its="one")
summary(im12$ITS)
interpret(im12)$its
plot(im12, "ITS", add.legend="top", ylim=c(2, 8))

# Assessing hospital inpatient bed use pre-covid, pre-and-post vaccine

# Public domain data found here:
# https://healthdata.gov/dataset/COVID-19-Estimated-Inpatient-Beds-Occupied-by-Stat/jjp9-htie/about_data

# Make California the intervention or exposure group
cts_small$program <- ifelse(cts_small$state=="CA", 1, 0)
ctca <- cts_small[cts_small$state=="CA", ]

## two group, one interruption ##

im21 <- assess(formula=inpatient_beds_used ~ ., data=cts_small, intervention = "program",
               int.time="Month", interrupt= c(4), its="two", newdata=TRUE)

summary(im21$ITS)
interpret(im21)$its
itsEffect(model= im21$ITS, type= "mgst")
plot(x=im21, y="ITS", add.legend="center")

## Two groups, two interruptions ##
im22 <- assess(formula=inpatient_beds_used ~ ., data=cts_small, intervention = "program",
               int.time="Month", interrupt= c(4,13), its="two", newdata=TRUE)
summary(im22$ITS)
interpret(im22)$its
itsEffect(model= im22$ITS, type= "mgmt")
plot(x=im22, y="ITS", add.legend="center")

## Run the model with the rms and nlme packages and new data if you suspect 
## autocorrelation such as in different states

# Make a new data frame
beds <- im22$newdata
#set up rms
library(rms)
library(nlme)  # corCompSymm function from this package
dd <- datadist(beds); options(datadist='dd');

gls22 <- Gls(inpatient_beds_used ~ ITS.Time + ITS.Int + txi + post4 + txp4 + ixp4 + txip4 + post13 + txp13 + ixp13 + txip13, 
             x=TRUE, data=beds, correlation=corCompSymm(form= ~Month|state))

# Results, you can substitute these value into the interpretations
gls22

# You will need to calculate the ITS effects for both intervention periods
# itsEffect will calculate the values from the Gls model in rms

# from the ham package
itsEffect(model= im22$ITS, type= "mgmt")
# compare with rms package results
itsEffect(gls22, "mgmt")




