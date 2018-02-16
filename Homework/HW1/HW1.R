###########################################
# Assignment: Homework 1
# Author: Caroline Ledbetter
# Date: 2/16/18
###########################################

library(foreign)
library(rms)
library(pROC)

################################################################################
############################# Question 2 #######################################
################################################################################
set.seed(88)
x <- rnorm(100)
set.seed(99)
eps<- rnorm(100, 0, 0.25)
y <- -1 + 0.5*x + eps
plot(x, y)
ols <- lm(y ~ x)
ols
summary(ols)

two_f <- lm(y ~ x + I(x^2))
summary(two_f)
anova(two_f, ols)

set.seed(99)
eps_2 <- rnorm(100, 0, 0.1)
y_2 <- -1 + 0.5*x + eps_2
plot(y_2, x)
two_g <- lm(y_2 ~ x)
summary(two_g)
two_g_f <- lm(y_2 ~ x + I(x^2))
summary(two_g_f)
anova(two_g_f, two_g)

validate(ols(y~x, x = T, y = T))
validate(ols(y_2~x, x = T, y = T))


################################################################################
############################# Question 3 #######################################
################################################################################


TBI <- read.spss('../DATA/TBI.sav')
TBI <- as.data.frame(TBI)

TBI$motor2 <- TBI$d.motor == 2
TBI$motor3 <- TBI$d.motor == 3
TBI$motor4 <- TBI$d.motor == 4
TBI$motor5 <- TBI$d.motor == 5
TBI$motor6 <- TBI$d.motor == 6
# check
lapply(c('motor2', 'motor3', 'motor4', 'motor5', 'motor6'), 
       function(x) table(TBI$d.motor, TBI[, x], useNA = 'ifany'))

TBI$pupil2 <- TBI$d.pupil == 'one reactive'
TBI$pupil3 <- TBI$d.pupil == 'no reactive pupils'
#check
lapply(c('pupil2', 'pupil3'), 
       function(x) table(TBI$d.pupil, TBI[, x], useNA = 'ifany'))

TBI$d.motor <- factor(TBI$d.motor, levels = 1:6)
US_TBI <- TBI[TBI$trial == "Tirilazad US", ]
# n = 1041 check

plot(US_TBI$age, US_TBI$d.unfav)
plot(US_TBI$d.motor, US_TBI$d.unfav)
plot(US_TBI$d.pupil, US_TBI$d.unfav)

three_a <- lrm(d.unfav ~ motor2 + motor3 + motor4 + motor5 + motor6 
               + pupil2 + pupil3 + age, data = US_TBI)
three_a

three_a_fac <- lrm(d.unfav ~ d.motor + d.pupil + age, 
                   data = US_TBI, x = T, y = T)
three_a_fac

three_a_spline <- lrm(d.unfav ~ d.motor + d.pupil + rcs(age), 
                     data = US_TBI, x = T, y = T)
three_a_spline
anova(three_a_spline)
anova(three_a_fac)

predict3b <- predict(three_a_fac, US_TBI, type = 'fitted')
val3b <- val.prob(predict3b, US_TBI$d.unfav, m=20, cex=.5) 
roc3b <- roc(US_TBI$d.unfav ~ predict3b)
plot.roc(roc3b, print.auc=TRUE)


validate3d <- validate(three_a_fac, method="boot", B=200)
validate3d

cal3d <- calibrate(three_a_fac, method="boot", B=200)
plot(cal3d)

Int_TBI <- TBI[TBI$trial == "Tirilazad International", ]
three_f <- predict(three_a_fac, 
                   Int_TBI, 
                   type = 'fitted')
three_g <- val.prob(three_f, Int_TBI$d.unfav, m=20, cex=.5) 
roc3g <- roc(Int_TBI$d.unfav ~ three_f)
plot.roc(roc3g, print.auc=TRUE)

save.image('HW1.RData')
