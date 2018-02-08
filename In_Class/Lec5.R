library(foreign) # for reading data from other sources, e.g. SPSS
library(rms) # Harrell multi-purpose library
library(Deducer) # for ROC curve plotting 
library(pROC) # another package for ROC curve plotting 
gust <- read.spss("../DATA//GustoW.sav", to.data.frame=T) 
describe(gust)


### Fit a model  ###
# Use lrm function from rms library
fit.lrm <- lrm(DAY30 ~ SEX + AGE + KILLIP + DIA + HTN, 
               data=gust, x=TRUE, y=TRUE)
fit.lrm

# Use Glm function from rms library
fit.Glm <- Glm(DAY30 ~ SEX + AGE + KILLIP + DIA + HTN, family=binomial, 
               data=gust, x=TRUE, y=TRUE)
fit.Glm


# Use glm function from stats library
fit.glm <- glm(DAY30 ~ SEX + AGE + KILLIP + DIA + HTN, 
               family=binomial, data=gust, x=TRUE, y=TRUE)
fit.glm

# lrm doesn't work with Deducer library
# roc.lrm <- rocplot(fit.lrm, AUC=TRUE)
# roc.lrm
# Glm does work with Deducer library
roc.Glm <- rocplot(fit.Glm, AUC=TRUE)
roc.Glm>  
# glm also works with Deducer library
roc.glm <- rocplot(fit.glm, AUC=TRUE)
roc.glm # gives same plot as above

### Validate predictive accuracy from a model ###
### Stick to rms package for now ...

### Optimism correction applied via simple bootstrap sampling

validate.lrm <- validate(fit.lrm, method="boot", B=200)
validate.lrm

### Estimate internal calibration  ###
### Stick to rms package for now ...
cal.lrm <- calibrate(fit.lrm, method="boot", B=200)
plot(cal.lrm)
