#' ---
#' title: "variable selection"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#'
#' This processes and scrubs the files
#'
#'# Load libraries
#'

rm(list = ls(all = TRUE)) #clear memory
library(stringr)

setwd("/practicum2")
source("common.R")

filenames=c(Sys.glob('/practicum2/data/county/2015*.csv'),
            Sys.glob('/practicum2/data/irsclean/2015*.csv'),
            Sys.glob('/practicum2/data/wonderclean/2015*.csv'))

bigdata=NULL
for(f in filenames){# f=filenames[2]
  message(f)
  fn=str_match(f, '/(\\d+)')[,2]
  d=read.csv(f)
  
  if(!is.null(bigdata)){
    bigdata=(merge(bigdata,d))
  }else{
    bigdata=d
  }
  # bigdata=dplyr::bind_rows(bigdata,sheet1)
}
d=bigdata

#########################################################
#impute child deaths for missing data
m.impute.childdeaths=lm(Deaths~child_mortality.num_deaths+0,d[d$Age.Grouping=='YOUTH',])
local({
  ##for sufficiently rare events, there is missing signal when the number of events is too small
  d=d[d$Age.Grouping=='YOUTH',]
  plot(d$Deaths,predict(m.impute.childdeaths,d));grid()
  plot((d$demographics.num_lt_18),
       d$child_mortality.num_deaths/d$demographics.num_lt_18*100000,
       col=rgb(0,0,0,0.1),log='x'
  );grid()
  points((d$demographics.num_lt_18),d$child_mortality.num_deaths,col='red',pch='.')
  points(d$demographics.num_lt_18,1/d$demographics.num_lt_18*100000,col='blue',pch='.')
  plot((d$Population),
       d$Death.per.100k,
       col=rgb(0,0,0,0.1),log='x'
  );grid()
  points(d$Population,d$Deaths,col='red',pch='.')
  points(d$Population,1/d$Population*100000,col='blue',pch='.')
})
#'# There seems to be signal drop out in small counties <10000





#################
####impute missing youth data from Wonder data
#################
nrow(d[is.na(d$Death.per.100k & d$Age.Grouping=='YOUTH'),])
plot(density(d$Death.per.100k[d$Age.Grouping=='YOUTH'],na.rm = T))
d$Deaths=ifelse(is.na(d$Deaths) & d$Age.Grouping=='YOUTH',
                predict(m.impute.childdeaths,d),
                d$Deaths)
d$Death.per.100k=ifelse(is.na(d$Death.per.100k) & d$Age.Grouping=='YOUTH',
                        predict(m.impute.childdeaths,d)/d$demographics.num_lt_18*100000,
                        d$Death.per.100k)
d$Population=ifelse(is.na(d$Population) & d$Age.Grouping=='YOUTH',
                        d$demographics.num_lt_18,
                        d$Population)
local({
  d=d[d$Age.Grouping=='YOUTH',]
  plot(d$Population,d$demographics.num_lt_18);grid()
  plot(d$Population,d$Death.per.100k,log='xy',col=1+is.numeric(is.na(d$Unreliable)));grid()
})

plot(density(d$Death.per.100k[d$Age.Grouping=='YOUTH'],na.rm = T))
nrow(d[is.na(d$Death.per.100k & d$Age.Grouping=='YOUTH'),])

summary(d[ d$Age.Grouping=='YOUTH',]$Population)
summary(d[ d$Age.Grouping=='YOUTH',]$Unreliable)
#View(d[ d$Age.Grouping=='YOUTH',c('Population','Death.per.100k')])


##graph results

local({
  d=d[d$Age.Grouping=='YOUTH',]
  plot(d$Deaths,predict(m.impute.childdeaths,d));grid()
  plot((d$demographics.num_lt_18),
       d$child_mortality.num_deaths/d$demographics.num_lt_18*100000,
       col=rgb(0,0,0,0.1),log='x'
  );grid()
  points((d$demographics.num_lt_18),d$child_mortality.num_deaths,col='red',pch='.')
  plot((d$Population),
       d$Death.per.100k,
       col=rgb(0,0,0,0.1),log='x'
  );grid()
  points(d$Population,d$Deaths,col='red',pch='.')
  plot((d$demographics.population),
       d$Death.per.100k,
       col=rgb(0,0,0,0.1),log='x'
  );grid()
  points(d$demographics.population,d$Deaths,col='red',pch='.')
})

#####################
#### END OF IMPUTATION OF YOUTH CDC WONDER DATA
########################


local({
  d=d[d$Age.Grouping=='SENIOR',]
  plot((d$Population),
       d$Death.per.100k,
       col=rgb(0,0,0,0.1),log='x',main='SENIOR'
  );grid()
  points(d$Population,d$Deaths,col='red',pch='.')
})

local({
  d=d[d$Age.Grouping=='ADULT',]
  plot((d$Population),
       d$Death.per.100k,
       col=rgb(0,0,0,0.1),log='x',main='ADULT'
  );grid()
  points(d$Population,d$Deaths,col='red',pch='.')
})


sort(apply(d,2,function(x) mean(is.na(x))))

hist(sort(apply(d,2,function(x) mean(is.na(x)))),main='missing variables')

hist(d$uninsured.pct)
plot(d$Death.per.100k,d$uninsured.pct,col=d$Age.Grouping)

###
### Modeling
###

##Note: need to use 10k from the target population as a cut-off to avoid signal loss

names(d)

predictors=read.csv('/practicum2/data/predictors.csv',stringsAsFactors = F)

setdiff(names(d),predictors$column)

ignore=predictors$column[predictors$predictor==0]

discardVars=qw(ignore)
yvar='Death.per.100k'
age='SENIOR'
age='YOUTH'
age='ADULT'
predictorVars=predictors$column[predictors$predictor==1 & predictors$column!=yvar]
d=d[d$Age.Grouping==age & !is.na(d$Death.per.100k),] 

mean(is.na(d$Death.per.100k))

d=winsor1Df(d,ignore = ignore)

plot(d$Population,d$Death.per.100k,log='x')
plot(d$Population,d$demographics.population,log='xy');grid()


d$Unreliable[is.na(d$Unreliable)]=1

local({
#  d=d[d$Unreliable<0.1,]
  plot((d$demographics.population),d$Death.per.100k,col=rgb(1*d$Unreliable,0,0,0.2),log="x",main=age);grid()
  plot((d$Population),d$Death.per.100k,col=rgb(1*d$Unreliable,0,0,0.2),log="x",main=age);grid()
  abline(reg=lm(Death.per.100k~I(log(Population)),d))
  print(lm(Death.per.100k~Population,d))
})
moments::skewness(d$Death.per.100k,na.rm=T)
moments::skewness(winsor1(d$Death.per.100k),na.rm=T)
plot(density(winsor1(d$Death.per.100k),na.rm = T))
plot(density(winsor1(asinh(d$Death.per.100k)),na.rm = T))
#d=addLadders(d,yvar=yvar,ignore = ignore)
plot(d$Population_nrP,d$Death.per.100k)

d=impute(d,ignore = ignore)

plot(d$Deaths,d$Death.per.100k,log='xy')
plot(d$Deaths,d$Death.per.100k/100000*d$Population,log='xy')


m=lm(Death.per.100k~I(1/Population),d)
summary(m)

v=ezvif(df = discard(keepNumeric(d),discardVars),yvar = yvar,folds = 5)
v=ezvif(df = keepNumeric(d[complete.cases(d),]),yvar = yvar,folds = 5)
v
require(MASS)
m=lm(ezformula(v),d)
m=lm(ezformula(c(yvar,predictorVars)),d,weights = d$Population)
summary(m)
m=stepAIC(m)
summary(m)
coef.beta.lm.as.df(m)
fit1=predict(m,d)
plot(d$Death.per.100k,fit1) #,xlim=c(0,1),ylim=c(0,1))
grid()

require(rpart)
require(rpart.plot)
summary(d$Population)
mtree=rpart(ezformula(c(yvar,predictorVars)),d,weights = d$Population,control = rpart.control(cp = 0.004))
prp(mtree,varlen=55,cex=1)
plot(d$Death.per.100k,predict(mtree));grid()

#plot(d$uninsured.pct,d$uninsured_adults.pct);grid()
#hist(d$uninsured.pct-d$uninsured_adults.pct)

d$successes=d$Population-d$Deaths
d$failures=d$Deaths
m=glm(ezformula(c('cbind(failures,successes)',predictorVars)),d,family = binomial)
summary(m)
m=stepAIC(m)
summary(m)
coef.beta.logistic.as.df(m)
fit2=predict(m,d,type='response')
summary(fit2)
summary(d$Death.per.100k/100000)
length(fit2)
length(d$Death.per.100k)
plot(d$Death.per.100k/100000,fit2) #,xlim=c(0,1),ylim=c(0,1))
grid()



d$primary_care_physicians.pcp_rate
