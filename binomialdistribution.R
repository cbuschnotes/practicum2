#' ---
#' title: "binomial distribution 'flower'"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#


#' I have a series of binomial trials 
#' 

binom.prob=1/1169
set.seed(8)
x=seq(30,5000000,1000)
y=sapply(x,function(x) sum(rbinom(x,1,binom.prob)))
#y[y<10]=NA
plot(x,y,col=rgb(1,0,0,0.5),pch='.',main='binomial distribution');grid()
plot(x,y/x,col=rgb(1,0,0,0.7),pch='.',main='binomial distribution ratio',cex=1.5);grid()

(summary(lm(y~x)))$r.squared #obviously high r squared, returns 0.97

(summary(lm(I(y/x)~x)))$r.squared #obviously low r squared, returns 0.00001977375

d=expand.grid(num=0:50,dem=1:50)
d=d[d$num<=d$dem,]
plot(d$dem,d$num/d$dem,col=rgb(1-d$dem/50,d$dem/50,0,1),pch=20,main='binomial distribution flower')

#For a binomial GLM prior weights are used to give the number of trials when the response is the proportion of successes: 
summary(glm(I(y/x)~x,data.frame(x=x),family=binomial,weights=x))
summary(glm(cbind(x-y,y)~x,data.frame(x=x),family=binomial))
