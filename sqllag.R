

d=data.frame(x=1:366)
#d$x=d$x+rnorm(366)*100
d$yearly.avg=mean(d$x)

weights=seq(1:366)/366
d$w.avg=sum(d$x*weights)/sum(weights)

window.width=30
lag=window.width/2
alpha=1/(lag)

##{1^0, (1 - a)^1, (1 - a)^2, (1 - a)^3, ...}. from most to least weight
##https://en.wikipedia.org/wiki/Exponential_smoothing
##let b=(1-a)
##1+{0, b, b^2, b^3, ...}. A 

weights=1+ceiling(366*(1-alpha)^(seq(366,1,-1)-1))

## ceiling( window.width * pow(( 1 - 1.0/(lag) ) ),days.ago )

d$ema.avg=sum(d$x*weights)/sum(weights)

# for(i in 1:366){
#   weights=ceiling(366*(1-alpha)^(seq(366,1,-1)-1))
#   d$ema.avg=sum(d$x*weights)/sum(weights)
# }

ezplot(d)
summary(d)
summary(weights)
plot(weights)


x=runif(100,2,4)
y=rnorm(100,6,7)

mean(x)
mean(y)
mean(c(x,y))
sd(x)
sd(y)
pop.sd(c(x,y))




grand.mean(c(mean(x),mean(y)),c(length(x),length(y)))
grand.sd(c(pop.sd(x),pop.sd(y)),c(mean(x),mean(y)),c(length(x),length(y)))

rmse(x,rep(mean(x),length(x)))

rmse(c(x,y),c(rep(mean(x),length(x)),rep(mean(y),length(y))))

sqrt(sum(c( length(x)*(pop.sd(x)^2),length(y)*(pop.sd(y)^2) ))/(length(x)+length(y))) 

pop.sd(x)











