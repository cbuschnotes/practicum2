#' ---
#' title: "data merge"
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

setwd("~/../practicum2")
source("common.R")

###############load all the data

bigdata=NULL
for(year in 2010:2015){
  filenames=c(Sys.glob(paste0('data/county/',year,'*.csv')),
              Sys.glob(paste0('data/irsclean/',year,'*.csv')),
              Sys.glob(paste0('data/wonderclean/',year,'*.csv')))
  yeardata=NULL
  for(f in filenames){# f=filenames[2]
    ##gotta go by year
    message(year,' ',f)
    fn=str_match(f, '/(\\d+)')[,2]
    d=read.csv(f,stringsAsFactors = T)
    
    if(!is.null(yeardata)){
      message('merging')
      yeardata=(merge(yeardata,d,by=c('fips','Year')))
    }else{
      yeardata=d
    }
  }
  message('storing')
  if(is.null(bigdata)){
    bigdata=yeardata
  }else{
    bigdata=dplyr::bind_rows(bigdata,yeardata)
  }
}

rm(yeardata)
rm(d)

###done loading

bigdata=bigdata[!is.na(bigdata$Death.per.100k),] ##only data with Death.per.100k

##first model all years data


#################  define the predictors under consideration
yvar='Death.per.100k'
predictors=read.csv('data/predictors.csv',stringsAsFactors = F,na.strings = "")
predictors$column=coalesce(predictors$shorter,predictors$origcolumn)
##rename those pesky long column names
changelist=list()
for(i in 1:nrow(predictors)){
  if(!is.na(predictors$shorter[i])){
    changelist[[ predictors$origcolumn[i] ]]= predictors$shorter[i]
  }
}
predictors[!is.na(predictors$shorter),c('shorter','origcolumn')]
bigdata=rename.columns(bigdata,changelist)
setdiff(names(bigdata),predictors$column) ##what is missing in the first compared to second
setdiff(predictors$column,names(bigdata)) ##what is missing in the first compared to second
ignore=unique(predictors$column[predictors$predictor==0])
discardVars=c(ignore)
predictorVars=unique(predictors$column[predictors$predictor==1 & predictors$column!=yvar])



if(F) { #clustering
  #####################
  #'
  #' Lantz (2015) suggests the elbow method for cluster size determination.
  #'
  #' Compute and plot wss for k = 1 to k = 17:
  #'
  #'
  require(factoextra)
  require(cluster)
  data=bigdata[!is.na(bigdata$Death.per.100k) & bigdata$Age.Grouping=='YOUTH',]
  data=impute(data,missing.threshold = 0.1)
  data=as.data.frame(scale(keepNumeric( data)))
  
  set.seed(7)
  k.max <- 17 # Maximal number of clusters
  wss <- sapply(1:k.max, function(k){set.seed(17); kmeans(data, k, nstart=5 )$tot.withinss})
  plot(1:k.max, wss, type="b", pch = 19, frame = FALSE,    xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares",main='elbow method');grid()
  
  
  cl=clara(data, 5, metric = "manhattan",
           correct.d=TRUE)
  fviz_cluster(cl, stand = FALSE, geom = "point",
               pointsize = 1)
  for(i in unique(cl$clustering)){
    #  print(summary(bigdata$Deaths[cl$clustering==i]))
    #  print(summary(bigdata$Death.per.100k[cl$clustering==i]))
    catln(i,'deathRate per 100k',sum(1.0*bigdata$Deaths[cl$clustering==i])/
            sum(1.0*bigdata$Population[cl$clustering==i])*100000,
          'counties=',(sum(cl$clustering==i)))
  }
  
  
  
  
  
  #' The above suggests 5.
  #'
  
  
  # set.seed(7)
  # trainindex=sample(1:nrow(bigdata),100) %>% (1:nrow(bigdata))
  # distance=dist(bigdata[trainindex,predictorVars],method='manhattan')
  # hc=hclust(distance,"ward.D2")
  # plot(hc,labels=paste(round(bigdata$Death.per.100k[trainindex]),'!'))
  # rect.hclust(hc,k=3,border="blue")
  
  ######################
}

# length(unique(bigdata$access_to_healthy_foods.pct.y))
age='SENIOR'
age='YOUTH'
#age='ADULT'

for(age in unique(bigdata$Age.Grouping)){
  for(year in c(0,unique(bigdata$Year))) {
    
    d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k) & (bigdata$Year==year | year==0),] 
    label=paste(age,ifelse(year==0,'',year))
    mean(is.na(d$Death.per.100k))
    
    d=winsor1Df(d,ignore = ignore)
    
    d=impute(d,ignore = ignore,missing.threshold = 0.25)
    
    require(MASS)
    
    predictorVars=intersect(names(d),predictorVars)
    
    # m=lm(ezformula(c(yvar,predictorVars)),d,weights = d$Population)
    # summary(m)
    # m=stepAIC(m)
    # summary(m)
    # coef.beta.lm.as.df(m)
    # fit1=predict(m,d)
    # plot(d$Death.per.100k,fit1) #,xlim=c(0,1),ylim=c(0,1))
    # grid()
    
    require(rpart)
    require(rpart.plot)
    summary(d$Population)
    # mtree=rpart(ezformula(c(yvar,grep('insured',predictorVars,value=T))),d,weights = d$Population,control = rpart.control(cp = 0.01))
    # prp(mtree,varlen=55,cex=0.8,nn=T,main=paste(label,'insurance'))
    mtree=rpart(ezformula(c(yvar,predictorVars)),d,weights = d$Population,control = rpart.control(cp = 0.01))
    
    prp(mtree,varlen=ceiling(max(nchar(names(mtree$variable.importance)))),cex=0.8,nn=F,main=label,box.palette="GnRd",fallen.leaves = F)
    library(partykit)
    require(dplyr)
    message(label)
    cbind(d,nodes=predict(as.party(mtree),d,type='node')) %>%
      dplyr::group_by(nodes) %>% 
      dplyr::summarise(counties=length(fips),dead=sum(Deaths),pop=sum(Population),dr100k=mean(Death.per.100k)) %>% 
      dplyr::mutate(deadrate=dead*100000/pop) %>% as.data.frame %>% print
    # rpart.plot(mtree,tweak=4,fallen.leaves = F,ycompress=T,extra=0)
    
    plot.tree=function(mtree,age){
      if(age=='ADULT'){
        rpart.plot(mtree,branch.lty=3, #varlen=35,
                   box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
                   digits=3)
        
      }else if(age=='YOUTH'){
        rpart.plot(mtree,branch.lty=3, #varlen=35,
                   box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
                   digits=3)
      }else{
        rpart.plot(mtree,branch.lty=3, #varlen=35,
                   box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
                   digits=3)
      }
    }
    plot.tree(mtree,age)
    plot(d$Deaths,(predict(mtree)/100000)*d$Population,col=rgb(0,0,0,0.2),main=label);grid()
    
    hist(predict(mtree),main=label)
  }
}  
###################END OF TREE
  
#plot(d$uninsured.pct,d$uninsured_adults.pct);grid()
#hist(d$uninsured.pct-d$uninsured_adults.pct)

years=list()
for(y in unique(bigdata$Year)){
  years[[as.character(y)]]=0
  for(n in names(bigdata)){
    #catln(y,n,mean(!is.na(bigdata[[n]][bigdata$Year==y])))
    years[[as.character(y)]]=years[[as.character(y)]]+mean(!is.na(bigdata[[n]][bigdata$Year==y]))
  }
}
years


for(age in unique(bigdata$Age.Grouping)){
  for(year in c(0,unique(bigdata$Year))) {
    
    d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k) & (bigdata$Year==year | year==0),] 
    label=paste(age,ifelse(year==0,'',year))
    
    m=glm(ezformula(c('cbind(Deaths,I(Population-Deaths))',grep('insured',predictorVars,value=T))),
          d,family = binomial)
    summary(m)
    m=stepAIC(m)
    summary(m)
    coef.beta.logistic.as.df(m)
    hist(predict(m,type='response'))
    names(coef(m))
    
    fit2=predict(m,d,type='response')
    summary(fit2)
    summary(d$Death.per.100k/100000)
    length(fit2)
    length(d$Death.per.100k)
    plot(d$Deaths,fit2*d$Population,col=rgb(0,0,0,0.2)) #,xlim=c(0,1),ylim=c(0,1))
    grid()
    
    #names(d)[order(nchar(names(d)))]
    
    mtree2=rpart(ezformula(c(yvar,names(coef(m)[-1]))),d,weights = d$Population,control = rpart.control(cp = 0.001))
    #mtree=rpart(ezformula(c(yvar,grep('insured',predictorVars,value=T))),d,weights = d$Population,control = rpart.control(cp = 0.04))
summary(mtree2)

min(table(mtree2$where))

plot(mtree2)
prp(mtree2,varlen=55,cex=0.8,nn=T)
plot.tree(mtree2,age)
library(partykit)
nodes=predict(as.party(mtree2),d,type='node')
require(dplyr)
cbind(d,nodes=nodes) %>%
  dplyr::group_by(nodes) %>% 
  dplyr::summarise(dead=sum(Deaths),pop=sum(Population),dr100k=mean(Death.per.100k)) %>% 
  dplyr::mutate(deadrate=dead*100000/pop) %>% as.data.frame
plot(jitter(predict(mtree2)),d[[yvar]],col=rgb(0,0,0,0.5))

data.frame(b=names(mtree2$variable.importance),a=names(mtree$variable.importance))

setdiff(names(mtree$variable.importance),names(mtree2$variable.importance))

setdiff(names(coef(m)[-1]),names(mtree2$variable.importance))
setdiff(names(mtree2$variable.importance),names(coef(m)[-1]))

###########################

# library(partykit) 
# mtree=ctree(ezformula(c(yvar,predictorVars)),d,weights = d$Population)

# png("airct.png", res=80, height=800, width=1600) 
# plot(airct) 
# dev.off()

