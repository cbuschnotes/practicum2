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

#+ loaddata

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

#+ data density

#'# What years hold what data

fullness=function() {
  years=c()
  for(y in unique(bigdata$Year)){
    years[[as.character(y)]]=0
    for(n in names(bigdata)){
      #catln(y,n,mean(!is.na(bigdata[[n]][bigdata$Year==y])))
      years[[as.character(y)]]=years[[as.character(y)]]+mean(!is.na(bigdata[[n]][bigdata$Year==y]))
    }
  }
  years
}
fullness()

#+ impute data
any(is.na(bigdata$Death.per.100k))
shush({
  bigdata=bigdata[!is.na(bigdata$Death.per.100k),] ##only data with Death.per.100k
  impute.df=as.data.frame(aggregate(bigdata,list(fips=bigdata$fips),FUN=function(x) mean(x,na.rm=T)))
})
## bigdata hotdeck mean imputation
for(n in names(bigdata)){
  if(is.numeric(bigdata[[n]]) && any(is.na(bigdata[[n]]) )){
    
    lookup=impute.df[[n]]
    names(lookup)=as.character(impute.df$fips)
    
    bigdata[[n]]=ifelse(is.na(bigdata[[n]]), lookup[as.character(bigdata$fips)] ,bigdata[[n]])
  }
}

fullness()


##first model all years data

#+ define predictors

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
predictorVarsRaw=unique(predictors$column[predictors$predictor==1 & predictors$column!=yvar])


#+ clustering

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

#+ trees

#+ fig.width=10, fig.height=10

# length(unique(bigdata$access_to_healthy_foods.pct.y))
age='YOUTH'
year=0
importance=data.frame()
for(age in unique(bigdata$Age.Grouping)){
  for(year in 0) { #c(0,unique(bigdata$Year))) {
    shush({
      d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k) & (bigdata$Year==year | year==0),] 
      label=paste(age,ifelse(year==0,'',year))
      mean(is.na(d$Death.per.100k))
      
      d=winsor1Df(d,ignore = ignore)
      
      #trees handle missing data
      #d=impute(d,ignore = ignore,missing.threshold = 0.25)
      
      require(MASS)
      
      predictorVars=intersect(names(d),predictorVarsRaw)
      predictorVars=predictorVarsRaw
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
      library(partykit)
      require(dplyr)
      summary(d$Population)
    }) #end of shush output
      # mtree=rpart(ezformula(c(yvar,grep('insured',predictorVars,value=T))),d,weights = d$Population,control = rpart.control(cp = 0.01))
      # prp(mtree,varlen=55,cex=0.8,nn=T,main=paste(label,'insurance'))
      mtree=rpart(ezformula(c(yvar,predictorVars)),d,weights = d$Population,control = rpart.control(minsplit=if(age=='YOUTH') {20}else{ 2000},cp = 0.0))
      printcp(mtree) # display the results 
      plotcp(mtree,main=label) # visualize cross-validation results 
      cp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"] ##best CP
      message(label,'cp=',cp)
      mtree=prune(mtree,cp)
      prp(mtree,varlen=ceiling(max(nchar(names(mtree$variable.importance)))),cex=0.8,nn=F,main=label,box.palette="GnRd",fallen.leaves = F)

    message(label)

    agedata=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k),] 
    
    # str(mtree)
    
    cbind(agedata,nodes=round(predict(mtree,agedata,type='vector')),
          response=predict(mtree,agedata,type='vector')) %>%
      dplyr::group_by(nodes) %>% 
      dplyr::summarise(counties=length(fips),
                       deaths.predicted=round(sum(response/100000*Population)),
                       deaths.actual=sum(Deaths),
                       pop=sum(Population),
                       dr100k.mean=mean(Death.per.100k),
                       dr100k.fit=mean(response)) %>% 
      dplyr::mutate(dr100k.group=deaths.actual*100000/pop) %>% as.data.frame %>% print
    
    importance=dplyr::bind_rows(importance,
                                cbind(data.frame(age=age,year=year),
                                      as.data.frame(t(as.data.frame(mtree$variable.importance)))))
  
    
    
    # rpart.plot(mtree,tweak=4,fallen.leaves = F,ycompress=T,extra=0)
    
    # plot.tree=function(mtree,age){
    #   if(age=='ADULT'){
    #     rpart.plot(mtree,branch.lty=3, #varlen=35,
    #                box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
    #                digits=3)
    #     
    #   }else if(age=='YOUTH'){
    #     rpart.plot(mtree,branch.lty=3, #varlen=35,
    #                box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
    #                digits=3)
    #   }else{
    #     rpart.plot(mtree,branch.lty=3, #varlen=35,
    #                box.palette="GnRd",tweak=2,fallen.leaves = F,ycompress=T,extra=0,main=label, cex.main=2,
    #                digits=3)
    #   }
    # }
    # plot.tree(mtree,age)
    plot(d$Deaths,(predict(mtree)/100000)*d$Population,col=rgb(0,0,0,0.2),main=label);grid()
    catln(label,
          '\nrmse deaths=',rmse(d$Deaths,(predict(mtree)/100000)*d$Population),
          '\nrmse Deaths by prior=',rmse(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
          '\ntree Deaths rsq=',rsq(d$Deaths,(predict(mtree)/100000)*d$Population),
          '\nprior Deaths rsq=',rsq(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
          
          '\nfitted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree)),
          '\nweighted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree),weights = d$Population),
          '\nfitted Death.per.100k rsq=',rsq(d$Death.per.100k,(predict(mtree))))
    
    
    
    #hist(predict(mtree),main=label)
  }
}  
#importance
#View(importance)
###################END OF TREE
  
#plot(d$uninsured.pct,d$uninsured_adults.pct);grid()
#hist(d$uninsured.pct-d$uninsured_adults.pct)



#+ logistic regression
if(F) {
  ################################
  year=0
  age='ADULT'
  coefreport=data.frame(names='x',age='x')
  for(age in unique(bigdata$Age.Grouping)){
    agereport=data.frame(names='x',age='x')
    for(year in c(0,unique(bigdata$Year))) {
      d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k) & (bigdata$Year==year | year==0),] 
      d=winsor1Df(d,ignore = ignore)
      d=impute(d,ignore = ignore,missing.threshold = 0.25)
      label=paste(age,ifelse(year==0,'',year))
      predictorVars=intersect(names(d),predictorVarsRaw)
      d=d[complete.cases(d[,c('Deaths','Population',grep('insured',predictorVars,value=T))]),]
      if(age=='YOUTH'){
        if(all(is.na(d[['uninsured_children.pct']]))) next;
        m=glm(ezformula(c('cbind(Deaths,I(Population-Deaths))',grep('uninsured_children.pct',predictorVars,value=T))),
              d,family = 'binomial')   
      }else{
        d$uninsured_children.pct=NULL
        m=glm(ezformula(c('cbind(Deaths,I(Population-Deaths))',base::intersect(names(d),grep('insured',predictorVars,value=T)))),
              d,family = 'binomial')  
      }
      
      m=stepAIC(m,trace=F)
      cdf=coef.beta.logistic.as.df(m)
      m=glm(ezformula(c('cbind(Deaths,I(Population-Deaths))',row.names(cdf)[1])),
            d,family = 'binomial')
      summary(m)
      
      cdf=coef.beta.logistic.as.df(m)
      names(cdf)[1]=as.character(ifelse(year==0,'ALL',year))
      cdf$age=ifelse(age=='','ALL',age)
      cdf[[paste0('rSqr',year)]]=McFaddenR2(m)['adj.mr2']
      agereport=merge(agereport,cdf,by = c('names','age'),all=T)
      ##barplot(coef.beta.logistic.as.df(m)[,1],names.arg=row.names(coef.beta.logistic.as.df(m)),main=label)
      hist(predict(m,type='response'),main=label)
      names(coef(m))
      
      fit2=predict(m,d,type='response')
      summary(fit2)
      summary(d$Death.per.100k/100000)
      length(fit2)
      length(d$Death.per.100k)
      plot(d$Deaths,fit2*d$Population,col=rgb(0,0,0,0.2),main=label) #,xlim=c(0,1),ylim=c(0,1))
      grid()
      tdf=data.frame(uninsured_adults.pct=0:100, uninsured.pct=0:100,uninsured_children.pct=0:100)
      plot(0:100,predict(m,tdf,type='response')*100000,col=rgb(0,0,0,0.2),main=label,
           ylab='Death per 100k',xlab=names(coef(m))[2]) #,xlim=c(0,1),ylim=c(0,1))
      
      plot(d$Deaths,fit2*d$Population,col=rgb(0,0,0,0.2),main=label) #,xlim=c(0,1),ylim=c(0,1))
      grid()
    }
    coefreport=dplyr::bind_rows(coefreport,agereport)
  }
  #agereport[agereport$names!='x',]
  coefreport[coefreport$names!='x',] 
  t(coefreport[coefreport$names!='x',] )[order(row.names(t(coefreport[coefreport$names!='x',] ))),]
  ###########################
}
# library(partykit) 
# mtree=ctree(ezformula(c(yvar,predictorVars)),d,weights = d$Population)

# png("airct.png", res=80, height=800, width=1600) 
# plot(airct) 
# dev.off()

