#' ---
#' title: "modeling"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#'
#' This does the modeling
#'
#'# Load libraries
#'

##regis\practicum2\test>pandoc -f docx -t markdown "Chris Busch - practicum2 proposal.docx" -o foo.md
##pandoc --extract-media ./myMediaFolder input.docx -o output.md

rm(list = ls(all = TRUE)) #clear memory
library(stringr)

setwd("~/../practicum2")
source("common.R")
require(ggplot2)

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


#+combine duplicate names

checkVar=function(var1){
  message(var1,'not na=',length(which(!is.na(bigdata[[var1]]))))
  print(table(!is.na(bigdata[[ var1  ]]),bigdata$Year,dnn=c(var1,'Year')))
  summary(bigdata[[ var1  ]])
}


#this checks to see if a var was renamed
sapply(qw('diabetes.pct_diabetic pct_diabetic.diabetes'),checkVar)



##              #thisVar becomes thatVar
varsToCombine=
  c(pct_diabetic.diabetes='diabetes.pct_diabetic',
    some_college_post_secondary_education.psed='some_college_post_secondary_education.pct',
    some_college_post_secondary_education.pct_psed='some_college_post_secondary_education.pct',
    access_to_healthy_foods.pct.x='access_to_healthy_foods.pct_food',
    access_to_recreational_facilities.rec_facility_rate='access_to_recreational_facilities.rec_fac_rate',
    air_pollution_particulate_matter.average_daily_pm2_5='daily_fine_particulate_matter.average_pm25',
    air_pollution_particulate_matter.average_daily_pm25='daily_fine_particulate_matter.average_pm25',
    diabetic_monitoring.pct_receiving_hba1c='diabetic_screening.pct_hba1c'
  )
for(n in names(varsToCombine)){
  print(table(sign(bigdata[[ varsToCombine[n]  ]]),bigdata$Year,dnn=c(varsToCombine[n],'Year (before)')))
  bigdata[[ varsToCombine[n]  ]]  = ifelse(is.na(bigdata[[ varsToCombine[n]  ]]),bigdata[[ n  ]],bigdata[[ varsToCombine[n]  ]])
  bigdata[[ n  ]]=NULL
  print(table(sign(bigdata[[ varsToCombine[n]  ]]),bigdata$Year,dnn=c(varsToCombine[n],'Year (after)')))
}



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


#+ data density


#+ impute data

average.out=function(bigdata){
  shush({
    bigdata=bigdata[!is.na(bigdata$Death.per.100k),] ##only data with Death.per.100k
    impute.df=as.data.frame(aggregate(bigdata,list(fips=bigdata$fips),FUN=function(x) mean(x,na.rm=T)))
  })
  impute.df
}

median.out=function(bigdata){
  shush({
    bigdata=bigdata[!is.na(bigdata$Death.per.100k),] ##only data with Death.per.100k
    impute.df=as.data.frame(aggregate(bigdata,list(fips=bigdata$fips),FUN=function(x) median(x,na.rm=T)))
  })
  impute.df
}

impute.df=average.out(bigdata);




cor(bigdata[,qw('other_primary_care_providers.pcp_rate
previous_other_primary_care_providers_data.pcp_rate
                previous_primary_care_physician_data_used_to_calculate_rankings.pcp_rate
                primary_care_physicians.pcp_rate
                ')],use = "pairwise.complete.obs")


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
#' bigdata hotdeck mean imputation to only impute those measures for the same fips county
for(n in names(bigdata)){
  if(is.numeric(bigdata[[n]]) && any(is.na(bigdata[[n]]) )){
    
    lookup=impute.df[[n]]
    names(lookup)=as.character(impute.df$fips)
    
    bigdata[[n]]=ifelse(is.na(bigdata[[n]]), lookup[as.character(bigdata$fips)] ,bigdata[[n]])
  }
}
fullness()




require(usmap)

#+ plot median

#+ fig.width=7, fig.height=5
for(age in unique(bigdata$Age.Grouping)){
  # plot_counties(bigdata[bigdata$Age.Grouping==age,c('fips','Deaths')],
  #               yvar='Deaths',low='green',high='red',main=paste(age,"Deaths"))
  plot_counties(winsor1Df(
    median.out(bigdata[bigdata$Age.Grouping==age,c('fips','Death.per.100k')]),
                          ignore='fips',fraction=.05),
                yvar='Death.per.100k',low='green',high='red',
                main=paste(age,"Death Rates (Winsored)"),ylab='Deaths/\nPopulation\n*100k')
}



#+

a100k=100000
n='ADULT' #hand executing this line allows one to step into the loop to bypass the for loop
for(n in unique(bigdata$Age.Grouping)){
  
  d=bigdata[bigdata$Age.Grouping==n,]
  d=d[!is.na(d$Deaths) & !is.na(d$Population),]
  #plot(density(log(d$Population)),main=paste(n,'log(Age Group Populations) Density'))
  m=sum(d$Deaths)/sum(d$Population)
  priori=sum(d$Deaths,na.rm = T)/sum(d$Population,na.rm = T)
  prioriweight=1/priori * 10
  catln(n,priori,prioriweight)
  priori=sum(d$Deaths[d$Population>prioriweight],na.rm = T)/sum(d$Population[d$Population>prioriweight],na.rm = T)
  prioriweight=1/priori * 10
  catln(n,priori,prioriweight)
  
  ##good
  plot(d$Population, (d$Deaths)/(d$Population)*a100k,
       col=rgb(1,0,0,0.2),log='x',main=paste('Deaths/Population for',n,'Group'),
       xlab='Age Group Population in County',
       ylab='Deaths/Population*100k in Age Group')
  # points(d$Population,
  #        (d$Deaths+10/2)/(d$Population+prioriweight/2), #*a100k,
  #        col='purple',pch='.')
  abline(v=10*1/priori,col='gray');
  grid()
  abline(h=quantile((d$Deaths)/(d$Population)*a100k,0.95),col='blue') #winsor
  text(10*1/priori,max((d$Deaths)/(d$Population)*a100k)*0.05,round(prioriweight))
  ##
  # plot(d$Population, 
  #      d$Deaths,pch=20,
  #      col=rgb(1,0,0,0.1),main=n,log='xy',xlab='Age Group Population',ylab='Deaths in Age Group');grid()
  # points(sort(d$Population),sort(d$Population)*priori,type='l')
  # abline(v=prioriweight,col='gray');grid()
  # text(prioriweight,800,round(10*1/priori))
  ###
  print(summary(d$Deaths))
  catln(n,'priori',priori,
        'prioriweight',prioriweight,
        'need at least a pop of this size',min(d$Deaths)*1/priori,
        " small counties ",      sum(d$Population<round(10*1/priori)),
        " big counties ",sum(d$Population>=round(10*1/priori) ))
}






#+ trees

#'
#'#Trees
#'

age='SENIOR' #hand executing this line allows one to step into the loop to bypass the for loop
age='ADULT'
age='YOUTH'
year=0
importance=data.frame()
trees=list()
perf.table=NULL
for(age in unique(bigdata$Age.Grouping)){
  
  d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k),] 
  set.seed(7)
  trainset=runif(nrow(d))<0.8
  label=paste(age,ifelse(year==0,'',year))
  mean(is.na(d$Death.per.100k))
  
  d=winsor1Df(d,ignore = ignore,trace=F) 
  
  #trees handle missing data
  #d=impute(d,ignore = ignore,missing.threshold = 0.25)
  
  require(MASS)
  
  predictorVars=intersect(names(d),predictorVarsRaw)

  colinearvars=caret::findCorrelation(cor(d[,predictorVars],use="pairwise.complete.obs"),names = T)
  colinearpos=caret::findCorrelation(cor(d[,predictorVars],use="pairwise.complete.obs"),names = F)
  catln('co-linear variables to be ignored:',caret::findCorrelation(cor(d[,predictorVars],use="pairwise.complete.obs"),names = T))
  if(! setequal(predictorVars[colinearpos],colinearvars)) stop('vars mismatch')
  predictorVars=predictorVars[-colinearpos]
  
  require(rpart)
  require(rpart.plot)
  library(partykit)
  require(dplyr)
  summary(d$Population)
  
  mtree=rpart(ezformula(c(yvar,predictorVars)),d[trainset,],weights = d$Population[trainset],
              control = rpart.control(cp = 0.005))
  #printcp(mtree) # display the results 
  plotcp(mtree,main=label) # visualize cross-validation results 
  cp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"] ##best CP
  message(label,'cp=',cp)
  mtree=prune(mtree,cp)

  catln(label,'tree depth is',max(rpart:::tree.depth(as.numeric(rownames(mtree$frame)))))
  print_rpart(mtree,digits=2,nlab = 'Counties:',ylab=paste0(yvar,':'))
  
  agedata=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k),] 
  if(nrow(agedata)!=nrow(d))stop('the winsored and not-winsored should be the same length')

  catln(age,'all data')
  cbind(agedata,node=round((predict(mtree,agedata,type='vector'))),
        response=(predict(mtree,agedata,type='vector'))) %>%
    dplyr::group_by(node) %>% 
    dplyr::summarise(counties=length(fips),
                     deaths.pred=round(sum(response/100000*Population)),
                     deaths.act=sum(Deaths),
                     age.pop=sum(Population),
                     #dr100k.mean=mean(Death.per.100k),
                     dr100k.fit=mean(response)) %>% 
    dplyr::mutate(dr100k.group=deaths.act*100000/age.pop) %>% as.data.frame %>% print
  
  catln(age,'test data')
  cbind(agedata[!trainset,],node=round((predict(mtree,agedata[!trainset,],type='vector'))),
        response=(predict(mtree,agedata[!trainset,],type='vector'))) %>%
    dplyr::group_by(node) %>% 
    dplyr::summarise(counties=length(fips),
                     deaths.pred=round(sum(response/100000*Population)),
                     deaths.act=sum(Deaths),
                     age.pop=sum(Population),
                     #dr100k.mean=mean(Death.per.100k),
                     dr100k.fit=mean(response)) %>% 
    dplyr::mutate(dr100k.group=deaths.act*100000/age.pop) %>% as.data.frame %>% print
  
  importance=dplyr::bind_rows(importance,
                              cbind(data.frame(age=age,year=year),
                                    as.data.frame(t(as.data.frame(mtree$variable.importance)))))
  
  #savedPlots=list()
  for(n in (used.rpart.vars(mtree))){
    lm.m=lm(ezformula(c(yvar,n)),d[trainset,],weights = d$Population[trainset])
    s.lm.m=summary(lm.m)
    c.lm.m=coef(s.lm.m)
    if(c.lm.m[2,"Pr(>|t|)"]<0.05){
      catln(n,paste("slope=",signif(c.lm.m[2,"Estimate"],2),
                    "r^2=",signif(s.lm.m$adj.r.squared,2)))
      plot(d[trainset,c(n,yvar)],ylab=yvar,
           xlab=n,main=age,
           sub=paste0(round(mtree$variable.importance[[n]]/sum(mtree$variable.importance)*100),'% importance'),
           #col=rgb(0,0,0,0.1/2)
           col=rgb(0,0,0,(log(d$Population[trainset])/log(max(d$Population[trainset])))/5)
           );grid()
      text(mean(d[[n]],na.rm=T),mean(d[[yvar]]),
           paste("slope=",signif(c.lm.m[2,"Estimate"],2),
                 "\nr^2=",signif(s.lm.m$adj.r.squared,2)),
           col=ifelse(c.lm.m[2,"Estimate"]<0,'darkgreen','red'),font=2,cex=1.5)
      abline(lm.m,col='steelblue')  
    }
  }
  
  
  trees[[age]]=mtree;
  
  #plot(d$Deaths,(predict(mtree)/100000)*d$Population,col=rgb(0,0,0,0.2),main=label);grid()
  catln(label,'all',
        '\nrmse deaths=',rmse(d$Deaths,(predict(mtree,d)/100000)*d$Population),
        '\nrmse Deaths by priori=',rmse(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
        '\ntree Deaths rsq=',rsq(d$Deaths,(predict(mtree,d)/100000)*d$Population),
        '\nprior Deaths rsq=',rsq(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
        
        '\nfitted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree,d)),
        '\nweighted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree,d),weights = d$Population),
        '\nfitted Death.per.100k rsq=',rsq(d$Death.per.100k,(predict(mtree,d))))
  .=data.frame(#'Age Group'=label,
        'Deaths RMSE'=rmse(d$Deaths[!trainset],((predict(mtree,d[!trainset,]))/100000)*d$Population[!trainset]),
        'Deaths by Priori RMSE'=rmse(d$Deaths[!trainset],sum(d$Deaths[!trainset])/sum(d$Population[!trainset])*d$Population[!trainset]),
        'Tree Deaths RSq'=rsq(d$Deaths[!trainset],(predict(mtree,d[!trainset,])/100000)*d$Population[!trainset]),
        'Priori Deaths RSq'=rsq(d$Deaths[!trainset],sum(d$Deaths[!trainset])/sum(d$Population[!trainset])*d$Population[!trainset]),
        'Fitted Death.per.100k RMSE'=rmse(d$Death.per.100k[!trainset],(predict(mtree,d[!trainset,]))),
        'Weighted Death.per.100k RMSE'=rmse(d$Death.per.100k[!trainset],predict(mtree,d[!trainset,]),weights = d$Population[!trainset]),
        'Fitted Death.per.100k RSq'=rsq(d$Death.per.100k[!trainset],(predict(mtree,d[!trainset,]))))
  rownames(.)=age
  if(is.null(perf.table))perf.table=.
  else perf.table=rbind(perf.table,.)
  
  print(summary(mtree))
  
}  

#'
#'# Performance Table
#'

require(gridExtra)
g <- tableGrob(signif(t(perf.table),2))
grid.newpage()
grid.draw(g)


#'
#'# Variable.Importance Barchart
#'

#+ fig.width=7, fig.height=10

for(age in unique(bigdata$Age.Grouping)){
  mtree=trees[[age]]
  data=mtree$variable.importance
  names(data)=abbreviate(names.arg = names(data),
                         minlength = floor(mean(nchar(names(data)))+sd(nchar(names(data)))))
  print(lattice::barchart( rev(data/sum(data)*100),main=age,
                           xlab='Variable Importance'))
  catln(age,'very important vars:',names(mtree$variable.importance)[(mtree$variable.importance/max(mtree$variable.importance)*100)>50])
}

#'
#'# prp chart
#'

#+ fig.width=12, fig.height=12
for(age in unique(bigdata$Age.Grouping)){
  mtree=trees[[age]]
  prp(mtree,varlen=ceiling(max(nchar(names(mtree$variable.importance)))),cex=0.8,nn=F,main=age,box.palette="GnRd",fallen.leaves = F)
}

#+ fig.width=7, fig.height=5

for(age in unique(bigdata$Age.Grouping)){
  mtree=trees[[age]]
  print(
    plot_counties(data.frame(Death.per.100k=(round(predict(mtree,impute.df,type='vector'))),
                             fips=impute.df$fips),'Death.per.100k',low='green',high='red',
                  main=paste(age,'Fitted'),print = F)
  )
}

#importance
#View(importance)
###################END OF TREE



#+clustering

#'
#'# Clustering
#'

#https://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r


#####################
#'
#' Lantz (2015) suggests the elbow method for cluster size determination.
#'
#' Compute and plot wss for k = 1 to k = 17:
#'
#'

#+ fig.width=7, fig.height=5
require(factoextra)
require(cluster)

age='SENIOR'
age='YOUTH'
allclusters=list()
allcentermeans=list()
for(age in unique(bigdata$Age.Grouping)){
  shush({
    mtree=trees[[age]]
    vip=names(mtree$variable.importance)[(mtree$variable.importance/max(mtree$variable.importance))>0.3]
    data=as.data.frame(aggregate(bigdata[,vip],
                                 list(fips=bigdata$fips),FUN=function(x) mean(x,na.rm=T)))
    fips=data$fips
    data$fips=NULL
    data=winsor1Df(data,trace=F)
    odata=data
    data=impute(data,missing.threshold = 0.1,trace=F)
    data=as.data.frame(scale(keepNumeric( data)))
  })
  #'VIF Double Check
  #'
  #' The HH library allows for the calculation Variance Inflation Factor for checking for collinearity
  #' without requiring a response variable.
  catln('removing multi-collinear vars via vif:',names(data)[HH::vif(data)>=10])
  data=data[,HH::vif(data)<10]
  
  # myvif=car::vif(lm(ezformula(c(yvar,predictorVars)), impute(d[,c(yvar,predictorVars)],trace=F)))
  # names(myvif)[myvif > 10] # problem?
  # 
  set.seed(7)
  k.max <- 8 # Maximal number of clusters
  wss <- sapply(1:k.max, function(k){set.seed(17); kmeans(data, k, nstart=5 )$tot.withinss})
  plot(1:k.max, wss, type="b", pch = 19, frame = FALSE,    xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares",main=paste(age,'elbow method'));grid()

  bestK=4
 
  set.seed(7)
  cl=kmeans(data, bestK, nstart=5,iter.max = 30 )
  print(fviz_cluster(cl, data=data,stand = FALSE, geom = "point",
                     pointsize = 1,main=paste(age,'Cluster Plot')))
  cl$clustering=cl$cluster
  allclusters[[age]]=cl
 
  data$fips=fips
  
  plot_counties(df = data.frame(fips=data$fips,cluster=as.factor(cl$clustering)),
                main=age,yvar = 'cluster')
  center.means=NULL
  for(i in sort(unique(cl$clustering))){
    w=impute.df$fips %in% data$fips[cl$clustering==i]
    catln(age,'cluster=',i,'deathRate per 100k:',sum(1.0*impute.df$Deaths[w])/
            sum(1.0*impute.df$Population[w])*100000,
          'counties:',length(unique(impute.df$fips[w])))
    .=as.data.frame(colMeans(odata[cl$clustering==i,],na.rm = T))
    names(.)=paste0('Cluster',i)
    if(is.null(center.means)) center.means=.
    else center.means=cbind(center.means,.)
  }
  allcentermeans[[age]]=center.means
  ######################
}


draw.table=function(data,main){
  library(grid)
  library(gridExtra)
  library(gtable)
  
  t1 <- tableGrob(data)
  title <- textGrob(main) #,gp=gpar(fontsize=50)
  padding <- unit(5,"mm")
  
  table <- gtable_add_rows(
    t1, 
    heights = grobHeight(title) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    title, 
    1, 1, 1, ncol(table))
  
  grid.newpage()
  grid.draw(table)
}

#+ fig.width=7, fig.height=3
for(age in unique(bigdata$Age.Grouping)){
  data=allcentermeans[[age]]
  
  names(data)=abbreviate(names.arg = names(data),
                         minlength = floor(mean(nchar(names(data)))))
  
  draw.table(signif(data,2),main=age)
  gplots::textplot(signif(data,2),valign='top');title(age)
}

#+ fig.width=7, fig.height=5
for(age in unique(bigdata$Age.Grouping)){
  cl=allclusters[[age]]
  data=as.data.frame(cl$centers)
  names(data)=abbreviate(names.arg = names(data),
                         minlength = floor(mean(nchar(names(data)))+sd(nchar(names(data)))))
  ezplot2(data,xlab='cluster',col=darken(rainbow(1+ncol(data))),
          title = paste(age,'Cluster Centers'),type='bar')
  
}
# 
# #+ fig.width=7, fig.height=5
# for(age in unique(bigdata$Age.Grouping)){
#   cl=allclusters[[age]]
#   mtree=trees[[age]]
#   vip=names(mtree$variable.importance)[(
#     mtree$variable.importance/max(mtree$variable.importance))>0.3]
#   vip=base::intersect(colnames(cl$centers),vip)
#   catln(age,vip)
#   data=as.data.frame(cl$centers[,vip,drop=F])
#   
#   names(data)=abbreviate(names.arg = names(data),
#                          minlength = floor(mean(nchar(names(data)))+sd(nchar(names(data)))))
#   
#   ezplot2(data,xlab='cluster',col=darken(rainbow(1+ncol(data))),
#           title = paste(age,'Very Important Cluster Centers'),type='bar')
# }
#### end 

#### end 