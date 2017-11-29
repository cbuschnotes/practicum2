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





##              #thisVar becomes thatVar
varsToCombine=
  c(some_college_post_secondary_education.psed='some_college_post_secondary_education.pct',
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

sapply(qw('other_primary_care_providers.pcp_rate
previous_other_primary_care_providers_data.pcp_rate
          previous_primary_care_physician_data_used_to_calculate_rankings.pcp_rate
          primary_care_physicians.pcp_rate
          '),checkVar)

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



# length(unique(bigdata$access_to_healthy_foods.pct.y))
age='SENIOR'
age='ADULT'
age='YOUTH'
year=0
importance=data.frame()
trees=list()
for(age in unique(bigdata$Age.Grouping)){
  shush({
    d=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k) & (bigdata$Year==year | year==0),] 
    label=paste(age,ifelse(year==0,'',year))
    mean(is.na(d$Death.per.100k))
    
    d=winsor1Df(d,ignore = c('Death.per.100k',ignore))
    
    #trees handle missing data
    #d=impute(d,ignore = ignore,missing.threshold = 0.25)
    
    require(MASS)
    
    predictorVars=intersect(names(d),predictorVarsRaw)
    predictorVars=predictorVarsRaw
    
    require(rpart)
    require(rpart.plot)
    library(partykit)
    require(dplyr)
    summary(d$Population)
  }) #end of shush output
  if(age=='SENIOR'){
    mtree=rpart(ezformula(c(yvar,grep('insured',predictorVars,invert = T,value = T))),
                d,weights = d$Population,
                control = rpart.control(minsplit= 2000,cp = 0.0)) 
  }else{
    mtree=rpart(ezformula(c(yvar,predictorVars)),d,weights = d$Population,
                control = rpart.control(minsplit=if(age=='YOUTH') {20}else{ 2000},cp = 0.0))
  }
  #printcp(mtree) # display the results 
  plotcp(mtree,main=label) # visualize cross-validation results 
  cp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"] ##best CP
  message(label,'cp=',cp)
  if(cp==0){
    cp=mtree$cptable[which.min(mtree$cptable[,"xerror"]>1.01*min(( mtree$cptable[,"xerror"]))),"CP"]
  }
  message(label,'cp=',cp)
  mtree=prune(mtree,cp)
  
  message(label)
  
  agedata=bigdata[bigdata$Age.Grouping==age & !is.na(bigdata$Death.per.100k),] 
  
  # str(mtree)
  catln(age)
  cbind(agedata,node=round(predict(mtree,agedata,type='vector')),
        response=predict(mtree,agedata,type='vector')) %>%
    dplyr::group_by(node) %>% 
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
  
  trees[[age]]=mtree;
  
  plot(d$Deaths,(predict(mtree)/100000)*d$Population,col=rgb(0,0,0,0.2),main=label);grid()
  catln(label,
        '\nrmse deaths=',rmse(d$Deaths,(predict(mtree)/100000)*d$Population),
        '\nrmse Deaths by priori=',rmse(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
        '\ntree Deaths rsq=',rsq(d$Deaths,(predict(mtree)/100000)*d$Population),
        '\nprior Deaths rsq=',rsq(d$Deaths,sum(d$Deaths)/sum(d$Population)*d$Population),
        
        '\nfitted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree)),
        '\nweighted Death.per.100k rmse=',rmse(d$Death.per.100k,predict(mtree),weights = d$Population),
        '\nfitted Death.per.100k rsq=',rsq(d$Death.per.100k,(predict(mtree))))
  
  
  
}  

#+ fig.width=10, fig.height=10
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
for(age in unique(bigdata$Age.Grouping)){
  
  data=as.data.frame(aggregate(bigdata[,names(trees[[age]]$variable.importance)],
                               list(fips=bigdata$fips),FUN=function(x) mean(x,na.rm=T)))
  fips=data$fips
  data$fips=NULL
  data=winsor1Df(data)
  data=impute(data,missing.threshold = 0.1)
  data=as.data.frame(scale(keepNumeric( data)))
  set.seed(7)
  k.max <- 8 # Maximal number of clusters
  wss <- sapply(1:k.max, function(k){set.seed(17); kmeans(data, k, nstart=5 )$tot.withinss})
  plot(1:k.max, wss, type="b", pch = 19, frame = FALSE,    xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares",main=paste(age,'elbow method'));grid()
  ####
  # require(mclust)
  # d_clust <- Mclust(as.matrix(data), G=1:15, 
  #                   modelNames = mclust.options("emModelNames"))
  # d_clust$BIC
  # plot(d_clust)
  ####
  # library(NbClust)
  # nb <- NbClust(data, diss=NULL, distance = "euclidean", 
  #               min.nc=2, max.nc=5, method = "kmeans", 
  #               index = "all", alphaBeale = 0.1)
  # hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
  # fviz_nbclust(nb) + theme_minimal()
  # mean(t(nb$Best.nc['Number_clusters',]))
  # ####
  # 
  # library(cluster)
  # clusGap(data, kmeans, 10, B = 100, verbose = interactive())
  
  bestK=3
  #clara
  # cl=clara(data, bestK, metric = "manhattan",
  #          correct.d=TRUE,samples=50)
  # print(fviz_cluster(cl, data=data,stand = FALSE, geom = "point",
  #                    pointsize = 1))
  #kmeans
  set.seed(7)
  cl=kmeans(data, bestK, nstart=5 )
  print(fviz_cluster(cl, data=data,stand = FALSE, geom = "point",
                     pointsize = 1))
  cl$clustering=cl$cluster
  allclusters[[age]]=cl
  ###
  data$fips=fips
  #map("county",fill=T,col='white',myborder=0,border=NA)
  #map("county", fips2statecounty[names(fips2statecounty) %in% bigdata$fips[w]],
  #    add=T,fill=T,col=1+i) 
  
  plot_counties(df = data.frame(fips=data$fips,cluster=as.factor(cl$clustering)),
                main=age,yvar = 'cluster')
  for(i in sort(unique(cl$clustering))){
    w=impute.df$fips %in% data$fips[cl$clustering==i]
    catln(age,'cluster=',i,'deathRate per 100k:',sum(1.0*impute.df$Deaths[w])/
            sum(1.0*impute.df$Population[w])*100000,
          'counties:',length(unique(impute.df$fips[w])))
  }

  ######################
}

#+ fig.width=10, fig.height=5
for(age in unique(bigdata$Age.Grouping)){
  cl=allclusters[[age]]
  data=as.data.frame(cl$centers)
  names(data)=abbreviate(names.arg = names(data),
                         minlength = floor(mean(nchar(names(data)))+sd(nchar(names(data)))))
  ezplot2(data,xlab='cluster',col=darken(rainbow(1+ncol(data))),
        title = paste(age,'Cluster Centers'),type='bar')
}
#### end 