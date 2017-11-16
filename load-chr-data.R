#' ---
#' title: "process CHR data files"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#'
#' NOTE: This file is very rough draft and needs to be cleaned up.
#'
#'# Load libraries
#'


library(stringr)

setwd("/practicum2")
source("common.R")


filenames=c('2010 County Health Rankings National Data.csv',
            '2011 County Health Rankings National Data_v2.csv',
            '2012 County Health Rankings National Data_v2.csv',
            '2013 County Health Rankings National Data.csv',
            '2014 County Health Rankings Data - v6.csv',
            '2015 County Health Rankings Data - v3.csv',
            '2016 County Health Rankings Data - v3.csv')
vars=c()

for(f in filenames){ # f='2016 County Health Rankings Data - v3.csv'
  fn=str_match(f, '^(\\d+)')[,1]
  h=read.csv(normalizePath(paste0("/practicum2/data/county/raw/",f)),nrows = 1,header=F,as.is = T)
  names(h)
  h2=unlist(h)
  h2[is.na(h2)]='NA'
  h2=tolower(gsub('_+','_',gsub('[-]','_',gsub('\\s+','_',gsub('\\#','num',gsub('\\%','pct',h2))))))
  h2=gsub('<','lt',h2,fixed = T)
  h2=gsub('>','gt',h2,fixed=T)
  h2=gsub('/','over',h2,fixed=T)
  h2=gsub(',','',h2,fixed=T)
  d=read.csv(      normalizePath(paste0("/practicum2/data/county/raw/",f)))
  names(d)=h2
  if(length(grep('95pct_ci_',names(d)))>0) d=d[,-1*grep('95pct_ci_',names(d))]
  if(length(grep('quartile',names(d)))>0) d=d[,-1*grep('quartile',names(d))]
  if(length(grep('unreliable',names(d)))>0) d=d[,-1*grep('unreliable',names(d))]
  if(length(grep('sample_size',names(d)))>0) d=d[,-1*grep('sample_size',names(d))]
  d$na=NULL
  vars=c(vars,paste0('chr',fn))
  assign(paste0('chr',fn),d)
}

pop=chr2010[,qw('fips aggregate_population')]

for(v in vars){
  message(v)
  tmp=get(v)
  tmp$aggregate_population=NULL
  tmp=merge(pop,tmp,by='fips',all=FALSE)
  assign(v,tmp)
  rm(tmp)
}
for(v in vars){message(v)
  tmp=get(v)
  tmp$death.pct=tmp$deaths/tmp$aggregate_population
  tmp$no_of_diabetics.pct=tmp$no_of_diabetics/tmp$aggregate_population
  tmp$deaths=NULL
  tmp$no_of_diabetics=NULL
  assign(v,tmp)
  rm(tmp)
}



for(v in vars){
  message(v)
  print(grep('death',names(get(v)),value = T))
  print(grep('aggregate_population',names(get(v)),value = T))
}
for(v in vars){
  message(v)
  print(grep('death',names(get(v)),value = T))
}


head(chr2010$deaths)
head(chr2011$deaths)
head(chr2012$deaths)
head(chr2013$deaths)
head(chr2014$deaths)
head(chr2015$deaths)


require(MASS)
n=find.bad.contrasts(chr2010,good=T)
for(n in names(chr2010)){
  if(is.factor(chr2010[[n]]))
    catln(n,length(levels(chr2010[[n]])))
}

d=impute(chr2010,missing.threshold = .2)
library(caret)
hc = findCorrelation(cor.numeric(d), cutoff=0.8)
colnames(cor.numeric(d))[hc]
d=d[,- hc]
xvars=names(discard(d,qw('deaths fips state aggregate_population
                         population.2 aggregate_population population ypll_rate
                         enrollment diplomas no_of_medicare_enrollees cases num_zip_codes' )))
yvar='death.pct'
ezezformula(c(yvar,xvars))
summary(m<-stepAIC(lm(ezformula(c(yvar,xvars)),d)))

coef.beta.lm.as.df(m)
``
names(d)




  

require(sqldf)
chr2010=files$chr2010
chr2011=files$chr2011
chr2016=files$chr2016
grep('nins',names(chr2010),value = T)

library(dplyr)
library(choroplethrMaps)
library(choroplethr)
data(county.regions)
#https://www.gislounge.com/mapping-county-demographic-data-in-r/
choroplethrMaps::county_choropleth(diffdf)
require(maps)

data(countyMapEnv)
str(countyMapEnv)

dd=data.frame(names=map("county", plot=FALSE)$names)

diffdf=sqldf("select `state.name` || ',' ||  `county.name` as spot,
             a.fips as region,b.pct_uninsured as after,a.pct_uninsured as before,
             (100-b.pct_uninsured)-(100-a.pct_uninsured) as value 
             from chr2010 a  join chr2016 b on a.fips=b.fips 
             join `county.regions` c on a.fips=c.region
             join dd n on n.names=`state.name` || ',' ||  `county.name`")
#diffdf$value=sign(diffdf$value)

summary(diffdf)

map("county",fill=T,col='white',myborder=0,border=NA)
map("county", diffdf$spot[diffdf$value<0],add=T,fill=T,col='red',border=NA) 
map("county", diffdf$spot[diffdf$value==0],add=T,fill=T,col='gray',border=NA) 
map("county", diffdf$spot[diffdf$value>0],add=T,fill=T,col='yellow',border=NA) 
map("county", diffdf$spot[diffdf$value>2],add=T,fill=T,col='green',border=NA) 
map("county", diffdf$spot[diffdf$value>4],add=T,fill=T,col='darkgreen',border=NA) 


map("county",fill=T,col='white',myborder=0,border=NA)
map("county", diffdf$spot[diffdf$after<15],add=T,fill=T,col='red',border=NA) 
map("county", diffdf$spot[diffdf$after>=15],add=T,fill=T,col='gray',border=NA) 
map("county", diffdf$spot[diffdf$after>19],add=T,fill=T,col='green',border=NA) 
map("county", diffdf$spot[diffdf$after>24],add=T,fill=T,col='darkgreen',border=NA) 



map("county", 'minnesota,rice',add=T,fill=T,col='red') 
map("county", 'alabama,dekalb',add=T,fill=T,col='red') 
where
grep('dekalb',county.regions$county.name,value=T)

head(county.regions)


str(dd)

files=list()
for(f in names(filenames)){ #}
  # h=read.csv(normalizePath(paste0("/practicum2/data/county/",filenames[[f]])),nrows = 1,header=F,as.is = T)
  # names(h)
  # h2=unlist(h)
  # h2[is.na(h2)]='NA'
  # h2=tolower(gsub('_+','_',gsub('[-]','_',gsub('\\s+','_',gsub('\\#','num',gsub('\\%','pct',h2))))))
  # names(h2)=NULL
  # print(h2)
  # #h2=h2[1:(length(h2)-1)]
  # d=files[[f]]=spark_read_csv(sc,f,
  #                             normalizePath(paste0("/practicum2/data/county/",filenames[[f]])))
  d=files[[f]]=read.csv(      normalizePath(paste0("/practicum2/data/county/",filenames[[f]])))
}
icols=colnames(d)
for(f in names(files)){ 
  print(icols)
  icols=base::intersect(icols,colnames(files[[f]]))
  
}
icols
big=NULL
for(f in names(files)){ 
  big=rbind(big,cbind(files[[f]][,icols],file=f))
}
big$deaths=big$deaths/big$population
big=big[complete.cases(big),]
names(big)
summary(m<-lm(deaths ~ pct_uninsured+file,big))
coef(m)

d2=as.data.frame(expand.grid(file=unique(big$file),pct_uninsured=seq(0,1,0.1)))
d2$fit=pmax(0,predict(m,d2))

require(ggplot2)
ggplot(d2) +
  geom_point(aes(x=file,y=fit,col=pct_uninsured)) 

for(f in names(files)){ 
  print(f)
  print(summary(files[[f]]$deaths[files$chr2013$county!=''])) 
}

for(f in names(files)){ 
  print(f)
  print(summary(files[[f]]$deaths[files$chr2013$county!='']/files[[f]]$population[files$chr2013$county!=''])) 
}



big[big$deaths==80666,]
files$chr2010[!is.na(files$chr2010$deaths) &  files$chr2010$deaths==80666,]

any(files$chr2013$county=='')

head(files$chr2013$county=='')

d=files[[f]]

str(files[[f]])
class(files[[f]])
tbl_vars(files[[f]]) ##instead of names
colnames(files[[f]]) ##instead of names
rename(files[[f]],'FIPS','f')
colnames(d)=as.vector(h2)
rename(files[[f]],'FIPS','f')
dn=dimnames(files[[f]])
dn[[2]]=h2
dimnames(files[[f]])<-dn

SparkR::colnames

d2=dbGetQuery(sc, "create table foo as SELECT cast(regexp_replace(Premature_death_Value,',','') as double) as x from `2016CHR_CSV_Analytic_Data_v2` where State='MN'")

d %>% dplyr::mutate(Premature_death_Value=as.numeric(sub(',','',Premature_death_Value))) -> d

d %>% select(Premature_death_Value) %>% collect()

spark_disconnect(sc)
## END OF SCRIPT
