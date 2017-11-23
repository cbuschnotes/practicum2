#' ---
#' title: "process-wonder-files.R"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#


#'
#'# Load libraries
#'
rm(list = ls(all = TRUE)) #clear memory
library(stringr)
library(psych)
setwd("../practicum2")
source("common.R")
require(dplyr)

#'
#'# Process SAHIE files
#'

allSahie=NULL
for(f in Sys.glob('data/sahie/*.csv')){
  message('processing ',f)
  fn=as.numeric(str_match(f, '(\\d+)')[,2])
  d=read.csv(f,stringsAsFactors = F,sep=",",na.strings = c('','Not Applicable',"   . "),skip=79,
             colClasses = 'character')
  d=d[d$geocat==50 & d$agecat %in% c(4,1) & d$racecat==0 & d$sexcat==0 & d$iprcat==0,
      qw('year agecat statefips countyfips PCTUI')]
  names(d)[1]='Year'
  d$fips=paste0(d$statefips,d$countyfips)
  d$statefips=NULL
  d$countyfips=NULL
  d$Age.Grouping=ifelse(d$agecat==4,'YOUTH',ifelse(d$agecat==5,'ADULT',ifelse(d$agecat==1,'ADULT',NA)))
  d$agecat=NULL
  d$sahie.pct.uninsured=as.numeric(d$PCTUI)
  d$PCTUI=NULL
  allSahie=rbind(allSahie,d)
}
rm(d)

#'
#'# Process IRS data files
#'


#Header:
#
# "Notes" "County"        "County Code"   "Age Group"     "Age Group Code"   
#     "Year"   "Year Code"     Deaths  Population      Crude Rate

condensedGroups=c("1"='YOUTH',
                  "1-4"='YOUTH',
                  "5-9"='YOUTH',
                  "10-14"='YOUTH',
                  "15-19"='YOUTH',
                  "20-24"='ADULT', "25-34"='ADULT', "35-44"='ADULT', "45-54"='ADULT', 
                  "55-64"='ADULT', 
                  "65-74"='SENIOR',
                  "75-84"='SENIOR', "85+"='SENIOR')
alld=NULL
for(f in Sys.glob('data/wonder/2*.txt')){
  message('processing ',f)
  fn=as.numeric(str_match(f, '\\/(\\d+)')[,2])
  d=read.csv(f,stringsAsFactors = F,sep="\t",na.strings = c('','Not Applicable'))
  dt=read.csv(f,stringsAsFactors = F,colClasses = 'character',sep="\t",na.strings = c('','Not Applicable'))
  d$Year=fn
  d$County.Code=NULL
  d$fips=dt$County.Code ##repair
  d$Age.Group=NULL
  d$Notes=NULL
  d$Unreliable=as.numeric(regexpr("Unreliable",d$Crude.Rate)>0)
  d$Crude.Rate=as.numeric(gsub("\\s*\\(Unreliable\\)","",d$Crude.Rate))
  d$X..of.Total.Deaths=NULL
  d$Age.Grouping=condensedGroups[d$Age.Group.Code]
  
  sum(is.na(d$Age.Grouping))
  sum(is.na(d$Age.Group.Code))
  
  catln(f,"Incomplete cases:",sum(!complete.cases(d)))
  d=d[complete.cases(d),]
  names(d)
  d %>% group_by(Year,fips,Age.Grouping) %>%
    summarise(
      Unreliable=sum(Unreliable*Population)/sum(Population),
      County=min(County),
      Deaths=sum(Deaths),
      Population=sum(Population)) %>% 
    mutate(Death.per.100k=Deaths/Population*100000) %>% as.data.frame -> d
  d=merge(d,allSahie,by=qw('fips Age.Grouping Year'),all.x = T)
  ###fill in NAs for missing counties
  # d4=expand.grid(fips=unique(d$fips),Age.Grouping=unique(d$Age.Grouping),Year=unique(d$Year))
  # d=merge(d4,d,all=T)
  #######
  # d$Unreliable[is.na(d$Deaths)]=1
  # d$Deaths[is.na(d$Deaths)]=0
  # d$Death.per.100k[is.na(d$Death.per.100k)]=0
  # DO NOT NEED TO IMPUTE THIS, COUNTY HAS THE MISSING POPULATIONS
  # d4=merge(d[d$Age.Grouping=='YOUTH',],d[d$Age.Grouping=='SENIOR',],by = 'fips',all = T,
  #          suffixes = c(".youth",".senior"))
  # d4=merge(d4,d[d$Age.Grouping=='ADULT',],by = 'fips',all = T,
  #          suffixes = c("",".adult"))
  # d4=d4[,qw('Population.adult Population.youth Population.senior')]
  # d4=d4[complete.cases(d4),]
  # imputeYouthPop=
  #View(d4)
  alld=rbind(alld,d)
  #Crude Rate = Count / Population * 100,000
  summary(d)
  print(table(d$Age.Grouping))
  rm(dt)
  describe(d)
  write.csv(d,paste0("data/wonderclean/",fn,"cdc.csv"),row.names = F)
}

d=alld
sort(unique(d$Age.Grouping))
pie(table(sort(d$Age.Grouping)),main='Counties with >=10 mortality')


#' Noticed a lot of skew
summary(d)
summary(d[d$Age.Grouping=='SENIOR',])
summary(d[d$Age.Grouping=='YOUTH',])
summary(d[d$Age.Grouping=='ADULT',])

plot(density(log(d$Population)))

require(binom)
binom.confint(1,1000,methods = "exact")$lower

a100k=100000

for(n in unique(d$Age.Grouping)){
  ##perhaps smooth the data to reduce leverage or just let winsor handle it?
  ##or weight the training by ceiling(log(population size))
  local({
    d=d[d$Age.Grouping==n,]
    d=d[!is.na(d$Deaths) & !is.na(d$Population),]
    Unreliable=d$Unreliable
    Unreliable[is.na(Unreliable)]=1
    m=sum(d$Deaths)/sum(d$Population)
    priori=sum(d$Deaths,na.rm = T)/sum(d$Population,na.rm = T)
    prioriweight=1/priori * 2
    print(sum(d$Deaths,na.rm = T)/sum(d$Population,na.rm = T))
    plot((d$Population),d$Death.per.100k,col=rgb(1*Unreliable,0,1-Unreliable,0.2),log="x",main=n);grid()
    #abline(lm(Death.per.100k~I(Population),d),col='pink')
    points(d$Population, binom.confint(d$Deaths,d$Population,methods = "exact")$lower*a100k,col='pink',pch='.')
    points(d$Population, 
           (d$Deaths+prioriweight*priori)/(d$Population+prioriweight)*a100k,
           col='purple',pch='.')
    plot(d$Population, 
         (d$Deaths+prioriweight*priori)/(d$Population+prioriweight)*a100k,
         col='purple',log='x',main='priori corrected')
    plot(d$Population, (d$Deaths)/(d$Population),
         col=rgb(1,0,0,0.2),log='x',main=paste('Deaths/Population for ',n))
    abline(v=10*1/priori,col='gray');grid()
    abline(v=20*1/priori,col='gray');
    text(10*1/priori,0.005,round(10*1/priori))
    ##
    plot(density(d$Population),main=n)
    plot(d$Population, 
         d$Deaths,pch='.',
         col=rgb(1,0,0,0.1),main=n,log='xy');grid()
    abline(lm(Deaths~Population,d))
    abline(v=10*1/priori,col='gray');grid()
    abline(v=2*10*1/priori,col='gray');grid()
    text(10*1/priori,800,round(10*1/priori))
    #pie(c(small=sum(d$Population<round(10*1/priori)),large=sum(d$Population>=round(10*1/priori))),main=n)
       
    print(summary(d$Deaths))
    catln(n,'need at least a pop of this size',min(d$Deaths)*1/priori," small counties ",
          sum(d$Population<round(10*1/priori))," big counties ",sum(d$Population>=round(10*1/priori) ))
    # points((d$Population),(d$Deaths+500*m)/(d$Population+500)*100000,col=rgb(1*d$Unreliable,0,1,1),
    #        pch='.')
  })
}
summary(d$Deaths)
hist(d$Unreliable)

plot(d$Population, 
     d$Deaths,pch='.',
     col=rgb(as.numeric(d$Age.Grouping=='SENIOR'),as.numeric(d$Age.Grouping=='ADULT'),
             as.numeric(d$Age.Grouping=='YOUTH'),0.5),
#     ylim=c(1,50000),
     main='Senior, Adult, Minor deaths by population',log='xy');grid()

for(n in unique(d$Age.Grouping)){
  ##perhaps smooth the data to reduce leverage or just let winsor handle it?
  ##or weight the training by ceiling(log(population size))
  local({
    d=d[d$Age.Grouping==n & d$Unreliable<10.1,]
    m=sum(d$Deaths)/sum(d$Population)
    print(sum(d$Deaths)/sum(d$Population))
    plot(1/(d$Population),d$Death.per.100k,col=rgb(1*d$Unreliable,0,1-d$Unreliable,0.2),main=n);grid()
    abline(lm(Death.per.100k~I(1/Population),d),col='pink')
    # points((d$Population),(d$Deaths+500*m)/(d$Population+500)*100000,col=rgb(1*d$Unreliable,0,1,1),
    #        pch='.')
  })
}

#death rates higher in rural ares
#https://ruralhealth.und.edu/projects/health-reform-policy-research-center/pdf/mapping-rural-urban-mortality-differences-hhs-regions.pdf




#' May need to transform or winsor data
#' With outliers:
moments::skewness(keepNumeric(d))

#' Without outliers via winsoring:
moments::skewness(winsor(keepNumeric(d)))



#' De-leveraged outliers via transformation:

moments::skewness(apply(keepNumeric(d),2,asinh))

require(ggplot2)
ggplot(winsor1Df(d))+
  geom_density(aes(Death.per.100k))+facet_wrap(~Age.Grouping)



library(dplyr)
library(choroplethr)
library(choroplethrMaps)

#' Helpful text: https://www.gislounge.com/mapping-county-demographic-data-in-r/

#'
#'# County Plots of Derived Data
#'
for(ag in unique(d$Age.Grouping)){
  for(n in names(d)){
    if(is.numeric(d[[n]])){
      user.df= d %>% 
        mutate(region=as.numeric(fips)) %>% rename_('value'=n) %>% 
        group_by_('region')  %>% summarise(value=mean(value)) 
      names(user.df)
      print(county_choropleth(title=paste('  ',n,ag),user.df))
    }
  }
}
#' Another option for county maps:
#' https://stackoverflow.com/questions/25875877/remove-border-lines-in-ggplot-map-choropleth
#' https://www.arilamstein.com/blog/2015/07/02/exploring-the-demographics-of-ferguson-missouri/



# end of file
