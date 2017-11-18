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
setwd("/practicum2")
source("common.R")
require(dplyr)
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
for(f in Sys.glob('/practicum2/data/wonder/2*.txt')){
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
  d4=expand.grid(fips=unique(d$fips),Age.Grouping=unique(d$Age.Grouping),Year=unique(d$Year))
  
  d=merge(d4,d,all=T)
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
  write.csv(d,paste0("/practicum2/data/wonderclean/",fn,"cdc.csv"),row.names = F)
}

d=alld
sort(unique(d$Age.Grouping))
pie(table(sort(d$Age.Grouping)),main='Counties with >1 mortality')


#' Noticed a lot of skew
summary(d)

plot(density(log(d$Population)))



for(n in unique(d$Age.Grouping)){
  ##perhaps smooth the data to reduce leverage or just let winsor handle it?
  ##or weight the training by ceiling(log(population size))
  local({
    d=d[d$Age.Grouping==n,]
    m=sum(d$Deaths)/sum(d$Population)
    print(sum(d$Deaths)/sum(d$Population))
    plot((d$Population),d$Death.per.100k,col=rgb(1*d$Unreliable,0,1-d$Unreliable,0.2),log="x",main=n);grid()
    abline(lm(Death.per.100k~I(Population),d),col='pink')
    # points((d$Population),(d$Deaths+500*m)/(d$Population+500)*100000,col=rgb(1*d$Unreliable,0,1,1),
    #        pch='.')
  })
}

hist(d$Unreliable)

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
ggplot(d)+
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
