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
barplot(table(d$fips,sort(d$Age.Grouping)),main='Counties with >=10 mortality',col=2:4)

local({
  .=round((apply(table(d$fips,sort(d$Age.Grouping)),2,FUN=function(x) sum(x>0))/3142)*100  )
  b=barplot(.,col='lightblue')
  text(b[,1],./2,paste(.,'%'))
  title('Data Completeness by Age Group')
  .
})
  



#' Noticed a lot of skew
summary(d)
summary(d[d$Age.Grouping=='SENIOR',])
summary(d[d$Age.Grouping=='YOUTH',])
summary(d[d$Age.Grouping=='ADULT',])

plot(density(log(d$Population)),main='log(Age Group Populations) Density')

require(binom)


a100k=100000
n='ADULT'

for(n in unique(d$Age.Grouping)){
  ##perhaps smooth the data to reduce leverage or just let winsor handle it?
  ##or weight the training by ceiling(log(population size))
  d=alld
  d=d[d$Age.Grouping==n,]
  d=d[!is.na(d$Deaths) & !is.na(d$Population),]
  plot(density(log(d$Population)),main=paste(n,'log(Age Group Populations) Density'))
  Unreliable=d$Unreliable
  Unreliable[is.na(Unreliable)]=1
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
summary(d$Deaths)

###graph of overall by population
d=alld
plot(d$Population, 
     d$Deaths,pch='.',
     col=rgb(as.numeric(d$Age.Grouping=='SENIOR'),as.numeric(d$Age.Grouping=='ADULT'),
             as.numeric(d$Age.Grouping=='YOUTH'),0.5),
  xlab='Age Group Population in County',
  ylab='Deaths in Age Group',
     main='Senior, Adult, Youth Deaths',log='xy');grid() 
for(n in unique(d$Age.Grouping)){
  local({
    d=d[d$Age.Grouping==n,]
    d=d[!is.na(d$Deaths) & !is.na(d$Population),]
    priori=sum(d$Deaths,na.rm = T)/sum(d$Population,na.rm = T)
    prioriweight=1/priori * 10
    points(sort(d$Population),sort(d$Population)*priori,type='l',col='black')
    #abline(v=10*1/priori,col='gray');
    text(10*1/priori,5,round(prioriweight))
  })
}
legend("topleft",legend = c('SENIOR','ADULT','YOUTH','Overall'),col=c('red','green','blue','black'),pch=c(20,20,20,NA),lty=c(NA,NA,NA,1))
###done with graph of overall by population


###graph of overall by population
d=alld
plot(d$Population, 
     d$Deaths/d$Population*a100k,pch='.',
     col=rgb(as.numeric(d$Age.Grouping=='SENIOR'),as.numeric(d$Age.Grouping=='ADULT'),
             as.numeric(d$Age.Grouping=='YOUTH'),0.5),
     xlab='Age Group Population in County',
     ylab='Deaths per Capita in Age Group * 100k',
     main='Senior, Adult, Youth Death Rates',log='xy');grid() 
for(n in unique(d$Age.Grouping)){
  local({
    d=d[d$Age.Grouping==n,]
    d=d[!is.na(d$Deaths) & !is.na(d$Population),]
    priori=sum(d$Deaths,na.rm = T)/sum(d$Population,na.rm = T)
    prioriweight=1/priori * 10
    points(sort(d$Population),sort(d$Population)*priori/sort(d$Population)*a100k,type='l',col='black')
    #abline(v=10*1/priori,col='gray');
    text(1000000,priori*a100k*1.2,round(priori*a100k))
  })
}
legend("bottomleft",legend = c('SENIOR','ADULT','YOUTH','Overall'),col=c('red','green','blue','black'),pch=c(20,20,20,NA),lty=c(NA,NA,NA,1))
###done with graph of overall by population




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
