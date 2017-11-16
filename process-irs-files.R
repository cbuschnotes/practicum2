#' ---
#' title: "process-irs-files.R"
#' author: "Chris Busch cbusch002@regis.edu"
#' date: "2017"
#' ---
#


#'
#'# Load libraries
#'

library(stringr)
library(psych)
setwd("/practicum2")
source("common.R")

#'
#'# Process IRS data files
#'


for(f in Sys.glob('/practicum2/data/irs/1*.csv')){
  message('processing ',f)
  fn=2000+as.numeric(str_match(f, '\\/(\\d+)')[,2])
  d=read.csv(f,stringsAsFactors = F)
  dt=read.csv(f,stringsAsFactors = F,colClasses = 'character')
  d$COUNTYFIPS=dt$COUNTYFIPS ##repair
  d$STATEFIPS=dt$STATEFIPS ##repair
  rm(dt)
  str(d)
  d2=d[,c("STATEFIPS"       ,          "STATE"  ,                   "COUNTYFIPS",               
          "COUNTYNAME"       )]
  d2$year=fn
  d2$num.returns=d$N1
  d2$married.pct=d$MARS2/d$N1
  d2$dependents.ratio=d$NUMDEP/d$N1
  d2$adjusted.gross.income.avg=d$A00100/d$N1
  d2$wages.avg=d$A00200/d$N1
  d2$farming.ratio=d$SCHF/d$N1
  d2$unemployed.ratio=d$N02300/d$N1
  d2$dividends.ratio=d$N00600/d$N1
  d2$business.ratio=d$N00900/d$N1
  d2$realestate.ratio=d$N18500/d$N1  #indicator of ownership
  d2$mortgage.ratio=d$N19300/d$N1 #indicator of ownership
  d2$contributions.ratio=d$A19700/d$N1 #indicator of giving?
  d2$taxcredits.ratio = d$N07100
  write.csv(d2,paste0("/practicum2/data/irsclean/",fn,"-irs-soi.csv"))
  #summary(d2) 
  #describe(d2)
}

#' Noticed a lot of skew
summary(d2)

hist(d2$unemployed.ratio)
plot(density(asinh(d2$contributions.ratio)))


#' May need to transform or winsor data
#' With outliers:
moments::skewness(d2[,-1*(1:5)])

#' Without outliers via winsoring:
moments::skewness(winsor(d2[,-1*(1:5)]))

#' De-leveraged outliers via transformation:

moments::skewness(apply(d2[,-1*(1:5)],2,asinh))



library(dplyr)
library(choroplethr)
library(choroplethrMaps)

#' Helpful text: https://www.gislounge.com/mapping-county-demographic-data-in-r/

#'
#'# County Plots of Derived Data
#'

for(n in names(d2[,-1*(1:5)])){
  print(county_choropleth(title=paste0('  ',n),data.frame(region=as.numeric(paste0(d2$STATEFIPS,d2$COUNTYFIPS)),
                                             value=d2[[n]])))
}

#' Another option for county maps:
#' https://stackoverflow.com/questions/25875877/remove-border-lines-in-ggplot-map-choropleth
#' https://www.arilamstein.com/blog/2015/07/02/exploring-the-demographics-of-ferguson-missouri/



# end of file
