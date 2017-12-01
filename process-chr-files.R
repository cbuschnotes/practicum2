#' ---
#' title: "process CHR data files"
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

scrubnames=function(h2){
  h2[is.na(h2)]=''
  h2=sub('% Households with high housing costs','Households with high housing costs',h2,fixed = T)
  h2=tolower(gsub('_+','_',gsub('[-]','_',gsub('\\s+','_',gsub('\\#','num',gsub('\\%','pct',h2))))))
  h2=gsub('<','lt',h2,fixed = T)
  h2=gsub('>','gt',h2,fixed=T)
  h2=gsub('/','over',h2,fixed=T)
  h2=gsub(',','',h2,fixed=T)
  h2=gsub("'",'',h2,fixed=T)
  h2=gsub('^','',h2,fixed=T)
  h2=gsub('(','',h2,fixed=T)
  h2=gsub(')','',h2,fixed=T)
  h2=gsub('*','',h2,fixed=T)
  h2=gsub('^\\.','',h2,fixed=F,perl=T)
  h2
}

purgeUselessColumn=function(d,pattern){
  if(length(grep(pattern,names(d)))>0) d=d[,-1*grep(pattern,names(d))]
  d
}

purgeUselessColumnIfText=function(d,pattern){
  for(n in grep(pattern,names(d),perl = T,value = T)){
      if(! is.numeric(d[[n]])){
        message('deleting ',n)
        d[[n]]=NULL
      }
  }
  d
}


purgeUselessColumns=function(d){
  if(length(grep('95pct_ci_',names(d)))>0) d=d[,-1*grep('95pct_ci_',names(d))]
  if(length(grep('quartile',names(d)))>0) d=d[,-1*grep('quartile',names(d))]
  if(length(grep('unreliable',names(d)))>0) d=d[,-1*grep('unreliable',names(d))]
  if(length(grep('sample_size',names(d)))>0) d=d[,-1*grep('sample_size',names(d))]
  if(length(grep('_percentile',names(d)))>0) d=d[,-1*grep('_percentile',names(d))]
  if(length(grep('cohort',names(d)))>0) d=d[,-1*grep('cohort',names(d))]
  d$na=NULL
  d
}

shortenNames=function(d){
  h=names(d)
  h=gsub('\\.','_._',h)
  h2=strsplit(h,'([_])',perl=T)
  for(i in 1:length(h2)){ #i=7; h2[i]
    names(d)[i]=sub('\\.$','',gsub('_*\\._*','.',paste0(unique(unlist(h2[i])),collapse = '_'),perl=T,fixed = F))
    names(d)[i]=sub('data_for_measures_with_an_asterisk_should_not_be_compared_prior_years_due_to_changes_in_definition.','',names(d)[i])
  }
  d
}



filenames=Sys.glob('data/county/xls/*.xls')

require(readxl)
for(f in filenames){ 
  message(f)
  print(excel_sheets(f))
}
require(stringr)

loadData=function(f,fn,sheetname) {
  if( sheetname %in% excel_sheets(f)){
    catln(fn,f,sheetname)
    h=as.data.frame(read_excel(f,sheetname ,n_max = 2,col_names = F))
    row1=h[1,]
    row2=h[2,]
    prev=''
    for(i in 1:ncol(row1)){
      if(!is.na(row1[i])) prev=row1[i]
      if(is.na(row1[i])) row1[i]=prev
    }
    header=scrubnames(paste0(row1,'.',row2))
    d=as.data.frame(read_excel(f,sheetname ,skip = 2,col_names = F))
    names(d)=header
    d=purgeUselessColumns(d)
    #d=purgeUselessColumn(d,'\\.num')
    d=purgeUselessColumnIfText(d,'ratio$')
    d=shortenNames(d)
    d$Year=as.numeric(fn)
    write.csv(d,paste0('data/county/',fn,sheetname,'.csv'),row.names = F)
    return(d)
  }
  NULL
}

# bigdata=data.frame()
for(f in filenames){# f=filenames[2]
  message(f)
  fn=str_match(f, '/(\\d+)')[,2]
  sheet1=loadData(f,fn,"Ranked Measure Data")
  sheet2=loadData(f,fn,"Additional Measure Data")
}



# ## END OF SCRIPT
