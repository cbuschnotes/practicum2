

#' https://spark.rstudio.com/
#' https://spark.rstudio.com/h2o.html
setwd("/practicum2")


library(sparklyr)
library(rsparkling)
library(dplyr)
library(DBI)
sc <- spark_connect(master = "local")
# iristbl <- copy_to(sc,iris, overwrite = TRUE)
# colnames(iristbl) ##lists the columns
# colnames(iristbl)=paste0('silly',1:5) ##i want to rename to silly names



#data   <- h2o.importFile(path = normalizePath("/practicum2/data/county/2016CHR_CSV_Analytic_Data_v2.csv"))


#https://rdrr.io/cran/sparklyr/man/spark_read_csv.html

filenames=list(chr2010='2010CountyHealthRankingsNationalData.csv',
           chr2011='2011CountyHealthRankingsNationalData_v2.csv',
           chr2012='2012CountyHealthRankingsNationalData_v2.csv',
           chr2013='2013CountyHealthRankingsNationalData.csv',
           chr2014='2014CountyHealthRankingsData-v6.csv',
           chr2015='2015CountyHealthRankingsData-v3.csv',
           chr2016='2016CountyHealthRankingsData-v3.csv'
)

require(sqldf)
chr2010=files$chr2010
chr2011=files$chr2011
chr2016=files$chr2016
grep('nins',names(chr2010),value = T)

library(dplyr)
library(choroplethrMaps)
data(county.regions)
#https://www.gislounge.com/mapping-county-demographic-data-in-r/
county_choropleth(diffdf)
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
