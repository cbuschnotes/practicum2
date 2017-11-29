This processes and scrubs the files

Load libraries
==============

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

    ## Loading required package: readxl

    ## Warning: package 'readxl' was built under R version 3.4.1

    for(f in filenames){ 
      message(f)
      print(excel_sheets(f))
    }

    ## data/county/xls/2010 County Health Rankings National Data.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Ranked Measure Sources & Years"

    ## data/county/xls/2011 County Health Rankings National Data_v2.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Ranked Measure Sources & Years" "Additional Measure Data"       
    ## [7] "Addtl Measure Sources & Years"

    ## data/county/xls/2012 County Health Rankings National Data_v2.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Ranked Measure Sources & Years" "Additional Measure Data"       
    ## [7] "Addtl Measure Sources & Years"

    ## data/county/xls/2013 County Health Rankings National Data.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Additional Measure Data"        "Ranked Measure Sources & Years"
    ## [7] "Addtl Measure Sources & Years"

    ## data/county/xls/2014 County Health Rankings Data - v6.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Ranked Measure Sources & Years" "Additional Measure Data"       
    ## [7] "Addtl Measure Sources & Years"

    ## data/county/xls/2015 County Health Rankings Data - v3.xls

    ## [1] "Introduction"                   "Outcomes & Factors Rankings"   
    ## [3] "Outcomes & Factors SubRankings" "Ranked Measure Data"           
    ## [5] "Ranked Measure Sources & Years" "Additional Measure Data"       
    ## [7] "Addtl Measure Sources & Years"

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

    ## data/county/xls/2010 County Health Rankings National Data.xls

    ## 2010 data/county/xls/2010 County Health Rankings National Data.xls Ranked Measure Data

    ## data/county/xls/2011 County Health Rankings National Data_v2.xls

    ## 2011 data/county/xls/2011 County Health Rankings National Data_v2.xls Ranked Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in AX1406 / R1406C50: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in CI2453 / R2453C87: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in AX2963 / R2963C50: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in AX2965 / R2965C50: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in AX3089 / R3089C50: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in BL3145 / R3145C64: got '*Recently, we detected
    ## an error in our estimates of Primary Care Physicians. The number of General
    ## Practice Physicians was double counted. As a result, the estimate of
    ## Primary Care Physicians was incorrect. We have corrected the errors. The
    ## correct estimates of the rate of Primary Care Physicians are available
    ## on our website and in the downloadable data files. The specific county
    ## Rankings for 2010 - 2012 will not be recalculated based on the updated
    ## data. The old data that the Rankings are based on is included in all Data
    ## Files. You can find all data files at: http://www.countyhealthrankings.org/
    ## rankings/ranking-methods/download-rankings-data'

    ## deleting primary_care_physicians.pcp_ratio

    ## deleting previous_primary_care_physician_data_used_to_calculate_rankings.pcp_ratio

    ## 2011 data/county/xls/2011 County Health Rankings National Data_v2.xls Additional Measure Data

    ## deleting mental_health_providers_mhp.mph_ratio

    ## data/county/xls/2012 County Health Rankings National Data_v2.xls

    ## 2012 data/county/xls/2012 County Health Rankings National Data_v2.xls Ranked Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D1608 / R1608C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D1621 / R1621C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D1636 / R1636C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D1691 / R1691C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D2410 / R2410C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D2592 / R2592C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D2601 / R2601C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting logical in D2696 / R2696C4: got 'x'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in BR3145 / R3145C70: got '*Recently, we detected
    ## an error in our estimates of Primary Care Physicians. The number of General
    ## Practice Physicians was double counted. As a result, the estimate of
    ## Primary Care Physicians was incorrect. We have corrected the errors. The
    ## correct estimates of the rate of Primary Care Physicians are available
    ## on our website and in the downloadable data files. The specific county
    ## Rankings for 2010 - 2012 will not be recalculated based on the updated
    ## data. The old data that the Rankings are based on is included in all Data
    ## Files. You can find all data files at: http://www.countyhealthrankings.org/
    ## rankings/ranking-methods/download-rankings-data'

    ## deleting primary_care_physicians.pcp_ratio

    ## deleting previous_primary_care_physician_data_used_to_calculate_rankings.pcp_ratio

    ## 2012 data/county/xls/2012 County Health Rankings National Data_v2.xls Additional Measure Data

    ## deleting mental_health_providers.mph_ratio

    ## deleting dentists.dentist_ratio

    ## data/county/xls/2013 County Health Rankings National Data.xls

    ## 2013 data/county/xls/2013 County Health Rankings National Data.xls Ranked Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in BT3196 / R3196C72: got '^This data was updated
    ## on August 21, 2014. Please see http://www.countyhealthrankings.org/content/
    ## data-changes for more information.'

    ## deleting primary_care_physicians.pcp_ratio

    ## deleting dentists_.dentist_ratio

    ## deleting previous_dentists_data_used_to_calculate_rankings_.dentist_ratio

    ## 2013 data/county/xls/2013 County Health Rankings National Data.xls Additional Measure Data

    ## deleting mental_health_providers.mhp_ratio

    ## data/county/xls/2014 County Health Rankings Data - v6.xls

    ## 2014 data/county/xls/2014 County Health Rankings Data - v6.xls Ranked Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in AT3145 / R3145C46: got '^This data was updated
    ## on March 25, 2015. Please see http://www.countyhealthrankings.org/content/
    ## data-changes for more information.'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in BX3145 / R3145C76: got '^This data was updated
    ## on August 21, 2014. Please see http://www.countyhealthrankings.org/content/
    ## data-changes for more information.'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim
    ## = shim, : Expecting logical in CE3145 / R3145C83: got '^An error was
    ## found in this data, and corrected data is not currently available. Please
    ## see http://www.countyhealthrankings.org/content/data-changes for more
    ## information.'

    ## deleting primary_care_physicians.pcp_ratio

    ## deleting dentists.dentist_ratio

    ## deleting previous_dentists_data_used_to_calculate_rankings.dentist_ratio

    ## deleting mental_health_providers.mhp_ratio

    ## deleting previous_mental_health_providers_data_used_to_calculate_rankings.mhp_ratio

    ## 2014 data/county/xls/2014 County Health Rankings Data - v6.xls Additional Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim
    ## = shim, : Expecting logical in BG3145 / R3145C59: got '^An error was
    ## found in this data, and corrected data is not currently available. Please
    ## see http://www.countyhealthrankings.org/content/data-changes for more
    ## information.'

    ## deleting other_primary_care_providers.other_pcp_ratio

    ## deleting previous_other_primary_care_providers_data.other_pcp_ratio

    ## data/county/xls/2015 County Health Rankings Data - v3.xls

    ## 2015 data/county/xls/2015 County Health Rankings Data - v3.xls Ranked Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in CB3145 / R3145C80: got '^This data was updated
    ## on March 29, 2017. Please see http://www.countyhealthrankings.org/content/
    ## data-changes for more information.'

    ## deleting primary_care_physicians.pcp_ratio

    ## deleting dentists.dentist_ratio

    ## deleting mental_health_providers.mhp_ratio

    ## deleting previous_mental_health_providers_data_used_to_calculate_rankings.mhp_ratio

    ## 2015 data/county/xls/2015 County Health Rankings Data - v3.xls Additional Measure Data

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in BR3145 / R3145C70: got '^This data was updated
    ## on March 29, 2017. Please see http://www.countyhealthrankings.org/content/
    ## data-changes for more information.'

    ## deleting other_primary_care_providers.other_pcp_ratio

    ## deleting previous_other_primary_care_providers_data.other_pcp_ratio

    # ## END OF SCRIPT
