##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Obtain a List of World Bank Data Sources 
##' @param url URL of World Bank API
##' @return data.table with WB data sources and their descriptions
##' @author Janko Cizel
getWorldBankSources <- function(url="http://api.worldbank.org"
                                )
    {
        string<- paste(url
                       ,"sources"
                       ,sep = "/"
                       )

        query <- paste0(string,
                        "?",
                        "format=json",
                        "&per_page=1000000")

        raw.data<- fromJSON(query)
        
        data <- raw.data[[2]]
        
        dat <- lapply(data, function(l){
            as.data.table(as.list(unlist(l)))
        })

        dat = rbindlist(dat, fill = TRUE)
        
        return(dat)
    }


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Get a List of Countries, ISO code lookup, and region definitions
##' @param url URL of World Bank API
##' @param type 
##' @return data.table with country information
##' @author Janko Cizel
getWorldBankCountries <- function(url="http://api.worldbank.org",
                                  type = "countries"
                                  )
    {
        string<- paste(url
                       ,type
                       ,sep = "/"
                       )

        query <- paste0(string,
                        "?",
                        "format=json",
                        "&per_page=1000000")

        raw.data<- fromJSON(query)
        
        data <- raw.data[[2]]
        
        dat <- lapply(data, function(l){
            as.data.table(as.list(unlist(l)))
        })
        
        dat = rbindlist(dat, fill = TRUE)
        
        return(dat)
    }


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Get a list of available variables for a given WB data source
##' @param url URL of World Bank API
##' @param source ID of the data source
##' @param topic Topic ID
##' @return data.table with indicator descriptions   
##' @author Janko Cizel
getWorldBankIndicatorList <- function(url="http://api.worldbank.org"
                                     ,source="20"
                                     ,topic=NULL
                                      )
    {
        if (is.null(topic)){
            string<- paste(url
                           ,"source",source
                           ,"indicators"
                           ,sep = "/"
                           )
        } else {
            string<- paste(url
                           ,"topic",topic
                           ,"indicator"
                           ,sep = "/"
                           )
        }
       


        query <- paste0(string,
                        "?",
                        "format=json",
                        "&per_page=100000")

        raw.data<- fromJSON(query)
        
        data <- raw.data[[2]]
        
        dat <- lapply(data, function(l){
            as.data.table(as.list(unlist(l)))
        })
        
        
        ## dat = data.frame(do.call("rbind", dat))
        dat <- rbindlist(dat, fill = TRUE)
        
        return(dat)
    }

## options(width = 120)
## get.WorldBank.souces()
## get.WorldBank.indicatorlist(source = 23)


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Obtain additional infomration on individual data series 
##' @param url 
##' @param source 
##' @return data.table with query results
##' @author Janko Cizel
getWorldBankSeriesInfo <- function(url="http://api.worldbank.org"
                                  ,source = "20"
                                   )
    {
        ids <- getWorldBankIndicatorList(source = source)[["id"]]

        result.list <- list()
        for (x in ids){
            
            string<- paste(url
                           ,"indicators", x
                           ,sep = "/"
                           )

            query <- paste0(string,
                            "?",
                            "format=json",
                            "&per_page=1000000")

            raw.data<- fromJSON(query)
            
            data <- raw.data[[2]]

            dat <- lapply(data, function(l){
                as.data.table(as.list(unlist(l)))
            })
            
            dat <- rbindlist(dat)
            
            result.list[[x]] <- dat
        }
        
        result <- rbindlist(result.list, fill = TRUE, use.names = TRUE)
        return(result)
    }

## s <- getWorldBankIndicatorList(source = "2")
## s[toupper(sourceNote) %like% 'LABOR'][['name']]



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Request a World Bank Dataset
##' @param url 
##' @param source 
##' @param topic 
##' @param countries 
##' @param frequency 
##' @param MRV 
##' @param format 
##' @param per_page 
##' @return data.table with query result.
##' @author Janko Cizel
getWorldBankDataset <- function(
    url="http://api.worldbank.org"
   ,source = "20"
   ,topic = NULL
   ,countries = "all"
   ,MRV = "100"
   ,format = "json"
   ,per_page = "10000"
){
    if (is.null(topic))
        ids <- getWorldBankIndicatorList(source = source)[["id"]]
    else
        ids <- getWorldBankIndicatorListt(topic = topic)[["id"]]
    
    result.list <-
        foreach (x = ids) %dopar% {
            cat(x,"\n")
            
            string<- paste(url
                          ,"countries", paste(countries,sep=";")
                          ,"indicators", x
                          ,sep = "/"
                           )

            query <- paste0(string,
                            "?",
                            "format=",format,
                            "&per_page=",per_page
                            )

            cat(query,"\n")
            raw.data <- try(fromJSON(query))

            if ("try-error" %in% class(raw.data)){
                warning(x, " failed to load.")
                next
            } else {
                if (raw.data[[1]][["pages"]] <= 1){
                    data <- raw.data[[2]]
                } else {
                    data <- raw.data[[2]]
                    for (y in 2:raw.data[[1]][["pages"]]){
                        info <- raw.data[[1]]
                        paste(str(info))
                        raw.data <- try(fromJSON(paste0(query,"&page=",y)))
                        if ("try-error" %in% class(raw.data)){
                            warning(x, " page ", y , " failed to load.")
                            next
                        } else {
                            data <- c(data,raw.data[[2]])
                        }
                    }
                }
            }            

            dat <- lapply(data, function(l){
                as.data.table(as.list(unlist(l)))
            })

            dat <- rbindlist(dat, fill = TRUE)                        

            dat[!is.na(value)]
        }
    
    result <- rbindlist(result.list, fill = TRUE, use.names = TRUE)
    return(result)
}

## df <- getWorldBankDataset(source = 20)

getAllWorldBankData <- function(datafolder = './inst/extdata'){

    sources <- getWorldBankSources()

    sources[, name := gsub("[[:punct:]]","_",name)]    
    sources[, name := gsub("[[:space:]]",".",name)]
    
    ## CHECK WHETHER THE FILE ALREADY EXISTS
    .fileExists <- function(file,folder){
        return(file %in% list.files(path = folder))
    }

    for (x in sources$id){
        folder <- paste0(datafolder,'/',sources[id==x]$name)
        if (!.fileExists(file = sources[id==x]$name, folder = paste0(datafolder))){
            system(command = paste0('mkdir ',folder))
        }

        flist <- list.files(path = folder, full.names = TRUE)

        if (length(flist) > 0){
            if (sum(sapply(flist, function(x) {
                o <- file.info(x)$size/(2^20)
                if (is.na(o)) o <- 0
                o
            }))>1){
                next
            }
        }
        
        .o <- try(getWorldBankDataset(source = x))

        if ("try-error" %in% class(.o))
            next
        
        write.csv(x = .o,
                  file = paste0(folder,"/DATA.csv"))
    }
    return(NULL)
}

## getAllWorldBankData()

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Reshape the result from WorldBankAPI:::getWorldBankDataset function
##' @param df 
##' @param colVar 
##' @param labelVar 
##' @param valueVar 
##' @return data.table with the reshaped WB dataset
##' @author Janko Cizel
createWorldBankDataset <- function(df,
                                   colVar = 'indicator.id',
                                   labelVar = 'indicator.value',
                                   valueVar = 'value'){

    dt <- copy(df)
    rowVar <- setdiff(names(dt),
                      c(colVar,
                        labelVar,
                        valueVar))
    formula <-
        paste0(
            paste(rowVar, collapse = ' + '),
            " ~ ",
            paste(colVar, collapse = ' + ')
        )

    dt[, val := as.numeric(get(valueVar))]
    dt <- dt[!is.na(val)]
    
    out <- dcast.data.table(data = dt,
                            formula = formula,
                            value.var = 'val')

    attributes(out)$colnames <- local({
        o <- unique(dt[, c(colVar,labelVar), with = FALSE])
        o[,nonmissRate:=0]
        setkeyv(o, colVar)
        
        for (x in o$indicator.id){
            mr <- out[, sum(!is.na(get(x)),na.rm = TRUE)/length(get(x))]
            o[x, nonmissRate := mr]
        }
        
        o
    })

    return(out)
}

## test <- .createDataset(df)
## test
## options(width = 200)
## lookup <- (attributes(test)$colnames)

## options(width = 120)
## test[, DATE := {
##     x <- date
##     year <- gsub("([0-9]{4})Q([0-9]{1})","\\1",x)
##     qtr <- gsub("([0-9]{4})Q([0-9]{1})","\\2",x)
##     as.numeric(year) + as.numeric(qtr)/4
## }]

## require(ggplot2)
## test[country.id == 'SI',
##      qplot(x = DATE,
##            y = DP.DOD.DECF.CR.CG.CD,
##            geom = 'line')]


## data.frame(lookup)
## test[country.id == 'SI',
##      qplot(x = DATE,
##            y = DP.DOD.DECF.CR.CG.CD,
##            geom = 'line',
##            main = .lookupName(query = 'DP.DOD.DECF.CR.CG.CD',lookup.table = lookup,id.var = 'indicator.id', label.var = 'indicator.value'))]


## require(data.table)
## get.WorldBank.indicatorlist(source = 20)
## get.WorldBank.indicatorlist(topic = 20)

## get.WorldBank.indicatorlist(source = 6)
## get.WorldBank.indicatorlist(source = 2)
## get.WorldBank.indicatorlist(source = 22)
## get.WorldBank.indicatorlist(source = 23)

## get.WorldBank.countries()
## get.WorldBank.countries(type = "incomeLevels")
## get.WorldBank.countries(type = "lendingTypes")

## get.WorldBank.countries(type = "topics")



## PATH.DATA <- "~/Documents/Dropbox/Data"


## write.table(get.WorldBank.souces(),
##             file = paste0(PATH.DATA, "/World Bank/wb.sources.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.countries(type = "topics"),
##             file = paste0(PATH.DATA, "/World Bank/wb.topics.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.countries(),
##             file = paste0(PATH.DATA, "/World Bank/wb.countries.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(topic = 20),
##             file = paste0(PATH.DATA, "/World Bank/WB.ExternalDebt.Indicators.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(topic = 3),
##             file = paste0(PATH.DATA, "/World Bank/WB.EconomyAndGrowth.Indicators.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(topic = 13),
##             file = paste0(PATH.DATA, "/World Bank/WB.PublicSector.Indicators.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(topic = 7),
##             file = paste0(PATH.DATA, "/World Bank/financial.sector.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(topic = 12),
##             file = paste0(PATH.DATA, "/World Bank/WB.PrivateSector.Indicators.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(get.WorldBank.indicatorlist(source = 15),
##             file = paste0(PATH.DATA, "/World Bank/WB.GlobalEconomicMonitor.Indicators.csv"),
##             row.names = FALSE,
##             sep = "||")

## write.table(dt <- get.WorldBank.dataset(source = "15"
##                                         ),
##             file = paste0(PATH.DATA, "/World Bank/Source_GlobalEconomicMonitor.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(dt <- get.WorldBank.dataset(topic = "7"),
##             file = paste0(PATH.DATA, "/World Bank/Topic_FinancialSector.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(dt <- get.WorldBank.dataset(topic = "3"),
##             file = paste0(PATH.DATA, "/World Bank/Topic_EconomyAndGrowth.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(dt <- get.WorldBank.dataset(topic = "20"),
##             file = paste0(PATH.DATA, "/World Bank/Topic_ExternalDebt.csv"),
##             row.names = FALSE,
##             sep = ",")

## write.table(dt <- get.WorldBank.dataset(topic = "13"),
##             file = paste0(PATH.DATA, "/World Bank/Topic_PublicSector.csv"),
##             row.names = FALSE,
##             sep = ",")



## dt <- fread(paste0(PATH.DATA, "/World Bank/Topic_FinancialSector.csv"))






## ################################################################################
## ## QPSD                                                                       ##
## ################################################################################
## wb.qpsd <- get.WorldBank.dataset(source = 20)
## wb.qpsd.static <- get.WorldBank.SeriesInfo(source = 20)

## write.table(wb.qpsd,
##             file = paste0(PATH.DATA, "/World Bank/QPSD.csv"),
##             row.names = FALSE,
##             sep = ",")

## wb.qpsd <- fread(paste0(PATH.DATA, "/World Bank/QPSD.csv"))

## wb.qpsd <- wb.qpsd[!is.na(value) & value!=""]

## wb.qpsd[, c("year","month","date") := {
##     d <- str_split(date, "Q")

##     year = as.numeric(sapply(d, function(x) x[[1]]))
##     month = 3 * as.numeric(sapply(d, function(x) x[[2]]))

##     list(year,
##          month,
##          as.Date(paste0(year,"-",month,"-1",format = "%Y-%m-%d")))
## }]



## wb.qpsd.summary <-
##     wb.qpsd[!is.na(value), {
##         dt <- .SD[, list(year, value)]
##         if (dim(dt)[[1]] == 0){
##             list(min.year = 0,
##                  max.year = 0)
##         } else {
##             list(min.year = as.numeric(min(dt$year, na.rm = TRUE)),
##                  max.year = as.numeric(max(dt$year, na.rm = TRUE)))
##         }
##     }
##         , by = list(country.id, indicator.id, indicator.value)]

## wb.qpsd.summary[country.id == "RU"]


## ################################################################################
## ## GET SOURCES                                                                ##
## ################################################################################
## get.WorldBank.souces()

## source.ids <- c(6,22)

## result.list <- list()
## for (x in source.ids){
##     dt <- get.WorldBank.dataset(source = x)
##     ## dt.static <- get.WorldBank.SeriesInfo(source = x)

##     write.table(dt,
##                 file = paste0(PATH.DATA, "/World Bank/SOURCE_",x,".csv"),
##                 row.names = FALSE,
##                 sep = ",")
## }



## -------------------------------------------------------------------------- ##
## TESTS                                                                      ##
## -------------------------------------------------------------------------- ##
## countries <- get.WorldBank.countries()

## get.WorldBank.souces()
## d <- get.WorldBank.dataset(source = 43)
## res <- list()
## res[['DATASET']] <- .createDataset(d)
## res[['VARIABLES']] <- attributes(res[['DATASET']])$colnames



## get.WorldBank.souces()
## df <- get.WorldBank.dataset(source = 20)
## res <- list()
## res[['DATASET']] <- .createDataset(df)
## res[['VARIABLES']] <- attributes(res[['DATASET']])$colnames
## .saveExcel(l = res,
##            file = "/Users/jankocizel/Downloads/test.xlsx")

