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


            ## tryCatch()
            
            if ("try-error" %in% class(raw.data)){
                warning(x, " failed to load.")
                return(NULL)
            } else {
                if (raw.data[[1]][["pages"]] <= 1){
                    data <- raw.data[[2]]
                } else {
                    data <- raw.data[[2]]
                    for (y in 2:raw.data[[1]][["pages"]]){
                        info <- raw.data[[1]]
                        ## paste(str(info))
                        raw.data <- try(fromJSON(paste0(query,"&page=",y)))
                        if ("try-error" %in% class(raw.data)){
                            warning(x, " page ", y , " failed to load.")
                            next()
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

            return(dat[!is.na(value)])
        }

    result.list <- Filter(function(x) !is.null(x), result.list)
    
    result <- rbindlist(result.list, fill = TRUE, use.names = TRUE)
    return(result)
}

## df <- getWorldBankDataset(source = 20)

getAllWorldBankData <- function(datafolder = './inst/extdata',
                                sourceids  = c('2','30','15','6','20','23','22','43','3')){

    sources <- getWorldBankSources()

    if (!is.null(sourceids))
        sources <- sources[id %in% sourceids]

    sources[, name := gsub("[[:punct:]]","_",name)]    
    sources[, name := gsub("[[:space:]]",".",name)]
    
    ## CHECK WHETHER THE FILE ALREADY EXISTS
    .fileExists <- function(file,folder){
        return(file %in% list.files(path = folder))
    }

    for (x in sources$id){
        cat("Processing:",sources[id==x]$name,"\n\n")
        
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

    lookup <- local({
        o <- unique(dt[, c(colVar,labelVar), with = FALSE])
        o[,nonmissRate:=0]
        setkeyv(o, colVar)
        
        for (x in o$indicator.id){
            mr <- out[, sum(!is.na(get(x)),na.rm = TRUE)/length(get(x))]
            o[x, nonmissRate := mr]
        }
        
        o
    })

    out[, iso3 := .lookupISOCode(country.id)]

    out <- 
        structure(out,
                  lookup = lookup)

    return(out)
}

lookup <- function(dt){
    o <- attributes(dt)$lookup[order(nonmissRate, decreasing = TRUE)]
    ## o[, paste(strwrap(indicator.value,width = 20, simplify = FALSE), collapse = "\\n")]
    return(o)
}


## l <- queryWorldBankVariableList("revenue")
## undebug(getWorldBankDataSeries)
## dt <- getWorldBankDataSeries(indicators = l$id)
## out <- createWorldBankDataset(dt)
## lookup(out)
