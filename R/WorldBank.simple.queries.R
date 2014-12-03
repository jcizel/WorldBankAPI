getWorldBankListOfVariables <- function(outfile = "./inst/extdata/WorldBankListOfVariables.csv"){
    sources <- getWorldBankSources()[,list(id,name)]

    .o <-
        foreach(x = sources$id,
                .errorhandling = 'pass') %dopar% {
            o <- WorldBankAPI:::getWorldBankIndicatorList(source = x)
            out <- o[, list(id, name, source.id, source.value, sourceNote)]
        }

    .o2 <- Filter(function(d) ('data.table' %in% class(d)), .o)
    out <- rbindlist(.o2)

    if (!is.null(outfile)) write.csv(out, file = outfile)
    
    return(out)
}

queryWorldBankVariableList <- function(pattern = "") {
    .files <- list.files('./inst/extdata')
    if ('WorldBankListOfVariables.csv' %in% .files){
        v <- fread(input = './inst/extdata/WorldBankListOfVariables.csv')
    } else {
        v <- WorldBankAPI:::getWorldBankListOfVariables()
    }

    out <- v[ (toupper(id) %like% toupper(pattern))
             |(toupper(name) %like% toupper(pattern))
             |(toupper(sourceNote) %like% toupper(pattern))][,list(source.id,id,name)]

    return(out)
}

## indicators <- c("IQ.CPA.BREG.XQ", "IQ.CPA.DEBT.XQ", "IQ.CPA.ECON.XQ", "IQ.CPA.ENVR.XQ", 
## "IQ.CPA.FINQ.XQ", "IQ.CPA.FINS.XQ", "IQ.CPA.FISP.XQ", "IQ.CPA.GNDR.XQ", 
## "IQ.CPA.HRES.XQ", "IQ.CPA.IRAI.XQ")

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param cache.dir 
##' @param indicators 
##' @return 
##' @author Janko Cizel
checkWorldBankCache <- function(
    cache.dir = '../WorldBankAPI/inst/extdata',
    indicators)
{
    .f <- list.files(paste0(cache.dir),
                     all.files = TRUE,
                     full.names = FALSE,
                     recursive = TRUE
                     )
    if ('WorldBank-TS.csv' %in% .f){
        .cache <- fread(input = paste0(cache.dir,'/WorldBank-TS.csv'),row.names = FALSE)
    } else {
        .cache <- getWorldBankDatasetFromExistingCSV()
    }

    out <- .cache[indicator.id %in% indicators][,-c(1,2), with = FALSE]

    out[, value := as.numeric(value)]
    out[, date := GeneralUtilities::convertToDate(date)]

    return(out)
}

getWorldBankDataSeries <- function(
    url="http://api.worldbank.org"
   ,indicators = c('')
   ,countries = "all"
   ,MRV = "100"
   ,format = "json"
   ,per_page = "10000"
){
    result.list <- 
        foreach(x = indicators,
                .errorhandling = 'stop') %dopar%
    {
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
            return(NULL)
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
        
        dat
    }
    
    .r <- rbindlist(result.list, fill = TRUE, use.names = TRUE)

    ## POST-PROCESSING
    .r[, value := as.numeric(value)]
    .r[, date := GeneralUtilities::convertToDate(date)]

    result <- .r[!is.na(value)]
    
    return(unique(result))
}

getQuickWorldBankDataset <- function(pattern){
    ids <- queryWorldBankVariableList(pattern = pattern)[['id']]
    cat("Downloading the following set of indicators: \n")
    cat(ids,'\n')
    dt <- getWorldBankDataSeries(indicators = ids)
    out <- createWorldBankDataset(dt)

    out <- structure(out,
                     searchquery = pattern)
    return(out)
}

## queryWorldBankVariableList("debt")
## queryWorldBankVariableList("gdp")
## queryWorldBankVariableList("revenue")
## l <- queryWorldBankVariableList(".")
## dt <- getWorldBankDataSeries(indicators = l)
## dt <- getQuickWorldBankDataset('revenue')
## lookup(dt)

## dt2 <- getQuickWorldBankDataset('inflation')
## lookup(dt2)
## dt2[iso3 =='USA']


## dt3 <- getQuickWorldBankDataset('gdp growth')
## lookup(dt3)
## dt3[iso3 =='USA']

## dt4 <- getQuickWorldBankDataset('dummy')
## lookup(dt3)
## dt4[iso3 =='NLD']
## dt4 <- getQuickWorldBankDataset('systemic')
## lookup(dt4)
## dt4 <- getQuickWorldBankDataset('imf credit')
## lookup(dt4)

## dt4[, table(iso3)]

## require(pipeR)
## queryWorldBankVariableList("government") %>>% data.frame
## queryWorldBankVariableList("regulat") %>>% data.frame
## queryWorldBankVariableList("sme ") %>>% data.frame



