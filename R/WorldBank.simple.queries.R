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

    out <- v[ (toupper(name) %like% toupper(pattern))
             |(toupper(sourceNote) %like% toupper(pattern))][,list(source.id,id,name)]

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
        
        dat
    }
    
    .r <- rbindlist(result.list, fill = TRUE, use.names = TRUE)

    ## POST-PROCESSING
    .r[, value := as.numeric(value)]
    .r[, date := as.Date(paste0(date,"-12-31"),"%Y-%m-%d")]

    result <- .r[!is.na(value)]
    
    return(unique(result))
}


## queryWorldBankVariableList("debt")
## queryWorldBankVariableList("gdp")
## queryWorldBankVariableList("revenue")
l <- queryWorldBankVariableList(".")
dt <- getWorldBankDataSeries(indicators = l)


