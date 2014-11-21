getWorldBankListOfVariables <- function(cores = 4,
                                        outfile = "./inst/extdata/WorldBankListOfVariables.csv"){
    cl <- parallel:::makeCluster(cores)
    doParallel:::registerDoParallel(cl)

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

    out <- v[toupper(sourceNote) %like% toupper(pattern)][,list(source.id,id,name)]

    return(out)
}

## queryWorldBankVariableList("debt")
## queryWorldBankVariableList("gdp")
## queryWorldBankVariableList("revenue")



