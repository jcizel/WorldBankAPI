.lookupISOCode <- function (
    query,
    queryType = NULL
) 
{
    .files <- list.files("./inst/extdata")
    if ("countries.csv" %in% .files) {
        ctry <- fread(input = "./inst/extdata/countries.csv")
    }
    else {
        ctry <- WorldBankAPI:::getWorldBankCountries()
    }
    isotype <- max(nchar(query))
    if (is.null(queryType)) {
        if (isotype == 2) {
            setkeyv(ctry, "iso2Code")
            out <- ctry[J(query)][["id"]]
        }
        else {
            setkeyv(ctry, "id")
            out <- ctry[J(query)][["iso2Code"]]
        }
    }
    else if (queryType == "name") {
        ctry[, `:=`(country, toupper(name))]
        setkeyv(ctry, "country")
        out <- ctry[J(toupper(query))][["id"]]
    }
    else if (queryType == "iso2") {
        setkeyv(ctry, "iso2Code")
        out <- ctry[J(query)][["id"]]
    }
    else if (queryType == "iso3") {
        setkeyv(ctry, "id")
        out <- ctry[J(query)][["iso2Code"]]
    }
    return(out)
}

.saveExcel <- function(l,
                       file){
    require(xlsx)
    options(java.parameters = "-Xmx8000m")

    names(l) <- gsub("[[:punct:]]+","_",names(l))
    
    for (x in names(l)){
        ## IF NUMBER OF ROWS EXCEEDS 1000000, EXCEL SHEET CANNOT BE WRITTEN
        if (nrow(l[[x]]) > 1e6){
            write.csv(x = l[[x]],
                      file = paste0(file,".",x,".csv"))
        } else {
            write.xlsx2(x = l[[x]],
                        file = file,
                        sheetName = x,
                        append = TRUE)
        }
    }
}

.lookupName <- function(query,
                        lookup.table,
                        id.var = 'REF_AREA',
                        label.var = 'LABEL',
                        label.len = 20){
    setkeyv(lookup.table, id.var)
    out <- paste(lookup.table[query, get(label.var)][[1]],
                 collapse = ';')

    out.m <- out
    
    return(out.m)
}
