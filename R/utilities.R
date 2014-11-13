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
