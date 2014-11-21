load_all()

sources <- WorldBankAPI:::getWorldBankSources()

ctry <- WorldBankAPI:::getWorldBankCountries()

WorldBankAPI:::getWorldBankDataset(source = '11')


## CREATE A TABLE WITH ALL AVAILABLE VARIABLES

require(foreach)
require(doParallel)
listAllFun('doParallel')
listAllFun('parallel')

cl <- parallel:::makeCluster(8)
doParallel:::registerDoParallel(cl)


sources <- getWorldBankSources()[,list(id,name)]

out <-
    foreach(x = sources$id,
            .errorhandling = 'pass') %dopar% {
        o <- WorldBankAPI:::getWorldBankIndicatorList(source = x)
        out <- o[, list(id, name, source.id, source.value, sourceNote)]
    }


out <- Filter(function(d) ('data.table' %in% class(d)), out)
lookup <- rbindlist(out)

lookup[toupper(name) %like% toupper('debt')][,list(source.id,id,name)]





