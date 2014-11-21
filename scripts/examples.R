load_all()

cl <- parallel:::makeCluster(8)
doParallel:::registerDoParallel(cl)


sources <- WorldBankAPI:::getWorldBankSources()

ctry <- WorldBankAPI:::getWorldBankCountries()

WorldBankAPI:::getWorldBankDataset(source = '11')


## CREATE A TABLE WITH ALL AVAILABLE VARIABLES


indicators <- queryWorldBankVariableList('gross domestic product')
o <- getWorldBankDataSeries(indicators = indicators[,id])

o[,table(country.value)]
o[,table(date)]





