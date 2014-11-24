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
o[,table(decimal)]
o[country.id == 'NL']


indicators <- queryWorldBankVariableList('public debt')
o <- getWorldBankDataSeries(indicators = indicators[,id])

o[,table(country.value)]
o[,table(date)]
o[,table(decimal)]
o[country.id == 'US']
o[indicator.id == 'GFDD.DM.04', table(country.value)]
o[indicator.id == 'GFDD.DM.04' & country.id == "AR"]
o[indicator.id == 'GFDD.DM.04' & country.id == "IT"]

o[indicator.id == 'GFDD.DM.06' & country.id == "DE"]
o[indicator.id == 'GFDD.DM.06' & country.id == "US"]
o[indicator.id == 'GFDD.DM.06' & country.id == "SI"]
o[indicator.id == 'GFDD.DM.06' & country.id == "IT"]

indicators <- queryWorldBankVariableList('interest payment')
indicators[['name']]
o <- getWorldBankDataSeries(indicators = indicators[,id])
o[,table(country.value)]
o[,table(date)]
o[,table(decimal)]
o[country.id == 'US']
o[indicator.id == 'GC.XPN.INTP.RV.ZS', table(country.value)]
o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'SI']
o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'AR']
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'IT'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'ES'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'GR'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'CN'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'IE'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'PT'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'SI'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'BR'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'FR'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'DE'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'CH'])
unique(o[indicator.id == 'GC.XPN.INTP.RV.ZS' & country.id == 'RU'])


indicators <- queryWorldBankVariableList('government exp')
indicators[['name']]
o2 <- getWorldBankDataSeries(indicators = indicators[,id])
o2[,table(country.value)]
o2[,table(date)]
o2[,table(decimal)]
o2[country.id == 'SI']
o2[indicator.id == 'GC.XPN.INTP.RV.ZS', table(country.value)]
unique(o2[indicator.id == 'SH.XPD.PUBL.GX.ZS' & country.id == 'SI'])


indicators <- queryWorldBankVariableList('default')
indicators
o2 <- getWorldBankDataSeries(indicators = indicators[,id])
o2[country.id == 'AR']

indicators <- queryWorldBankVariableList('consumption')
indicators

indicators <- queryWorldBankVariableList('use of imf')
