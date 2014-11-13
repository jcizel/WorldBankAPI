load_all()

sources <- WorldBankAPI:::getWorldBankSources()

ctry <- WorldBankAPI:::getWorldBankCountries()

WorldBankAPI:::getWorldBankDataset(source = '11')
