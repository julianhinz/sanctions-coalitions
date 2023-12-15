###
# 0 - prepare data
# 231201
###

if (!require("pacman")) install.packages("pacman"); library("pacman")
pacman::p_load(data.table)
pacman::p_load(readr)
pacman::p_load(magrittr)
pacman::p_load(purrr)
pacman::p_load(readxl)
pacman::p_load(stringr)
pacman::p_load(bit64)
pacman::p_load(countrycode)
pacman::p_load_gh("julianhinz/KITE")

# 0 - custom functions and definitions ----

## match and replace ----
match_replace <- function (code, from, to, dictionary) {
  dictionary[match(code, dictionary[[from]]),][[to]]
}

## sanctioning countries ----
countries_sanctions_iran = c("Belgium", "Denmark", "France", "Germany",
                             "Greece", "Ireland", "Italy", "Luxembourg",
                             "Netherlands", "Portugal", "Spain", "United Kingdom",
                             "Austria", "Finland", "Sweden", "Czech Republic",
                             "Cyprus", "Estonia", "Hungary", "Latvia",
                             "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia",
                             "Romania", "Bulgaria", "Croatia", "United States",
                             "Canada", "Japan", "Australia", "Korea",
                             "Switzerland", "Norway", "Turkey") %>%
  countrycode("country.name", "iso3c")

countries_sanctions_russia = c("Belgium", "Denmark", "France", "Germany",
                               "Greece", "Ireland", "Italy", "Luxembourg",
                               "the Netherlands", "Portugal", "Spain", "United Kingdom",
                               "Austria", "Finland", "Sweden", "Czech Republic",
                               "Cyprus", "Estonia", "Hungary", "Latvia",
                               "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia",
                               "Romania", "Bulgaria", "Croatia", "United States",
                               "Japan", "Canada", "Australia", "New Zealand",
                               "Norway", "Ukraine", "Georgia", "Albania", "Montenegro") %>%
  countrycode("country.name", "iso3c")

## concordance iso3c GTAP ----
concordance_countries = fread("input/metadata/concordance_iso3_GTAP10.csv")

## energy and services sectors ----
sectors_energy = c("OIL", "GAS")
sectors_services = c("GDT", "ELY", "WTR", "CNS", "TRD", "AFS",
                     "OTP", "WTP", "ATP", "WHS",
                     "CMN", "OFI", "INS", "RSA",
                     "OBS", "ROS", "OSG", "EDU",
                     "HHT", "DWE")

# 1 - prepare gravity data ----

## load data GTAP skeleton ----
data_skeleton = read_rds("input/metadata/skeleton_origin_destination_sector.rds")

data_skeleton[sector %in% sectors_services, sector := "SERVICES"] # aggregate services sectors for regression
data_skeleton = unique(data_skeleton)

data_skeleton[origin == "XCA", origin := "BLZ"]
data_skeleton[destination == "XCA", destination := "BLZ"]
data_skeleton[origin == "XEE", origin := "MDA"]
data_skeleton[destination == "XEE", destination := "MDA"]

data_skeleton = rbind(data_skeleton,
                      data_skeleton[, .(sector = "TOTAL"),
                                    by = .(origin, destination)])

data_skeleton = merge(data.table(X = 1, year = 2000:2019),
                      data_skeleton[, .(X = 1, origin, destination, sector)],
                      allow.cartesian = T,
                      by = "X")
data_skeleton[, X := NULL]


## load gravity data ----
data_gravity = list.files("input/gravity/", full.names = T) %>% 
  map(~ fread(.x)) %>%
  rbindlist()
data_gravity = data_gravity[iso3_o != iso3_d,
                            .(wto = max(member_wto_joint),
                              eu = max(member_eu_joint),
                              cu = max(agree_cu),
                              fta = max(agree_fta)),
                            by = .(year, origin = iso3_o, destination = iso3_d)]
data_gravity = data_gravity[year %in% 2000:2019]

## load general trade data and add total flows ----
data_trade_general = read_rds("input/trade/BACI_HS92_gtap.rds")

# stats on number of countries
data_trade_general[, uniqueN(origin)]
data_trade_general[, uniqueN(destination)]

data_trade_general = data_trade_general[!sector %in% sectors_energy] # remove energy sectors
data_trade_general = data_trade_general[!sector %in% sectors_services] # remove services sectors

# rename some regions
data_trade_general[origin == "XCA", origin := "BLZ"]
data_trade_general[destination == "XCA", destination := "BLZ"]
data_trade_general[origin == "XEE", origin := "MDA"]
data_trade_general[destination == "XEE", destination := "MDA"]

# subsect to years and create total
data_trade_general = data_trade_general[year %in% 2000:2019]
data_trade_general = rbind(data_trade_general,
                           data_trade_general[, .(value = sum(value),
                                                  sector = "TOTAL"),
                                              by = .(year, origin, destination)])
data_trade_general[, value := as.numeric(value)]
data_trade_general[, sector := toupper(sector)]
data_trade_general[, year := as.integer(year)]


## load energy trade data ----
data_trade_oil_imports = rbind(fread("input/oil_gas_services/oil_imports_2000_2010.csvy.gz"),
                               fread("input/oil_gas_services/oil_imports_2011_2020.csvy.gz"))

# stats on number of countries
data_trade_oil_imports[, uniqueN(COUNTRY)]
data_trade_oil_imports[, unique(Product)]

setnames(data_trade_oil_imports, c("COUNTRY", "Country"), c("DESTINATION", "Destination"))
data_trade_oil_exports = rbind(fread("input/oil_gas_services/oil_exports_2000_2010.csvy.gz"),
                               fread("input/oil_gas_services/oil_exports_2011_2020.csvy.gz"))

# stats on number of countries
data_trade_oil_exports[, uniqueN(COUNTRY)]
data_trade_oil_imports[, unique(Product)]

setnames(data_trade_oil_exports, c("COUNTRY", "Country"), c("ORIGIN", "Origin"))
data_trade_oil = rbind(data_trade_oil_imports,
                       data_trade_oil_exports)
rm(data_trade_oil_imports,
   data_trade_oil_exports)

data_trade_oil = data_trade_oil[TIME %in% c(2000:2019)]
data_trade_oil = data_trade_oil[Product == "Crude oil"]

data_trade_oil = data_trade_oil[, .(value = mean(Value),
                                    sector = "OIL"),
                                by = .(year = Time,
                                       origin = ORIGIN,
                                       destination = DESTINATION)]
data_trade_oil = data_trade_oil[!is.na(origin) & !is.na(destination) &
                                  origin != destination &
                                  origin %in% data_skeleton[, unique(origin)] &
                                  destination %in% data_skeleton[, unique(origin)]]
fwrite(data_trade_oil, "temp/data/data_trade_oil.csv.gz", compress = "gzip")

data_trade_gas_imports = fread("input/oil_gas_services/gas_imports_2000_2021.csvy.gz")
setnames(data_trade_gas_imports, c("COUNTRY", "Country"), c("DESTINATION", "Destination"))
data_trade_gas_exports = fread("input/oil_gas_services/gas_exports_2000_2021.csvy.gz")
setnames(data_trade_gas_exports, c("COUNTRY", "Country"), c("ORIGIN", "Origin"))
data_trade_gas = rbind(data_trade_gas_imports,
                       data_trade_gas_exports)
rm(data_trade_gas_imports,
   data_trade_gas_exports)

data_trade_gas = data_trade_gas[TIME %in% c(2000:2019)]
data_trade_gas = data_trade_gas[, .(sector = "GAS",
                                    value = mean(Value)),
                                by = .(year = Time,
                                       origin = ORIGIN,
                                       destination = DESTINATION)]
data_trade_gas = data_trade_gas[!is.na(origin) & !is.na(destination) &
                                  origin != destination &
                                  origin %in% data_skeleton[, unique(origin)] &
                                  destination %in% data_skeleton[, unique(origin)]]
fwrite(data_trade_gas, "temp/data/data_trade_gas.csv.gz", compress = "gzip")


## load services trade data ----
data_trade_services_imports = fread("input/oil_gas_services/services_imports_2000-2019.csv.gz")
data_trade_services_exports = fread("input/oil_gas_services/services_exports_2000-2019.csv.gz")

data_trade_services = rbind(data_trade_services_imports,
                            data_trade_services_exports)
rm(data_trade_services_imports,
   data_trade_services_exports)

# stats on number of countries
data_trade_services[, uniqueN(Country)]
data_trade_services[, uniqueN(Partner)]

data_trade_services = data_trade_services[Year %in% c(2000:2019)]
data_trade_services[EXP == "IMP", origin := PAR]
data_trade_services[EXP == "IMP", destination := COU]
data_trade_services[EXP == "EXP", origin := COU]
data_trade_services[EXP == "EXP", destination := PAR]
data_trade_services = data_trade_services[, .(value = mean(Value),
                                              sector = "SERVICES"),
                                          by = .(year = Year,
                                                 origin,
                                                 destination)]
data_trade_services = data_trade_services[!is.na(origin) & !is.na(destination) &
                                            origin != destination &
                                            origin %in% data_skeleton[, unique(origin)] &
                                            destination %in% data_skeleton[, unique(origin)]]
data_trade_services[value < 0, value := 0]
fwrite(data_trade_services, "temp/data/data_trade_services.csv.gz", compress = "gzip")

## combine all trade data ----
data_trade = rbind(data_trade_general,
                   data_trade_oil,
                   data_trade_gas,
                   data_trade_services)
rm(data_trade_general,
   data_trade_oil,
   data_trade_gas,
   data_trade_services)
gc()


## create gravity dataset ----

# merge to add zeroes to trade data 
data = merge(data_skeleton,
             data_trade,
             by = c("year", "origin", "destination", "sector"),
             all.x = T)
data[!sector %in% c("ENERGY", "SERVICES") & is.na(value), value := 0]
rm(data_skeleton, data_trade)

## merge with gravity variables 
data = merge(data,
             data_gravity,
             by = c("year", "origin", "destination"),
             all.x = T)
rm(data_gravity)

# remove non-traded sectors
data = data[sector %in% data[, .(value = sum(value, na.rm = T)), by = sector][value > 0, sector]]

## create sanctions dummies ----
data[, sanctions_russia := as.integer(origin %in% countries_sanctions_russia &
                                        destination == "RUS" &
                                        year >= 2014)]
data[, sanctions_russia_rev := as.integer(origin == "RUS" &
                                            destination %in% countries_sanctions_russia &
                                            year >= 2014)]

data[, sanctions_iran := as.integer(origin %in% countries_sanctions_iran &
                                      destination == "IRN" &
                                      year >= 2012)]
data[, sanctions_iran_rev := as.integer(origin == "IRN" &
                                          destination %in% countries_sanctions_iran &
                                          year >= 2012)]

## fill zeros ----
setnafill(data, type = "const", fill = 0, cols = c("wto", "eu", "cu", "fta"))

## remove intra flows, statistical artifact
data = data[origin != destination]


## write to disk ----
fwrite(data, "temp/data/sanctions_gravity_dataset.csvy.gz", compress = "gzip", yaml = T)


# 2 - prepare initial conditions ----
initial_conditions = read_rds("input/kite_initial_conditions/initial_conditions_gtap10_2011.rds")
initial_conditions$expenditure = NULL

# remove puerto rico, Taiwan and rest of the world as they are faulty in GTAP 10
initial_conditions$consumption_share = initial_conditions$consumption_share[!country %in% c("PRI", "TWN", "XTW")]
initial_conditions$factor_share = initial_conditions$factor_share[!country %in% c("PRI", "TWN", "XTW")]
initial_conditions$intermediate_share = initial_conditions$intermediate_share[!country %in% c("PRI", "TWN", "XTW")]
initial_conditions$trade_share = initial_conditions$trade_share[!destination %in% c("PRI", "TWN", "XTW")]
initial_conditions$trade_share = initial_conditions$trade_share[!origin %in% c("PRI", "TWN", "XTW")]
initial_conditions$trade_share[, value := value / sum(value), by = .(destination, sector)]
initial_conditions$value_added = initial_conditions$value_added[!country %in% c("PRI", "TWN", "XTW")]
initial_conditions$export_subsidy = initial_conditions$export_subsidy[!destination %in% c("PRI", "TWN", "XTW")]
initial_conditions$export_subsidy = initial_conditions$export_subsidy[!origin %in% c("PRI", "TWN", "XTW")]
initial_conditions$tariff = initial_conditions$tariff[!destination %in% c("PRI", "TWN", "XTW")]
initial_conditions$tariff = initial_conditions$tariff[!origin %in% c("PRI", "TWN", "XTW")]
initial_conditions$trade_balance = initial_conditions$trade_balance[!country %in% c("PRI", "TWN", "XTW")]
initial_conditions$trade_balance[, value := value - mean(value)]


## Iran ----
initial_conditions_iran = copy(initial_conditions)

### update external trade shares ----
trade_shares_iran = data[year == 2011 &
                           !sector %in% c("ENERGY", "SERVICES", "TOTAL") &
                           !origin %in% c("PRI", "TWN", "XTW") &
                           !destination %in% c("PRI", "TWN", "XTW"), .(origin,
                                                                       value_baci = value / sum(value, na.rm = T)),
                         by = .(destination, sector = str_to_lower(sector))]

trade_shares_iran[is.na(value_baci), value_baci := 0]
trade_shares_iran[origin == "BLZ", origin := "XCA"]
trade_shares_iran[destination == "BLZ", destination := "XCA"]
trade_shares_iran[origin == "MDA", origin := "XEE"]
trade_shares_iran[destination == "MDA", destination := "XEE"]

trade_shares_iran = merge(initial_conditions_iran$trade_share,
                          trade_shares_iran,
                          by = c("origin", "destination", "sector"),
                          all = T)

trade_shares_iran[, baci := sector %in%
                    data[!sector %in% c(sectors_energy, sectors_services, "SERVICES"), str_to_lower(unique(sector))]]
trade_shares_iran[, internal := origin == destination]
trade_shares_iran[, share_external_goods := sum(value[internal == F & baci == T]), by = .(destination, sector)]
trade_shares_iran[internal == T | baci == F, value_new := value]
trade_shares_iran[internal == F & baci == T, value_new := value_baci * share_external_goods]

trade_shares_iran = trade_shares_iran[, .(origin, destination, sector, value = value_new)]
trade_shares_iran[is.na(value), value := 0]
trade_shares_iran[, total := sum(value), by = .(destination, sector)]
trade_shares_iran[, value := value / total]
trade_shares_iran[, total := NULL]

setorder(trade_shares_iran, origin, destination, sector)
initial_conditions_iran$trade_share = trade_shares_iran
rm(trade_shares_iran)


### compute baseline ----

# unbalanced
baseline_iran_unbalanced = update_equilibrium(initial_conditions = initial_conditions_iran,
                                              model = compute_initial_conditions_cp_2015,
                                              settings = list(verbose = 2L,
                                                              tolerance = 1e-4))
initial_conditions_iran_unbalanced = baseline_iran_unbalanced$output

write_rds(initial_conditions_iran_unbalanced,
          "temp/initial_conditions/initial_conditions_iran_unbalanced.rds", compress = "gz")
rm(baseline_iran_unbalanced)

# balanced
baseline_iran_balanced = update_equilibrium(initial_conditions = initial_conditions_iran_unbalanced,
                                            model_scenario = list(trade_balance = initial_conditions_iran_unbalanced$trade_balance[, .(country, value = 0)]),
                                            model = caliendo_parro_2015_cpp,
                                            settings = list(verbose = F,
                                                            tolerance = 1e-8))
initial_conditions_iran_balanced = copy(initial_conditions_iran_unbalanced)
initial_conditions_iran_balanced$trade_share = baseline_iran_balanced$output$trade_share_new
initial_conditions_iran_balanced$trade_balance = initial_conditions_iran_balanced$trade_balance[, .(country, value = 0)]
initial_conditions_iran_balanced$value_added$value = baseline_iran_balanced$output$wage_change$value * initial_conditions_iran_balanced$value_added$value
initial_conditions_iran_balanced$expenditure = baseline_iran_balanced$output$expenditure_new

write_rds(initial_conditions_iran_balanced,
          "temp/initial_conditions/initial_conditions_iran_balanced.rds", compress = "gz")

rm(baseline_iran_balanced,
   initial_conditions_iran_unbalanced,
   initial_conditions_iran_balanced,
   initial_conditions_iran)
gc()


## Russia ----
initial_conditions_russia = copy(initial_conditions)

### update external trade shares ----
trade_shares_russia = data[year == 2013 &
                             !sector %in% c("ENERGY", "SERVICES", "TOTAL") &
                             !origin %in% c("PRI", "TWN", "XTW") &
                             !destination %in% c("PRI", "TWN", "XTW"), .(origin,
                                                                         value_baci = value / sum(value, na.rm = T)),
                           by = .(destination, sector = str_to_lower(sector))]

trade_shares_russia[is.na(value_baci), value_baci := 0]
trade_shares_russia[origin == "BLZ", origin := "XCA"]
trade_shares_russia[destination == "BLZ", destination := "XCA"]
trade_shares_russia[origin == "MDA", origin := "XEE"]
trade_shares_russia[destination == "MDA", destination := "XEE"]

trade_shares_russia = merge(initial_conditions_russia$trade_share,
                            trade_shares_russia,
                          by = c("origin", "destination", "sector"),
                          all = T)

trade_shares_russia[, baci := sector %in%
                    data[!sector %in% c(sectors_energy, sectors_services, "SERVICES"), str_to_lower(unique(sector))]]
trade_shares_russia[, internal := origin == destination]
trade_shares_russia[, share_external_goods := sum(value[internal == F & baci == T]), by = .(destination, sector)]
trade_shares_russia[internal == T | baci == F, value_new := value]
trade_shares_russia[internal == F & baci == T, value_new := value_baci * share_external_goods]

trade_shares_russia = trade_shares_russia[, .(origin, destination, sector, value = value_new)]
trade_shares_russia[is.na(value), value := 0]
trade_shares_russia[, total := sum(value), by = .(destination, sector)]
trade_shares_russia[, value := value / total]
trade_shares_russia[, total := NULL]
setorder(trade_shares_russia, origin, destination, sector)
initial_conditions_russia$trade_share = trade_shares_russia
rm(trade_shares_russia)

### update value added ----
value_added_growth = fread("input/gdp_growth/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_4492338.csv", skip = 3, header = T)
value_added_growth[, c("Country Name", "Indicator Name", "Indicator Code") := NULL]
setnames(value_added_growth, "Country Code", "country")
value_added_growth = melt(value_added_growth, id.vars = "country", variable.name = "year")
value_added_growth = value_added_growth[!is.na(value) & year %in% c(2012:2013)]
value_added_growth[, country := match_replace(country, from = "comtrade_code", to = "gtap_code",
                                              dictionary = concordance_countries), by = country]
value_added_growth = value_added_growth[!is.na(country)]
value_added_growth = value_added_growth[, .(value = mean(value)), by = .(year, country)]
value_added_growth[, value := value / 100 + 1]
value_added_growth = value_added_growth[, .(value = prod(value)), by = .(country)]
value_added_growth[country == "XCA", country := "BLZ"]
value_added_growth[country == "XEE", country := "MDA"]
value_added_growth = value_added_growth[!country %in% c("PRI", "TWN", "XTW")]
setorder(value_added_growth, country)
initial_conditions_russia$value_added$value = value_added_growth$value * initial_conditions_russia$value_added$value
rm(value_added_growth)

### compute baseline ----

# unbalanced
baseline_russia_unbalanced = update_equilibrium(initial_conditions = initial_conditions_russia,
                                                model = compute_initial_conditions_cp_2015,
                                                settings = list(verbose = 2L,
                                                                tolerance = 1e-4))
initial_conditions_russia_unbalanced = baseline_russia_unbalanced$output

write_rds(initial_conditions_russia_unbalanced,
          "temp/initial_conditions/initial_conditions_russia_unbalanced.rds", compress = "gz")
rm(baseline_russia_unbalanced)

# balanced
baseline_russia_balanced = update_equilibrium(initial_conditions = initial_conditions_russia_unbalanced,
                                              model_scenario = list(trade_balance = initial_conditions_russia_unbalanced$trade_balance[, .(country, value = 0)]),
                                              model = caliendo_parro_2015_cpp,
                                              settings = list(verbose = F,
                                                              tolerance = 1e-8))
initial_conditions_russia_balanced = copy(initial_conditions_russia_unbalanced)
initial_conditions_russia_balanced$trade_share = baseline_russia_balanced$output$trade_share_new
initial_conditions_russia_balanced$trade_balance = initial_conditions_russia_balanced$trade_balance[, .(country, value = 0)]
initial_conditions_russia_balanced$value_added$value = baseline_russia_balanced$output$wage_change$value * initial_conditions_russia_balanced$value_added$value
initial_conditions_russia_balanced$expenditure = baseline_russia_balanced$output$expenditure_new

write_rds(initial_conditions_russia_balanced,
          "temp/initial_conditions/initial_conditions_russia_balanced.rds", compress = "gz")

rm(baseline_russia_balanced,
   initial_conditions_russia_unbalanced,
   initial_conditions_russia_balanced,
   initial_conditions_russia,
   initial_conditions)
gc()


# EOF ----
rm(list = ls())
