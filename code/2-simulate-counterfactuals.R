###
# 2 - simulate counterfactuals
# 240201
###

if (!require("pacman")) install.packages("pacman"); library("pacman")
pacman::p_load(data.table)
pacman::p_load(readr)
pacman::p_load(haven)
pacman::p_load(magrittr)
pacman::p_load(purrr)
pacman::p_load(readxl)
pacman::p_load(stringr)
pacman::p_load(bit64)
pacman::p_load(reshape2)
pacman::p_load(countrycode)
pacman::p_load(foreach)
pacman::p_load(doParallel)
pacman::p_load_gh("julianhinz/KITE")


# custom functions and definitions ----

source("code/functions/compute_welfare_change.R")

## initialize results files ----
initialize_results_files = function (file) {
  if (!file.exists(file)) {
    fwrite(data.table(country = character(),
                      i = integer(),
                      scenario = character(),
                      variable = character(),
                      value = numeric()),
           file)
  }
}

## match and replace ----
match_replace <- function (code, from, to, dictionary) {
  dictionary[match(code, dictionary[[from]]),][[to]]
}

## extract variables from equilibrium new ----
extract_variables = function (results, scenario, i, variable = "welfare_change") {
  results[, .(country,
              i = i,
              scenario = scenario,
              variable = variable,
              value)]
}

extract_transfer = function (results) {
  data.table(country = names(results$output$transfer),
             value = results$output$transfer)
}


## country definitions ----
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

countries_EU = c("Belgium", "Denmark", "France", "Germany",
                 "Greece", "Ireland", "Italy", "Luxembourg",
                 "the Netherlands", "Portugal", "Spain", "United Kingdom",
                 "Austria", "Finland", "Sweden", "Czech Republic",
                 "Cyprus", "Estonia", "Hungary", "Latvia",
                 "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia",
                 "Romania", "Bulgaria", "Croatia") %>%
  countrycode("country.name", "iso3c")

# concordance ISO3 to GTAP10
concordance_countries = fread("input/metadata/concordance_iso3_GTAP10.csv")


## energy and services sectors ----
sectors_energy = c("OIL", "GAS")
sectors_services = c("GDT", "ELY", "WTR", "CNS", "TRD", "AFS",
                     "OTP", "WTP", "ATP", "WHS",
                     "CMN", "OFI", "INS", "RSA",
                     "OBS", "ROS", "OSG", "EDU",
                     "HHT", "DWE")

# cluster parallelization ----
n_cores = parallel::detectCores()

# create the cluster
sanctions_cluster <- parallel::makeCluster(
  n_cores - 1,
  type = ifelse(Sys.info()["sysname"] == "Windows", "PSOCK", "FORK")
)
# print(sanctions_cluster)

# register cluster to be used by %dopar%
doParallel::registerDoParallel(cl = sanctions_cluster)
# foreach::getDoParRegistered()
# foreach::getDoParWorkers()


# 1 - load initial conditions ----
type = "balanced"
# type = "unbalanced"
allow_positive_shock = F

if (type == "balanced") {
  initial_conditions_iran = read_rds("temp/initial_conditions/initial_conditions_iran_balanced.rds")
  initial_conditions_russia = read_rds("temp/initial_conditions/initial_conditions_russia_balanced.rds")
}
if (type == "unbalanced") {
  initial_conditions_iran = read_rds("temp/initial_conditions/initial_conditions_iran_unbalanced.rds")
  initial_conditions_russia = read_rds("temp/initial_conditions/initial_conditions_russia_unbalanced.rds")
}

# read elasticities from Fontagné et al. https://sites.google.com/view/product-level-trade-elasticity)
elasticity = read_dta("input/elasticities/final_tariff_GTAP_2020_10_08.dta") %>% setDT
elasticity = elasticity[, .(sector = gtap_sector, value = -1/epsilon_GTAP)] # inverse of epsilon
elasticity = merge(initial_conditions_iran$trade_elasticity[, .(sector, value_gtap = value)],
                   elasticity,
                   by = "sector",
                   all.x = T)
elasticity[is.na(value), value := value_gtap]
elasticity[, value_gtap := NULL]

initial_conditions_iran$trade_elasticity = elasticity
initial_conditions_russia$trade_elasticity = elasticity


# 2 - simulations for point estimates ----

# load coefficients
coefs = read_rds("temp/regressions/regressions_sectors_bb.rds")
coefs = coefs %>% map(~.x$coefs %>% t() %>% data.table) %>%
  rbindlist(idcol = "sector", fill = T) %>%
  melt.data.table(id.var = "sector")
coefs[, sector := str_to_lower(sector)]
coefs = coefs[!variable %in% c("wto", "cu", "fta")]

# prepare sanctions shock for services, oil, gas and traded goods, and other other stuff
shock_services = merge(elasticity[sector %in% str_to_lower(sectors_services),
                                  .(sector, value_elasticity = value, X = 1)],
                       coefs[sector == "services",
                             .(variable, value_coef = value, X = 1)],
                       by = "X",
                       allow.cartesian = T)
shock_services = shock_services[, .(sector, variable, value_coef, value_elasticity)]

shock_sector = merge(elasticity[!sector %in% str_to_lower(sectors_services),
                                .(sector, value_elasticity = value)],
                     coefs[sector != "services",
                           .(variable, sector, value_coef = value)],
                     by = "sector")
shock_sector = shock_sector[, .(sector, variable, value_coef, value_elasticity)]
shock = rbind(shock_services, shock_sector)

shock_other = merge(elasticity[!sector %in% shock[, unique(sector)],
                               .(sector, value_elasticity = value, X = 1)],
                    coefs[sector == "total",
                          .(variable, value_coef = value, X = 1)],
                    by = "X",
                    allow.cartesian = T)
shock_other = shock_other[, .(sector, variable, value_coef, value_elasticity)]                

shock = rbind(shock, shock_other)
rm(shock_services, shock_sector, shock_other)

# no estimated change for gas imports of iran and russia
shock[is.na(value_coef), value_coef := 0]

shock[, value := exp(- value_coef * value_elasticity)]

shock[is.infinite(value), value := 10]
shock[value < 1, value := 1]

shock = shock[, .(variable, sector, value)]

## benchmark status quo Iran and Russia sanctions ----
cat(format(Sys.time()), "- Benchmark")

sanctions_baseline = copy(initial_conditions_iran$tariff)
sanctions_baseline[, value := 1]

sanctions_iran = copy(sanctions_baseline)
sanctions_iran[destination == "IRN" & origin %in% countries_sanctions_iran,
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
sanctions_iran[origin == "IRN" & destination %in% countries_sanctions_iran,
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]

sanctions_russia = copy(sanctions_baseline)
sanctions_russia[destination == "RUS" & origin %in% countries_sanctions_russia,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
sanctions_russia[origin == "RUS" & destination %in% countries_sanctions_russia,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]

### iran ----
benchmark_iran = update_equilibrium(initial_conditions = initial_conditions_iran,
                                    model_scenario = list(ntb = sanctions_baseline,
                                                          ntb_new = sanctions_iran),
                                    model = caliendo_parro_2015,
                                    settings = list(verbose = F,
                                                    tolerance = 1e-4)) %>% compute_welfare_change()

### russia ----
benchmark_russia = update_equilibrium(initial_conditions = initial_conditions_russia,
                                      model_scenario = list(ntb = sanctions_baseline,
                                                            ntb_new = sanctions_russia),
                                      model = caliendo_parro_2015,
                                      settings = list(verbose = F,
                                                      tolerance = 1e-4)) %>% compute_welfare_change()
rm(sanctions_iran,
   sanctions_russia)
gc()


## benchmark global implementation of Western sanctions to Iran and Russia sanctions ----
cat(format(Sys.time()), "- Global implementation of Western sanctions")

sanctions_iran = copy(sanctions_baseline)
sanctions_iran[destination == "IRN" & origin != "IRN",
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
sanctions_iran[origin == "IRN" & destination != "IRN",
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]

sanctions_russia = copy(sanctions_baseline)
sanctions_russia[destination == "RUS" & origin != "RUS",
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
sanctions_russia[origin == "RUS" & destination != "RUS",
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]

### iran ----
benchmark_global_implementation_iran = update_equilibrium(initial_conditions = initial_conditions_iran,
                                                          model_scenario = list(ntb = sanctions_baseline,
                                                                                ntb_new = sanctions_iran),
                                                          model = caliendo_parro_2015,
                                                          settings = list(verbose = F,
                                                                          tolerance = 1e-4)) %>% compute_welfare_change()

### russia ----
benchmark_global_implementation_russia = update_equilibrium(initial_conditions = initial_conditions_russia,
                                                            model_scenario = list(ntb = sanctions_baseline,
                                                                                  ntb_new = sanctions_russia),
                                                            model = caliendo_parro_2015,
                                                            settings = list(verbose = F,
                                                                            tolerance = 1e-4)) %>% compute_welfare_change()

rm(sanctions_iran,
   sanctions_russia)
gc()


## benchmark autarky ----
sanctions_iran = copy(sanctions_baseline)
sanctions_iran[destination == "IRN" & origin != "IRN", value := 10]
sanctions_iran[origin == "IRN" & destination != "IRN", value := 10]

sanctions_russia = copy(sanctions_baseline)
sanctions_russia[destination == "RUS" & origin != "RUS", value := 10]
sanctions_russia[origin == "RUS" & destination != "RUS", value := 10]

if (type == "unbalanced") {
  # construct new trade balance
  trade_balance_iran = merge(initial_conditions_iran$trade_share,
                             initial_conditions_iran$expenditure[, .(destination = country, sector, value_expenditure = value)],
                             by = c("destination", "sector"))
  trade_balance_iran = trade_balance_iran[, .(origin, destination, sector, value = value * value_expenditure)]
  trade_balance_iran[origin == "irn" & destination != "irn", value := 0]
  trade_balance_iran[destination == "irn" & origin != "irn", value := 0]
  trade_balance_iran = merge(trade_balance_iran[origin != destination, .(exports = sum(value)), by = .(country = origin)],
                             trade_balance_iran[origin != destination, .(imports = sum(value)), by = .(country = destination)],
                             by = "country")
  trade_balance_iran = trade_balance_iran[, .(country, value = exports - imports)]
  
  trade_balance_russia = merge(initial_conditions_russia$trade_share,
                               initial_conditions_russia$expenditure[, .(destination = country, sector, value_expenditure = value)],
                               by = c("destination", "sector"))
  trade_balance_russia = trade_balance_russia[, .(origin, destination, sector, value = value * value_expenditure)]
  trade_balance_russia[origin == "rus" & destination != "rus", value := 0]
  trade_balance_russia[destination == "rus" & origin != "rus", value := 0]
  trade_balance_russia = merge(trade_balance_russia[origin != destination, .(exports = sum(value)), by = .(country = origin)],
                               trade_balance_russia[origin != destination, .(imports = sum(value)), by = .(country = destination)],
                               by = "country")
  trade_balance_russia = trade_balance_russia[, .(country, value = exports - imports)]
}

### iran ----
benchmark_autarky_iran = update_equilibrium(initial_conditions = initial_conditions_iran,
                                            model_scenario = list(ntb = sanctions_baseline,
                                                                  ntb_new = sanctions_iran),
                                            model = caliendo_parro_2015,
                                            settings = list(verbose = F,
                                                            tolerance = 1e-4,
                                                            vfactor = 0.01)) %>% compute_welfare_change()

### russia ----
benchmark_autarky_russia = update_equilibrium(initial_conditions = initial_conditions_russia,
                                              model_scenario = list(ntb = sanctions_baseline,
                                                                    ntb_new = sanctions_russia),
                                              model = caliendo_parro_2015,
                                              settings = list(verbose = F,
                                                              tolerance = 1e-4,
                                                              vfactor = 0.01)) %>% compute_welfare_change()

rm(sanctions_iran,
   sanctions_russia)
gc()


## benchmark embargo ----
sanctions_iran = copy(sanctions_baseline)
sanctions_iran[destination == "IRN" & origin %in% countries_sanctions_iran, value := 10]
sanctions_iran[origin == "IRN" & destination %in% countries_sanctions_iran, value := 10]

sanctions_russia = copy(sanctions_baseline)
sanctions_russia[destination == "RUS" & origin %in% countries_sanctions_russia, value := 10]
sanctions_russia[origin == "RUS" & destination %in% countries_sanctions_russia, value := 10]

if (type == "unbalanced") {
  # construct new trade balance
  trade_balance_iran = merge(initial_conditions_iran$trade_share,
                             initial_conditions_iran$expenditure[, .(destination = country, sector, value_expenditure = value)],
                             by = c("destination", "sector"))
  trade_balance_iran = trade_balance_iran[, .(origin, destination, sector, value = value * value_expenditure)]
  trade_balance_iran[origin == "irn" & destination %in% countries_sanctions_iran, value := 0]
  trade_balance_iran[destination == "irn" & origin %in% countries_sanctions_iran, value := 0]
  trade_balance_iran = merge(trade_balance_iran[origin != destination, .(exports = sum(value)), by = .(country = origin)],
                             trade_balance_iran[origin != destination, .(imports = sum(value)), by = .(country = destination)],
                             by = "country")
  trade_balance_iran = trade_balance_iran[, .(country, value = exports - imports)]
  
  trade_balance_russia = merge(initial_conditions_russia$trade_share,
                               initial_conditions_russia$expenditure[, .(destination = country, sector, value_expenditure = value)],
                               by = c("destination", "sector"))
  trade_balance_russia = trade_balance_russia[, .(origin, destination, sector, value = value * value_expenditure)]
  trade_balance_russia[origin == "rus" & destination %in% countries_sanctions_russia, value := 0]
  trade_balance_russia[destination == "rus" & origin %in% countries_sanctions_russia, value := 0]
  trade_balance_russia = merge(trade_balance_russia[origin != destination, .(exports = sum(value)), by = .(country = origin)],
                               trade_balance_russia[origin != destination, .(imports = sum(value)), by = .(country = destination)],
                               by = "country")
  trade_balance_russia = trade_balance_russia[, .(country, value = exports - imports)]
}


### iran ----
benchmark_embargo_iran = update_equilibrium(initial_conditions = initial_conditions_iran,
                                            model_scenario = list(ntb = sanctions_baseline,
                                                                  ntb_new = sanctions_iran),
                                            model = caliendo_parro_2015,
                                            settings = list(verbose = F,
                                                            tolerance = 1e-4,
                                                            vfactor = 0.01)) %>% compute_welfare_change()

### russia ----
benchmark_embargo_russia = update_equilibrium(initial_conditions = initial_conditions_russia,
                                              model_scenario = list(ntb = sanctions_baseline,
                                                                    ntb_new = sanctions_russia),
                                              model = caliendo_parro_2015,
                                              settings = list(verbose = F,
                                                              tolerance = 1e-4,
                                                              vfactor = 0.01)) %>% compute_welfare_change()

rm(sanctions_iran,
   sanctions_russia)
gc()

## save and clean up ----
benchmark = rbind(benchmark_iran[, .(scenario = "status_quo_iran", country, value)],
                  benchmark_russia[, .(scenario = "status_quo_russia", country, value)],
                  benchmark_global_implementation_iran[, .(scenario = "global_implementation_iran", country, value)],
                  benchmark_global_implementation_russia[, .(scenario = "global_implementation_russia", country, value)],
                  benchmark_embargo_iran[, .(scenario = "embargo_iran", country, value)],
                  benchmark_embargo_russia[, .(scenario = "embargo_russia", country, value)],
                  benchmark_autarky_iran[, .(scenario = "autarky_iran", country, value)],
                  benchmark_autarky_russia[, .(scenario = "autarky_russia", country, value)])
write_rds(benchmark, str_c("temp/simulations/", type, "/benchmark.rds"), compress = "gz")

rm(benchmark_iran,
   benchmark_russia,
   benchmark_global_implementation_iran,
   benchmark_global_implementation_russia,
   benchmark_embargo_iran,
   benchmark_embargo_russia,
   benchmark_autarky_iran,
   benchmark_autarky_russia,
   coefs,
   shock)
gc()


# 3 - bootstraps for standard errors ----

## load coefficients ----
coefs = fread("temp/regressions/coefs_bb_clustered_pair.csv")
coefs = coefs[str_detect(variable, "sanctions_")]
coefs = coefs[!is.na(value)]
coefs[!str_detect(variable, "TOTAL") &
        !str_detect(variable, "SERVICES"), sector := str_sub(variable, -3)]
coefs[!str_detect(variable, "TOTAL") &
        !str_detect(variable, "SERVICES"), variable := str_sub(variable, 1,-5)]
coefs[str_detect(variable, "TOTAL"), sector := "TOTAL"]
coefs[str_detect(variable, "TOTAL"), variable := str_remove(variable, "_TOTAL")]
coefs[str_detect(variable, "SERVICES"), sector := "SERVICES"]
coefs[str_detect(variable, "SERVICES"), variable := str_remove(variable, "_SERVICES")]
coefs[, sector := str_to_lower(sector)]

## run iterations ----
if (!dir.exists(str_c("temp/simulations/", type))) dir.create(str_c("temp/simulations/", type))

initialize_results_files(str_c("temp/simulations/", type, "/scenario0_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario0_russia_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario0b_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario0b_russia_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario1_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario1_russia_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario1b_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario1b_russia_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario2_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario2_russia_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario4_iran_clustered_pair.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario4_russia_clustered_pair.csv"))

episodes = c("RUS",
             "IRN"
             )
scenarios = c("s0",
              "s0b",
              "s1",
              "s23",
              "s4",
              "s5"
              )

for (iteration in coefs[, unique(i)]) { # [840:coefs[, uniqueN(i)]]) {

  cat(format(Sys.time()), "-", iteration)
  
  # prepare sanctions shock for this round
  shock_services = merge(elasticity[sector %in% str_to_lower(sectors_services),
                                    .(sector, value_elasticity = value, X = 1)],
                         coefs[i == iteration & sector == "services",
                               .(variable, value_coef = value, X = 1)],
                         by = "X",
                         allow.cartesian = T)
  shock_services = shock_services[, .(sector, variable, value_coef, value_elasticity)]
  
  shock_sector = merge(elasticity[!sector %in% str_to_lower(sectors_services),
                                  .(sector, value_elasticity = value)],
                       coefs[i == iteration & sector != "services",
                             .(variable, sector, value_coef = value)],
                       by = "sector")
  shock_sector = shock_sector[, .(sector, variable, value_coef, value_elasticity)]
  shock = rbind(shock_services, shock_sector)
  
  shock_other = merge(elasticity[!sector %in% shock[, unique(sector)],
                                 .(sector, value_elasticity = value, X = 1)],
                      coefs[i == iteration & sector == "total",
                            .(variable, value_coef = value, X = 1)],
                      by = "X",
                      allow.cartesian = T)
  shock_other = shock_other[, .(sector, variable, value_coef, value_elasticity)]                
  
  shock = rbind(shock, shock_other)
  rm(shock_services, shock_sector, shock_other)
  
  shock[, value := exp(- value_coef * value_elasticity)]
  
  shock[is.infinite(value), value := 10]
  if (allow_positive_shock == F) shock[value < 1, value := 1]
  
  shock = shock[, .(variable, sector, value)]
  cat("\n")
  
  
  # set baseline scenario
  sanctions_baseline = copy(initial_conditions_iran$tariff)
  sanctions_baseline[, value := 1]
  
  sanctions_iran = copy(sanctions_baseline)
  sanctions_iran[destination == "IRN" & origin %in% countries_sanctions_iran,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
  sanctions_iran[origin == "IRN" & destination %in% countries_sanctions_iran,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]
  sanctions_iran[is.na(value), value := 1]
  
  sanctions_russia = copy(sanctions_baseline)
  sanctions_russia[destination == "RUS" & origin %in% countries_sanctions_russia,
                   value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
  sanctions_russia[origin == "RUS" & destination %in% countries_sanctions_russia,
                   value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]
  sanctions_russia[is.na(value), value := 1]
  
  ### scenario 0: benchmark to Iran and Russia sanctions ----
  if ("s0" %in% scenarios) {
    cat(format(Sys.time()), "- Benchmark")
    
    
    #### iran ----
    if ("IRN" %in% episodes) {
      
      cat(" - IRN")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario0_iran_clustered_pair.csv"))[, unique(i)]) {
        
        update_equilibrium(initial_conditions = initial_conditions_iran,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_iran),
                           model = caliendo_parro_2015,
                           settings = list(verbose = F,
                                           tolerance = 1e-4,
                                           vfactor = 0.1)) %>%
          compute_welfare_change() %>%
          extract_variables(scenario = "benchmark", i = iteration) %>%
          fwrite(str_c("temp/simulations/", type, "/scenario0_iran_clustered_pair.csv"),
                 append = T)
      }
    }
    
    #### russia ----
    if ("RUS" %in% episodes) {
      
      cat(" - RUS")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario0_russia_clustered_pair.csv"))[, unique(i)]) {
        
        update_equilibrium(initial_conditions = initial_conditions_russia,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_russia),
                           model = caliendo_parro_2015,
                           settings = list(verbose = F,
                                           tolerance = 1e-4,
                                           vfactor = 0.1)) %>%
          compute_welfare_change() %>%
          extract_variables(scenario = "benchmark", i = iteration) %>%
          fwrite(str_c("temp/simulations/", type, "/scenario0_russia_clustered_pair.csv"),
                 append = T)
      }
    }
    gc()
    cat("\n")
  }
  
  if ("s0b" %in% scenarios) {
    cat(format(Sys.time()), "- Benchmark - Global implementation of current measures")
    
    sanctions_iran_s0b = copy(sanctions_baseline)
    sanctions_iran_s0b[destination == "IRN" & origin != "IRN",
                   value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
    sanctions_iran_s0b[origin == "IRN" & destination != "IRN",
                   value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]
    sanctions_iran_s0b[is.na(value), value := 1]
    
    sanctions_russia_s0b = copy(sanctions_baseline)
    sanctions_russia_s0b[destination == "RUS" & origin != "RUS",
                     value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
    sanctions_russia_s0b[origin == "RUS" & destination != "RUS",
                     value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]
    sanctions_russia_s0b[is.na(value), value := 1]
    
    #### iran ----
    if ("IRN" %in% episodes) {
      
      cat(" - IRN")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario0b_iran_clustered_pair.csv"))[, unique(i)]) {
        
        update_equilibrium(initial_conditions = initial_conditions_iran,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_iran_s0b),
                           model = caliendo_parro_2015,
                           settings = list(verbose = F,
                                           tolerance = 1e-4,
                                           vfactor = 0.1)) %>%
          compute_welfare_change() %>%
          extract_variables(scenario = "benchmark_global_implementation", i = iteration) %>%
          fwrite(str_c("temp/simulations/", type, "/scenario0b_iran_clustered_pair.csv"),
                 append = T)
      }
    }
    
    #### russia ----
    if ("RUS" %in% episodes) {
      
      cat(" - RUS")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario0b_russia_clustered_pair.csv"))[, unique(i)]) {
        
        update_equilibrium(initial_conditions = initial_conditions_russia,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_russia_s0b),
                           model = caliendo_parro_2015,
                           settings = list(verbose = F,
                                           tolerance = 1e-4,
                                           vfactor = 0.1)) %>%
          compute_welfare_change() %>%
          extract_variables(scenario = "benchmark_global_implementation", i = iteration) %>%
          fwrite(str_c("temp/simulations/", type, "/scenario0b_russia_clustered_pair.csv"),
                 append = T)
      }
    }
    
    rm(sanctions_iran_s0b,
       sanctions_russia_s0b)
    gc()
    cat("\n")
  }
  

  ### scenario 1: compute individual contributions to Iran and Russia sanctions (each country individually sanctions) ----
  if ("s1" %in% scenarios) {
    
    cat(format(Sys.time()), "- Scenario 1")
    
    #### iran ----
    if ("IRN" %in% episodes) {
      
      cat(" - IRN")

      # multilateral sans country
      tasks = c(countries_sanctions_iran, "XEU")
      completed = fread(str_c("temp/simulations/", type, "/scenario1_iran_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_iran_s1 = copy(sanctions_iran)
          
          c_name = c
          if (c == "XEU") c = countries_EU
          
          sanctions_iran_s1[destination == "IRN" & origin %in% c, value := 1]
          sanctions_iran_s1[origin == "IRN" & destination %in% c, value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_iran,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_iran_s1),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario1_", c_name), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario1_iran_clustered_pair.csv"),
                   append = T)
        }
      }
      
      # unilateral
      tasks = c(countries_sanctions_iran, "XEU")
      completed = fread(str_c("temp/simulations/", type, "/scenario1b_iran_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_iran_s1b = copy(sanctions_baseline)
          
          c_name = c
          if (c == "XEU") c = countries_EU
          
          sanctions_iran_s1b[destination == "IRN" & origin %in% c,
                             value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
          sanctions_iran_s1b[origin == "IRN" & destination %in% c,
                             value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]
          sanctions_iran_s1b[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_iran,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_iran_s1b),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario1b_", c_name), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario1b_iran_clustered_pair.csv"),
                   append = T)
        }
      }
      gc()
    }
    
    #### russia ----
    if ("RUS" %in% episodes) {
      
      cat(" - RUS")
      
      # multilateral sans country
      tasks = c(countries_sanctions_russia, "XEU")
      completed = fread(str_c("temp/simulations/", type, "/scenario1_russia_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_russia_s1 = copy(sanctions_russia)
          
          c_name = c
          if (c == "XEU") c = countries_EU
          
          sanctions_russia_s1[destination == "RUS" & origin %in% c, value := 1]
          sanctions_russia_s1[origin == "RUS" & destination %in% c, value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_russia,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_russia_s1),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario1_", c_name), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario1_russia_clustered_pair.csv"),
                   append = T)
        }
      }
      
      # unilateral
      tasks = c(countries_sanctions_russia, "XEU")
      completed = fread(str_c("temp/simulations/", type, "/scenario1b_russia_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]

      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_russia_s1b = copy(sanctions_baseline)
          
          c_name = c
          if (c == "XEU") c = countries_EU
          
          sanctions_russia_s1b[destination == "RUS" & origin %in% c,
                               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
          sanctions_russia_s1b[origin == "RUS" & destination %in% c,
                               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]
          sanctions_russia_s1b[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_russia,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_russia_s1b),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario1b_", c_name), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario1b_russia_clustered_pair.csv"),
                   append = T)
        }
      }
      gc()
    }
    cat("\n")
  }

  
  ### scenario 2 and 3: compute individual and ranked cumulative ideal coalition partners to Iran and Russia sanctions (china alone is scenario 2) ----
  if ("s23" %in% scenarios) {
    
    cat(format(Sys.time()), "- Scenario 2/3")
    
    #### iran ----
    if ("IRN" %in% episodes) {
      
      cat(" - IRN")
      
      # individual
      tasks = sanctions_baseline[!origin %in% countries_sanctions_iran, unique(origin)] %>% as.character() %>% unique()
      completed = fread(str_c("temp/simulations/", type, "/scenario2_iran_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_iran_s2 = copy(sanctions_iran)
          sanctions_iran_s2[destination == "IRN" & origin %in% c,
                            value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
          sanctions_iran_s2[origin == "IRN" & destination %in% c,
                            value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]
          sanctions_iran_s2[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_iran,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_iran_s2),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario2_", c), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario2_iran_clustered_pair.csv"),
                   append = T)
        }
      }
    }
    

    #### russia ----
    if ("RUS" %in% episodes) {
      
      cat(" - RUS")
      
      # individual
      tasks = sanctions_baseline[!origin %in% countries_sanctions_russia, unique(origin)] %>% as.character() %>% unique()
      completed = fread(str_c("temp/simulations/", type, "/scenario2_russia_clustered_pair.csv"))[i == iteration, unique(str_sub(scenario, -3))]
      tasks = tasks[!tasks %in% completed]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_russia_s2 = copy(sanctions_russia)
          sanctions_russia_s2[destination == "RUS" & origin %in% c,
                              value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
          sanctions_russia_s2[origin == "RUS" & destination %in% c,
                              value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]
          sanctions_russia_s2[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_russia,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_russia_s2),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario2_", c), i = iteration) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario2_russia_clustered_pair.csv"),
                   append = T)
        }
      }
    }
    gc()
    cat("\n")
  }
  
  ### scenario 4: transfers ----
  if ("s4" %in% scenarios) {
    cat(format(Sys.time()), "- Scenario 4")
    
    gtap_countries = initial_conditions_iran$consumption_share[, unique(country)]
    
    #### iran ----
    if ("IRN" %in% episodes) {
      
      cat(" - IRN")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario4_iran_clustered_pair.csv"))[, unique(i)]) {
        
        
        countries_alliance = countries_sanctions_iran[countries_sanctions_iran %in% gtap_countries]
        
        update_equilibrium(initial_conditions = initial_conditions_iran,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_iran,
                                                 coalition_member = countries_alliance),
                           model = chowdhry_hinz_kamin_wanner_2022,
                           settings = list(verbose = F,
                                           vfactor = 0.1,
                                           tolerance = 1e-4)) %>%
          extract_transfer() %>%
          extract_variables(scenario = "scenario4",
                            i = iteration,
                            variable = "transfer") %>%
          fwrite(str_c("temp/simulations/", type, "/scenario4_iran_clustered_pair.csv"),
                 append = T)
      }
    }
    
    #### russia ----
    if ("RUS" %in% episodes) {
      
      cat(" - RUS")
      
      if (!iteration %in% fread(str_c("temp/simulations/", type, "/scenario4_russia_clustered_pair.csv"))[, unique(i)]) {
        
        countries_alliance = countries_sanctions_russia[countries_sanctions_russia %in% gtap_countries]
        
        update_equilibrium(initial_conditions = initial_conditions_russia,
                           model_scenario = list(ntb = sanctions_baseline,
                                                 ntb_new = sanctions_russia,
                                                 coalition_member = countries_alliance),
                           model = chowdhry_hinz_kamin_wanner_2022,
                           settings = list(verbose = F,
                                           vfactor = 0.1,
                                           tolerance = 1e-4)) %>%
          extract_transfer() %>%
          extract_variables(scenario = "scenario4",
                            i = iteration,
                            variable = "transfer") %>%
          fwrite(str_c("temp/simulations/", type, "/scenario4_russia_clustered_pair.csv"),
                 append = T)
      }
    }
    gc()
    cat("\n")
  }
  
  # clean up for next round
  rm(shock,
     sanctions_baseline,
     sanctions_iran,
     sanctions_russia)
  
}


# 4 - optimal coalitions with point estimates ----

# load point estimates
coefs = read_rds("temp/regressions/regressions_sectors_bb.rds")
coefs = coefs %>% map(~.x$coefs %>% t() %>% data.table) %>%
  rbindlist(idcol = "sector", fill = T) %>%
  melt.data.table(id.var = "sector")
coefs[, sector := str_to_lower(sector)]
coefs = coefs[!variable %in% c("wto", "cu", "fta")]

# prepare sanctions shock for services, oil, gas and traded goods, and other other stuff
shock_services = merge(elasticity[sector %in% str_to_lower(sectors_services),
                                  .(sector, value_elasticity = value, X = 1)],
                       coefs[sector == "services",
                             .(variable, value_coef = value, X = 1)],
                       by = "X",
                       allow.cartesian = T)
shock_services = shock_services[, .(sector, variable, value_coef, value_elasticity)]

shock_sector = merge(elasticity[!sector %in% str_to_lower(sectors_services),
                                .(sector, value_elasticity = value)],
                     coefs[sector != "services",
                           .(variable, sector, value_coef = value)],
                     by = "sector")
shock_sector = shock_sector[, .(sector, variable, value_coef, value_elasticity)]
shock = rbind(shock_services, shock_sector)

shock_other = merge(elasticity[!sector %in% shock[, unique(sector)],
                               .(sector, value_elasticity = value, X = 1)],
                    coefs[sector == "total",
                          .(variable, value_coef = value, X = 1)],
                    by = "X",
                    allow.cartesian = T)
shock_other = shock_other[, .(sector, variable, value_coef, value_elasticity)]                

shock = rbind(shock, shock_other)
rm(shock_services, shock_sector, shock_other)

# no estimated change for gas imports of iran and russia
shock[is.na(value_coef), value_coef := 0]

shock[, value := exp(- value_coef * value_elasticity)]

shock[is.infinite(value), value := 10]
shock[value < 1, value := 1]

shock = shock[, .(variable, sector, value)]

# construct sanctions baseline
sanctions_baseline = copy(initial_conditions_iran$tariff)
sanctions_baseline[, value := 1]

sanctions_iran = copy(sanctions_baseline)
sanctions_iran[destination == "IRN" & origin %in% countries_sanctions_iran,
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
sanctions_iran[origin == "IRN" & destination %in% countries_sanctions_iran,
               value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]

sanctions_russia = copy(sanctions_baseline)
sanctions_russia[destination == "RUS" & origin %in% countries_sanctions_russia,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
sanctions_russia[origin == "RUS" & destination %in% countries_sanctions_russia,
                 value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]

# initialize
initialize_results_files(str_c("temp/simulations/", type, "/scenario5_iran.csv"))
initialize_results_files(str_c("temp/simulations/", type, "/scenario5_russia.csv"))

# add one country after the other based on highest bang for the buck
if ("s5" %in% scenarios) {
  
  cat(format(Sys.time()), "- Scenario 5")
  
  ## iran ----
  if ("IRN" %in% episodes) {
    
    cat(" - IRN")
    
    # set up
    all_countries = initial_conditions_iran$consumption_share[, unique(country)]
    coalition_countries = c()
    iter = 1
    
    # first round: unilateral
    while (iter <= length(all_countries)) {
        
      tasks = all_countries
      tasks = tasks[tasks != "IRN"]
      completed = fread(str_c("temp/simulations/", type, "/scenario5_iran.csv"))
      completed = completed[i == iter, unique(str_sub(scenario, -3, -1))]
      tasks = tasks[!tasks %in% completed]
      
      # exclude coalition countries from tasks
      tasks = tasks[!tasks %in% coalition_countries]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_iran_s5 = copy(sanctions_baseline)
          
          sanctions_iran_s5[destination == "IRN" & origin %in% c(c, coalition_countries),
                            value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran"])]
          sanctions_iran_s5[origin == "IRN" & destination %in% c(c, coalition_countries),
                            value := match_replace(sector, "sector", "value", shock[variable == "sanctions_iran_rev"])]
          sanctions_iran_s5[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_iran,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_iran_s5),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario5_", c), i = iter) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario5_iran.csv"),
                   append = T)
        }
      }
      
      # load gdp data
      data_gdp = fread("input/gdp_ppp/API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_5455171.csv", skip = 2, header = T)
      data_gdp = data_gdp[, .(country = `Country Code`, gdp_ppp = `2011`)]
      data_gdp = data_gdp[!is.na(country) & !is.na(gdp_ppp)]
      data_gdp[, country := match_replace(country,
                                          from = "comtrade_code", to = "gtap_code",
                                          dictionary = concordance_countries), by = country]
      data_gdp = data_gdp[!is.na(country)]
      data_gdp = data_gdp[, .(gdp_ppp = sum(gdp_ppp, na.rm = T)), by = country]
      
      # merge with costs
      costs = fread(str_c("temp/simulations/", type, "/scenario5_iran.csv"))
      
      # get current iteration and last iteration with winning scenario
      if (iter == 1) {
        costs = costs[(i == iter)]
      } else {
        costs = costs[(i == iter) |
                        (i == (iter - 1) & str_sub(scenario, -3, -1) == coalition_countries[length(coalition_countries)])]
      }
      
      cost_incurred = merge(costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value_new = value)],
                            costs[(i == (iter - 1)), .(country, value_old = value)],
                            by = "country")
      cost_incurred = cost_incurred[, .(country, value = value_new - value_old)]
      
      cost_incurred = merge(cost_incurred,
                            data_gdp,
                            by = "country",
                            all.x = T)
      cost_incurred[, value := value * gdp_ppp]
      cost_incurred[, gdp_ppp := NULL]
      
      cost_imposed = merge(costs[(i == iter & country == "IRN"), .(country, scenario, value_new = value)],
                            costs[(i == (iter - 1) & country == "IRN"), .(country, value_old = value)],
                            by = "country")
      cost_imposed = cost_imposed[, .(country, scenario, value = value_new - value_old)]
      cost_imposed = merge(cost_imposed,
                           data_gdp,
                           by = "country",
                           all.x = T)
      cost_imposed[, value := value * gdp_ppp]
      cost_imposed[, gdp_ppp := NULL]
      
      cost_combined = merge(cost_incurred[, .(country, value_incurred = value)],
                            cost_imposed[, .(country = str_sub(scenario, -3, -1), value_imposed = value)],
                            by = c("country"))
      
      # select next coalition country
      cost_combined = cost_combined[, .(country, value = value_imposed / value_incurred, value_imposed, value_incurred)]
      
      cost_combined = cost_combined[value_imposed < 0]
      if (nrow(cost_combined[value_incurred > 0]) > 0) {
        coalition_countries = append(coalition_countries,
                                     cost_combined[value_incurred > 0][order(-value), country][1])
      } else {
        coalition_countries = append(coalition_countries,
                                     cost_combined[order(-value), country][1])
      }
      print(coalition_countries)
      
      iter = iter + 1
    }
    gc()
  }
  cat("\n")
  
  ## russia ----
  if ("RUS" %in% episodes) {
    
    cat(" - RUS")
    
    # set up
    all_countries = initial_conditions_iran$consumption_share[, unique(country)]
    coalition_countries = c()
    iter = 1
    
    # first round: unilateral
    while (iter <= length(all_countries)) {
      
      tasks = all_countries
      tasks = tasks[tasks != "RUS"]
      completed = fread(str_c("temp/simulations/", type, "/scenario5_russia.csv"))
      completed = completed[i == iter, unique(str_sub(scenario, -3, -1))]
      tasks = tasks[!tasks %in% completed]
      
      # exclude coalition countries from tasks
      tasks = tasks[!tasks %in% coalition_countries]
      
      if (length(tasks) > 0) {
        foreach(c = tasks, .packages = c("data.table", "dplyr", "KITE", "stringr")) %dopar% {
          sanctions_russia_s5 = copy(sanctions_baseline)
          
          sanctions_russia_s5[destination == "RUS" & origin %in% c(c, coalition_countries),
                              value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia"])]
          sanctions_russia_s5[origin == "RUS" & destination %in% c(c, coalition_countries),
                              value := match_replace(sector, "sector", "value", shock[variable == "sanctions_russia_rev"])]
          sanctions_russia_s5[is.na(value), value := 1]
          
          update_equilibrium(initial_conditions = initial_conditions_russia,
                             model_scenario = list(ntb = sanctions_baseline,
                                                   ntb_new = sanctions_russia_s5),
                             model = caliendo_parro_2015,
                             settings = list(verbose = F,
                                             tolerance = 1e-4,
                                             vfactor = 0.1)) %>%
            compute_welfare_change() %>%
            extract_variables(scenario = str_c("scenario5_", c), i = iter) %>%
            fwrite(str_c("temp/simulations/", type, "/scenario5_russia.csv"),
                   append = T)
        }
      }
      
      # load gdp data
      data_gdp = fread("input/gdp_ppp/API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_5455171.csv", skip = 2, header = T)
      data_gdp = data_gdp[, .(country = `Country Code`, gdp_ppp = `2013`)]
      data_gdp = data_gdp[!is.na(country) & !is.na(gdp_ppp)]
      data_gdp[, country := match_replace(country,
                                          from = "comtrade_code", to = "gtap_code",
                                          dictionary = concordance_countries), by = country]
      data_gdp = data_gdp[!is.na(country)]
      data_gdp = data_gdp[, .(gdp_ppp = sum(gdp_ppp, na.rm = T)), by = country]
      
      # merge with costs
      costs = fread(str_c("temp/simulations/", type, "/scenario5_russia.csv"))
      
      # get current iteration and last iteration with winning scenario
      if (iter == 1) {
        costs = costs[(i == iter)]
        cost_incurred = costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value)]
        cost_imposed = costs[(i == iter & country == "RUS"), .(country, scenario, value)]
      } else {
        costs = costs[(i == iter) |
                        (i == (iter - 1) & str_sub(scenario, -3, -1) == coalition_countries[length(coalition_countries)])]
        cost_incurred = merge(costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value_new = value)],
                              costs[(i == (iter - 1)), .(country, value_old = value)],
                              by = "country")
        cost_incurred = cost_incurred[, .(country, value = value_new - value_old)]
        
        cost_imposed = merge(costs[(i == iter & country == "RUS"), .(country, scenario, value_new = value)],
                             costs[(i == (iter - 1) & country == "RUS"), .(country, value_old = value)],
                             by = "country")
        cost_imposed = cost_imposed[, .(country, scenario, value = value_new - value_old)]
      }

      cost_incurred = merge(cost_incurred,
                            data_gdp,
                            by = "country",
                            all.x = T)
      cost_incurred[, value := value * gdp_ppp]
      cost_incurred[, gdp_ppp := NULL]
      
      cost_imposed = merge(cost_imposed,
                           data_gdp,
                           by = "country",
                           all.x = T)
      cost_imposed[, value := value * gdp_ppp]
      cost_imposed[, gdp_ppp := NULL]
      
      cost_combined = merge(cost_incurred[, .(country, value_incurred = value)],
                            cost_imposed[, .(country = str_sub(scenario, -3, -1), value_imposed = value)],
                            by = c("country"))
      
      
      # select next coalition country
      cost_combined = cost_combined[, .(country, value = value_imposed / value_incurred, value_imposed, value_incurred)]
      
      cost_combined = cost_combined[value_imposed < 0]
      if (nrow(cost_combined[value_incurred > 0]) > 0) {
        coalition_countries = append(coalition_countries,
                                     cost_combined[value_incurred > 0][order(-value), country][1])
      } else {
        coalition_countries = append(coalition_countries,
                                     cost_combined[order(-value), country][1])
      }
      print(coalition_countries)
      
      iter = iter + 1
    }
    gc()
  }
  
}


# EOF ----
rm(list = ls())
