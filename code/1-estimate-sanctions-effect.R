###
# 1 - estimate sanctions effect
# 231201
###

if (!require("pacman")) install.packages("pacman"); library("pacman")
pacman::p_load(data.table)
pacman::p_load(readr)
pacman::p_load(magrittr)
pacman::p_load(purrr)
pacman::p_load(stringr)
pacman::p_load(fixest)
pacman::p_load(bit64)
pacman::p_load(countrycode)
pacman::p_load(tictoc)
pacman::p_load(ggplot2)
pacman::p_load(R.utils)
pacman::p_load(insight)
pacman::p_load(haven)

setFixest_nthreads(16)


# custom functions and definitions ----
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

# load data ----
data = fread("temp/data/sanctions_gravity_dataset.csvy.gz")

# construct fixed effects and dummies ----
data[, origin_year := str_c(origin, year)]
data[, destination_year := str_c(destination, year)]
data[, origin_destination := str_c(origin, destination)]


# estimations ----

## run gravity regression on total flows ----
reg = feglm(value ~ sanctions_russia + sanctions_russia_rev +
              sanctions_iran + sanctions_iran_rev +
              wto + cu + fta | 
              origin_year + destination_year + origin_destination,
            data = data[sector == "TOTAL"],
            family = poisson())
summary(reg, cluster = c("origin_destination"))
write_rds(reg, "temp/regressions/regression_TOTAL.rds", compress = "gz")
rm(reg)

## run gravity regression on services flows ----
reg = feglm(value ~ sanctions_russia + sanctions_russia_rev +
              sanctions_iran + sanctions_iran_rev +
              wto + cu + fta | 
              origin_year + destination_year + origin_destination,
            data = data[sector == "SERVICES"],
            family = poisson())
summary(reg, cluster = c("origin_destination"))
write_rds(reg, "temp/regressions/regression_SERVICES.rds", compress = "gz")
rm(reg)

## run gravity regression on oil flows ----
reg = feglm(value ~ sanctions_russia + sanctions_russia_rev +
              sanctions_iran + sanctions_iran_rev +
              wto + cu + fta | 
              origin_year + destination_year + origin_destination,
            data = data[sector %in% c("OIL")],
            family = poisson())
summary(reg, cluster = c("origin_destination"))
write_rds(reg, "temp/regressions/regression_OIL.rds", compress = "gz")
rm(reg)

## run gravity regression on gas flows ----
reg = feglm(value ~ sanctions_russia + sanctions_russia_rev +
              sanctions_iran + sanctions_iran_rev +
              wto + cu + fta | 
              origin_year + destination_year + origin_destination,
            data = data[sector %in% c("GAS")],
            family = poisson())
summary(reg, cluster = c("origin_destination"))
write_rds(reg, "temp/regressions/regression_GAS.rds", compress = "gz")
rm(reg)

## run time-interacted gravity regression on total flows ----
data[, time := as.character(year)]

reg_time = feglm(value ~ time:sanctions_russia +
                   time:sanctions_russia_rev +
                   time:sanctions_iran +
                   time:sanctions_iran_rev +
                   wto + cu + fta | 
                   origin_year + destination_year + origin_destination,
                 data = data[sector == "TOTAL"],
                 family = poisson())
summary(reg_time, cluster = c("origin", "destination", "year"))
write_rds(reg_time, "temp/regressions/regression_time_TOTAL.rds", compress = "gz")
rm(reg_time)

## run gravity regression on sectoral flows ----

# because of structure of the model (everything is indexed k), the estimation is separable along k
sectors = data[, unique(sector)]
models = list()

for (s in sectors) {
  cat(format(Sys.time()), "-", s, "\n")
  reg_temp = feglm(value ~ 
                     sanctions_russia + sanctions_russia_rev +
                     sanctions_iran + sanctions_iran_rev +
                     wto + cu + fta |
                     origin_year + 
                     destination_year + 
                     origin_destination,
                   data = data[sector == s],
                   family = poisson(),
                   glm.iter = 200)
  models[[s]]$obs = reg_temp$obs_selection$obsRemoved
  models[[s]]$coefs = reg_temp$coefficients
}
write_rds(models, "temp/regressions/regressions_sectors_bb.rds", compress = "gz")
gc()


# retrieve data
data_models = list()
for (m in names(models)) data_models[[m]] <- data[sector == m][models[[m]]$obs]
data_models = rbindlist(data_models)
rm(data, models)
gc()


# run bootstap iterations ----
set.seed(1234)

sectors = data_models[, unique(sector)]
clustered = F
n = 1000
i = 1
j = 1 # counter for successful runs
while (j <= n) {
  
  cat(format(Sys.time()), "-", "run", i, "|", j, "\n")
  
  # cluster weights by origin_destination?
  if (clustered) {
    data_models[, w := rexp(1), by = origin_destination]
  } else {
    data_models[, w := rexp(.N)]
  }
  
  coefs = c()
  for (s in sectors) {
  
    cat("\t", "-", s, "\n")
    
    temp_coefs = fixest::feglm(value ~ 
                                 sanctions_russia + sanctions_russia_rev +
                                 sanctions_iran + sanctions_iran_rev +
                                 wto + cu + fta |
                                 origin_year + 
                                 destination_year + 
                                 origin_destination,
                               data = data_models[sector == s],
                               weights = data_models[sector == s, w],
                               family = poisson(),
                               glm.iter = 1000)
    
    if (!temp_coefs$convStatus) {
      coefs = NA
      names(coefs) = NA
      rm(temp_coefs)
      cat("!! Absence of convergence !!\n")
      break
    } else {
      temp_coefs = temp_coefs %>% coef()
      names(temp_coefs) = str_c(names(temp_coefs), "_", s)
      coefs = append(coefs, temp_coefs)
      rm(temp_coefs)
    }
  }
  
  coefs = data.table(i = i,
                     variable = names(coefs),
                     value = coefs)
  
  if (length(coefs) > 1) {
    if (clustered) {
      fwrite(coefs, "temp/regressions/coefs_bb_clustered_pair.csv", append = T)
    } else {
      fwrite(coefs, "temp/regressions/coefs_bb_no_cluster.csv", append = T)
    }
  }
  
  i = i + 1
  if (nrow(coefs) > 1) j = j + 1
  rm(coefs)
}

# EOF ----
rm(list = ls())
