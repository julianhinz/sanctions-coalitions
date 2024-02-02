###
# 3 - generate output 
# 231201
###

if (!require("pacman")) install.packages("pacman"); library("pacman")
pacman::p_load(data.table)
pacman::p_load(readr)
pacman::p_load(magrittr)
pacman::p_load(purrr)
pacman::p_load(stringr)
pacman::p_load(bit64)
pacman::p_load(ggplot2)
pacman::p_load(ggridges)
pacman::p_load(ggthemes)
pacman::p_load(viridis)
pacman::p_load(wesanderson)
pacman::p_load(countrycode)
pacman::p_load(scales)
pacman::p_load(fixest)
pacman::p_load(sf)
pacman::p_load(dplyr)
pacman::p_load(xtable)
pacman::p_load(R.utils)
pacman::p_load(ggmap)
pacman::p_load(readxl)


# custom functions and definitions ----
## match and replace ----
match_replace <- function (code, from, to, dictionary) {
  dictionary[match(code, dictionary[[from]]),][[to]]
}

## function to extract specific coefficients and standard errors ----
extract_coef_se = function (x, variable = NULL, cluster) {
  if (is.null(variable)) {
    data.table(variable = names(summary(x, cluster = cluster)$coefficients),
               coef = summary(x, cluster = cluster)$coefficients,
               se = summary(x, cluster = cluster)$se)
  } else {
    data.table(variable = variable,
               coef = summary(x, cluster = cluster)$coefficients[variable],
               se = summary(x, cluster = cluster)$se[variable])
  }
}
# extract_coef_se(regs[[1]], c("sanctions_russia", "sanctions_iran"), ~ origin + destination + year)

## compute t and p values for bootstrapped standard errors ----
compute_t = function (coef, se) coef / se
compute_p = function (coef, se) 2*pnorm(-abs(coef / se))

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

# table 1: aggregate sanctions impact ----
data = fread("temp/data/sanctions_gravity_dataset.csvy.gz") # needed for latex output
data[, origin_destination := str_c(origin, destination)]

# load regression outputs
reg_TOTAL = read_rds("temp/regressions/regression_TOTAL.rds") %>%
  summary(cluster = ~origin_destination)
reg_SERVICES = read_rds("temp/regressions/regression_SERVICES.rds") %>%
  summary(cluster = ~origin_destination)
reg_OIL = read_rds("temp/regressions/regression_OIL.rds") %>%
  summary(cluster = ~origin_destination)
reg_GAS = read_rds("temp/regressions/regression_GAS.rds") %>%
  summary(cluster = ~origin_destination)

# bootstrapped SEs
se = fread("temp/regressions/coefs_bb_clustered_pair.csv")
se_TOTAL = se[str_detect(variable, "TOTAL"), .(value = sd(value)), by = variable] # change manually in tex file
reg_TOTAL$coeftable[, 2] = se_TOTAL$value
reg_TOTAL$coeftable[, 3] = compute_p(reg_TOTAL$coeftable[, 1], reg_TOTAL$coeftable[, 2])
reg_TOTAL$coeftable[, 4] = compute_p(reg_TOTAL$coeftable[, 1], reg_TOTAL$coeftable[, 2])
se_SERVICES = se[str_detect(variable, "SERVICES"), .(value = sd(value)), by = variable] # change manually in tex file
reg_SERVICES$coeftable[, 2] = se_SERVICES$value
reg_SERVICES$coeftable[, 3] = compute_p(reg_SERVICES$coeftable[, 1], reg_SERVICES$coeftable[, 2])
reg_SERVICES$coeftable[, 4] = compute_p(reg_SERVICES$coeftable[, 1], reg_SERVICES$coeftable[, 2])
se_OIL = se[str_detect(variable, "OIL"), .(value = sd(value)), by = variable] # change manually in tex file
reg_OIL$coeftable[, 2] = se_OIL$value
reg_OIL$coeftable[, 3] = compute_p(reg_OIL$coeftable[, 1], reg_OIL$coeftable[, 2])
reg_OIL$coeftable[, 4] = compute_p(reg_OIL$coeftable[, 1], reg_OIL$coeftable[, 2])

etable(reg_TOTAL,
       reg_SERVICES,
       reg_OIL,
       reg_GAS,
       tex = T,
       file = "output/tables/regression_gravity.tex",
       replace = T,
       fitstat = c("n", "pr2"),
       digits = "r2",
       order = c("Iran",
                 "Russia"),
       dict = c(sanctions_russia = "Sanctions on flows to Russia",
                sanctions_russia_rev = "Sanctions on flows from Russia",
                sanctions_iran = "Sanctions on flows to Iran",
                sanctions_iran_rev = "Sanctions on flows from Iran",
                wto = "WTO",
                cu = "Common currency",
                fta = "FTA",
                origin_year = "origin $\\times$ year",
                destination_year = "destination $\\times$ year",
                origin_destination = "origin $\\times$ destination"),
       fontsize = "small",
       headers = c("Goods", "Services", "Oil", "Gas"),
       depvar = F,
       style.tex = style.tex(line.top = "\\toprule",
                             line.bottom = "\\bottomrule",
                             depvar.title = "Dependent variable: ",
                             model.title = "",
                             model.format = "",
                             var.title = "\\midrule",
                             fixef.title = "\\midrule",
                             fixef.suffix = " FE",
                             stats.title = "\\midrule"
       ))
rm(reg_TOTAL,
   reg_SERVICES,
   reg_OIL,
   reg_GAS,
   se_TOTAL,
   se_SERVICES,
   se_OIL,
   se_GAS,
   se, data)


# table appendix: sectoral elasticities ----
elasticity = read_rds("input/initial_conditions_gtap10.rds")$trade_elasticity
setnames(elasticity, "value", "elasticity")
elasticity[, sector := str_to_upper(sector)]
elasticity[sector %in% sectors_services, sector := "SERVICES"]
elasticity = unique(elasticity)

sectorlist = fread("metadata/sectorlist.csv")
sectorlist = sectorlist[, .(`short_description` = str_split_i(description, "\\:", 1)),
                        by = .(sector = code)]
table_elasticities = merge(elasticity,
                           sectorlist,
                           by = "sector",
                           all.x = T)
table_elasticities[sector == "SERVICES", short_description := "Services"]
table_elasticities[sector == "BPH", short_description := str_replace(short_description, "medicinal chemical and botanical products", "etc.")]
table_elasticities[sector == "FMP", short_description := str_remove(short_description, ", except machinery and equipment")]

setnames(table_elasticities, c("Sector", "Elasticity", "Description"))

table_elasticities <- xtable(table_elasticities[, .(Sector, Elasticity, Description)])

align(table_elasticities) = "llcl"
print.xtable(table_elasticities,
             file = str_c("output/tables/elasticities.tex"),
             booktabs = T,
             include.rownames = F,
             floating = F)





# figure 1: plot sectoral trade cost changes (tariff equivalent) ----
coefs = fread("temp/regressions/coefs_bb_clustered_pair.csv")
setnames(coefs, "value", "coef")
coefs = coefs[str_detect(variable, "sanctions")]
coefs[!str_detect(variable, "TOTAL") &
        !str_detect(variable, "SERVICES"), sector := str_sub(variable, -3)]
coefs[!str_detect(variable, "TOTAL") &
        !str_detect(variable, "SERVICES"), variable := str_sub(variable, 1,-5)]
coefs[str_detect(variable, "TOTAL"), sector := "TOTAL"]
coefs[str_detect(variable, "TOTAL"), variable := str_remove(variable, "_TOTAL")]
coefs[str_detect(variable, "SERVICES"), sector := "SERVICES"]
coefs[str_detect(variable, "SERVICES"), variable := str_remove(variable, "_SERVICES")]

# merge with elasticities
coefs = merge(coefs,
              elasticity,
              by = "sector", all.x = T)
rm(elasticity)

# compute tariff equivalents
coefs[sector == "TOTAL", elasticity := 1/5]
coefs[, value := (exp(- coef * elasticity) - 1) * 100]

# merge with sector names
gtap_sectors = fread("metadata/sectorlist.csv")
gtap_sectors[, name := str_split_fixed(description, pattern = "\\:", n = 2)[[1]], by = description]
gtap_sectors[, name := str_remove(name, "Manufacture of ") %>% str_to_sentence]
coefs[, sector_name := match_replace(sector, from = "code", to = "name", dictionary = data.frame(gtap_sectors))]
coefs[sector == "TOTAL", sector_name := "Total"]
coefs[sector == "SERVICES", sector_name := "Services"]
coefs[, sector_name := str_wrap(sector_name, width = 30)]
coefs[, unique(sector_name)]
rm(gtap_sectors)

# clean variable names
coefs[variable == "sanctions_russia", country := "RUS"]
coefs[variable == "sanctions_russia_rev", country := "RUS"]
coefs[variable == "sanctions_iran", country := "IRN"]
coefs[variable == "sanctions_iran_rev", country := "IRN"]
coefs[str_detect(variable, "_rev"), direction := "imports"]
coefs[!str_detect(variable, "_rev"), direction := "exports"]


## Iran exports ----
plot_data = coefs[country == "IRN" & direction == "exports"]
plot_data[value < 0 | is.infinite(value) | is.na(value), value := 0.001]
plot_data[value > 500, value := NA]

# remove total and add gas (if missing)
plot_data = plot_data[sector != "TOTAL"]
plot_data = rbind(plot_data,
                  plot_data[, .(sector = "GAS",
                                variable == "sanctions_iran",
                                value = 0,
                                sector_name = "Gas",
                                direction = "exports")],
                  fill = T)

plot_data[, median := median(value, na.rm = T), by = .(sector, variable)]
plot_data_quantiles = plot_data[, .(min = min(value),
                            max = max(value),
                            lower = quantile(value, 0.05, na.rm = T),
                            upper = quantile(value, 0.95, na.rm = T),
                            median = quantile(value, 0.5, na.rm = T),
                            mean = mean(value),
                            sd = sd(value, na.rm = T)),
                        by = .(sector_name, country, direction)]

plot_data_quantiles[sector_name == "Oil"]

plot = ggplot(plot_data) +
  theme_minimal() + 
  geom_point(aes(x = value,
                 y = reorder(sector_name, median),
                 group = direction,
                 color = direction),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(sector_name, median),
                   group = str_c(direction, sector_name),
                   color = direction,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA,
               width = 0.9) +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(limits = c(0,500)) +
  labs(x = "Tariff-equivalent of\ntrade cost change (in %)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 12, vjust = 0.5))
# plot
ggsave(plot, filename = "output/figures/tariff_equivalent_exports_iran.pdf",
       width = 12.5, height = 34, units = "cm")
ggsave(plot, filename = "output/figures/tariff_equivalent_exports_iran.png",
       width = 12.5, height = 34, units = "cm")


## Iran imports ----
plot_data = coefs[country == "IRN" & direction == "imports"]
plot_data[value < 0 | is.infinite(value) | is.na(value), value := 0.001]
plot_data[value > 500, value := NA]

# remove total
plot_data = plot_data[sector != "TOTAL"]

plot_data[, median := median(value, na.rm = T), by = .(sector, variable)]
plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05, na.rm = T),
                                    upper = quantile(value, 0.95, na.rm = T),
                                    median = quantile(value, 0.5, na.rm = T),
                                    mean = mean(value),
                                    sd = sd(value, na.rm = T)),
                                by = .(sector_name, country, direction)]

plot_data_quantiles[sector_name == "Oil"]

plot = ggplot(plot_data) +
  theme_minimal() + 
  geom_point(aes(x = value,
                 y = reorder(sector_name, median),
                 group = direction,
                 color = direction),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(sector_name, median),
                   group = str_c(direction, sector_name),
                   color = direction,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA,
               width = 0.9) +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(limits = c(0,500)) +
  labs(x = "Tariff-equivalent of\ntrade cost change (in %)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 12, vjust = 0.5))
# plot
ggsave(plot, filename = "output/figures/tariff_equivalent_imports_iran.pdf",
       width = 12.5, height = 34, units = "cm")
ggsave(plot, filename = "output/figures/tariff_equivalent_imports_iran.png",
       width = 12.5, height = 34, units = "cm")


## Russia exports ----
plot_data = coefs[country == "RUS" & direction == "exports"]
plot_data[value < 0 | is.infinite(value) | is.na(value), value := 0.001]
plot_data[value > 500, value := NA]

# remove total and add gas (if missing)
plot_data = plot_data[sector != "TOTAL"]
plot_data = rbind(plot_data,
                  plot_data[, .(sector = "GAS",
                                variable == "sanctions_russia",
                                value = 0,
                                sector_name = "Gas",
                                direction = "exports")],
                  fill = T)

plot_data[, median := median(value, na.rm = T), by = .(sector, variable)]
plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05, na.rm = T),
                                    upper = quantile(value, 0.95, na.rm = T),
                                    median = quantile(value, 0.5, na.rm = T),
                                    mean = mean(value, na.rm = T),
                                    sd = sd(value, na.rm = T)),
                                by = .(sector_name, country, direction)]

plot_data_quantiles[median == max(median, na.rm = T)]

plot = ggplot(plot_data) +
  theme_minimal() + 
  geom_point(aes(x = value,
                 y = reorder(sector_name, median),
                 group = direction,
                 color = direction),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(sector_name, median),
                   group = str_c(direction, sector_name),
                   color = direction,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA,
               width = 0.9) +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(limits = c(0,500)) +
  labs(x = "Tariff-equivalent of\ntrade cost change (in %)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 12, vjust = 0.5))
# plot
ggsave(plot, filename = "output/figures/tariff_equivalent_exports_russia.pdf",
       width = 12.5, height = 34, units = "cm")
ggsave(plot, filename = "output/figures/tariff_equivalent_exports_russia.png",
       width = 12.5, height = 34, units = "cm")


## Russia imports ----
plot_data = coefs[country == "RUS" & direction == "imports"]
plot_data[value < 0 | is.infinite(value) | is.na(value), value := 0.001]
plot_data[value > 500, value := NA]

# remove total
plot_data = plot_data[sector != "TOTAL"]

plot_data[, median := median(value, na.rm = T), by = .(sector, variable)]
plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05, na.rm = T),
                                    upper = quantile(value, 0.95, na.rm = T),
                                    median = quantile(value, 0.5, na.rm = T),
                                    mean = mean(value),
                                    sd = sd(value, na.rm = T)),
                                by = .(sector_name, country, direction)]

plot_data_quantiles[median == max(median, na.rm = T)]

plot = ggplot(plot_data) +
  theme_minimal() + 
  geom_point(aes(x = value,
                 y = reorder(sector_name, median),
                 group = direction,
                 color = direction),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(sector_name, median),
                   group = str_c(direction, sector_name),
                   color = direction,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA,
               width = 0.9) +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(limits = c(0,500)) +
  labs(x = "Tariff-equivalent of\ntrade cost change (in %)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 12, vjust = 0.5))
# plot
ggsave(plot, filename = "output/figures/tariff_equivalent_imports_russia.pdf",
       width = 12.5, height = 34, units = "cm")
ggsave(plot, filename = "output/figures/tariff_equivalent_imports_russia.png",
       width = 12.5, height = 34, units = "cm")
rm(plot_data,
   plot_data_quantiles,
   plot)


# table 2a and 2b: benchmarks ----
type = "balanced"
benchmark = read_rds(str_c("temp/simulations/", type, "/benchmark.rds"))
benchmark = benchmark[scenario %in% c("embargo_russia", "embargo_iran",
                                      "autarky_russia", "autarky_iran")]

## iran ----
benchmark_iran = benchmark[country == "IRN" & str_detect(scenario, "_iran")]
benchmark_iran[, scenario := str_remove(scenario, "_iran")]
benchmark_iran[, country := NULL]

benchmark_iran_bootstrapped = rbind(fread("temp/simulations/balanced/scenario0_iran_clustered_pair.csv"),
                                    fread("temp/simulations/balanced/scenario0b_iran_clustered_pair.csv"))
benchmark_iran_bootstrapped = benchmark_iran_bootstrapped[country == "IRN", .(value = mean(value), sd = sd(value)), by = scenario]
benchmark_iran_bootstrapped[scenario == "benchmark", scenario := "status_quo"]
benchmark_iran_bootstrapped[scenario == "benchmark_global_implementation", scenario := "global_implementation"]

benchmark_iran = rbind(benchmark_iran,
                       benchmark_iran_bootstrapped, fill = T)
benchmark_iran = melt(benchmark_iran, id.vars = "scenario")
benchmark_iran = benchmark_iran[!is.na(value)]

benchmark_iran[scenario %in% c("status_quo", "embargo"), coalition := "Current coalition"]
benchmark_iran[scenario %in% c("global_implementation", "autarky"), coalition := "Global implementation"]

benchmark_iran[scenario %in% c("status_quo", "global_implementation"), policy := "Current measures"]
benchmark_iran[scenario %in% c("embargo", "autarky"), policy := "Complete embargo"]

benchmark_iran[, value := round(value, digits = 2) %>% as.character()]
benchmark_iran[variable == "value", value := str_c(value, " %")]
benchmark_iran[variable == "sd", value := str_c("(", value, ")")]

benchmark_iran = dcast(benchmark_iran, policy + variable ~ coalition, value.var = "value")

benchmark_iran = benchmark_iran[c(2,3,1)]
benchmark_iran[, variable := NULL]
benchmark_iran[2, policy := ""]
setnames(benchmark_iran, "policy", "")

benchmark_iran = xtable(benchmark_iran) #,digits = c(0,0,2,2)
align(benchmark_iran) = "lrcc"
print.xtable(benchmark_iran,
             file = str_c("output/tables/benchmarks_iran.tex"),
             booktabs = T,
             include.rownames = F,
             floating = F)


## russia ----
benchmark_russia = benchmark[country == "RUS" & str_detect(scenario, "_russia")]
benchmark_russia[, scenario := str_remove(scenario, "_russia")]
benchmark_russia[, country := NULL]

benchmark_russia_bootstrapped = rbind(fread("temp/simulations/balanced/scenario0_russia_clustered_pair.csv"),
                                    fread("temp/simulations/balanced/scenario0b_russia_clustered_pair.csv"))
benchmark_russia_bootstrapped = benchmark_russia_bootstrapped[country == "RUS", .(value = mean(value), sd = sd(value)), by = scenario]
benchmark_russia_bootstrapped[scenario == "benchmark", scenario := "status_quo"]
benchmark_russia_bootstrapped[scenario == "benchmark_global_implementation", scenario := "global_implementation"]

benchmark_russia = rbind(benchmark_russia,
                       benchmark_russia_bootstrapped, fill = T)
benchmark_russia = melt(benchmark_russia, id.vars = "scenario")
benchmark_russia = benchmark_russia[!is.na(value)]

benchmark_russia[scenario %in% c("status_quo", "embargo"), coalition := "Current coalition"]
benchmark_russia[scenario %in% c("global_implementation", "autarky"), coalition := "Global implementation"]

benchmark_russia[scenario %in% c("status_quo", "global_implementation"), policy := "Current measures"]
benchmark_russia[scenario %in% c("embargo", "autarky"), policy := "Complete embargo"]

benchmark_russia[, value := round(value, digits = 2) %>% as.character()]
benchmark_russia[variable == "value", value := str_c(value, " %")]
benchmark_russia[variable == "sd", value := str_c("(", value, ")")]

benchmark_russia = dcast(benchmark_russia, policy + variable ~ coalition, value.var = "value")

benchmark_russia = benchmark_russia[c(2,3,1)]
benchmark_russia[, variable := NULL]
benchmark_russia[2, policy := ""]
setnames(benchmark_russia, "policy", "")

benchmark_russia = xtable(benchmark_russia) #,digits = c(0,0,2,2)
align(benchmark_russia) = "lrcc"
print.xtable(benchmark_russia,
             file = str_c("output/tables/benchmarks_russia.tex"),
             booktabs = T,
             include.rownames = F,
             floating = F)


# scenario plots ----
type = "balanced"
# type = "unbalanced"


## scenario 1 ----

### iran ----
data = rbind(fread(str_c("temp/simulations/", type, "/scenario0_iran_clustered_pair.csv")),
             fread(str_c("temp/simulations/", type, "/scenario1_iran_clustered_pair.csv")),
             fread(str_c("temp/simulations/", type, "/scenario1b_iran_clustered_pair.csv")))
data = data[country != "CYP"]

#### incurred costs ----
plot_data = data[country %in% countries_sanctions_iran &
                   !(str_detect(scenario, "XEU"))]
plot_data = plot_data[(scenario == "benchmark" | (str_detect(scenario, "scenario1b") & str_sub(scenario, -3) == country))]
plot_data[scenario != "benchmark", scenario := "unilateral"]
plot_data[scenario == "benchmark", scenario := "multilateral"]

plot_data[, country := countrycode(country, "iso3c", "country.name")]
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value)),
                                by = .(country, scenario)]

# overall numbers
domestic_cost_iran = plot_data[, .(`Loss incurred` = mean(value) %>% round(4) %>% str_c(" %")),
                               by = scenario]
plot_data[, .(median = median(value),
              sd = sd(value)), by = .(country, scenario)][scenario == "multilateral"][order(median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 group = scenario,
                 color = scenario),
             position = position_jitterdodge(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   group = str_c(country, scenario),
                   color = scenario,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Welfare change (in %)",
       y = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_iran_", type, ".pdf"),
       width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_iran_", type, ".png"),
       width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_iran_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_iran_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")
rm(plot,
   plot_data,
   plot_data_quantiles)


#### imposed costs ----
# eu as a whole
plot_data_eu = data[country %in% "IRN" & (str_detect(scenario, "XEU") | scenario == "benchmark"), .(value = mean(value)), by = scenario]
plot_data_eu_multilateral = data.frame(scenario = "multilateral",
                                       value = plot_data_eu[scenario == "benchmark", value] - plot_data_eu[scenario == "scenario1_XEU", value])
plot_data_eu_unilateral = plot_data_eu[scenario == "scenario1b_XEU", .(scenario = "unilateral", value)]
plot_data_eu = rbind(plot_data_eu_multilateral,
                     plot_data_eu_unilateral)
plot_data_eu

# individual plot
plot_data = data[country %in% "IRN" &
                   !(str_detect(scenario, "XEU"))]
plot_data[scenario != "benchmark", country := countrycode(str_sub(scenario, -3), "iso3c", "country.name")]

plot_data_multilateral = merge(plot_data[scenario == "benchmark", .(i, value_benchmark = value)],
                               plot_data[str_detect(scenario, "scenario1_"), .(i, country, value_scenario1 = value)],
                               by = "i")
plot_data_multilateral = plot_data_multilateral[, .(i, country, scenario = "multilateral", value = value_benchmark - value_scenario1)]

plot_data_unilateral = plot_data[str_detect(scenario, "scenario1b_"), .(i, country, scenario = "unilateral", value)]

plot_data = rbind(plot_data_multilateral,
                  plot_data_unilateral)
plot_data[, median := median(value[scenario == "multilateral"], na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value)),
                                by = .(country, scenario)]

# overall numbers
contribution_iran = plot_data[, .(`Loss imposed` = mean(value) %>% round(4) %>% str_c(" %")),
                              by = scenario]

plot_data[, .(median = median(value),
              sd = sd(value)), by = .(country, scenario)][scenario == "multilateral"][order(median)][1:10]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 group = scenario,
                 color = scenario),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   group = str_c(country, scenario),
                   color = scenario,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  scale_x_continuous(breaks = -c(1:10)/10) +
  labs(x = "Welfare change (in %)",
       y = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_iran_", type, ".pdf"),
                              width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_iran_", type, ".png"),
                              width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_iran_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_iran_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")

rm(plot,
   plot_data,
   plot_data_quantiles,
   plot_data_multilateral,
   plot_data_unilateral)

# table
table_iran = merge(domestic_cost_iran,
                   contribution_iran,
                   by = c("scenario"))
table_iran = table_iran[c(2,1)]
setnames(table_iran, "scenario", "")
table_iran = xtable(table_iran,
                    digits = c(0,0,4,4))
align(table_iran) = "lrcc"
print.xtable(table_iran,
             file = str_c("output/tables/scenario1_welfare_change_iran.tex"),
             booktabs = T,
             include.rownames = F,
             floating = F)


### russia ----
data = rbind(fread(str_c("temp/simulations/", type, "/scenario0_russia_clustered_pair.csv")),
             fread(str_c("temp/simulations/", type, "/scenario1_russia_clustered_pair.csv")),
             fread(str_c("temp/simulations/", type, "/scenario1b_russia_clustered_pair.csv")))

# remove faulty iterations
# d = fread(str_c("temp/simulations/", type, "/scenario1b_russia_clustered_pair.csv"))
# d[, .N, by = scenario]

#### incurred costs ----
plot_data = data[country %in% countries_sanctions_russia &
                   (scenario == "benchmark" | (str_detect(scenario, "scenario1b") & str_sub(scenario, -3) == country))]
plot_data[scenario != "benchmark", scenario := "unilateral"]
plot_data[scenario == "benchmark", scenario := "multilateral"]

plot_data[, country := countrycode(country, "iso3c", "country.name")]
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value)),
                                by = .(country, scenario)]

# overall numbers
domestic_cost_russia = plot_data_quantiles[, .(`Loss incurred` = mean(mean) %>% round(4) %>% str_c(" %")),
                                           by = scenario]
plot_data[, .(median = median(value),
              sd = sd(value)), by = .(country, scenario)][scenario == "multilateral"][order(median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 group = scenario,
                 color = scenario),
             position = position_jitterdodge(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   group = str_c(country, scenario),
                   color = scenario,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  scale_x_continuous(breaks = -2*(1:10)/10) +
  labs(x = "Welfare change (in %)",
       y = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_russia_", type, ".pdf"),
                              width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_russia_", type, ".png"),
                              width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_russia_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_incurred_russia_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")

rm(plot_data,
   plot,
   plot_data_quantiles)


#### imposed costs ----
# eu as a whole
plot_data_eu = data[country %in% "RUS" & (str_detect(scenario, "XEU") | scenario == "benchmark"), .(value = mean(value)), by = scenario]
plot_data_eu_multilateral = data.frame(scenario = "multilateral",
                                       value = plot_data_eu[scenario == "benchmark", value] - plot_data_eu[scenario == "scenario1_XEU", value])
plot_data_eu_unilateral = plot_data_eu[scenario == "scenario1b_XEU", .(scenario = "unilateral", value)]
plot_data_eu = rbind(plot_data_eu_multilateral,
                     plot_data_eu_unilateral)
plot_data_eu

# individual plot
plot_data = data[country %in% "RUS" & !str_detect(scenario, "XEU")]

# wrong first iteration for XEU in scenario 1b
plot_data[scenario == "scenario1b_ESP" & i == 1, value := max(value)]
plot_data = unique(plot_data)

plot_data[scenario != "benchmark", country := countrycode(str_sub(scenario, -3), "iso3c", "country.name")]

plot_data_multilateral = merge(plot_data[scenario == "benchmark", .(i, value_benchmark = value)],
                               plot_data[str_detect(scenario, "scenario1_"), .(i, country, value_scenario1 = value)],
                               by = "i")
plot_data_multilateral = plot_data_multilateral[, .(i, country, scenario = "multilateral", value = value_benchmark - value_scenario1)]

plot_data_unilateral = plot_data[str_detect(scenario, "scenario1b_"), .(i, country, scenario = "unilateral", value)]

plot_data = rbind(plot_data_multilateral,
                  plot_data_unilateral)
plot_data[, median := median(value[scenario == "multilateral"], na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value)),
                                by = .(country, scenario)]

# overall numbers
contribution_russia = plot_data_quantiles[, .(`Loss imposed` = mean(mean) %>% round(4) %>% str_c(" %")),
                                          by = scenario]
plot_data[, .(median = median(value),
              sd = sd(value)), by = .(country, scenario)][scenario == "multilateral"][order(median)][1:10]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 group = scenario,
                 color = scenario),
             position = position_jitterdodge(),
             size = 0.005,
             alpha = 0.05) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   group = str_c(country, scenario),
                   color = scenario,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Welfare change (in %)",
       y = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_russia_", type, ".pdf"),
                              width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_russia_", type, ".png"),
                              width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_russia_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario1_costs_imposed_russia_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")

rm(plot,
   plot_data,
   plot_data_quantiles,
   plot_data_multilateral,
   plot_data_unilateral)

# table
table_russia = merge(domestic_cost_russia,
                     contribution_russia,
                     by = c("scenario"))
table_russia = table_russia[c(2,1)]
setnames(table_russia, "scenario", "")
table_russia = xtable(table_russia,
                      digits = c(0,0,4,4))
align(table_russia) = "lrcc"
print.xtable(table_russia,
             file = str_c("output/tables/scenario1_welfare_change_russia.tex"),
             booktabs = T,
             include.rownames = F,
             floating = F)


## scenario 2 ----

# TODO: Add SEs in table

### table: welfare change iran ----
data = rbind(fread("temp/simulations/balanced/scenario2_iran_clustered_pair.csv"),
             fread("temp/simulations/balanced/scenario0_iran_clustered_pair.csv"))

table_data = data[scenario %in% c("benchmark", "scenario2_CHN"),
                  .(country, scenario, value)]
table_data[, country_group := case_when(country == "IRN" ~ "Iran",
                                        country == "CHN" ~ "China",
                                        country %in% countries_sanctions_iran ~ "Current coalition",
                                        TRUE ~ "Rest of the world")]
table_data = table_data[, .(value = mean(value), sd = sd(value)), by = .(country_group, scenario)]
table_data = melt(table_data, id.vars = c("country_group", "scenario"))

table_data[, value := round(value, digits = 4) %>% as.character()]
table_data[value == "7e-04", value := "0.0007"]
table_data[variable == "value", value := str_c(value, " %")]
table_data[variable == "sd", value := str_c("(", value, ")")]

table_data[, scenario := ifelse(scenario == "scenario2_CHN", "incl. China", "Current coalition")]
table_data = dcast(table_data, country_group + variable ~ scenario, value.var = "value")
table_data = table_data[c(5,6, 1,2, 3,7)]
table_data[, variable := NULL]
setnames(table_data, "country_group", "")

table_data = xtable(table_data, align = "rrcc")
print.xtable(table_data,
             file = str_c("output/tables/scenario2_welfare_change_iran.tex"),
             include.rownames = F,
             floating = F)
rm(data, table_data)


### table: welfare change russia ----
data = rbind(fread("temp/simulations/balanced/scenario2_russia_clustered_pair.csv"),
             fread("temp/simulations/balanced/scenario0_russia_clustered_pair.csv"))

table_data = data[scenario %in% c("benchmark", "scenario2_CHN"),
                  .(country, scenario, value)]
table_data[, country_group := case_when(country == "RUS" ~ "Russia",
                                        country == "CHN" ~ "China",
                                        country %in% countries_sanctions_russia ~ "Current coalition",
                                        TRUE ~ "Rest of the world")]
table_data = table_data[, .(value = mean(value), sd = sd(value)),
                        by = .(country_group, scenario)]
table_data = melt(table_data, id.vars = c("country_group", "scenario"))

table_data[, value := round(value, digits = 4) %>% as.character()]
table_data[variable == "value", value := str_c(value, " %")]
table_data[variable == "sd", value := str_c("(", value, ")")]

table_data[, scenario := ifelse(scenario == "scenario2_CHN", "incl. China", "Current coalition")]
table_data = dcast(table_data, country_group + variable ~ scenario, value.var = "value")
table_data = table_data[c(7,8, 1,2, 3, 5)]
table_data[, variable := NULL]
setnames(table_data, "country_group", "")

table_data = xtable(table_data, align = "rrcc")
print.xtable(table_data,
             file = str_c("output/tables/scenario2_welfare_change_russia.tex"),
             include.rownames = F,
             floating = F)
rm(data, table_data)


## scenario 3 ----

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

### map: welfare changes iran ----
data = rbind(fread("temp/simulations/balanced/scenario0_iran_clustered_pair.csv"),
             fread("temp/simulations/balanced/scenario2_iran_clustered_pair.csv"))

map_world_gtap = read_rds("metadata/map_world_gtap.rds")
map_world_gtap <- st_transform(map_world_gtap, crs = "+proj=robin")

plot_data = merge(data[country == "IRN" & !scenario %in% c("scenario2_IRN", "benchmark"),
                       .(i, gtap_code = str_sub(scenario, -3, -1), value)],
                  data[country == "IRN" & scenario == "benchmark", .(i, value_benchmark = value)],
                  by = "i")
plot_data = plot_data[, .(value = mean(value - value_benchmark)), by = gtap_code]

plot_data = merge(plot_data,
                  map_world_gtap,
                  by = "gtap_code", all = TRUE)
plot_data = plot_data[!is.na(gtap_code) & gtap_code != "XTW"]
min_value = plot_data[value == min(value, na.rm = T), value]

plot = ggplot(plot_data) +
  theme_map() +
  geom_sf(aes(fill = -value,
              geometry = geometry), 
          size = 0.05, # size of the border
          color = alpha("#7d7d7d", 1)) + # color of the border
  scale_fill_gradientn(name = "Additional welfare loss (in percentage points)",
                       colors = rev(wes_palette("Zissou1", 100, type = "continuous")),
                       trans = reverselog_trans(10),
                       breaks = c(abs(min_value), 0.1, 0.01, 0.001, 0.0001),
                       labels = c(round(min_value, 2), "-0.1", "-0.01", "-0.001", "-0.0001"),
                       guide = guide_colorbar(title.position = "top",
                                              title.vjust = 0.9)) +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null")
  )
# plot
ggsave(plot, filename = str_c("output/figures/scenario3_welfare_loss_iran_balanced.pdf"),
       width = 20, height = 10, units = "cm")

# table
table_data = plot_data[!is.na(value)][order(value)]
table_data[, geometry := NULL]
table_data[, Country := countrycode(gtap_code, "iso3c", "country.name")]
table_data = table_data[!is.na(Country)][1:10]
table_data = table_data[, .(Country, `Additional welfare change (pp)` = value %>% round(4))]
table_data = xtable(table_data, align = "rrc")
print.xtable(table_data,
             file = str_c("output/tables/scenario3_welfare_change_iran.tex"),
             include.rownames = F,
             floating = F)
rm(data, table_data)


### map: welfare changes russia ----
data = rbind(fread("temp/simulations/balanced/scenario0_russia_clustered_pair.csv"),
             fread("temp/simulations/balanced/scenario2_russia_clustered_pair.csv"))

map_world_gtap = read_rds("metadata/map_world_gtap.rds")
map_world_gtap <- st_transform(map_world_gtap, crs = "+proj=robin")

plot_data = merge(data[country == "RUS" & !scenario %in% c("scenario2_RUS", "benchmark"),
                       .(i, gtap_code = str_sub(scenario, -3, -1), value)],
                  data[country == "RUS" & scenario == "benchmark", .(i, value_benchmark = value)],
                  by = "i")
plot_data = plot_data[, .(value = mean(value - value_benchmark)), by = gtap_code]

plot_data = merge(plot_data,
                  map_world_gtap,
                  by = "gtap_code", all = TRUE)
plot_data = plot_data[!is.na(gtap_code) & gtap_code != "XTW"]
min_value = plot_data[value == min(value, na.rm = T), value]

plot = ggplot(plot_data) +
  theme_map() +
  geom_sf(data = plot_data,
          aes(fill = -value,
              geometry = geometry), 
          size = 0.05, # size of the border
          color = alpha("#7d7d7d", 1)) + # color of the border
  geom_sf(data = plot_data[value > 0 & !is.na(value)],
          aes(geometry = geometry), 
          fill = "lightgrey",
          size = 0.05, # size of the border
          color = alpha("#7d7d7d", 1)) + # color of the border
  scale_fill_gradientn(name = "Additional welfare loss (in percentage points)",
                       colors = rev(wes_palette("Zissou1", 100, type = "continuous")),
                       # breaks = c(-0.001, -0.01, -0.1,
                       #            plot_data[, round(min(value, na.rm = T), 1)]),
                       # labels = str_c(c(-0.001, -0.01, -0.1,
                       #                  plot_data[, round(min(value, na.rm = T), 1)]), "%"),
                       # trans = scales::log_trans(),
                       # limits = c(plot_data[, round(max(value, na.rm = T), 5)], plot_data[, round(min(value, na.rm = T), 1)]),
                       trans = reverselog_trans(10),
                       #labels = scales::percent_format(scale = 1, prefix = "-"),
                       breaks = c(abs(min_value), 0.1, 0.01, 0.001, 0.0001),
                       labels = c(round(min_value, 2), "-0.1", "-0.01", "-0.001", "-0.0001"),
                       guide = guide_colorbar(title.position = "top",
                                              title.vjust = 0.9)) +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null")
  )
# plot
ggsave(plot, filename = str_c("output/figures/scenario3_welfare_loss_russia_balanced.pdf"),
       width = 20, height = 10, units = "cm")

# table
table_data = plot_data[!is.na(value)][order(value)]
table_data[, geometry := NULL]
table_data[, Country := countrycode(gtap_code, "iso3c", "country.name")]
table_data = table_data[!is.na(Country)][1:10]
table_data = table_data[, .(Country, `Additional welfare change (pp)` = value %>% round(4))]
table_data = xtable(table_data, align = "rrc")
print.xtable(table_data,
             file = str_c("output/tables/scenario3_welfare_change_russia.tex"),
             include.rownames = F,
             floating = F)
rm(data, table_data)


## scenario 4 ----

### iran ----
data = fread("temp/simulations/balanced/scenario4_iran_clustered_pair.csv") %>% unique()
data_production = read_rds("temp/initial_conditions/initial_conditions_iran_balanced.rds")

data = data[country != "CYP"]

# transfers
plot_data = data[country %in% countries_sanctions_iran]

plot_data = merge(plot_data,
                  data_production$value_added[, .(country, value_added = value)])

plot_data[, country := countrycode(country, "iso3c", "country.name")]


# absolute
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value),
                                    sd = sd(value)),
                                by = .(country, scenario)]

# total size
plot_data[, .(value = sum(abs(value)) / 2), by = i][, .(mean = mean(value), sd = sd(value))]
plot_data[, .(median = median(value),
              sd = sd(value)), by = .(country)][order(median)]

# US
plot_data_quantiles[country == "United States"]

# top senders
plot_data_quantiles[order(median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 color = scenario,
                 y = reorder(country, -median),
                 group = country),
             position = position_jitter(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   color = scenario,
                   group = country,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Transfers (in million USD)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_iran_", type, ".pdf"),
       width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_iran_", type, ".png"),
       width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_iran_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_iran_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")


# relative
plot_data[, value := value / value_added * 100]
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value),
                                    sd = sd(value)),
                                by = .(country, scenario)]

plot_data_quantiles[order(-median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 color = scenario,
                 group = country),
             position = position_jitter(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   color = scenario,
                   group = country,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Transfers (in % of GDP)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_iran_", type, ".pdf"),
                              width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_iran_", type, ".png"),
                              width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_iran_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_iran_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")

rm(plot,
   plot_data,
   plot_data_quantiles)


### russia ----
data = fread("temp/simulations/balanced/scenario4_russia_clustered_pair.csv") %>% unique()
data_production = read_rds("temp/initial_conditions/initial_conditions_russia_balanced.rds")

# data = data[country != "CYP"]

# transfers
plot_data = data[country %in% countries_sanctions_russia]

plot_data = merge(plot_data,
                  data_production$value_added[, .(country, value_added = value)])

plot_data[, country := countrycode(country, "iso3c", "country.name")]


# absolute
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value),
                                    sd = sd(value)),
                                by = .(country, scenario)]

# total size
plot_data[, .(value = sum(abs(value)) / 2), by = i][, .(mean = mean(value), sd = sd(value))]
plot_data[, .(median = median(value),
              mean = mean(value),
              sd = sd(value)), by = .(country)][order(median)]

# US
plot_data_quantiles[country == "United States"]
plot_data_quantiles[order(median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 color = scenario,
                 group = country),
             position = position_jitter(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   color = scenario,
                   group = country,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Transfers (in million USD)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_russia_", type, ".pdf"),
                              width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_russia_", type, ".png"),
                              width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_russia_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_abs_russia_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")


# relative
plot_data[, value := value / value_added * 100]
plot_data[, median := median(value, na.rm = T), by = country]

plot_data_quantiles = plot_data[, .(min = min(value),
                                    max = max(value),
                                    lower = quantile(value, 0.05),
                                    upper = quantile(value, 0.95),
                                    median = quantile(value, 0.5),
                                    mean = mean(value),
                                    sd = sd(value)),
                                by = .(country, scenario)]
plot_data_quantiles[order(-median)]

plot = ggplot() +
  theme_minimal() + 
  geom_point(data = plot_data,
             aes(x = value,
                 y = reorder(country, -median),
                 color = scenario,
                 group = country),
             position = position_jitter(),
             size = 0.01,
             alpha = 0.1) +
  geom_boxplot(data = plot_data_quantiles,
               stat = "identity",
               aes(y = reorder(country, -median),
                   color = scenario,
                   group = country,
                   xmiddle = median,
                   xlower = lower,
                   xupper = upper,
                   xmin = min,
                   xmax = max
               ),
               fill = NA,
               outlier.shape = NA) +
  scale_color_viridis_d(end = 0.6) +
  labs(x = "Transfers (in % of GDP)",
       y = NULL) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank())
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_russia_", type, ".pdf"),
       width = 10, height = 20, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_russia_", type, ".png"),
       width = 10, height = 20, units = "cm")

# flip coords for presentation
plot = plot +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# plot
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_russia_", type, "_flipped.pdf"),
       width = 20, height = 10, units = "cm")
ggsave(plot, filename = str_c("output/figures/scenario4_transfers_rel_russia_", type, "_flipped.png"),
       width = 20, height = 10, units = "cm")

rm(plot,
   plot_data,
   plot_data_quantiles)


## scenario 5 ----

# load map
map_world_gtap = read_rds("metadata/map_world_gtap.rds")
map_world_gtap <- st_transform(map_world_gtap, crs = "+proj=robin")

# load gdp data
data_gdp = fread("input/gdp_ppp/API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_5455171.csv", skip = 2, header = T)
data_gdp = data_gdp[, .(country = `Country Code`, `2011`, `2013`)]
data_gdp = melt(data_gdp, id.var = "country", variable.name = "year", value.name = "gdp_ppp")
data_gdp = data_gdp[!is.na(country) & !is.na(gdp_ppp)]
data_gdp[, country := match_replace(country,
                                    from = "comtrade_code", to = "gtap_code",
                                    dictionary = concordance_countries), by = country]
data_gdp = data_gdp[!is.na(country)]
data_gdp = data_gdp[, .(gdp_ppp = sum(gdp_ppp, na.rm = T)), by = .(country, year)]


### iran ----

# load costs
costs = fread("temp/simulations/balanced/scenario5_iran.csv")

# load initial conditions
initial_conditions_iran = read_rds("temp/initial_conditions/initial_conditions_iran_balanced.rds")

all_countries = initial_conditions_iran$consumption_share[, unique(country)]
coalition_countries = c()
cost_list = data.table()
iter = 1

# first round: unilateral
while (iter <= length(all_countries)) {
  
  # get current iteration and last iteration with winning scenario
  if (iter == 1) {
    this_costs = costs[(i == iter)]
    cost_incurred = this_costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value)]
    cost_imposed = this_costs[(i == iter & country == "IRN"), .(country, scenario, value)]
  } else {
    this_costs = costs[(i == iter) |
                         (i == (iter - 1) & str_sub(scenario, -3, -1) == coalition_countries[length(coalition_countries)])]
    cost_incurred = merge(this_costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value_new = value)],
                          this_costs[(i == (iter - 1)), .(country, value_old = value)],
                          by = "country")
    cost_incurred = cost_incurred[, .(country, value = value_new - value_old)]
    
    cost_imposed = merge(this_costs[(i == iter & country == "IRN"), .(country, scenario, value_new = value)],
                         this_costs[(i == (iter - 1) & country == "IRN"), .(country, value_old = value)],
                         by = "country")
    cost_imposed = cost_imposed[, .(country, scenario, value = value_new - value_old)]
  }
  
  cost_incurred = merge(cost_incurred,
                        data_gdp[year == 2011, .(country, gdp_ppp)],
                        by = "country",
                        all.x = T)
  cost_incurred[, value := value * gdp_ppp]
  cost_incurred[, gdp_ppp := NULL]
  
  cost_imposed = merge(cost_imposed,
                       data_gdp[year == 2011, .(country, gdp_ppp)],
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
    this_country = cost_combined[value_incurred > 0][order(-value), country][1]
    coalition_countries = append(coalition_countries,
                                 this_country)
  } else {
    this_country = cost_combined[order(-value), country][1]
    coalition_countries = append(coalition_countries,
                                 this_country)
  }
  cost_list = rbind(cost_list,
                    cost_combined[country == this_country, .(i = iter, country, ratio = value)])
  
  iter = iter + 1
}
rm(this_costs,
   this_country,
   cost_imposed,
   cost_incurred,
   cost_combined)

# combine costs and plot
plot_data = merge(cost_list,
                  costs[country == "IRN", .(i, country = str_sub(scenario, -3, -1), value)],
                  by = c("i", "country"))

plot_data = plot_data[, .(gtap_code = country, value, optimal = F, actual = F)]
plot_data[, i := .I]

plot_data[1:plot_data[value > (-1.91), max(i)+1], optimal := T] # find country that pushes over threshold
plot_data[gtap_code %in% countries_sanctions_iran, actual := T]
plot_data[, type := character()]
plot_data[actual == T, type := "actual"]
plot_data[optimal == T & actual == T, type := "both"]
plot_data[optimal == T & actual == F, type := "optimal"]

# number of countries in each group
plot_data[, .N, by = type]

# china fills bucket quickly, who would be next 10? 7 of the next ten are in actual coalition!
plot_data[plot_data[value > (-1.91), max(i)+2]:(plot_data[value > (-1.91), max(i)+2]+10),
          .(country = countrycode(gtap_code, "iso3c", "country.name"), type)]

plot_data = plot_data[, .(gtap_code, type)]
plot_data = merge(plot_data,
                  map_world_gtap,
                  by = "gtap_code", all = TRUE)
plot_data = plot_data[!is.na(gtap_code) & gtap_code != "XTW"]

plot = ggplot(plot_data) +
  theme_map() +
  geom_sf(aes(fill = type,
              geometry = geometry), 
          size = 0.05, # size of the border
          color = alpha("#7d7d7d", 1)) + # color of the border
  scale_fill_manual(name = NULL,
                    values = c("#39568C", "#FDE725", "#29AF7F"),
                    labels = c("Optimal", "Actual", "Both"),
                    breaks = c("optimal", "actual", "both")) +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null")
  )
# plot
ggsave(plot, filename = str_c("output/figures/scenario5_iran_balanced.pdf"),
       width = 20, height = 10, units = "cm")


### russia ----

# load costs
costs = fread("temp/simulations/balanced/scenario5_russia.csv")

# load initial conditions
initial_conditions_russia = read_rds("temp/initial_conditions/initial_conditions_russia_balanced.rds")

all_countries = initial_conditions_russia$consumption_share[, unique(country)]
coalition_countries = c()
cost_list = data.table()
iter = 1

# first round: unilateral
while (iter <= length(all_countries)) {
  
  # get current iteration and last iteration with winning scenario
  if (iter == 1) {
    this_costs = costs[(i == iter)]
    cost_incurred = this_costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value)]
    cost_imposed = this_costs[(i == iter & country == "RUS"), .(country, scenario, value)]
  } else {
    this_costs = costs[(i == iter) |
                         (i == (iter - 1) & str_sub(scenario, -3, -1) == coalition_countries[length(coalition_countries)])]
    cost_incurred = merge(this_costs[(i == iter & str_sub(scenario, -3, -1) == country), .(country, value_new = value)],
                          this_costs[(i == (iter - 1)), .(country, value_old = value)],
                          by = "country")
    cost_incurred = cost_incurred[, .(country, value = value_new - value_old)]
    
    cost_imposed = merge(this_costs[(i == iter & country == "RUS"), .(country, scenario, value_new = value)],
                         this_costs[(i == (iter - 1) & country == "RUS"), .(country, value_old = value)],
                         by = "country")
    cost_imposed = cost_imposed[, .(country, scenario, value = value_new - value_old)]
  }
  
  cost_incurred = merge(cost_incurred,
                        data_gdp[year == "2013", .(country, gdp_ppp)],
                        by = "country",
                        all.x = T)
  cost_incurred[, value := value * gdp_ppp]
  cost_incurred[, gdp_ppp := NULL]
  
  cost_imposed = merge(cost_imposed,
                       data_gdp[year == "2013", .(country, gdp_ppp)],
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
    this_country = cost_combined[value_incurred > 0][order(-value), country][1]
    coalition_countries = append(coalition_countries,
                                 this_country)
  } else {
    this_country = cost_combined[order(-value), country][1]
    coalition_countries = append(coalition_countries,
                                 this_country)
  }
  cost_list = rbind(cost_list,
                    cost_combined[country == this_country, .(i = iter, country, ratio = value)])
  
  iter = iter + 1
}
rm(this_costs,
   this_country,
   cost_imposed,
   cost_incurred,
   cost_combined)

# combine costs and plot
plot_data = merge(cost_list,
                  costs[country == "RUS", .(i, country = str_sub(scenario, -3, -1), value)],
                  by = c("i", "country"))

plot_data = plot_data[, .(gtap_code = country, value, optimal = F, actual = F)]
plot_data[, i := .I]

plot_data[1:plot_data[value > (-1.45), max(i)+1], optimal := T] # find country that pushes over threshold
plot_data[gtap_code %in% countries_sanctions_russia, actual := T]
plot_data[, type := character()]
plot_data[actual == T, type := "actual"]
plot_data[optimal == T & actual == T, type := "both"]
plot_data[optimal == T & actual == F, type := "optimal"]

# number of countries in each group
plot_data[, .N, by = type]

# who would be next 10? 5 of the next ten are in actual coalition!
plot_data[plot_data[value > (-1.45), max(i)+2]:(plot_data[value > (-1.45), max(i)+2]+10),
          .(country = countrycode(gtap_code, "iso3c", "country.name"), type)]

plot_data = plot_data[, .(gtap_code, type)]
plot_data = merge(plot_data,
                  map_world_gtap,
                  by = "gtap_code", all = TRUE)
plot_data = plot_data[!is.na(gtap_code) & gtap_code != "XTW"]

# plot
plot = ggplot(plot_data) +
  theme_map() +
  geom_sf(aes(fill = type,
              geometry = geometry), 
          size = 0.05, # size of the border
          color = alpha("#7d7d7d", 1)) + # color of the border
  scale_fill_manual(name = NULL,
                    values = c("#39568C", "#FDE725", "#29AF7F"),
                    labels = c("Optimal", "Actual", "Both"),
                    breaks = c("optimal", "actual", "both")) +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null")
  )
# plot
ggsave(plot, filename = str_c("output/figures/scenario5_russia_balanced.pdf"),
       width = 20, height = 10, units = "cm")


# additional maps for presentations ----
pacman::p_load(rnaturalearth)
pacman::p_load(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

## iran ----
coords_iran <- c(left = 5, bottom = 5, right = 100, top = 60)
plot = ggplot(data = world) +
  theme_map() +
  geom_sf(fill = "white", size = 0.2, color = "black") +
  geom_sf(data = world[world$iso_a3 == "IRN",],
          fill = "lightgrey", size = 0.2, color = "black") +
  coord_sf(xlim = coords_iran[c(1,3)], ylim = coords_iran[c(2,4)], expand = FALSE) +
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"))
plot
ggsave(plot, filename = "output/figures/map_iran.pdf",
       width = 23, height = 15, units = "cm")
ggsave(plot, filename = "output/figures/map_iran.png",
       width = 23, height = 15, units = "cm")
rm(coords_iran,
   plot)

## russia ----
coords_russia <- c(left = -10, bottom = 10, right = 180, top = 85)
plot = ggplot(data = world) +
  theme_map() +
  geom_sf(fill = "white", size = 0.2, color = "black") +
  geom_sf(data = world[world$iso_a3 == "RUS",],
          fill = "lightgrey", size = 0.2, color = "black") +
  coord_sf(xlim = coords_russia[c(1,3)], ylim = coords_russia[c(2,4)], expand = FALSE) +
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"))
plot
ggsave(plot, filename = "output/figures/map_russia.pdf",
       width = 23, height = 15, units = "cm")
ggsave(plot, filename = "output/figures/map_russia.png",
       width = 23, height = 15, units = "cm")


## china ----
coords_china <- c(left = -10, bottom = 10, right = 180, top = 85)
plot = ggplot(data = world) +
  theme_map() +
  geom_sf(fill = "white", size = 0.2, color = "black") +
  geom_sf(data = world[world$iso_a3 == "CHN",],
          fill = "lightgrey", size = 0.2, color = "black") +
  coord_sf(xlim = coords_china[c(1,3)],
           ylim = coords_china[c(2,4)], expand = FALSE) +
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"))
plot
ggsave(plot, filename = "output/figures/map_china.pdf",
       width = 23, height = 15, units = "cm")
ggsave(plot, filename = "output/figures/map_china.png",
       width = 23, height = 15, units = "cm")


## africa ----
african_countries <- c(
  "DZA",  # Algeria
  "AGO",  # Angola
  "BEN",  # Benin
  "BWA",  # Botswana
  "BFA",  # Burkina Faso
  "BDI",  # Burundi
  "CPV",  # Cape Verde
  "CMR",  # Cameroon
  "CAF",  # Central African Republic
  "TCD",  # Chad
  "COM",  # Comoros
  "COG",  # Congo
  "COD",  # Democratic Republic of the Congo
  "DJI",  # Djibouti
  "EGY",  # Egypt
  "GNQ",  # Equatorial Guinea
  "ERI",  # Eritrea
  "SWZ",  # Eswatini
  "ETH",  # Ethiopia
  "GAB",  # Gabon
  "GMB",  # Gambia
  "GHA",  # Ghana
  "GIN",  # Guinea
  "GNB",  # Guinea-Bissau
  "CIV",  # Ivory Coast
  "KEN",  # Kenya
  "LSO",  # Lesotho
  "LBR",  # Liberia
  "LBY",  # Libya
  "MDG",  # Madagascar
  "MWI",  # Malawi
  "MLI",  # Mali
  "MRT",  # Mauritania
  "MUS",  # Mauritius
  "MAR",  # Morocco
  "MOZ",  # Mozambique
  "NAM",  # Namibia
  "NER",  # Niger
  "NGA",  # Nigeria
  "RWA",  # Rwanda
  "STP",  # Sao Tome and Principe
  "SEN",  # Senegal
  "SYC",  # Seychelles
  "SLE",  # Sierra Leone
  "SOM",  # Somalia
  "ZAF",  # South Africa
  "SSD",  # South Sudan
  "SDN",  # Sudan
  "TZA",  # Tanzania
  "TGO",  # Togo
  "TUN",  # Tunisia
  "UGA",  # Uganda
  "ZMB",  # Zambia
  "ZWE"   # Zimbabwe
)

coords_africa <- c(left = -80, bottom = -10, right = 130, top = 65)
plot = ggplot(data = world) +
  theme_map() +
  geom_sf(fill = "white", size = 0.2, color = "black") +
  geom_sf(data = world[world$iso_a3 %in% african_countries,],
          fill = "lightgrey", size = 0.2, color = "black") +
  coord_sf(xlim = coords_africa[c(1,3)],
           ylim = coords_africa[c(2,4)], expand = FALSE) +
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000"))
plot
ggsave(plot, filename = "output/figures/map_africa.pdf",
       width = 23, height = 15, units = "cm")
ggsave(plot, filename = "output/figures/map_africa.png",
       width = 23, height = 15, units = "cm")
