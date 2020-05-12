source("jh-cov2-sars-functions.R")

## output plotter
plotter <- function (p) {
  png(file="cov19-growth.png", width=1440, height=900)
  plot(p)
  dev.off()
}

#######
## Run 
#######

s3 <- ensure_data() %>%
    calculate_population_stats(get_population_table()) %>%
    apply_growth_function(safe_div)

s6 <- s3 %>% get_significant_caseload(300)

## get starting date /UI
starting_date <- as.Date("20-02-14", format="%y-%m-%d")

## selected countries -- choose up to 8 from population$country
## selectable_countries <- s6 %>% get_countries()

countries <- c("US", "Russia", "United Kingdom", "Germany", "Italy", "Poland", "Sweden")

s7 <- s6 %>% filter(country %in% countries, date > starting_date)

plotter(plot_confirmed_cases_growth(s7))
