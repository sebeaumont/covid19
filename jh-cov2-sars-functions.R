library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

# Made using the R language platform, tidyverse and RStudio - This
# work dedicated to all open source heroes the world over and
# celebrating the industry of so many volunteers who are willing to
# toil and share their remarkable work for the common good.  I salute
# you.

#############################################################################
# Copyright (C) 2020 Simon Beaumont 
# Released under terms of GNU GPLv3 see: LICENSE.md for details
# ---------------------------------------------------------------------------
# Research & Entertainnmet Purposes Only (un-reviewed methodology)
# ===========================================================================
# Caution: Do not base any real world decisions of this data exploration.
# All source data is Copyright (C) 2020 John Hopkins University.
# All uses of graphics must include citation caption. 
# All re-distributions this notice and LICENSE verbatim.
#############################################################################

# The author accepts all repsonsibility for errors and omissions
# and welcomes PRs for improvements and better ideas.
# Created: Apr 2020 Simon Beaumont

###################
## Data wrangling #
###################

## Population Data 
## from 2018 WorldBank held locally is joined with JH data
## to do per capita calculations

get_population_data <- function () { 
  read_csv("population.csv",
           col_types = cols(.default = col_double(),
                           `Country Name` = col_character(),
                           `Country Code` = col_character(),
                           `Indicator Name` = col_character()),
           col_names = TRUE) %>% 
    select(`Country Name`, `2018`) %>% 
    rename(country=`Country Name`,population=`2018`) %>%
    mutate(country=replace(country, country=="United States", "US"))
}

## growth ratio computation - take sqrt to dilute todo make nth root a paramter
safe_ratio <- function(b,a) { 
  ifelse(a==0, 0, ifelse(a<0, -sqrt(abs(b/a)), sqrt(abs(b/a))))
}

## Use [John Hopkins Repo](https://github.com/CSSEGISandData/COVID-19) to get the pandemic data
get_time_series_covid19_confirmed_global <- function(population_table, growth_function, source_uri) {
  
    ## read data
    read_csv(source_uri,
             col_types = cols(.default = col_double(),
                              `Province/State` = col_character(),
                              `Country/Region` = col_character())) %>%
     # rename columns
    rename(province = "Province/State", country = "Country/Region") %>%
    # drop columns
    select(-c(Lat, Long)) %>%
    # tidy to long table
    pivot_longer(-c(province, country), 
                  names_to = "date",
                  # this maybe needs a format string to convert - see below
                  names_ptypes = c("date", date),
                  values_to = "cumulative_cases") %>%
    # as this is time series data convert from US colloquial date format to a proper date type
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    # get data in shape for summarizing by administrative region/country
    arrange(date, country, province) %>% group_by(date, country) %>%
    # total to date by country
    summarise(Total=sum(cumulative_cases)) %>% ungroup() %>%
    arrange(country) %>% group_by(country) %>%
    # get the delta of new cases (discrete derivative) 
    mutate(new_cases = c(0, diff(Total))) %>% ungroup() %>% 
    # join population by country
    inner_join(population_table, by=c("country")) %>% 
    # add new columns for growth and PerCapita total cases to date
    mutate(growth = growth_function(new_cases,Total), PerCapita = Total/population) 
}

ensure_data <- function (population_table, growth_fn) {
    ## remote and local cache for data
    jhcsse_github_cov2_conf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    cov2_conf_local <- "covid19_time_series_confirmed_global_wrangled.tsv"

    tryCatch({
        data <- get_time_series_covid19_confirmed_global(population_table,
                                                         growth_fn,
                                                         jhcsse_github_cov2_conf)
        write_tsv(data, cov2_conf_local)
        return(data)
    },
    error = function(cond) {
        message(paste("error: ", cond, " reading url: ", jhcsse_github_cov2_conf))
        message(paste("attempting failover from cached file: ", cov2_conf_local))
        read_tsv(cov2_conf_local);
    })
}

#############
## Plotting #
#############

plot_confirmed_cases_growth <- function (data) {
  ## 8 custom colours 
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

  ## Required under terms of distibution please.
  caption <- paste("Data provided by Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                   "\nWrangling and visualization by Simon Beaumont <datalligator@icloud.com>")

  ## Gorgeous ggplot..
  ggplot(data, aes(x=date,y=growth,colour=country,group=country)) + 
    # geom_line(size=2, alpha=0.3) + 
    labs(title="SARS-CoV-2 Confirmed Cases", subtitle="(starting when more than 100 cases)",
        x="2020", y="Growth (new/total)", color="Region", points="Total Cases", caption=caption) +
    geom_point(aes(size=PerCapita), alpha=0.7) +
    geom_smooth(method='loess', formula='y ~ x', size=1, alpha=0.2) +
    #geom_text(aes(label=round(Total/1000, digits=0)), hjust=0, vjust=0) +
    scale_color_manual(values=cbPalette)
}


#######################
## Parameter selection
#######################

get_significant_caseload <- function (data, threshold) {
  data %>% filter(Total>threshold)
}

## unique set of countries from data
get_countries <- function (data) {
  data %>% select(country) %>% unique()
}

## output plotter
plotter <- function (p) {
  png(file="cov19-growth.png", width=1440, height=900)
  plot(p)
  dev.off()
}

#######
## Run 
#######

s3 <- ensure_data(get_population_data(), safe_ratio)

s6 <- s3 %>% get_significant_caseload(500)

## get starting date /UI
starting_date <- as.Date("20-02-14", format="%y-%m-%d")

## TODO choose up to 8 from selectable_countries

## selected countries -- choose up to 8 from population$country
selectable_countries <- s6 %>% get_countries()

countries <- c("US", "China", "United Kingdom","Germany", "Italy", "France", "Sweden")

s7 <- s6 %>% filter(country %in% countries, date > starting_date)

plotter(plot_confirmed_cases_growth(s7))



