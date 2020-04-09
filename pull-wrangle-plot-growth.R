# making grateful use of...
library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

# and the wonderful R platform and RStudio - dedicated to all open source heros the
# world over - we should celebrate the industry of so many volunteers who are
# willing to toil and share their remarkable work.

#############################################################################
# Copyright (C) 2020 Simon Beaumont 
# Released under terms of GNU GPLv3 see: LICENSE.md for details
# ---------------------------------------------------------------------------
# Research & Entertainnmet Purposes Only (unreviewed methodology)
# ===========================================================================
# Caution: Do not base any real world decisions of this data exploration.
# All source data is Copyright (C) 2020 John Hopkins University.
# All uses of graphics must include citation caption. 
# All re-distributions this notice and LICENSE verbatim.
#############################################################################

# The author accepts all repsonsibility for errors and omissions
# and gratefully receives PRs for improvements and better ideas.
# Created: Apr 2020 Simon Beaumont

# Subject to plotting preferecnces this script should work merely invoking R from
# the command line -- see: below. RStudio is more fun.

# N.B. I save most of the major intermediate transformations on data
# so I can back up without running the whole thing over.
# YMMV so if required chain all the wrangling functions together to save memory.

# pull the latest from JH repo
system2("git", c("pull", "upstream", "master"))

# read the confirmed data from the local git repo -- TODO xxx sort date repr here?
time_series_covid19_confirmed_global <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                                                 col_types = 
                                                   cols(
                                                      .default = col_double(),
                                                      `Province/State` = col_character(),
                                                      `Country/Region` = col_character()
                                                    ))

# rename some columns
s1 = time_series_covid19_confirmed_global %>% 
      rename(province = "Province/State", country_region = "Country/Region")

# Since I'm not doing GIS or maps -- YMMV
s1$Lat <- NULL
s1$Long <- NULL

# pivot to a long table so we can make sense of it
s2 <-  s1 %>% pivot_longer(-c(province, country_region), 
                           names_to = "date",
                           # this maybe needs a format string to convert - see below
                           names_ptypes = c("date", date),
                           values_to = "cumulative_cases")

# this is time series after all with US coloquial date format - so sort that out now.
s2$date=as.Date(s2$date, format="%m/%d/%y")

##############################
# get data in shape for summarizing by adminstrative region or nation state - 
# not too worried about geography here tho' it is a consideration. Need GIS and Lat long back for that.

s3 <- s2 %>% arrange(date, country_region, province) %>% group_by(date, country_region)

s3a <- s3 %>% summarise(Total=sum(cumulative_cases)) %>% ungroup()

s3b <- s3a %>% arrange(country_region) %>% group_by(country_region)

# get the delta of new cases (discrete derivative) 
s4 <-  s3b %>% mutate(new_cases = c(0, diff(Total))) %>% ungroup()

###############################
# TODO: 
# - I'd also like to normalise with the population and the population density.
# - Second differences 
# - Proper model

# N.B. now copes with negative growth ratio (though not an issue at time of writing :()
safe_ratio <- function(b,a) { 
  ifelse(a==0, 0, ifelse(a<0, -sqrt(abs(b/a)), sqrt(abs(b/a))))
}

s5 <- s4 %>% mutate(growth = safe_ratio(new_cases,Total))

###########
# plotting 

# selected countries...
countries <- c("US", "China", "United Kingdom","Germany", "Italy", "France", "Sweden")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

# wait until there's a few cases as acceleration is hard in the early phases
s6 <- s5 %>% filter(country_region %in% countries, date > as.Date("2020-02-14"), Total > 100)

caption <- paste("Data provided by Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)",
                 "\nWrangling and visualization by Simon Beaumont <datalligator@icloud.com>")

# the gorgeoous ggplot..
p1 <- ggplot(s6, aes(x=date,y=growth,colour=country_region,group=country_region)) + 
  # geom_line(size=2, alpha=0.3) + 
  labs(title="SARS-CoV-2 Confirmed Cases", subtitle="(more than one hunderd cases)",
       x="2020", y="Growth (new/total)", color="Region", points="Total Cases", caption=caption) +
  geom_point(aes(size=Total)) +
  geom_smooth(method='loess', formula='y ~ x', size=2, alpha=0.2) +
  #geom_text(aes(label=round(Total/1000, digits=0)), hjust=0, vjust=0) +
  scale_color_manual(values=cbPalette)

#plot(p1)

# YMMV
plotter <- function (p) {
  png(file="cov19-growth.png", width=1440, height=900)
  plot(p)
  dev.off()
}

plotter(p1)
