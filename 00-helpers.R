################################################
# libraries
################################################
library(tidyverse)
library(lubridate)
library(vroom)
library(zoo)

library(skimr)
library(janitor)
library(leaflet)
library(patchwork)
library(feather)


# Final prices
#   Price = $/Mwh

# FTRs
#   Price = price per *hour* ?
#   AcquisitionCost = Price * days in the month * 24 * MW

# Price is the same across FTR Source Sink AuctionName


################################################
# Set up
################################################
NODE_ENDS_WITH <- '2201'

################################################
# Read data
################################################

read_data <- function() {
  ftrs            <<- read_ftrs()
  final_prices    <<- read_final_prices()
  asx_hedges_aggr <<- read_asx_hedges_aggr()
  nodes           <<- read_nodes()
}

read_ftrs <- function() {
  print("Reading ftrs.csv")
  read_csv("data/ftrs.csv", col_types = 'dccDDccccccddddDcdD')
}

read_final_prices <- function() {
  print("Reading final_prices.csv")
  vroom("data/final_prices.csv", col_types='DdcdcT')
}

read_asx_hedges_aggr <- function() {
  print("Reading asx_hedges_aggr.csv")
  read_csv("data/asx_hedges_aggr.csv", col_types='Dcdc')
}

read_nodes <- function() {
  print("Reading nodes.csv")
  read_csv("data/nodes.csv", col_types='ccc')
}

##########################################
# Data munging and filtering functions
##########################################

# work out the number of years back to go when looking at ftr data
get_start_year <- function(ftr_year) { ymd(paste(as.numeric(ftr_year) - 2,1,1,sep='0')) }
get_end_year   <- function(ftr_year) { ymd(paste(as.numeric(ftr_year) + 1,1,1,sep='0')) }

#
# Calculate the price difference between *two* nodes
# used by shiny app and for interactive
#
create_final_node_price_differences <- function(final_src_snk_year) {

  # from the doco "Relevant price differences for FTR options can, of course, never be negative."
  final_src_snk_year %>% pivot_wider(names_from  = Node,
                                     values_from = Price) %>%
                         mutate(
                                price_source_sink_diff = reduce(across(ends_with(NODE_ENDS_WITH)), `-`),
                                price_sink_source_diff = -1 * price_source_sink_diff,
                                trading_month          = floor_date(Trading_date, "month"),
                                price_diff_positive    = ifelse(price_sink_source_diff > 0, price_sink_source_diff, 0)
                               ) %>%
                         group_by(trading_month) %>%
                         summarise(
                                   price_source_sink_aggr = mean(price_source_sink_diff),
                                   price_sink_source_aggr = mean(price_sink_source_diff),
                                   price_positive_aggr    = mean(price_diff_positive, na.rm = TRUE)
                                  ) %>%
                         ungroup %>%
                         mutate(price_sink_source_rolling = rollmean(price_sink_source_aggr, k = 7, fill = NA)) # rolling weekly
}

#
# Retrieve only the years and nodes we are interested in
#
filter_price_data <- function() {

  ftrs_src_snk_year  <- ftrs %>%
                          filter(year(StartDate) == ftr_year & Source == source_node & Sink == sink_node)

  final_src_snk_year <- final_prices %>%
                          filter(year(Trading_date) <= ftr_year & Node %in% c(source_node_poc, sink_node_poc))

  return(list(ftrs_src_snk_year, final_src_snk_year))
}

