library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(feather)


# Final prices
# Price = $/Mwh

# FTRs
# Price = price per *hour* ?
# AcquisitionCost = Price * days in the month * 24 * MW


################
# Set up
################

ftr_year <- 2018

node_ends_with  <- '2201'
source_node     <- "BEN"
sink_node       <- "OTA"
source_node_poc <- str_glue(source_node, node_ends_with)
sink_node_poc   <- str_glue(sink_node,   node_ends_with)

ftr_cols <- cols(
  FTR_ID                  = col_double(),
  AuctionName             = col_character(),
  StartDate               = col_date(format = "%d/%m/%Y"),
  EndDate                 = col_date(format = "%d/%m/%Y"),
  CurrentOwner            = col_character(),
  PreviousOwner           = col_character(),
  FirstOwner              = col_character(),
  HedgeType               = col_character(),
  Source                  = col_character(),
  Sink                    = col_character(),
  MW                      = col_double(),
  Price                   = col_double(),
  AcquisitionCost         = col_double(),
  OriginalAcquisitionCost = col_double(),
  DateAcquired            = col_date(format = "%d/%m/%Y"),
  Status                  = col_character(),
  AllocationPlan          = col_double()
)

################
# Read data
################

ftrs         <- read_csv("data/FTR_Register.csv", col_types = ftr_cols)
final_prices <- read_feather("../nzfpg-explorer/data/finalprices.feather")

################
# Clean and filter
################

ftrs <- ftrs %>% separate(AuctionName, '_', into = c("auction_type"), extra = "drop", remove = FALSE) %>%
                 mutate(auction_year_month = floor_date(DateAcquired, unit = 'month'),
                        auction_type = ifelse(is.na(auction_type), Status, auction_type))

ftrs_src_snk_year  <- ftrs         %>% filter(year(StartDate) == ftr_year & Source == source_node & Sink == sink_node)
final_src_snk_year <- final_prices %>% filter(year(Trading_date) == ftr_year & Node %in% c(source_node_poc, sink_node_poc))

final_node_differences <- final_src_snk_year %>%
                            mutate(trading_month = floor_date(Trading_date, "month")) %>%
                            group_by(trading_month, Node) %>%
                            summarise(price_average = mean(Price)) %>%
                            pivot_wider(names_from = Node, values_from = price_average) %>%
                            mutate(price_difference = -1 * reduce(across(ends_with("2201")), `-`)) %>%
                            ungroup

ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE) +
                        geom_line(data = final_node_differences, aes(x = trading_month, y = price_difference), colour = "red")

#####
# Scratch
#####

reports <- function() {
  ftrs_src_snk <- ftrs %>%
                    filter(Source == source_node & Sink == sink_node & Price > 0 & HedgeType == 'OPT') %>%
                    mutate(start_year = year(StartDate))

  ftrs_src_snk_aggr  <- ftrs_src_snk %>%
                          group_by(DateAcquired, CurrentOwner, start_year) %>%
                          summarise(mean_price = mean(Price), max_price = max(Price))

  ftrs_src_snk_aggr %>% ggplot(aes(x = DateAcquired, y = max_price, colour = start_year, group = start_year)) +
                          geom_point() +
                          geom_smooth() +
                          facet_wrap(~CurrentOwner)


                  # filter(Source == source_node & Sink == sink_node & year(StartDate) == ftr_year & Price > 0 & HedgeType == 'OPT') %>%
  ftrs_snk_src_snk <- ftrs %>% filter(Source %in% c(sink_node, source_node) & Sink %in% c(source_node, sink_node) & MW > 0 & HedgeType == 'OPT')

  ftrs_snk_src_snk_aggr  <- ftrs_snk_src_snk %>%
                              group_by(StartDate, Source, Sink) %>%
                              filter(DateAcquired == max(DateAcquired)) %>%
                              summarise(aggr_price = max(Price)) %>%
                              ungroup

  # FTR Service Provider Graphs

  # Assignments Traded
  ftrs %>% filter(Status == 'ASSIGNED') %>% group_by(DateAcquired) %>% summarise(count_per_auction = n()) %>% as.data.frame

  # Latest Clearing Prices at BEN<>HAY |2019-> 2021
  ftrs_snk_src_snk_aggr %>% filter(StartDate > ymd(20190301) & StartDate < ymd(20210901)) %>%
                            ggplot(aes(x = StartDate, y = aggr_price, group = Source, colour = Source)) + geom_line()


  ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                          geom_point() +
                          geom_smooth(se=F) +
                          theme(legend.title=element_blank())

}

