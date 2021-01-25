# Go to and https://www.electricitycontract.co.nz/hedge/list_hedges and download csv
# save to data/downloads/
# Note - takes ages and may time out

source("00-helpers.R")

clean_hedge_contracts <- function() {

  hedge_spec <- cols(
   id                              =  col_double(),
   contract_type                   =  col_character(),
   party_code                      =  col_character(),
   party_email_address             =  col_character(),
   party_role                      =  col_character(),
   party_external_reference        =  col_character(),
   other_party_legal_name          =  col_character(),
   other_party_email_address       =  col_character(),
   other_party_external_reference  =  col_character(),
   trade_date                      =  col_date(format = "%d/%m/%Y"),
   effective_date                  =  col_date(format = "%d/%m/%Y"),
   end_date                        =  col_date(format = "%d/%m/%Y"),
   quantity                        =  col_double(),
   adjustment_clause               =  col_character(),
   force_majeure_clause            =  col_character(),
   suspension_clause               =  col_character(),
   special_credit_clause           =  col_character(),
   other_clauses                   =  col_character(),
   traded_on_energyhedge           =  col_character(),
   isda_schedule_used              =  col_character(),
   for_all_trading_periods         =  col_character(),
   volume_type                     =  col_character(),
   grid_zone_area                  =  col_character(),
   contract_price                  =  col_double(),
   contract_status                 =  col_character(),
   dispute_notes                   =  col_character(),
   created_at                      =  col_datetime(format = "%d/%m/%Y %H:%M:%S"),
   created_by                      =  col_character(),
   updated_at                      =  col_datetime(format = "%d/%m/%Y %H:%M:%S"),
   updated_by                      =  col_character(),
   traded_on_asx                   =  col_character()
  )


  hedge_data <- read_csv("data/downloads/contract_download.csv", col_types = hedge_spec)

  hedge_data <- hedge_data %>%
                  select(!contains('party'), party_role, dispute_notes, -created_by, -updated_by) %>% # these cols are anon'd
                  mutate(trade_month = floor_date(trade_date, "month"))

  grid_zones <- read_csv("data/nodes.csv") %>%
                  filter(!is.na(hedge_grid_zone))

  asx_hedges <- hedge_data %>%
                  filter(!is.na(contract_price), !is.na(traded_on_asx))

  asx_hedges_aggr <- asx_hedges %>%
                       group_by(trade_month, grid_zone_area) %>%
                       summarise(price_mean = mean(contract_price)) %>%
                       ungroup

  asx_hedges_aggr <- inner_join(asx_hedges_aggr, grid_zones, by = c("grid_zone_area" = "hedge_grid_zone"))

  write_csv(asx_hedges,      "data/asx_hedges.csv")
  write_csv(asx_hedges_aggr, "data/asx_hedges_aggr.csv")
}


# quick plot for checking
plot_hedges_check <- function() {
  asx_hedges_aggr %>% ggplot(aes(trade_month, price_mean, colour = node, group = node)) + geom_line(stat="smooth")
}

