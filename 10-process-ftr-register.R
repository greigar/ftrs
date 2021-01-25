# Create an account on www.ftr.co.nz
# go to https://www.ftr.co.nz/register and download csv
# save to data/downloads/

clean_ftrs <- function() {
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

  ftrs <- read_csv("data/downloads/FTR_Register.csv", col_types = ftr_cols)

  ftrs <- ftrs %>% separate(AuctionName, '_', into = c("auction_type"), extra = "drop", remove = FALSE) %>%
                   mutate(auction_year_month = floor_date(DateAcquired, unit = 'month'),
                          auction_type       = ifelse(is.na(auction_type), Status, auction_type))

  write_csv(ftrs, "data/ftrs.csv")
}

create_lookup_data <- function() {

  # Need to look at the hedge disclosure site to get the zones
  hedge_grid_zones <- read_csv("data/hedge_grid_zones.csv")

  nodes <- ftrs %>%
             distinct(Source, Sink) %>%
             pivot_longer(cols = c("Source", "Sink"), names_to = "nodes", values_to = "node") %>%
             distinct(node) %>%
             arrange %>%
             mutate(poc = str_c(node, '2201'))

  nodes <- left_join(nodes, hedge_grid_zones)

  write_csv(nodes, "data/nodes.csv")
}

