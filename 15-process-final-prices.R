# Final prices are per month
#
# download_final_prices(start_month <-- first month to download
#                       number_of_months_to_download <-- plus any subsequent months
#
# e.g. Download September and October : download_final_prices("2020-09-01", 2)
#
# clean_combine_final_prices() - create one file for all final prices with just the nodes we are interested in

library(tidyverse)

# Note that this can change
EA_EMI_BASE_URL <- "http://www.emi.ea.govt.nz/Wholesale/Datasets"
DATA_DIR <- "data/downloads/"

download <- function(start_month_date, download_period, url) {

  month_year   <- format(start_month_date + months(download_period), "%Y%m")
  csv_data_url <- str_glue(url) # month_year
  file_name    <- str_glue("{DATA_DIR}{basename(csv_data_url)}")

  print(str_glue("Downloading {csv_data_url} -> {file_name}"))

  tryCatch(
    req <- download.file(csv_data_url, file_name),
    error = function(e) { message("download file failed: ", conditionMessage(e)) }
  )
}

download_final_prices <- function(start_month, number_of_months_to_download = 1) {

  url <- str_glue("{EA_EMI_BASE_URL}/Final_pricing/Final_prices/{{month_year}}_Final_prices.csv")

  start_month_date <- as.Date(start_month) # 2020-09-01"
  month_range      <- 0:(number_of_months_to_download - 1)

  map(month_range, function(download_period) download(start_month_date, download_period, url))
}


# read in the files, extract just the nodes we are interested in
clean_combine_final_prices <- function() {
  # Final prices file column spec
  col_spec <- cols(Trading_date   = col_date(format = ""),
                   Trading_period = col_integer(),
                   Node           = col_character(),
                   Price          = col_double())

  final_price_files <- dir(DATA_DIR, pattern = "......_Final_prices.csv")

  pocs <- read_nodes()$poc

  final_prices <- map_dfr(final_price_files,
                          function(filename) {
                            read_csv(filename, col_types = col_spec) %>%
                              filter(Node %in% pocs)
                          })

  write_csv(final_prices, "data/final_prices.csv")
}


# for large final price data set in feather format
create_final_prices_from_feather_to_csv <- function() {
  pocs <- read_nodes$poc
  final_prices <- read_feather("../../nzfpg-explorer/data/finalprices.feather")
  final_prices <- final_prices %>% filter(Node %in% pocs)
  write_csv(final_prices, "data/final_prices.csv")
}

