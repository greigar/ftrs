# TODO
# 1. Check for script and data tags r.t. hard coded in data_script_tag

library(tidyverse)
library(stringr)
library(polite)
library(rvest)
library(lubridate)
library(patchwork)

print("Checking robots preventing scraping...")
session <- bow("https://www.emi.ea.govt.nz/Forward%20markets/Reports/DRERRQ?DateFrom=20180101&DateTo=20181231&_rsdr=Y1&Show=DOL_NET&_si=v|3", force = T)
print(session)

# the index of the javascript tag where the chart data lives
data_script_tag <- 16

# some dates are a bit weird, so ignore those before 2018
start_date <- 20180101

# dates and values are "Date.UTC(2018,9,24),6912259.2"
# Months start at 0 !!! [Date.UTC(2018,0,18),21622637.75]
re <- "Date.UTC.([0-9]+).([0-9]+).([0-9]+).,([0-9]+).([0-9]*)"

# find the place where the data lives - sometimes needs tweaking
pattern_report_info_1 <- "(reportInfo =)(.*);( \\$reportInfo)"
pattern_report_info_2 <- "(reportInfo =)(.*);"

# get the data sets from the data: field - returns a chr vector
pattern_data_node <- "((\"data\":)(\\[\\[.*?]]))"

# Convert Date.UTC() dates to date
extract_date <- function(date_text) {
  extract <- str_match(date_text, re) %>% unlist

  year  <- as.integer(extract[2])
  month <- as.integer(extract[3]) + 1
  day   <- as.integer(extract[4])

  date  <- make_date(year, month, day)
  value <- paste(extract[5], extract[6], sep = '.')

  tibble(date = date, value = as.numeric(value))
}

# get all the dates and values
extract_date_data <- function(data_node, index) {
  data_node %>%
    str_extract_all(re) %>%
    unlist %>%
    map_df(extract_date) %>%
    arrange(date) %>%
    filter(date > ymd(start_date)) %>% # TODO look into weird dates
    mutate(date_str = strftime(date, "%Y-%m-%d"), data_set = index)
}

parse_html <- function(html_source){

  html_file <- read_html(html_source)

  tryCatch(html_node <- html_nodes(html_file, 'script')[data_script_tag] %>%
                        html_text(trim = TRUE),
           error = function(e) { message("A script tag at index specified by data_script_tag was not found - try a different value. ", conditionMessage(e)) })

  # get the chunk with the chart data
  report_info <- str_match(html_node, pattern_report_info_1)[[3]]
  if (is.na(report_info)) {
    report_info <- str_match(html_node, pattern_report_info_2)[[3]]
  }

  # get the data: sets
  data_nodes <- str_extract_all(report_info, pattern_data_node)
  data_nodes_count <- length(data_nodes[[1]])

  # convert dates and values into a data frame
  map2_df(data_nodes[[1]], 1:data_nodes_count, extract_date_data)
}


####################################
# TEST Functions                   #
####################################

test_from_web <- function(node = "BOTH") {

  url <- 'https://www.emi.ea.govt.nz/Forward%20markets/Reports/DRERRQ?DateFrom=20180101&DateTo=20181231<location-option>&Show=DOL_NET&seriesFilter=ALL&_rsdr=Y1&_si=v|3'

  location_option <- ifelse(node == "BOTH", '', str_glue('&Location=', node))

  html_source <- str_replace(pattern = fixed("<location-option>"), string = url, replacement = location_option)

  data <- parse_html(html_source)

  write_csv(x = data, path = "data/asx_data.csv")

  return(data)
}

test_from_file <- function() {
  # html_source <- "data/asx-2018-dollars.htm"
  html_source <- 'data/test.html'
  return(parse_html(html_source))
}

test_plot <- function(data) {
  data %>% ggplot(aes(x = date, y = value, fill = as.factor(data_set))) +
         geom_bar(stat = "identity") +
         scale_fill_manual(values = c("#FF7B00", "#61BC3A", "#70C9F2", "#0F7BAF"))
}

