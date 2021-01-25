# Set up data sets etc for interactive

print("sourcing 00-helpers.R")
source("00-helpers.R")

print("sourcing 30-plots.R")
source("30-plots.R")

# Load data and do some filtering
# The FTR year that we want to have a look at and the source and sink FTR nodes
run <- function(ftr_year_in = 2018, source_node_in = "BEN", sink_node_in = "OTA") {

  # The nodes we want to look at
  source_node     <<- source_node_in
  sink_node       <<- sink_node_in
  source_node_poc <<- str_glue(source_node, NODE_ENDS_WITH)
  sink_node_poc   <<- str_glue(sink_node,   NODE_ENDS_WITH)

  ftr_year <<- ftr_year_in

  temp_data <- filter_price_data()

  ftrs_src_snk_year  <<- temp_data[[1]]
  final_src_snk_year <<- temp_data[[2]]

  final_node_price_differences <<- create_final_node_price_differences(final_src_snk_year)
}

print("reading data")
read_data()
print("doing some filtering")
run()

