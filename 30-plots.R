#############
# plot stuff
#############

plot_final <- function() {
  final_node_price_differences %>% ggplot(aes(x = trading_month, y = price_sink_source_aggr)) +
                                     geom_line() +
                                     theme(legend.title = element_blank()) +
                                     geom_line(aes(y = price_positive_aggr), colour = "green") +
                                     geom_line(aes(y = price_sink_source_rolling), colour = "blue")
}

plot_ftrs <- function() {
  ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                          geom_point() +
                          geom_line(stat = "smooth", se = FALSE, alpha = 0.4) +
                          theme(legend.title = element_blank())
}

# quick plot for checking
plot_hedges <- function() {
  asx_hedges_aggr %>% ggplot(aes(trade_month, price_mean, colour = node, group = node)) + geom_line(stat="smooth")
}

plot_scaled <- function() {
  temp <- ftrs_src_snk_year %>% group_by(DateAcquired, StartDate) %>%
                                summarise(price_aggr = mean(Price)) %>%
                                ungroup

  temp %>% ggplot(aes(DateAcquired, scale(price_aggr), colour = factor(StartDate))) +
             geom_line(stat = 'smooth', alpha = 0.4, se = FALSE) +
             geom_line(data = temp, aes(DateAcquired, scale(price_aggr)), stat = 'smooth', colour = "black" ) +
             geom_line(data = final_node_price_differences, aes(trading_month, scale(price_sink_source_rolling)), colour = "red")
}

plot_scaled_2 <- function() {
  ftrs_src_snk_year %>% group_by(DateAcquired, StartDate) %>%
                        summarise(price_aggr = mean(Price)) %>%
                        ggplot(aes(DateAcquired, scale(price_aggr), colour = factor(StartDate))) +
                          geom_point() +
                          geom_line(data = final_node_price_differences,
                                    aes(trading_month, scale(price_sink_source_aggr)),
                                    colour = "red")
}

# re-write to remove globals
plot_compare_years <- function(year_1, year_2) {
  run(year_1)
  p1 <- final_node_price_differences %>%
          filter(trading_month > get_start_year(year_1)) %>%
          ggplot(aes(x = trading_month, y = price_sink_source_aggr)) +
            geom_line()  +
            geom_line(data = ftrs_src_snk_year, aes(DateAcquired, Price, colour = factor(StartDate)))

  run(year_2)
  p2 <- final_node_price_differences %>%
          filter(trading_month > get_start_year(year_2)) %>%
          ggplot(aes(x = trading_month, y = price_sink_source_aggr)) +
            geom_line() +
            geom_line(data = ftrs_src_snk_year, aes(DateAcquired, Price, colour = factor(StartDate)))

  p1 + p2
}

plot_all_years <- function(){
  ftrs_src_snk_year  <- ftrs %>% filter(Source == source_node & Sink == sink_node)
  final_src_snk_year <- final_prices %>% filter(Node %in% c(source_node_poc, sink_node_poc))
  final_node_price_differences <- create_final_node_price_differences(final_src_snk_year)

  ftrs_src_snk_year  %>%
    ggplot(aes(DateAcquired, Price, colour = factor(year(StartDate)))) +
      geom_point() +
      geom_line(data = final_node_price_differences, aes(x = trading_month, y = price_sink_source_aggr), colour = "black")

}

plot_map <- function(){
  nodesll <- read_csv("data/nodesll.csv")
  nodesll %>% leaflet %>% addTiles %>% addMarkers(popup = nodesll$Description)
}

create_plot_combination <- function() {

  t_start_year <- get_start_year(ftr_year)
  t_end_year   <- get_end_year(ftr_year)

  t_hedges <- asx_hedges_aggr %>%
                filter(node == 'OTA', trade_month >= t_start_year, trade_month <= t_end_year)
  t_ftrs   <- ftrs_src_snk_year %>%
                group_by(DateAcquired, StartDate) %>% summarise(price_aggr = mean(Price)) %>% ungroup
  t_final  <- final_node_price_differences %>%
                filter(trading_month >= t_start_year, trading_month <= t_end_year)

  plot_combination(t_ftrs, t_final, t_hedges)
}

plot_combination <- function(t_ftrs, t_final, t_hedges) {

  colname_colors <- c("FTR"   = "black",
                      "Spot"  = "red",
                      "Hedge" = "blue")

  ggplot(t_ftrs, aes(DateAcquired, scale(price_aggr), colour = "FTR")) +
    #
    # plot lines
    #
    geom_line(stat = "smooth") +
    geom_line(data = t_final,  aes(trading_month, scale(price_sink_source_rolling), colour = "Spot" ),
              stat = "smooth") +
    geom_line(data = t_hedges, aes(trade_month,   scale(price_mean), colour = "Hedge" ),
              stat = "smooth") +
    #
    # set legend
    #
    scale_colour_manual("Legend", values = colname_colors) +
    guides(colour = guide_legend(title = "Market")) +
    theme(legend.position = "bottom",
          strip.text.y    = element_blank(),
          legend.title    = element_text(size = 14),
          legend.text     = element_text(size = 12 )) +
    #
    # set axis labels
    #
    labs(x = "Date FTR Acquired",
         y = "Scaled $")
}
