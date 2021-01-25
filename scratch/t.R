  ftrs_src_snk_year  <- ftrs %>% filter(Source == source_node & Sink == sink_node)
  final_src_snk_year <- final_prices %>% filter(Node %in% c(source_node_poc, sink_node_poc))

  t_hedges <- asx_hedges_aggr %>% filter(node == 'OTA', trade_month >= ymd(20160101))

  t_ftrs   <- ftrs_src_snk_year %>% filter(DateAcquired >= ymd(20160101)) %>%
                group_by(DateAcquired, StartDate) %>%
                summarise(price_aggr = mean(Price)) %>%
                ungroup %>%
                mutate(start_year = year(StartDate))

  t_final  <- create_final_node_price_differences(final_src_snk_year)

  ggplot(t_ftrs, aes(DateAcquired, scale(price_aggr), colour = factor(start_year))) + geom_line(stat = "smooth") +
    geom_line(data = t_final,  aes(trading_month, scale(price_sink_source_rolling)), colour = 'red',  linetype = 2, stat = "smooth") +
    geom_line(data = t_hedges, aes(trade_month,   scale(price_mean)),                colour = "blue", linetype = 2, stat = "smooth")

