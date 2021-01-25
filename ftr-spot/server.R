library(shiny)
source("../00-helpers.R")
source("../30-plots.R")

read_data()

get_ftr_data <- function(source_node, sink_node, ftr_year) {
  ftrs_src_snk_year  <- ftrs %>%
                          filter(year(StartDate) == ftr_year & Source == source_node & Sink == sink_node)

  ftrs_src_snk_year_aggr <- ftrs_src_snk_year %>%
                              group_by(DateAcquired, StartDate) %>%
                              summarise(price_aggr = mean(Price)) %>%
                              ungroup

  return(list(ftrs_src_snk_year, ftrs_src_snk_year_aggr))
}

get_final_data <- function(source_node, sink_node, ftr_year) {
  source_node_poc <- str_glue(source_node, NODE_ENDS_WITH)
  sink_node_poc   <- str_glue(sink_node,   NODE_ENDS_WITH)

  final_src_snk_year <- final_prices %>%
                          filter(year(Trading_date) <= ftr_year & Node %in% c(source_node_poc, sink_node_poc))

  final_node_price_differences <- create_final_node_price_differences(final_src_snk_year)

  return(list(final_src_snk_year, final_node_price_differences))
}


shinyServer(function(input, output) {

  ftr_data        <- reactive({ get_ftr_data(input$source_node,   input$sink_node, input$ftr_year) })
  final_data      <- reactive({ get_final_data(input$source_node, input$sink_node, input$ftr_year) })
  start_year      <- reactive({ get_start_year(input$ftr_year) })
  end_year        <- reactive({ get_end_year(input$ftr_year) })

  output$plot_scaled <- renderPlot({
    ftrs_src_snk_year_aggr       <- ftr_data()[[2]]
    final_node_price_differences <- final_data()[[2]]

    ftrs_src_snk_year_aggr %>%
      ggplot(aes(DateAcquired, scale(price_aggr), colour = factor(StartDate))) +
        geom_line(stat = 'smooth', alpha = 0.5, se = FALSE) +
        geom_line(data = ftrs_src_snk_year_aggr , aes(DateAcquired, scale(price_aggr)), stat = 'smooth', colour = "black" ) +
        geom_line(data = final_node_price_differences, aes(trading_month, scale(price_sink_source_aggr)), colour = "red") +
        theme(legend.title = element_blank())
  })

  output$plot_ftrs <- renderPlot({
    ftrs_src_snk_year  <- ftr_data()[[1]]

    ftrs_src_snk_year %>%
      ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
        geom_point() +
        geom_line(stat = "smooth", se = FALSE, alpha = 0.4) +
        theme(legend.title = element_blank())
  })

  output$plot_combination <- renderPlot({

    ftrs_src_snk_year            <- ftr_data()[[1]]
    final_node_price_differences <- final_data()[[2]]

    t_start_year <- start_year()
    t_end_year   <- end_year()

    t_hedges <- asx_hedges_aggr %>% filter(node == 'OTA', trade_month >= t_start_year, trade_month <= t_end_year)

    t_ftrs   <- ftrs_src_snk_year %>% group_by(DateAcquired, StartDate) %>% summarise(price_aggr = mean(Price)) %>% ungroup

    t_final  <- final_node_price_differences %>% filter(trading_month >= t_start_year, trading_month <= t_end_year)

    plot_combination(t_ftrs, t_final, t_hedges)

  })

  output$table_ftrs <- DT::renderDataTable({
    ftr_data()[[1]] %>% select(StartDate, auction_year_month, Price)
  })

  output$table_final <- DT::renderDataTable({
    final_data()[[1]] %>% group_by(Trading_date, Node) %>% summarise(mean_price = round(mean(Price), 2))
  })

  output$table_asx <- DT::renderDataTable({
    asx_hedges_aggr
  })

})

