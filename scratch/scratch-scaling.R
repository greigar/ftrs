source("ftr.R")
source("ftr.R", echo = T)
ls()
final_node_price_differences
final_node_price_differences <- final_src_snk_year %>%
                                  mutate(trading_month = floor_date(Trading_date, "month")) %>%
                                  group_by(trading_month, Node) %>%
                                  summarise(price_average = mean(Price)) %>%
                                  pivot_wider(names_from = Node, values_from = price_average) %>%
                                  mutate(price_difference_1 = reduce(across(ends_with(node_ends_with)), `-`)) %>%
                                  mutate(price_difference_2 = -1 * price_difference_1) %>%
                                  ungroup
final_node_price_differences 
final_node_price_differences <- final_src_snk_year %>%
                                  mutate(trading_month = floor_date(Trading_date, "month")) %>%
                                  group_by(trading_month, Node) %>%
                                  summarise(price_average = mean(Price)) %>%
                                  pivot_wider(names_from = Node, values_from = price_average) %>%
                                  mutate(price_difference_1 = reduce(across(ends_with(node_ends_with)), `-`)) %>%
                                  mutate(price_difference_2 = -1 * price_difference_1) %>%
                                  ungroup
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE) +
                        geom_line(data = final_node_price_differences,
                                  aes(x = trading_month, y = price_difference), colour = "red")
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_point()
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_point() + theme(legend.title = element_blank()) + geom_line(data = final_node_price_differences, aes(x = trading_month, y = price_difference_1), colour = "red")
final_node_price_differences %>% ggplot(aes(trading_month, price_difference_2), colour = "blue")) + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(x = trading_month, y = price_difference_2), colour = "red")
final_node_price_differences %>% ggplot(aes(trading_month, price_difference_2), colour = "blue") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(x = trading_month, y = price_difference_2), colour = "red")
final_node_price_differences %>% ggplot(aes(trading_month, price_difference_2), colour = "blue") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(x = trading_month, y = price_difference_1), colour = "red")
final_node_price_differences %>% ggplot(aes(trading_month, price_difference_2), colour = "blue") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(x = trading_month, y = price_difference_1), colour = "green")
final_node_price_differences %>% ggplot(aes(x = trading_month, y = price_difference_2), colour = "red") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(x = trading_month, y = price_difference_1), colour = "green")
final_node_price_differences %>% ggplot(aes(x = trading_month, y = price_difference_2), colour = "red") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(y = price_difference_1), colour = "green")
final_node_price_differences %>% ggplot(aes(x = trading_month, y = price_difference_2), colour = "red") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(y = price_difference_1))
final_node_price_differences %>% ggplot(aes(x = trading_month, y = price_difference_2), colour = "red") + geom_line() + theme(legend.title = element_blank()) + geom_line(aes(y = price_difference_1))
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE) +
                        geom_line(data = final_node_price_differences,
                                  aes(x = trading_month, y = price_difference), colour = "red")
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE) +
                        geom_line(data = final_node_price_differences,
                                  aes(x = trading_month, y = price_difference_1), colour = "red")
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        theme(legend.title = element_blank()) +
                        geom_smooth(se = FALSE)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_point() + theme(legend.title = element_blank()) + geom_smooth(se = FALSE, alpha = 0.2)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_point() + theme(legend.title = element_blank()) + geom_line(stat = "smooth", se = FALSE, alpha = 0.2)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_point() + theme(legend.title = element_blank()) + geom_line(stat = "smooth", se = FALSE, alpha = 0.4)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) + geom_boxplot() + theme(legend.title = element_blank()) + geom_line(stat = "smooth", se = FALSE, alpha = 0.4)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price) + geom_boxplot() + theme(legend.title = element_blank()) + geom_line(stat = "smooth", se = FALSE, alpha = 0.4)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price)) + geom_boxplot() + theme(legend.title = element_blank()) + geom_line(stat = "smooth", se = FALSE, alpha = 0.4)
ftrs_src_snk_year %>% ggplot(aes(StartDate, Price, colour = factor((year(auction_year_month))) )) +
                        geom_point() +
                        geom_line(stat = "smooth", se = FALSE, alpha = 0.4) +
                        theme(legend.title = element_blank())
ftrs_src_snk_year
ftrs_src_snk_year %>% group_by(StartDate, auction_year_month) %>% summarise(price_aggr = mean(Price)) %>% ungroup %>% ggplot(aes(StartDa
ftrs_src_snk_year %>% group_by(StartDate, auction_year_month) %>% summarise(price_aggr = mean(Price)) %>% ungroup %>% ggplot(aes(StartDate, price_aggr, colour = factor((year(auction_year_month))) + geom_point()
ftrs_src_snk_year %>% group_by(StartDate, auction_year_month) %>% summarise(price_aggr = mean(Price)) %>% ungroup %>% ggplot(aes(StartDate, price_aggr, colour = factor(year(auction_year_month)))) + geom_point()
ftrs_src_snk_year %>% group_by(StartDate, auction_year_month) %>% summarise(price_aggr = mean(Price)) %>% ungroup # %>% ggplot(aes(StartDate, price_aggr, colour = factor(year(auction_year_month)))) + geom_point()b
ftrs_src_snk_year %>% group_by(StartDate, auction_year_month) %>% summarise(price_aggr = mean(Price)) %>% arrange(StartDate, auction_year_month)
ftrs_src_snk_year %>% group_by(StartDate, year(auction_year_month)) %>% summarise(price_aggr = mean(Price))
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price))
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ggplot(aes(StartDate, price_aggr, colour = auction_year)) + geom_point()
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ggplot(aes(StartDate, price_aggr, colour = factor(auction_year))) + geom_point()
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ggplot(aes(StartDate, price_aggr, colour = factor(auction_year))) + geom_point()
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ungroup
ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ungroup %>% pivot_wider(names_from = auction_year, values_from = price_aggr)
final_node_price_differences
ftrs_src_snk_year_summary <- ftrs_src_snk_year %>% mutate(auction_year = year(auction_year_month)) %>% group_by(StartDate, auction_year) %>% summarise(price_aggr = mean(Price)) %>% ungroup %>% pivot_wider(names_from = auction_year, values_from = price_aggr)
ftrs_src_snk_year_summary 
?lefft_join
?left_join
# by = c("a" = "b")
left_join(final_node_price_differences, ftrs_src_snk_year_summary, by = c("trading_month" = "StartDate"))
data <- left_join(final_node_price_differences, ftrs_src_snk_year_summary, by = c("trading_month" = "StartDate"))
savehistory()
?lm
lm(price_difference_2 ~ `2016`, data)
x ,_ lm(price_difference_2 ~ `2016`, data)
x <- lm(price_difference_2 ~ `2016`, data)
summary(x)
x <- lm(price_difference_2 ~ `2016` + `2017`, data)
summary(x)
x <- lm(price_difference_2 ~ `2016` + `2017` + `2018`, data)
summary(x)
x <- lm(`2016` + price_difference_2, data)
x <- lm(`2016` ~ price_difference_2, data)
summary(x)
plot(x)
x <- lm(`2017` ~ price_difference_2, data)
summary(x)
x <- lm(`2018` ~ price_difference_2, data)
summary(x)
corr(data$`2018`, data$price_difference_2)
cor(data$`2018`, data$price_difference_2)
plot(data$`2018`, data$price_difference_2)
?cor
plot(data$`2016`, data$price_difference_2)
cor(data$`2016`, data$price_difference_2)
cor(data$`2017`, data$price_difference_2)
data %>% rm.na
data
names(data)
janitor::make_clean_names(data)
janitor:clean_names(data)
janitor::clean_names(data)
janitor::clean_names(data)
janitor::make_clean_names(names(data))
janitor::clean_names(names(data))
janitor::clean_names(data)
data
data <- left_join(final_node_price_differences, ftrs_src_snk_year_summary, by = c("trading_month" = "StartDate"))
janitor::clean_names(data)
data
janitor::make_clean_names(names(data))
names(data) <- janitor::make_clean_names(names(data))
data
 
 
data %>% mutate(x2018 = if_else(is.na(x2018), x2017, x2018))
data <- data %>% mutate(x2018 = if_else(is.na(x2018), x2017, x2018))
savehistory()
data
cor(data$`2018`, data$price_difference_2)
cor(data$x2018, data$price_difference_2)
lm(x2018 ~ price_difference_2, data)
summary(lm(x2018 ~ price_difference_2, data))
summary(lm(x2017 ~ price_difference_2, data))
summary(lm(x2016 ~ price_difference_2, data))
?scale
data
data %>% mutate_at(starts_with("x"), scale)
data %>% mutate(across(starts_with("x"), scale))
kkkdata_scaled 
janitor::make_clean_names(names(data_scaled))
names(data_scaled) <- janitor::make_clean_names(names(data_scaled))
data_scaled
janitor::make_clean_names(names(data_scaled))
names(data_scaled) <- janitor::make_clean_names(names(data_scaled))
data_scaled
z <- janitor::make_clean_names(names(data_scaled))
names(data_scaled)
names(data_scaled) <- z
data_scaled
data_scaled$x2016
data_scaled$x2016[[1]]
data_scaled$x2016[1]
data_scaled$x2016[,1]
unlist(data_scaled$x2016)
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point()
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = scale(price_difference_2)), colour = "red")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red")
data_scaled <- data %>% mutate(across(starts_with("x"), scale)) %>% mutate(price_difference_2 = scale(price_difference_2))
data_scaled
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red") + geom_point(aes(y = x2017), colour = green)
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red") + geom_point(aes(y = x2017), colour = "green")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_point() + geom_line(aes(y = price_difference_2), colour = "red") + geom_point(aes(y = x2017), colour = "green") + geom_point(aes(y = x2018), colour = "blue")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_line() + geom_line(aes(y = price_difference_2), colour = "red") + geom_line(aes(y = x2017), colour = "green") + geom_line(aes(y = x2018), colour = "blue")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_line() + geom_line(aes(y = price_difference_2), colour = "red", alpha = 0.5) + geom_line(aes(y = x2017), colour = "green") + geom_line(aes(y = x2018), colour = "blue")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_line(stat="smooth", se = F) + geom_line(aes(y = price_difference_2), colour = "red", alpha = 0.5) + geom_line(aes(y = x2017), colour = "green") + geom_line(aes(y = x2018), colour = "blue")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_line(stat="smooth", se = F) + geom_line(aes(y = price_difference_2), stat="smooth", colour = "red", alpha = 0.5) + geom_line(aes(y = x2017), colour = "green") + geom_line(aes(y = x2018), colour = "blue")
data_scaled %>% ggplot(aes(x = trading_month, y = x2016)) + geom_line(stat="smooth", se = F) + geom_line(aes(y = price_difference_2), stat="smooth", colour = "red", alpha = 0.5) + geom_line(aes(y = x2017), stat="smooth", colour = "green") + geom_line(aes(y = x2018), stat="smooth", colour = "blue")
savehistory()
