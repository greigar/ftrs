# ASX Screen scraper

## About
* Scrape data from
  * `https://www.emi.ea.govt.nz/Forward%20markets/Reports/DRERRQ`
  * `?DateFrom=20180101&DateTo=20181231&_rsdr=Y1&Show=DOL_NET&_si=v|3`
* The code looks for `<script>` tags and then the data variable within that.

## Testing
* Test by running from R:
  * `test_from_web("OTA") %>% test_plot`


Data sets

* 1 - Exchange for physical
* 2 - Strip legs
* 3 - Block trade
* 4 - Normal trade


