
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MedialandscrapeR

<!-- badges: start -->
<!-- badges: end -->

The goal of MedialandscrapeR is to provide a function to easily scrape
the starting pages of swiss online newsmedia. The package currently
supports the following media-outlets: Watson.ch, 20Minuten.ch, srf.ch,
tagesanzeiger.ch.

## Installation

You can install the development version of MedialandscrapeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jineichen/MedialandscrapeR")
```

## Mediascraper

The basic function of the MedialandscrapeR-package is called
“mediascraper”. You must only specify one of the supported outlets you
want to scrape and you are ready to go. After execution the function
will initiate a selenium-client to scrape the outlets that you
specified. The function will return a r-dataframe that includes title,
lead and body of a given news-article (the latter is not supported in
the case of srf.ch) as well as the link to the article, the outlet it
stems from and the exact timepoint at which it was scraped. You can
scrape only one or multiple outlets simultaneously.

``` r
library(MedialandscrapeR)
mediascraper(outlets = c("Watson"))
mediascraper(outlets = c("Watson", "20 Minuten"))
```

You can also adjust the browser and the port that is used by Rselenium.

``` r
mediascraper(outlets = c("Watson", "20 Minuten"), browser = "firefox", port = 4501L)
```

You can add *sqldb = TRUE* if you rather want to store the results of
the scraping-process in an sql database. It you decide to do so, you can
also specify a name for the database (the default is “scrapingresults”)
by passing a name to the dbname-parameter.

``` r
mediascraper(outlets = c("Watson", "20 Minuten"), browser = "firefox", port = 4501L, sqldb = TRUE,
                        dbname = "scrapingresults")
```

The function also has the ability to provide you with some summary-plots
that give you more detail about the nature of the articles you just
scraped. Setting *plots* to *TRUE* outputs the following graphs:
Absolute number of articles per outlet, Mean-length of article-titles
per outlet, Mean-length of article-leads per outlet, Mean-length of
article-bodies per outlet (only supported for Watson.ch and
20Minuten.ch).

## Mediaplots

If you forgot to call plots = TRUE when using the mediascraper-function
you can still produce simply overview-plots by calling the function
*mediaplots*. It produces the same plots as noted above. Simply provide
the function with a dataframe or an sql-connection element (and the
corresponding data-base name) that was produced by the
mediascraper-function and you are good to go! Moreover, if you want to
count the appearances of a specific term in the scraping-result you can
pass a regex-pattern to the parameter *searchterm*. The function will
then produce and return a plot that shows you the absolute number of
appearances of the provided pattern in the titles of the scraped
articles per outlet. You can change the target of your search endeavours
by passing “lead” or “body” to the *searchtarget* parameter.

``` r
mediaplots(con, dbname = "scrapingresults", plots = FALSE, searchterm = "Schweiz", searchtarget = "title")
```
