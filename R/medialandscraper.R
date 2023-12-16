#' Title
#'
#' @param outlets Vector of media-outlets that should be scraped. Currently supported: Watson.ch, 20 Minuten.ch
#' @param browser Browser that should be used for scraping. Default: Firefox
#' @param port Port that should be used for scraping. Default: 4491L
#' @param sqldb Whether scraping-results should to be stored in an sql-database or not. Default: FALSE
#' @param dbname Name for the sql-database if sqldb = TRUE
#' @param plots Provides a summary of the scraped data in the form of basic plots if TRUE. Plots show: Number of articles per outlet scraped and mean-length of title/lead/body of the articles that where scraped. Default: FALSE
#' @param searchterm Regular expression for which you want to search in the titles of the scraped articles. Result is a plot
#'
#' @return R-dataframe or sql-database
#' @export
#'
#' @examples
#' mediascraper(outlets = c("Watson", "20 Minuten"), plots = TRUE)
mediascraper = function(outlets, browser = "firefox", port = 4490L, sqldb = FALSE,
                        dbname = "scrapingresults", plots = FALSE, searchterm = NULL){

  suppressMessages(require(RSelenium))
  suppressMessages(require(stringr))
  suppressMessages(require(dplyr))
  suppressMessages(require(ggplot2))


  # Checking function-inputs
  if (any(outlets %in% c("Watson", "SRF", "20 Minuten", "Tagesanzeiger") == FALSE)){
    stop("Error: One of the newsoutlets you specified is not (yet) supported")
  }




  # Initiating sql-database (if requested by the user)
  if (sqldb == TRUE){require(DBI)
    con = dbConnect(RSQLite::SQLite(), dbname)
    dbSendQuery(con, "CREATE TABLE scrapingresults(
              title TEXT,
              lead TEXT,
              body TEXT,
              url VARCHAR(128),
              outlet VARCHAR(32) NOT NULL,
              time DATETIME
            )")
    dbListTables(con)
  }



  # List to store results
  df_list <- list()




  # Initiate scraper
  cat("Initiating Webscraper\n")
  driver = suppressMessages(rsDriver(browser=browser, port = port, chromever = NULL))
  rs = suppressMessages(driver$client)






  # Watson
  if ("Watson" %in% outlets){
    cat("Started scraping Watson\n")

    suppressMessages(rs$navigate("https://watson.ch"))
    Sys.sleep(10)

    html = rs$getPageSource() # get source of the page
    links = str_match_all(html, '<a class="watson-teaser__link[^"]*" href=\"(.*?)\"')[[1]]
    links = as.data.frame(links)[,2]

    # Delete links that do not begin with "https://www.watson.ch"
    links <- links[grep("^https://www.watson.ch", links)]


    df_watson = data.frame(title = rep(NA_character_, length(links)),
                           lead = rep(NA_character_, length(links)),
                           body = rep(NA_character_, length(links)),
                           url = rep(NA_character_, length(links)),
                           outlet = rep("Watson", length(links)),
                           time = rep(Sys.time(), length(links)))


    for(i in seq_along(links)){
      suppressMessages(rs$navigate(links[i]))
      # css: id: #id, class: .class, tag: tag
      title_e = rs$findElements(using = 'css selector', 'h2')
      title = title_e[[1]]$getElementText()[[1]]
      title = gsub("'", "", title) # escaping single quotations
      df_watson[i, "title"] = title
      lead_e = rs$findElements(using = 'css selector', '.watson-snippet__lead')
      if(length(lead_e)>0){
        lead = lead_e[[1]]$getElementText()[[1]]
        lead = gsub("'", "", lead)
        df_watson[i, "lead"] = lead
      }

      body_e = rs$findElements(using = 'css selector', 'p')
      if (length(body_e) > 0) {
        body_text = sapply(body_e, function(elem) elem$getElementText()[[1]])
        body = paste(body_text, collapse = " ")
        body = gsub("'", "", body)
        df_watson[i, "body"] = body
      }

      time = Sys.time()
      link = links[i]
      df_watson[i, "time"] = time
      df_watson[i, "url"] = link



      if (sqldb == TRUE){sql = paste0("INSERT INTO scrapingresults VALUES ('",title,"','",lead,"','",body,"','",link,"','Watson','",time,"')")
      suppressMessages(dbSendStatement(con, sql))}


    }

    df_list[["Watson"]] <- df_watson

  }



  # 20 Minuten
  if ("20 Minuten" %in% outlets){
    cat("Started scraping 20 Minuten\n")

    rs$navigate("https://20min.ch/")
    Sys.sleep(10)

    html = rs$getPageSource() # get source of the page
    links = str_match_all(html, '<a class="sc-bb81291f-1[^"]*" href=\"(.*?)\"')[[1]]
    links = as.data.frame(links)[,2]


    # Links have to be corrected because some of them do not begin with https://www.20min.ch
    indices <- !grepl("^https://", links)
    links[indices] <- paste0("https://www.20min.ch", links[indices])


    df_20minuten = data.frame(title = rep(NA_character_, length(links)),
                              lead = rep(NA_character_, length(links)),
                              body = rep(NA_character_, length(links)),
                              url = rep(NA_character_, length(links)),
                              outlet = rep("20 Minuten", length(links)),
                              time = rep(Sys.time(), length(links)))


    for(i in seq_along(links)){
      suppressMessages(rs$navigate(links[i]))
      # css: id: #id, class: .class, tag: tag
      title_e = rs$findElements(using = 'css selector', 'h2')
      title = title_e[[1]]$getElementText()[[1]]
      title = gsub("'", "", title) # escaping single quotations
      df_20minuten[i, "title"] = title

      lead_e = rs$findElements(using = 'css selector', '.Article_elementLead__a52sm')
      if(length(lead_e)>0){
        lead = lead_e[[1]]$getElementText()[[1]]
        lead = gsub("'", "", lead)
        df_20minuten[i, "lead"] = lead
      }


      body_e = rs$findElements(using = 'css selector', 'p')
      if (length(body_e) > 0) {
        body_text = sapply(body_e, function(elem) elem$getElementText()[[1]])
        body = paste(body_text, collapse = " ")
        body = gsub("'", "", body)
        df_20minuten[i, "body"] = body
      }

      time = Sys.time()
      link = links[i]
      df_20minuten[i, "time"] = time
      df_20minuten[i, "url"] = link




      if (sqldb == TRUE){sql = paste0("INSERT INTO scrapingresults VALUES ('",title,"','",lead,"','",body,"','",link,"','20 Minuten','",time,"')")
      suppressMessages(dbSendStatement(con, sql))}


    }

    df_list[["20 Minuten"]] <- df_20minuten

  }




  # SRF
  if ("SRF" %in% outlets){
    cat("Started scraping SRF\n")
    rs$navigate("https://srf.ch")
    Sys.sleep(2)

    html = rs$getPageSource() # get source of the page
    links = str_match_all(html, '<a class="teaser__main[^"]*" href=\"(.*?)\"')[[1]]
    links = as.data.frame(links)[,2]

    # Links have to be corrected because some of them do not begin with https://www.srf.ch
    indices <- !grepl("^https://", links)
    links[indices] <- paste0("https://www.srf.ch", links[indices])


    df_srf = data.frame(title = rep(NA_character_, length(links)),
                        lead = rep(NA_character_, length(links)),
                        body = rep(NA_character_, length(links)),
                        url = rep(NA_character_, length(links)),
                        outlet = rep("SRF", length(links)),
                        time = rep(Sys.time(), length(links)))

    # Scrape articles
    for(i in seq_along(links)){
      rs$navigate(links[i])
      title_e = rs$findElements(using = 'css selector', 'h1')
      title = title_e[[1]]$getElementText()[[1]]
      df_srf[i, "title"] = title

      lead_e = rs$findElements(using = 'css selector', '.article-lead')
      if(length(lead_e)>0){
        lead = lead_e[[1]]$getElementText()[[1]]
        df_srf[i, "lead"] = lead
      }

      body = NA

      time = Sys.time()
      link = links[i]
      df_srf[i, "time"] = time
      df_srf[i, "url"] = link


      if (sqldb == TRUE){sql = paste0("INSERT INTO scrapingresults VALUES ('",title,"','",lead,"','",body,"','",link,"','SRF','",time,"')")
      suppressMessages(dbSendStatement(con, sql))}


    }

    df_list[["SRF"]] <- df_srf

  }



  # Tagesanzeiger
  if ("Tagesanzeiger" %in% outlets){
    cat("Started scraping Tagesanzeiger\n")
    rs$navigate("https://www.tagesanzeiger.ch/")
    Sys.sleep(10)
    # Scroll down the page bit by bit to load all the content.
    for(i in 1:50){
      rs$executeScript("window.scrollBy(0,500);")
      Sys.sleep(0.5)
    }
    html = rs$getPageSource() # get source of the page
    links = str_match_all(html, '<a class="Teaser_link__aPG04[^"]*" href=\"(.*?)\"')[[1]]
    links = as.data.frame(links)[,2]

    # Links have to be corrected because they do not begin with https://www.tagesanzeiger.ch/
    indices <- !grepl("^https://", links)
    links[indices] <- paste0("https://www.tagesanzeiger.ch", links[indices])


    df_Tagesanzeiger = data.frame(title = rep(NA_character_, length(links)),
                                  lead = rep(NA_character_, length(links)),
                                  body = rep(NA_character_, length(links)),
                                  url = rep(NA_character_, length(links)),
                                  outlet = rep("Tagesanzeiger", length(links)),
                                  time = rep(Sys.time(), length(links)))


    for(i in seq_along(links)){
      suppressMessages(rs$navigate(links[i]))
      # css: id: #id, class: .class, tag: tag
      title_e = rs$findElements(using = 'css selector', 'h2')
      title = title_e[[1]]$getElementText()[[1]]
      title = gsub("'", "", title) # escaping single quotations
      df_Tagesanzeiger[i, "title"] = title

      lead_e = rs$findElements(using = 'css selector', '.HtmlText_root__A1OSq')
      if(length(lead_e)>0){
        lead = lead_e[[1]]$getElementText()[[1]]
        lead = gsub("'", "", lead)
        df_Tagesanzeiger[i, "lead"] = lead
      }

      # Body is only scrapped incompletedly for paid articles
      body_e = rs$findElements(using = 'css selector', 'p')
      if (length(body_e) > 0) {
        body_text = sapply(body_e, function(elem) elem$getElementText()[[1]])
        body = paste(body_text, collapse = " ")
        body = gsub("'", "", body)
        df_Tagesanzeiger[i, "body"] = body
      }

      time = Sys.time()
      link = links[i]
      df_Tagesanzeiger[i, "time"] = time
      df_Tagesanzeiger[i, "url"] = link



      if (sqldb == TRUE){sql = paste0("INSERT INTO scrapingresults VALUES ('",title,"','",lead,"','",body,"','",link,"','Tagesanzeiger','",time,"')")
      suppressMessages(dbSendStatement(con, sql))}


    }
    df_list[["Tagesanzeiger"]] <- df_Tagesanzeiger

  }




  # Create results-dataframe
  results_df <- do.call(rbind, df_list)
  rownames(results_df) <- NULL





  # Writing code to create analysis plots:
  # Number of articles
  if (plots == TRUE){

    # Setting colors for the different outlets
    colors <- c("Watson" = "#F40F96", "20 Minuten" = "#0D2880", "SRF" = "#AF001D", "Tagesanzeiger" = "#000000")

    nrart <- results_df |> group_by(outlet) |>
      summarize(n = n()) |> ggplot(aes(x = outlet, y = n, color = outlet, fill =
                                         outlet)) +
      geom_col(width = 0.5, show.legend = F) +
      xlab("Outlet") + ylab("Number of Articles")  +
      ggtitle("Number of Scrapped Articles per Outlet") + theme_bw() +
      scale_fill_manual(values = colors) + scale_color_manual(values = colors)
    print(nrart)

    # Length of title
    lete <- results_df |> group_by(outlet) |>
      summarize(titlelength = mean(nchar(title), na.rm = T)) |> ggplot(aes(x = outlet,
                                                                           y = titlelength,
                                                                           color = outlet,
                                                                           fill = outlet)) +
      geom_col(width = 0.5, show.legend = F) + xlab("Outlet") +
      ylab("Number of Characters")  + ggtitle("Mean Length of Article-Title") +
      theme_bw() + scale_fill_manual(values = colors) + scale_color_manual(values = colors)
    print(lete)

    # Length of lead
    lele <- results_df |> group_by(outlet) |>
      summarize(leadlength = mean(nchar(lead), na.rm = T)) |> ggplot(aes(x = outlet,
                                                                         y = leadlength,
                                                                         color = outlet,
                                                                         fill = outlet)) +
      geom_col(width = 0.5, show.legend = F) + xlab("Outlet") +
      ylab("Number of Characters")  +
      ggtitle("Mean Length of Article-Lead") + theme_bw() + scale_fill_manual(values = colors)  + scale_color_manual(values = colors)
    print(lele)

    # Length of body
    lebo <- results_df |> group_by(outlet) |>
      summarize(bodylength = mean(nchar(body), na.rm = T)) |>
      filter(!outlet == "SRF") |> filter(!outlet == "Tagesanzeiger") |> ggplot(aes(x = outlet,
                                                                                   y = bodylength,
                                                                                   color = outlet,
                                                                                   fill = outlet)) +
      geom_col(width = 0.5, show.legend = F) + xlab("Outlet") +
      ylab("Number of Characters")  + ggtitle("Mean Length of Article-Body") +
      theme_bw() + scale_fill_manual(values = colors)  + scale_color_manual(values = colors)
    print(lebo)
  }


  # Search for appearances of a specific word in the title of the articles
  if (!is.null(searchterm)){
    wordo <- results_df  |>
      mutate(word_count = str_count(title, searchterm)) |> group_by(outlet) |>
      summarise(
        word_count = sum(word_count)
      ) |> ggplot(aes(x = outlet, y = word_count, color = outlet, fill = outlet)) +
      geom_col(width = 0.5, show.legend = F) + xlab("Outlet") +
      ylab("Number of Appearances")  +
      ggtitle("Number of Appearances of Word Specified") + theme_bw() + scale_fill_manual(values = colors)  + scale_color_manual(values = colors)
    print(wordo)}



  # Saving results into an sql-database
  rs$close() # Closing the client
  rm(rs) # Removing the server-object to free up the port
  gc
  if (sqldb == TRUE){
    cat("Done. Saved results in sql-database 'scrapingresults'. Use object 'con' in environment to connect to database\n")
    assign("con", con, envir = .GlobalEnv)
    dbDisconnect(con)

  } else{
    cat("Done. Returned r-dataframe\n")
    assign("scrapingresults", results_df, envir = .GlobalEnv)
    return(results_df)}

}
