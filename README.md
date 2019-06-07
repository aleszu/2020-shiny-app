# How to build a Shiny app that visualizes media coverage, tweets and Google search interest

The following documentation explains the process of creating a Shiny app in RStudio. Inspired by ProPublica and Google's Election DataBot, an interactive exploration of the 2016 midterms, this Shiny app acts as a dynamic interface to explore 2020 Democratic candidate tweets, Google search interest and mainstream media attention. 

## Required R packages and API credentials

This Shiny app uses rtweet to access Twitter's REST API, gtrendsR to access Google Trends, and jsonlite to access the Media Cloud API's top online media sources, including The New York Times, Vox, Guardian, Slate, The Washington Post, Breitbart, Fox News, CNN and more. For Twitter and Media Cloud, you must request your own API keys. 

Other packages used are: dplyr, tidyverse, tidytext, stringr, scales, maps, plotly, lubridate, wordcloud and hrbrthemes, in addition to shiny and shinythemes. 

## Stringing together several functions 

This Shiny app is the sum of several parts: 

* a Google Trends search query parsed and plotted on a national map, 
* a Media Cloud search query plotted on a line chart,
* a Twitter 'get_timelines' query that is parsed, analyzed with a sentiment dictionary, and then plotted as an interactive scatterplot
* a word cloud of tokenized tweets

Shiny apps are not easy to build. So here's a breakdown of how each of the above ideas was coded. The examples below contain no Shiny functions or special Shiny syntax. I find those are easier to add in later. 

## How each function was coded

### Google Trends search query

The following R code queries Google Trends for "interest by region" data for "Elizabeth Warren" searches between March 30 and April 30, 2019. Next, it pulls in a U.S. national map and stores it in statesMap. Then, it merges the Google Trends search data with the map and plots it all using ggplot. This code is thanks to Peer Christensen's [Storybench post](http://www.storybench.org/mapping-search-data-from-google-trends-in-r/).
 
``` 
  user1 <- gtrendsR::gtrends(c("Elizabeth Warren"), time = "2019-03-30 2019-04-30", gprop = "web", geo = c("US"))

  InterestByRegion <- dplyr::as_tibble(user1$interest_by_region)
  InterestByRegion <- InterestByRegion %>% 
    dplyr::mutate(region = stringr::str_to_lower(location))

  statesMap <- ggplot2::map_data("state")

  Merged <- merge(statesMap, InterestByRegion, by = "region")
  Merged <- InterestByRegion %>% dplyr::left_join(x = ., y = statesMap, by = "region")

  legend_title <- "search volume"
  
  pmap1 <- ggplot2::ggplot(Merged, aes(x = long, y = lat)) +
    theme_void() +
    geom_polygon(aes(group = group, 
                     fill = log(hits))) +
    ggplot2::coord_fixed(1.3) +
    scale_fill_gradient(legend_title, low = "white", high = "black")
  pmap1
```

I usually try this code in a separate R file to make sure it works. 


### Media Cloud search query



```
    mc.key <- "XXXXXXXXXX"

    mc.q1 <- "https://api.mediacloud.org/api/v2/stories_public/count?q="
    mc.q2 <- "&split=1&split_period=day&fq=publish_date:%5B2019-03-25T00:00:00.000Z+TO+2019-04-25T00:00:00.000Z%5D&key="

    query1 <- URLencode(paste(c(datasetInput2()), collapse = "%20"))

    mc.query1 <- jsonlite::fromJSON(paste0(mc.q1, query1, mc.q2, mc.key))$counts

    mc.query1$date <- as.Date(mc.query1$date)
    mc.query1 <- mc.query1 %>% filter(date > "2019-03-25" & date < "2019-04-25")

    pmedia1 <- ggplot(mc.query1, aes(date, count)) +
      theme_minimal() +
      ylab("Sentences per day") +
      xlab("") +
        ylim(0,10000) +
      theme_ipsum() +
      scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b")) +
      geom_line(stat="identity")  
    pmedia1
```

### Twitter 'get_timelines' query


### Word Cloud of tokenized tweets




