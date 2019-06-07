# How to build a Shiny app that visualizes media coverage, tweets and Google search interest

The following documentation explains the process of creating a Shiny app in RStudio. 

Inspired by ProPublica and Google's Election DataBot, an interactive exploration of the 2016 midterms, [this Shiny app](storybench.shinyapps.io/twitter) acts as a dynamic interface to explore 2020 Democratic candidate tweets, Google search interest and mainstream media attention. 

Feedback? DM me at [@aleszubajak](http://twitter.com/@aleszubajak).

![img](https://raw.githubusercontent.com/aleszu/2020-shiny-app/master/img/shiny1.png)
![img](https://raw.githubusercontent.com/aleszu/2020-shiny-app/master/img/shiny2.png)
![img](https://raw.githubusercontent.com/aleszu/2020-shiny-app/master/img/shiny3.png)
![img](https://raw.githubusercontent.com/aleszu/2020-shiny-app/master/img/shiny4.png)

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

The following code queries the [Media Cloud API](https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md) for the count of stories mentioning "Elizabeth Warren" published across its U.S. mainstream media outlets collection between March 25 and April 25. It strings together a query using R's paste0() function and then fixes the date before plotting it as a line chart in ggplot. Thanks to [Cornelius Puschmann](http://cbpuschmann.net/) and [Martin Frigaard](http://www.storybench.org/how-to-access-apis-in-r/) for helping me with this code.

```
  mc.key <- "XXXXXXXXXX"

  mc.q1 <- "https://api.mediacloud.org/api/v2/stories_public/count?q="
  mc.q2 <- "&split=1&split_period=day&fq=publish_date:%5B2019-03-25T00:00:00.000Z+TO+2019-04-25T00:00:00.000Z%5D&key="

  query1 <- URLencode(paste(c("Elizabeth Warren"), collapse = "%20"))

  mc.query1 <- jsonlite::fromJSON(paste0(mc.q1, query1, mc.q2, mc.key))$counts

  mc.query1$date <- as.Date(mc.query1$date)
  mc.query1 <- mc.query1 %>% filter(date > "2019-03-25" & date < "2019-04-25")

  pmedia1 <- ggplot(mc.query1, aes(date, count)) +
    geom_line(stat="identity") +
    theme_minimal() +
    ylab("Sentences per day") +
    xlab("") +
    ylim(0,10000) +
    scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b")) 
  pmedia1
    
```

### Twitter 'get_timelines' query

The next chunk of code, inspired by Martin Frigaard's [tutorial on the rtweet package on Storybench](http://www.storybench.org/get-twitter-data-rtweet-r/), gets Elizabeth Warren's last 500 tweets, parses them, fixes their dates and then performs tokenization, sentiment analysis and calculating an average score for each tweet using this incredible tutorial from [tidytext package creators Julia Silge and David Robinson](https://www.tidytextmining.com/) though I opted to use the [labMT sentiment dictionary](https://buildmedia.readthedocs.org/media/pdf/labmt-simple/latest/labmt-simple.pdf) favored by UVM's Computational Story Lab. Finally, it builds an interactive scatterplot using the [plotly package which has a helpful tutorial here](https://plot.ly/ggplot2/geom_point/). 

```
  u <- get_timelines(c("Elizabeth Warren"), n = 500)
  u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
  u$post <- u$text
  u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
  tokenized_tweets <- u %>%
      select(post, created_at, text, date, favorite_count, retweet_count) %>%
      unnest_tokens(word, post) %>%
      anti_join(stop_words) %>%
      group_by(word, created_at, text, date, favorite_count, retweet_count) %>%
      tally() %>%
      arrange(desc(n))
    
  sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
  labMT <- sentiments %>%
      select(word, happs)
    
  all_sentiment <- tokenized_tweets %>%  
      inner_join(labMT, by = "word") %>%
      group_by(text, created_at, favorite_count, retweet_count) %>% 
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372) 
    
  all_sentiment$date <- as.Date(all_sentiment$created_at, format = "%m/%d/%y")
    
  all_sentiment <- all_sentiment %>% filter(created_at > "2019-03-25")
    
  allsentplot <- ggplot(all_sentiment, aes(date, score, color=score, text=text)) + # , text = text, score = score)) + 
      geom_point() + 
      scale_color_gradient(low="red", high="blue")

  ggplotly(allsentplot, tooltip=c("text"))
```


### Word cloud of tokenized tweets

Finally, I built a word cloud using the [wordcloud package](http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know) by re-tokenizing the Twitter timelines using the following code, which also cleans out [stop words](https://rdrr.io/cran/tidytext/man/stop_words.html) and some errant URL language like "amp" and "http."

```
  u <- get_timelines(c(input$user), n = 500)
  u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
  u$post <- u$text
  u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
  tokenized_tweets <- u %>%
      select(post, created_at, text, date, favorite_count, retweet_count) %>%
      unnest_tokens(word, post) %>%
      anti_join(stop_words) %>%
      filter(word != "amp") %>% 
      filter(word != "t.co") %>%
      filter(word != "http") %>% 
      filter(word != "https") %>% 
      group_by(word) %>% #, created_at, text, date, favorite_count, retweet_count) %>%
      tally() %>%
      arrange(desc(n))
    
  wordcloud(tokenized_tweets$word, scale=c(2, 0.4), freq=tokenized_tweets$n, max.words = 100)
```

## Preparing Shiny's user interface

Below is an excerpt of the user interface code that creates the dropdown menus. The reason everything is duplicated is that I want to create two columns with every function so that users can compare between 2020 candidates. For a barebones example of a Shiny app that will make all this more clear, check out [this example](https://shiny.rstudio.com/gallery/telephones-by-region.html). 

```
fluidRow(
    column(6, 
           selectInput("user", "Candidate:", 
                       choices = c("BernieSanders", "KamalaHarris", "EWarren", "BetoORourke", "JoeBiden", "PeteButtigieg", "AmyKlobuchar", "SenGillibrand", "CoryBooker", "JulianCastro", "SethMoulton", "TulsiGabbard"),
                       selected = "KamalaHarris"),
         submitButton("Submit"),
          h5("Google searches of candidate in last month", align = "center"),
          plotOutput("map1"),
          h5("Media attention of candidate in last month", align = "center"),
          plotOutput("media1"),
          h5("Sentiment of candidate’s last 500 tweets", align = "center"),
          plotlyOutput("p3", height=500),
          h5("Most frequent terms on Twitter in last month", align = "center"),
          plotOutput("freqterms1") 
    ),
     
    column(6, 
           selectInput("user2", "Candidate:", 
                       choices = c("BernieSanders", "KamalaHarris", "BetoORourke", "EWarren", "JoeBiden", "PeteButtigieg", "AmyKlobuchar", "SenGillibrand", "CoryBooker", "JulianCastro", "SethMoulton", "TulsiGabbard"),
                       selected = "BetoORourke"),
           submitButton("Submit"),
           h5("Google searches of candidate in last month", align = "center"),
           plotOutput("map2"),
           h5("Media attention of candidate in last month", align = "center"),
           plotOutput("media2"),
           h5("Sentiment of candidate’s last 500 tweets", align = "center"),
           plotlyOutput("p6", height=500),
           h5("Most frequent terms on Twitter in last month", align = "center"),
           plotOutput("freqterms2")
           )
     )
```

## Preparing Shiny's server-side code 

There are several Shiny tricks I've learned since I [started building these apps](https://github.com/aleszu/textanalysis-shiny). The following if statement returns nothing if no user has been chosen. This is helpful if you're waiting on a Shiny user's input before plotting a visualization.  

```
  if (is.null(input$user)){
    return(NULL)
  }
```

The following drives the dropdown menu that allows users to choose one of several 2020 candidates. What the user sees is on the left (i.e. EWarren). This reactive Shiny function then translates that choice into the string on the right (i.e. "Elizabeth Warren"). 

The only time I don't need this reactive function is for my rtweet queries, since I want the "EWarren" input to ask for @EWarren's timeline. If you've been paying attention you'll notice I could have instead just used this reactive function for the rtweet query.  

```
  datasetInput <- reactive({
    switch(input$user,
           EWarren = "Elizabeth Warren", 
           BetoORourke = "Beto O'Rourke", 
           JoeBiden="Joe Biden",
           BernieSanders="Bernie Sanders",  
           KamalaHarris="Kamala Harris",  
           AmyKlobuchar="Amy Klobuchar",  
           SenGillibrand="Kirsten Gillibrand", 
           CoryBooker="Cory Booker", 
           JulianCastro="Julian Castro", 
           PeteButtigieg="Pete Buttigieg",
           SethMoulton="SethMoulton", 
           TulsiGabbard = "TulsiGabbard")
  })
```

Finally, we need to swap out the queries in all the functions to reflect Shiny syntax. In the case of get_timelines, we use:  

```
  u <- get_timelines(c(input$user), n = 500)
```

You can scroll down and see how the rest were modified to accept the user's input. 

## Final code

```
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(plotly)
library(stringr)
library(rtweet)
library(scales)
library(gtrendsR)
library(maps)
library(jsonlite)
library(lubridate)
library(wordcloud)

ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"), 
  hr(),
  h4("Illuminating the road to 2020 using Twitter, Google searches and media coverage"),
  hr(),
  fluidRow(
    column(6, 
           selectInput("user", "Candidate:", 
                       choices = c("BernieSanders", "KamalaHarris", "EWarren", "BetoORourke", "JoeBiden", "PeteButtigieg", "AmyKlobuchar", "SenGillibrand", "CoryBooker", "JulianCastro", "SethMoulton", "TulsiGabbard"),
                       selected = "KamalaHarris"),
         submitButton("Submit"),
           
    br(),
    br(),
          h5("Google searches of candidate in last month", align = "center"),
          plotOutput("map1"),
    br(),
    br(),
          h5("Media attention of candidate in last month", align = "center"),
          plotOutput("media1"),
    br(),
    br(),
          h5("Sentiment of candidate’s last 500 tweets", align = "center"),
          plotlyOutput("p3", height=500),  
    br(),
    br(),
          h5("Most frequent terms on Twitter in last month", align = "center"),
          plotOutput("freqterms1") 
    ),
     
    column(6, 
           selectInput("user2", "Candidate:", 
                       choices = c("BernieSanders", "KamalaHarris", "BetoORourke", "EWarren", "JoeBiden", "PeteButtigieg", "AmyKlobuchar", "SenGillibrand", "CoryBooker", "JulianCastro", "SethMoulton", "TulsiGabbard"),
                       selected = "BetoORourke"),
           submitButton("Submit"),
           
           br(),
           br(),
           h5("Google searches of candidate in last month", align = "center"),
           plotOutput("map2"),
           br(),
           br(),
           h5("Media attention of candidate in last month", align = "center"),
           plotOutput("media2"),
           br(),
           br(),
           h5("Sentiment of candidate’s last 500 tweets", align = "center"),
           plotlyOutput("p6", height=500),
           br(),
           br(),
           h5("Most frequent terms on Twitter in last month", align = "center"),
           plotOutput("freqterms2")
           )
     ),
  br(),
  br(),
  br(),
  h6("Built by Aleszu Bajak at Northeastern University’s School of Journalism using rtweet to access Twitter's REST API, gtrendsR to access Google Trends, and jsonlite to access the Media Cloud API's top online media sources, including The New York Times, Vox, Guardian, Slate, The Washington Post, Breitbart, Fox News, CNN and more.")
)
  

server = function(input, output, session) {

  
create_token(app = "hacknews2014", 
                            consumer_key = "XXXXXXXX",
                            consumer_secret = "XXXXXXXX",
                            access_token = "XXXXXXXX",
                            access_secret = "XXXXXXXX")

output$media1 <- renderPlot({
    
    if (is.null(input$user)){
      return(NULL)
    }
    
    datasetInput2 <- reactive({
      switch(input$user,
             EWarren = "Elizabeth Warren", 
             BetoORourke = "Beto O'Rourke", 
             JoeBiden="Joe Biden",
             BernieSanders="Bernie Sanders",  
             KamalaHarris="Kamala Harris",  
             AmyKlobuchar="Amy Klobuchar",  
             SenGillibrand="Kirsten Gillibrand", 
             CoryBooker="Cory Booker", 
             JulianCastro="Julian Castro", 
             PeteButtigieg="Pete Buttigieg",
             SethMoulton="SethMoulton", 
             TulsiGabbard = "TulsiGabbard")
    })
  
    mc.key <- "XXXXXXXX"

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
      scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b")) +
      geom_line(stat="identity")  
    pmedia1
  
})
  
output$p3 <- renderPlotly({
    
    if (is.null(input$user)){
      return(NULL)
    }
    
    u <- get_timelines(c(input$user), n = 500)
    u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
    u$post <- u$text
    u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
    tokenized_tweets <- u %>%
      select(post, created_at, text, date, favorite_count, retweet_count) %>%
      unnest_tokens(word, post) %>%
      anti_join(stop_words) %>%
      group_by(word, created_at, text, date, favorite_count, retweet_count) %>%
      tally() %>%
      arrange(desc(n))
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    all_sentiment <- tokenized_tweets %>%  
      inner_join(labMT, by = "word") %>%
      group_by(text, created_at, favorite_count, retweet_count) %>% 
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372) 
    
    all_sentiment$date <- as.Date(all_sentiment$created_at, format = "%m/%d/%y")
    
    all_sentiment <- all_sentiment %>% filter(created_at > "2019-03-25")
    
    allsentplot <- ggplot(all_sentiment, aes(date, score, color=score, text=text)) + # , text = text, score = score)) + 
      geom_point() + 
      scale_color_gradient(low="red", high="blue")

    ggplotly(allsentplot, tooltip=c("text"))
    
  })
  
output$map1 <- renderPlot({ 
    
    if (is.null(input$user)){
      return(NULL)
    }
  
  datasetInputgtrend1 <- reactive({
    switch(input$user,
           EWarren = "Elizabeth Warren", 
           BetoORourke = "Beto O'Rourke", 
           JoeBiden="Joe Biden",
           BernieSanders="Bernie Sanders",  
           KamalaHarris="Kamala Harris",  
           AmyKlobuchar="Amy Klobuchar",  
           SenGillibrand="Kirsten Gillibrand", 
           CoryBooker="Cory Booker", 
           JulianCastro="Julian Castro", 
           PeteButtigieg="Pete Buttigieg",
           SethMoulton="SethMoulton", 
           TulsiGabbard = "TulsiGabbard")
  })
    
  user1 <- gtrends(c(datasetInputgtrend1()), time = "2019-03-30 2019-04-30", gprop = "web", geo = c("US"))
  
  InterestByRegion <- dplyr::as_tibble(user1$interest_by_region)
  InterestByRegion <- InterestByRegion %>% 
    dplyr::mutate(region = stringr::str_to_lower(location))
  
  statesMap <- ggplot2::map_data("state")
  
  Merged <- merge(statesMap, InterestByRegion, by = "region")
  Merged <- InterestByRegion %>% dplyr::left_join(x = ., y = statesMap, by = "region")
  
  legend_title <- "search volume"
  
  pmap1 <- ggplot(Merged, aes(x = long, y = lat)) +
    theme_void() +
    geom_polygon(aes(group = group, 
                     fill = log(hits))) +
    ggplot2::coord_fixed(1.3) +
    scale_fill_gradient(legend_title, low = "white", high = "black")
  pmap1
  
})
  

output$freqterms1 <- renderPlot({ 
    
    if (is.null(input$user)){
      return(NULL)
    }
    
    u <- get_timelines(c(input$user), n = 500)
    u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
    u$post <- u$text
    u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
    tokenized_tweets <- u %>%
      select(post, created_at, text, date, favorite_count, retweet_count) %>%
      unnest_tokens(word, post) %>%
      anti_join(stop_words) %>%
      filter(word != "amp") %>% 
      filter(word != "t.co") %>%
      filter(word != "http") %>% 
      filter(word != "https") %>% 
      group_by(word) %>% #, created_at, text, date, favorite_count, retweet_count) %>%
      tally() %>%
      arrange(desc(n))
    
    wordcloud(tokenized_tweets$word, scale=c(2, 0.4), freq=tokenized_tweets$n, max.words = 100)
  
  })
  





# *******************
# Column 2



output$media2 <- renderPlot({
  
  if (is.null(input$user2)){
    return(NULL)
  }
  
  datasetInput <- reactive({
    switch(input$user2,
           EWarren = "Elizabeth Warren", 
           BetoORourke = "Beto O'Rourke", 
           JoeBiden="Joe Biden",
           BernieSanders="Bernie Sanders",  
           KamalaHarris="Kamala Harris",  
           AmyKlobuchar="Amy Klobuchar",  
           SenGillibrand="Kirsten Gillibrand", 
           CoryBooker="Cory Booker", 
           JulianCastro="Julian Castro", 
           PeteButtigieg="Pete Buttigieg",
           SethMoulton="SethMoulton", 
           TulsiGabbard = "TulsiGabbard")
  })
    
  mc.key <- "XXXXXXXX"
  
  mc.q1 <- "https://api.mediacloud.org/api/v2/stories_public/count?q="
  mc.q2 <- "&split=1&split_period=day&fq=publish_date:%5B2019-03-25T00:00:00.000Z+TO+2019-04-25T00:00:00.000Z%5D&key="
  
  query2 <- URLencode(paste(c(datasetInput()), collapse = "%20"))
  
  mc.query2 <- jsonlite::fromJSON(paste0(mc.q1, query2, mc.q2, mc.key))$counts
  
  mc.query2$date <- as.Date(mc.query2$date)
  mc.query2 <- mc.query2 %>% filter(date > "2019-03-25" & date < "2019-04-25") 
  
  pmedia2 <- ggplot(mc.query2, aes(date, count)) + 
    theme_minimal() + 
    ylab("Sentences per day") +
    xlab("") +
    ylim(0,10000) +
    scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b")) +
    geom_line(stat="identity")   
  pmedia2

})

output$p6 <- renderPlotly({
  
  if (is.null(input$user2)){
    return(NULL)
  }
  
  u <- get_timelines(c(input$user2), n = 500)
  u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
  u$post <- u$text
  
  tokenized_tweets <- u %>%
    select(post, created_at, text, favorite_count, retweet_count) %>%
    unnest_tokens(word, post) %>%
    anti_join(stop_words) %>%
    group_by(word, created_at, text, favorite_count, retweet_count) %>%
    tally() %>%
    arrange(desc(n))
  
  sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
  labMT <- sentiments %>%
    select(word, happs)
  
  all_sentiment <- tokenized_tweets %>%  
    inner_join(labMT, by = "word") %>%
    group_by(text, created_at, favorite_count, retweet_count) %>% 
    summarize(sentiment = mean(happs)) %>%
    arrange(desc(sentiment)) %>%
    mutate("score" = sentiment-5.372) 
  
  all_sentiment$date <- as.Date(all_sentiment$created_at, format = "%m/%d/%y")
  
  all_sentiment <- all_sentiment %>% filter(created_at > "2019-03-25")
  
  allsentplot2 <- ggplot(all_sentiment, aes(date, score, color=score, text=text)) + # , text = text, score = score)) + 
    geom_point() + 
    scale_color_gradient(low="red", high="blue")
  
  ggplotly(allsentplot2, tooltip=c("text"))
  
})



output$map2 <- renderPlot({ 
  
  if (is.null(input$user2)){
    return(NULL)
  }
  
  datasetInputgtrend2 <- reactive({
    switch(input$user2,
           EWarren = "Elizabeth Warren", 
           BetoORourke = "Beto O'Rourke", 
           JoeBiden="Joe Biden",
           BernieSanders="Bernie Sanders",  
           KamalaHarris="Kamala Harris",  
           AmyKlobuchar="Amy Klobuchar",  
           SenGillibrand="Kirsten Gillibrand", 
           CoryBooker="Cory Booker", 
           JulianCastro="Julian Castro", 
           PeteButtigieg="Pete Buttigieg",
           SethMoulton="SethMoulton", 
           TulsiGabbard = "TulsiGabbard")
  })
  
  user2 <- gtrends(c(datasetInputgtrend2()), time = "2019-03-30 2019-04-30", gprop = "web", geo = c("US"))
  
  InterestByRegion2 <- dplyr::as_tibble(user2$interest_by_region)
  InterestByRegion2 <- InterestByRegion2 %>% 
    dplyr::mutate(region = stringr::str_to_lower(location))
  
  statesMap <- ggplot2::map_data("state")
  
  Merged2 <- merge(statesMap, InterestByRegion2, by = "region")
  Merged2 <- InterestByRegion2 %>% dplyr::left_join(x = ., y = statesMap, by = "region")
  
  legend_title <- "search volume"
  
  pmap2 <- ggplot(Merged2, aes(x = long, y = lat)) +
    theme_void() +
    geom_polygon(aes(group = group, 
                     fill = log(hits))) +
    ggplot2::coord_fixed(1.3) +
    scale_fill_gradient(legend_title, low = "white", high = "black")
  pmap2
  
})




output$freqterms2 <- renderPlot({ 

  if (is.null(input$user2)){
    return(NULL)
  }
  
  u <- get_timelines(c(input$user2), n = 500)
  u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
  u$post <- u$text
  u$date <- as.Date(u$created_at, format = "%m/%d/%y")
  
  tokenized_tweets <- u %>%
    select(post, created_at, text, date, favorite_count, retweet_count) %>%
    unnest_tokens(word, post) %>%
    anti_join(stop_words) %>%
    filter(word != "amp") %>%
    filter(word != "t.co") %>%
    filter(word != "http") %>% 
    filter(word != "https") %>% 
    group_by(word) %>% 
    tally() %>%
    arrange(desc(n))
  
  wordcloud(tokenized_tweets$word, scale=c(2, 0.4), freq=tokenized_tweets$n, max.words = 100)
  
})

}

shinyApp(ui = ui, server = server)
```


