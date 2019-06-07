library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(plotly)
library(stringr)
library(DT)
library(rtweet)
library(scales)
library(gtrendsR)
library(maps)
library(jsonlite)
library(lubridate)
library(wordcloud)
library(hrbrthemes)

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
                            consumer_key = "XXXXXXXXXXXXXX",
                            consumer_secret = "XXXXXXXXXXXXXX",
                            access_token = "XXXXXXXXXXXXXX",
                            access_secret = "XXXXXXXXXXXXXX")

output$p2 <- renderPlot({
    
    if (is.null(input$user)){
      return(NULL)      
    }
    
    u <- get_timelines(c(input$user), n = 500)
    u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
    u$post <- u$text
    u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
    p2 <- ggplot(u, aes(date)) + 
      geom_histogram(bins = 300, stat="count") +
      xlab("") +
      theme_minimal() 
    p2
    
  })
  
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
  
    mc.key <- "XXXXXXXXXXXXXX"

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
  
})
  
output$p3 <- renderPlotly({
    
    if (is.null(input$user)){
      return(NULL)
    }
    
    u <- get_timelines(c(input$user), n = 500)
    u <- u %>% select(screen_name, created_at, text, favorite_count, retweet_count)
    u$post <- u$text
    u$date <- as.Date(u$created_at, format = "%m/%d/%y")
    
    #selectcomments <- df %>% filter(str_detect(message, input$keyword))
    #selectcomments$post <- selectcomments$message
    
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
    
  mc.key <- "XXXXXXXXXXXXXX"
  
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
    theme_ipsum() +
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

