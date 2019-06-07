# 2020-shiny-app

The following documentation explains the process of creating a Shiny app in RStudio. Inspired by Election DataBot, ProPublica and Google’s interactive exploration of voters and candidates in the 2016 midterms, this Shiny app acts as a dynamic interface to explore 2020 Democratic candidate tweets, Google search interest and mainstream media attention. 

# Required R packages and API credentials

This Shiny app uses rtweet to access Twitter's REST API, gtrendsR to access Google Trends, and jsonlite to access the Media Cloud API's top online media sources, including The New York Times, Vox, Guardian, Slate, The Washington Post, Breitbart, Fox News, CNN and more. For Twitter and Media Cloud, you must request your own API keys. 

Other packages used are: dplyr, tidyverse, tidytext, stringr, scales, maps, plotly, lubridate, wordcloud and hrbrthemes, in addition to shiny and shinythemes. 

# Stringing together several functions 

This Shiny app is the sum of several parts: 

* a Google Trends search query parsed and plotted on a national map, 
* a Media Cloud search query plotted on a line chart,
* a Twitter 'get_timelines' query that is parsed, analyzed with a sentiment dictionary, and then plotted as an interactive scatterplot
* a word cloud of tokenized tweets

Shiny apps are not easy to build. So here's a breakdown of how each of the above ideas was coded. The examples below contain no Shiny functions or special Shiny syntax. I find those are easier to add in later. 

# How each function was coded

### Google Trends search query



### Media Cloud search query



### Twitter 'get_timelines' query


### Word Cloud of tokenized tweets




