#' Roxygen style comments
#' This structure supports later package documentation

library(shiny)
library(tidyverse)
library(leaflet)
library(usmap)
library(shinythemes)
library(plotly)
library(tmap)
library(spData)
library(rstatix)
library(broom)
library(bslib)

# set theme using BLM color palette
# used bootstrap css variables from this link:
# https://github.com/rstudio/bootstraplib/blob/ba67fa9f6/inst/node_modules/bootstrap/scss/_variables.scss#L829-L850
bs_theme_new()
bs_theme_base_colors(bg = "#535052", fg = "#FDFBFB")
bs_theme_accent_colors(primary = "#FBEE1F", secondary = "#252421")
bs_theme_add_variables(
  spacer = "1.5rem",
  "well-bg" = "mix(#535052, #FDFBFB, 80%)", # used this code to adjust wellPanel bg color: https://rstudio.github.io/bootstraplib/articles/recipes.html
  "card-border-color" = "darken($well-bg, 5%)",
  "card-border-radius" = 0,
  "card-border-width" = "0.5rem",
  "font-size-base"= "1.1rem",
  "font-weight-base" = 425,
  "table-bg" = "#252421",
  "headings-line-height" = 2.5
  )

# tmap world data
data(World)

#spData US states, Hawaii, & Alaska data
data(us_states)
data(hawaii)
data(alaska)

# increase maximum file size to 10 MB
options(shiny.maxRequestSize = 10*1024^2)

# read in & combine clean tweet files
for (i in 1:3) {
  filename <- str_c("../data-clean/clean_blm_tweets",
                    as.character(i),
                    ".rds",
                    collapse = "")
  if (i==1) {
    tweets <- read_rds(filename)
  } else {
    tweets <- bind_rows(tweets, read_rds(filename))
  }

}

# read in token files
tweets_words <- readRDS("../data-clean/tweets_words.rds")
tweets_hashtags <- readRDS("../data-clean/tweets_hashtags.rds")
tweets_mentions <- readRDS("../data-clean/tweets_mentions.rds")
tweets_emojis <- readRDS("../data-clean/tweets_emojis.rds")
tweets_world <- readRDS("../data-clean/world_tweets.rds")
tweets_states <- readRDS("../data-clean/states_tweets.rds")

# read in analysis files
tweets_analysis <- readRDS("../data-clean/tweets_analysis.rds")
tweets_incl <- readRDS("../data-clean/tweets_incl.rds")
tweets_excl <- readRDS("../data-clean/tweets_excl.rds")




loca <- c("Washington, DC", "New York", "New Jersey","California")
tweets %>%
  filter(is.na(location) != TRUE)%>%
  mutate(NewYork = str_detect(location, "New York"),NewJersey = str_detect(location, "New Jersey"), Calif=str_detect(location, "California"), DC = str_detect(location, "Washington, DC"))->
  bbb
bbb %>%
  count(NewYork, NewJersey, DC, Calif)->
  count_states

tibble(California = count_states$n[2], "New York" = count_states$n[5], "New Jersey"= count_states$n[4], "Washington, DC"= count_states$n[3])->
  states

states %>%
  pivot_longer(c("Washington, DC", "New York", "New Jersey","California"), names_to= "state", values_to= "count")->
  states

color_vars <- c(
  "# Favorites" = "favorite_count",
  "# Retweets" = "retweet_count"
)

map_color_options <- c("# of Tweets", "Average # of Hashtags", "SenticNet Score", "SlangSD Score")


twitterIcon <- makeIcon(
  iconUrl = "https://cdn2.iconfinder.com/data/icons/metro-uinvert-dock/128/Twitter_NEW.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 20)

#Variables for analysis
cont_vars <- c("SenticNet", "SlangSD")
discr_vars1 <- c("Inclusive", "Exclusive")
discr_vars2 <- c("BLM", "BlackLivesMatter", "PoliceBrutality", "DefundthePolice")

cbbPalette <- c("#A9A9A9", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


##############################################################################################
# UI Code
##############################################################################################
ui <- fluidPage(
  bootstrap(),
  titlePanel("Sentiment Analysis of BLM: Do Hashtags Matter?"), #title

  tabsetPanel( #starting 3 tabs
    type = "pills",
    # Tab 1
    tabPanel("EDA",
             
             h3("How popular are the tweets?"),
             ## Row 1
             fluidRow(
               ### Column 1
               column(4,
                      wellPanel(
                        sliderInput("retweet", "How many retweets do you want?", min = 0, max =35000, value = c(0,500)),
                        sliderInput("follower", "What minimal number of followers do you want?", min = 0, max =1000000, value = 1000) # Input tab 1
                        )
                      ),
               ### Column 2
               column(8,
                      plotOutput("freq")) # Output tab 1
               ),

             h3("Where are the most tweets coming from?"),
             ## Row 2
             fluidRow(
               ### Column 1
               column(6,
                      plotOutput("country")),
               ### Column 2
               column(6,
                      plotOutput("state")) # Output tab 1
               ), # End of row 2

             h3("What are people saying?"),
             ## Row 3
             fluidRow(
               ### Column 1
               column(4,
                      wellPanel(
                        radioButtons("term_type",
                                     "I would like to see the most popular...",
                                     c("Terms", "Hashtags", "User mentions", "Emojis"))
                        )
                      ),
               ### Column 2
               column(8,
                      plotlyOutput("termFreq_plot")
                      ) # End column 1
             )
             ), # End of tab one with two fluidrow.


    # Tab 2
    tabPanel("Mapping",
             ## Row 1
             fluidRow(
               ### Column 1
               column(2,
                      wellPanel(
                        radioButtons("map_color",
                                     "Select variable to color map",
                                     c("# of Tweets", "Average # of Hashtags", "SenticNet Score", "SlangSD Score")
                                     )
                        )
                      ),
               ### Column 2
               column(10,
                      tabsetPanel(
                        #### Tab 1: US states
                        tabPanel("Trends by U.S. state",
                                 tmapOutput("dyn_us", width = "100%", height = 500)),
                        tabPanel("Trends by country",
                                 tmapOutput("dyn_world", width = "100%", height = 500))
                      )
                      # h2("Trends by U.S. state"),
                      # tmapOutput("dyn_us")
                      )
             ),

             ## Row 2
             fluidRow(
               h3("Inspect individual tweets"),
               ### Column 1
               column(12,
                      leafletOutput("map")
                      )
               )


             ),# End of tab two with two fluidrow.


    # Tab 3
    tabPanel("Analysis",
             ## Row 1
             fluidRow(
               ### Column 1
               column(4,
                      wellPanel(
                        #Select inclusive/exclusive variables
                        radioButtons(inputId = "incl",
                                     label = "Allow tweets that include more than one hashtag?",
                                     choices = discr_vars1,
                                     selected = discr_vars1[1]),
                        
                        # Select sentiment variable for x-axis
                        selectInput(inputId = "sent",
                                    label = "Sentiment Source",
                                    choices = cont_vars,
                                    selected = cont_vars[1]),
                        
                        # Select level/s to show for the inclusive/exclusive category
                        uiOutput("selected_incl")
                      )
                      ), # End column 1

               ### Column 2
               column(8,
                      plotOutput("boxplot1"),
                      ) # End column 2

               ), # End row 1
             
             ## Row 2
             fluidRow(
               column(6, 
                      #h4("ANOVA Post-Hoc Results"),
                      tableOutput("anova_results")
                      ), #end column 1
               column(6, 
                      plotOutput("barplot"))
             ), #End row 2

             ## Row 3
             fluidRow(
               column(6,
                      #Select hashtag variables
                      selectInput(inputId = "hash",
                                  label = "Select a primary hashtag:",
                                  choices = discr_vars2,
                                  selected = discr_vars2[1]),
               ),
               column(6,

                      # Select level/s to show for the hash category
                      uiOutput("selected_hash")
               )
             ), #end of row 3
             ## Row 4
             fluidRow(
               ### Column 1
               column(12,
                      plotOutput("boxplot")
                      )
               ) # End of row 4
             ) #  End of tab three with 4 fluidrow.
    ) # End of all tabs
  ) # end of UI



##############################################################################################
# Server Code
##############################################################################################
server <- function(input, output, session) {
  output$freq <- renderPlot(height = 350,{tweets %>%
      filter(retweet_count<input$retweet[2] & retweet_count>input$retweet[1])%>%
      filter(followers_count>input$follower)%>%
      ggplot(aes(x=retweet_count))+
      theme_bw()+
      geom_freqpoly(bins= 1000)})

  output$country <- renderPlot(height = 350,{tweets %>%
      group_by(country_loc) %>%
      filter(n() >= 50) %>%
      ungroup %>%
      filter(is.na(country_loc) != TRUE)%>%
      ggplot(aes(x=country_loc))+
      geom_bar(color = "black", fill = "#FBEE1F", size = 1.5)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      xlab("Countries with 50+ tweets")+
      ylab("# of tweets")})

  output$state <- renderPlot(height = 350,{ggplot(data= states,aes(x=state, y=count))+
      geom_bar(stat = "identity", color = "black", fill = "#FBEE1F", size = 1.5)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      xlab("States with most tweets")+
      ylab("# of tweets")})

  # term frequency plots
  output$termFreq_plot <- renderPlotly({
    # Depending on input selection, show terms, hashtags, mentions, or emojis
    switch(input$term_type,
           "Terms" = tweets_words,
           "Hashtags" = tweets_hashtags,
           "User mentions" = tweets_mentions,
           "Emojis" = tweets_emojis %>% mutate(word = description)) %>%
      count(word) %>%
      top_n(20) %>%
      mutate(word = reorder(word, n)) ->
      df
    ggplot(df, aes(x = word, y = n, fill = word)) +
      geom_col(show.legend = FALSE) +
      scale_y_log10() +
      xlab(
        switch(input$term_type,
               "Terms" = "Word",
               "Hashtags" = "Hashtag",
               "User mentions" = "Mention",
               "Emojis" = "Emoji")
        ) +
      ylab("Count") +
      theme_bw() +
      coord_flip() -> myPlot

    myPlot %>%
      ggplotly(tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
    })


  # topic modeling plot
  # INSERT HERE


  # Create interactive map to show individual tweets
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.58,
              lat = 39.0,
              zoom = 4)  %>%
     addMarkers(data = tweets,
                lat = ~map_lat,
                lng = ~map_lon,
                popup = ~text,
                icon = twitterIcon,
                clusterOptions = markerClusterOptions())
  })

  # Create dynamic US map
  #
  # known issue with tmap not taking reactive inputs:
  # https://stackoverflow.com/questions/59643313/reactivity-problem-while-using-rendertmap-within-r-shiny
  #
  # create tmap reactive
  tmap_us <- reactive({
    tweets_states <- tweets_states %>%
      mutate(log_numTweets = log(num_tweets))
    us_df <- left_join(us_states, tweets_states, by = c("NAME" = "state"))
    us_df <- us_df[,c("NAME", setdiff(names(us_df), "NAME"))]

    hi_df <- left_join(hawaii, tweets_states, by = c("NAME" = "state"))
    hi_df <- hi_df[,c("NAME", setdiff(names(hi_df), "NAME"))]

    ak_df <- left_join(alaska, tweets_states, by = c("NAME" = "state"))
    ak_df <- ak_df[,c("NAME", setdiff(names(ak_df), "NAME"))]

    switch(input$map_color,
           "# of Tweets" = {
             tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap"))
             tmap_mode("view")
             tm_shape(us_df, projection = 2163) +
               tm_polygons("log_numTweets", title = "# of Tweets (Nat. log-scale)") +
               tm_shape(ak_df) +
               tm_polygons("log_numTweets", legend.show = FALSE) +
               tm_shape(hi_df) +
               tm_polygons("log_numTweets", legend.show = FALSE) +
               tm_view(set.view = c(-98.58, 49.0, 3.3))
           },
           "Average # of Hashtags" = {
             tmap_mode("view")
             tm_shape(us_df, projection = 2163) +
               tm_polygons("ave_num_ht", title = "Average # of Hashtags") +
               tm_shape(ak_df) +
               tm_polygons("ave_num_ht", legend.show = FALSE) +
               tm_shape(hi_df) +
               tm_polygons("ave_num_ht", legend.show = FALSE) +
               tm_view(set.view = c(-98.58, 49.0, 3.3))
           },
           "SenticNet Score" = {
             tmap_mode("view")
             tm_shape(us_df, projection = 2163) +
               tm_polygons("sent_score_senticnet", title = "Average SenticNet Score") +
               tm_shape(ak_df) +
               tm_polygons("sent_score_senticnet", legend.show = FALSE) +
               tm_shape(hi_df) +
               tm_polygons("sent_score_senticnet", legend.show = FALSE) +
               tm_view(set.view = c(-98.58, 49.0, 3.3))
           },
           "SlangSD Score" = {
             tmap_mode("view")
             tm_shape(us_df, projection = 2163) +
               tm_polygons("sent_score_slang", title = "Average SlangSD Score") +
               tm_shape(ak_df) +
               tm_polygons("sent_score_slang", legend.show = FALSE) +
               tm_shape(hi_df) +
               tm_polygons("sent_score_slang", legend.show = FALSE) +
               tm_view(set.view = c(-98.58, 49.0, 3.3))
           }
    )
  })

  # create output
  output$dyn_us <- renderTmap({tmap_us()})


  # Create dynamic world map
  #
  # create tmap reactive
  tmap_world <- reactive({
    map_tweets <- tweets_world[,c("iso_a3", setdiff(names(tweets_world), "iso_a3"))]
    switch(input$map_color,
           "# of Tweets" = {
             map_tweets %>%
               mutate(log_numTweets = log(num_tweets)) ->
               map_tweets
             tmap_mode("view")
             tm_shape(map_tweets) +
               tm_polygons("log_numTweets", title = "# of Tweets (Nat. log-scale)") +
               tm_view(set.view = c(0, 25, 1.5))
           },
           "Average # of Hashtags" = {
             tmap_mode("view")
             tm_shape(map_tweets) +
               tm_polygons("ave_num_ht", title = "Average # of Hashtags") +
               tm_view(set.view = c(0, 25, 1.5))
           },
           "SenticNet Score" = {
             tmap_mode("view")
             tm_shape(map_tweets) +
               tm_polygons("sent_score_senticnet", title = "Average SenticNet Score") +
               tm_view(set.view = c(0, 25, 1.5))
           },
           "SlangSD Score" = {
             tmap_mode("view")
             tm_shape(map_tweets) +
               tm_polygons("sent_score_slang", title = "Average SlangSD Score") +
               tm_view(set.view = c(0, 25, 1.5))
           }
    )
  })

  # create output
  output$dyn_world <- renderTmap({tmap_world()})


  # Show levels for the discrete variable selected in input$selected_incl
  output$selected_incl <- renderUI({
    checkboxGroupInput(inputId = "incl_levels",
                 label = "Boxplot: Select one or more hashtags",
                 choices = choices_incl(),
                 selected = choices_incl())
  })

  choices_incl <- reactive({
      df_incl <- select(tweets_incl, input$incl)
      return(levels(df_incl[[1]]))      
  })

  
    output$barplot <- renderPlot(height=350,{
      if (input$incl == "Inclusive") {
        b <- ggplot(tweets_incl, aes(x = word)) +
          labs(title = "Number of Tweets by Hashtag, Inclusive*",
              x = "Hashtag", y = "Count", caption = "*More than one hashtag may be counted in the same tweet")
      } #end if inclusive

      else if (input$incl == "Exclusive") {
      b <- ggplot(tweets_excl, aes(x = word)) +
          labs(title = "Number of Tweets by Hashtag, Exclusive*",
               x = "Hashtag", y = "Count", caption = "*Limited to tweets with only specified BLM-related hashtag")

      } #end if exclusive
      
      b <- b +
        geom_bar(stat = "count", color = "black", fill = "#FBEE1F", size = 1.5) +
        scale_y_continuous(limits = c(0, 30000)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      b
      }) 



  # output small boxplot for tab 3
    
  output$boxplot1 <- renderPlot(height = 350,{
  
    #generate df based on inputs selected
    if (input$incl == "Inclusive") {
      df_incl <- select(tweets_incl, input$sent, !!input$incl) %>%
        filter(!!(as.name(input$incl)) %in% input$incl_levels)
      
      p <- ggplot(data = df_incl, aes_string(x = input$incl,
                                          y = input$sent,
                                          fill = input$incl)) +
        labs(title = "Sentiment Score by Hashtag, Inclusive*",
             x = "Hashtag", caption = "*More than one hashtag may be counted in the same tweet")
      
      } else if (input$incl == "Exclusive") {
        df_excl <- select(tweets_excl, input$sent, !!input$incl) %>%
          filter(!!(as.name(input$incl)) %in% input$incl_levels)
        
        p <- ggplot(data = df_excl, aes_string(x = input$incl,
                                            y = input$sent,
                                            fill = input$incl)) +
          labs(title = "Sentiment Score by Hashtag, Exclusive*",
               x = "Hashtag", caption = "*Limited to tweets with only specified BLM-related hashtag")
   
    }

      p <- p + 
        geom_boxplot(is.na = FALSE) +
        scale_fill_manual(values=cbbPalette, na.translate = F) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
      p

  })  # end renderPlot for small boxplot
  
  #Output anova table for tab 1
  output$anova_results <- renderTable(
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    spacing = "s",
    { # start expression
    
    if (input$incl == "Exclusive") {
      tweets_excl %>%
        filter(Exclusive != "All blm-related hashtags") ->
        aov_df
      aov_results <- aov(aov_df[[input$sent]] ~ Exclusive, data = aov_df)

    } else {  
      
      #Add weight to inclusive dataset (to give less weight to tweets with more than 1 hashtag)  
      tweets_incl %>%
        add_count(status_id) %>%
        mutate(weight = case_when(n == 4 ~ 0.25,
                                  n == 3 ~ 0.33,
                                  n == 2 ~ 0.5,
                                  n == 1 ~ 1)) ->
        aov_df

      model = lm(aov_df[[input$sent]] ~ Inclusive, data = aov_df, weights = weight)
      aov_results <- aov(model)

    }
    
    #Tukey Honest Significant Difference for multiple pairwise-comparison between the means of groups
    TukeyHSD(aov_results) %>%
      tidy() %>%
      select("Comparison" = 2, 
             "Estimate" = estimate, 
             "95% Lower" = conf.low, 
             "95% Higher" = conf.high, 
             "Adjusted P-value" = adj.p.value)

  }) #end renderTable
  

  # Show levels for the discrete variable selected in input$selected_hash
  output$selected_hash <- renderUI({
    checkboxGroupInput(inputId = "show_levels",
                       label = "Boxplot: Select hashtag combinations of interest",
                       choices = choices_hash(),
                       selected = choices_hash())
  })

  choices_hash <- reactive({
    df <- select(tweets_analysis, input$hash)
    return(levels(df[[1]]))
  })

  #output large boxplot for tab 3
  output$boxplot <- renderPlot({

    #generate df based on inputs selected
    df <- select(tweets_analysis, input$sent, input$hash) %>%
      filter(!!(as.name(input$hash)) %in% input$show_levels)

    ggplot(data = df, aes_string(x = input$hash,
                                 y = input$sent,
                                 fill = input$hash)) +
      geom_boxplot() +
      scale_fill_manual(values=cbbPalette) +
      theme_bw() +
      theme(legend.position = "none") +
      coord_flip()

  }) #end renderPlot for boxplot

} #end of server

shinyApp(ui, server)
