library(shiny)
# Define server logic required to plot various variables 

library(RCurl)
library(bitops)
library(rjson)
library(streamR)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)
library(bitops)
library(chron)
library(data.table)

shinyServer(function(input, output) {
 
#Word Cloud  
  ##tweetsUSps1.df <- parseTweets("tweetsUSps1.json", simplify = TRUE)
  tweetsps1.df <- parseTweets("tweetsUSps1.json", verbose=TRUE)
  tweetsps1.df$text <- sapply(tweetsps1.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  TweetCorpus <- paste(unlist(tweetsps1.df$text), collapse =" ")
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords("english"))
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, c("https", 
                                                    "https...",
                                                    "via",
                                                    "use",
                                                    "just",
                                                    "think",
                                                    "more",
                                                    "turn", 
                                                    "hothandsome",
                                                    "watch",
                                                    "get",
                                                    "bad",
                                                    "dude",
                                                    "exatriz",
                                                    "when",
                                                    "you",
                                                    "want",
                                                    "retweet",
                                                    "how",
                                                    "never",
                                                    "make",
                                                    "with",
                                                    "will",
                                                    "say",
                                                    "hes",
                                                    "new",
                                                    "nadelpari",
                                                    "like",
                                                    "need",
                                                    "dont",
                                                    "must",
                                                    "porno",
                                                    "lt3",
                                                    "can",
                                                    "call",
                                                    "kissing",
                                                    "retira",
                                                    "and",
                                                    "good",
                                                    "now",
                                                    "for",
                                                    "can",
                                                    "pjnet",
                                                    "career",
                                                    "macro",
                                                    "love",
                                                    "danscavino",
                                                    "polit",
                                                    "join",
                                                    "htt.",
                                                    "know",
                                                    "cant",
                                                    "aparecia",
                                                    "enter",
                                                    "realli",
                                                    "not",
                                                    "time",
                                                    "that",
                                                    "amp",
                                                    "the",
                                                    "thing",
                                                    "what",
                                                    "presid",
                                                    "beli",
                                                    "back",
                                                    "tcot",
                                                    "porn",     
                                                    "httpstcoyev2vfwxkp",
                                                    "one",
                                                    "yes",
                                                    "record",
                                                    "whi",
                                                    "tell",
                                                    "this",
                                                    "macro",
                                                    "htt...",
                                                    "http.",
                                                    "httpstcongvjylzmet",
                                                    "way",
                                                    "htt.",
                                                    "nadelparis",
                                                    "movie",
                                                    "belies",
                                                    "see",
                                                    "video",
                                                    "macro",
                                                    "retweeted",
                                                    "why",
                                                    "tampa",
                                                    "please",
                                                    "enters",
                                                    "look",
                                                    "best",
                                                    "fuck",
                                                    "tonight",
                                                    "recommend",
                                                    "someone",
                                                    "opening",
                                                    "thank",
                                                    "valentines",
                                                    "night",
                                                    "going",
                                                    "state",
                                                    "happy",
                                                    "stop",
                                                    "man",
                                                    "click",
                                                    "home",
                                                    "apply",
                                                    "doesnt",
                                                    "always",
                                                    "birthday",
                                                    "retail",
                                                    "thanks",
                                                    "show",
                                                    "said",
                                                    "old",
                                                    "still",
                                                    "shit",
                                                    "even",
                                                    "ever",
                                                    "read",
                                                    "better",
                                                    "well",
                                                    "take",
                                                    "latest",
                                                    "ive",
                                                    "fit",
                                                    "team",
                                                    "last",
                                                    "who",
                                                    "getting",
                                                    "didnt",
                                                    "come",
                                                    "hospitality",
                                                    "game",
                                                    "youre",
                                                    "really",
                                                    "real",
                                                    "made",
                                                    "whats",
                                                    "sales",
                                                    "gonna",
                                                    "girl",
                                                    "many",
                                                    "much",
                                                    "first",
                                                    "were",
                                                    "thats",
                                                    "ill",
                                                    "its",
                                                    "feel",
                                                    "got",
                                                    "big",
                                                    "lol",
                                                    "next",
                                                    "might",
                                                    "que",
                                                    "anyone",
                                                    "today",
                                                    "wanna",
                                                    "done",
                                                    "check",
                                                    "year",
                                                    "years",
                                                    "wait",
                                                    "ass",
                                                    "store",
                                                    "nursing",
                                                    "qual"))
  
  
  library(RColorBrewer)
  library(ggplot2)
  library(grid)
  library(maps)

  
  #Mapinf the tweets
  map.data <- map_data("state")
  points <- data.frame(x = as.numeric(tweetsps1.df$lon), y = as.numeric(tweetsps1.df$lat))
  points <- points[points$y > 25, ]
 
  
  output$map <- renderPlot(
  {
    ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "light yellow", 
                                color = "green", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
      theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), plot.background = element_blank(), 
            plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                     aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark green")
    
    
      
  }
    
  )
  
  output$plot <- renderPlot({
    wordcloud(TweetCorpus, min.freq = 370, max.words = 100, random.order = FALSE, colors=brewer.pal(3, "Dark2"))
    
  })

  
})



