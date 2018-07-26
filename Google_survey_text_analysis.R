#A function for quickly visualizing google survey text data

#Sample data on EcoAp
setwd("z:/shardison/text_analyses/")

#libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(tidytext)

survey_summary <- function(filename, quest.vec, n.words){
  d <- read.csv(filename,na.strings=c("","NA"))
  
  d_l <- d_l <- gather(d, question, response, names(d[quest.vec[1]]):names(d[quest.vec[length(quest.vec)]]),
                       factor_key = T)
  d_l$response <- as.character(d_l$response)
  
  #break into words and remove stop words
  dl_t <- d_l %>% unnest_tokens(word, response) %>%
    anti_join(get_stopwords()) %>%
    filter(!is.na(word)) %>% filter(word != "e.g.") %>%
    filter(word != "e.g") %>% filter(word != "etc")
  
  #create gplot objects from unique questions
  plt_list <- list()
  for (i in 1:length(unique(dl_t$question))){
    plt <- dl_t[dl_t$question == unique(dl_t$question)[i],] %>%
      count(word, sort = T) %>%
      top_n(n.words) %>%
      head(n.words)
    
    plt$word <- factor(plt$word, levels = rev(plt$word))
    names(plt)[2] <- "Word Count"
    
    plt_list[[i]] <- plt  %>%
      ggplot(aes(word, `Word Count`)) +
      geom_col(color = "blue", fill = "lightgrey", size = 1) +
      ggtitle(unique(dl_t$question)[i]) +
      ylab("Word Count") +
      xlab(NULL) +
      coord_flip() +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            title = element_text(size = 12))
    
  }
  #get sentiment data frame
  bing <- get_sentiments("bing")
  
  #get overall sentiment
  sent_count <- dl_t %>% inner_join(bing) %>% count(word, sentiment, sort = T)
  
  plt_list[[length(unique(dl_t$question)) + 1]] <- sent_count %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(y = "Contribution to sentiment",  x = NULL) +
    ggtitle("Overall sentiment") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          title = element_text(size = 13))
  
  return(plt_list)
}


plt_list <- survey_summary("EDAB Questionnaire .csv", quest.vec = c(3:5), n.words = 15)

#Change titles of plots
plt_list[[1]]$labels$title <- "Your day-to-day Work?"
plt_list[[2]]$labels$title <- "Improving within center communications?"
plt_list[[3]]$labels$title <- "Improving comms. to outside audiences?"

#Arrange figures
grid.arrange(plt_list[[1]], plt_list[[2]], plt_list[[3]], plt_list[[4]], nrow = 1)
