
#loading libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(twitteR)
library(tm)

#To get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key<-'2nSlqvVYhQGUxUMY8ATwOSE6v'
consumer_secret<-'RM9xtXqMZXrNyBjMDx1X1ScUvx1AEhmIO0m9FXAlKAr89IszCK'
access_token<- '2758475550-AtHCc2rolkr2ld78aEzoav2BL8efGFbJ6dfqDci'
access_secret<-'GJFJep5OnqqVpO6emhXWVIHfP6R37rupK1SmOGnfSjO9z'

setup_twitter_oauth(consumer_key, consumer_secret,
      access_token=access_token, access_secret=access_secret)

McDonald<- twitteR::searchTwitter('#McDonald', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(McDonald)

Starbucks<- twitteR::searchTwitter('#Starbucks', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(Starbucks)

SUBWAY<-twitteR::searchTwitter('#SUBWAY', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(SUBWAY)



# Cleaning the datasets
# Remove http and https elements manually
d$text <- gsub("http[^[:space:]]*","",  d$text) # For http
d$text <- gsub("http[^[:space:]]*","", d$text) # For https

e$text <- gsub("http[^[:space:]]*","",  e$text) # For http
e$text <- gsub("http[^[:space:]]*","", e$text) # For https

a$text <- gsub("http[^[:space:]]*","",  a$text) # For http
a$text <- gsub("http[^[:space:]]*","", a$text) # For https


#Tokenizing all 3 datasets from twitter
tidy_McDonald<- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_Starbucks<- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_SUBWAY<- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#################################################
### Association analysis: Combining all 3 tidy data frames and creating correlograms
#################################################

library(tidyr)
frequency<-bind_rows(mutate(tidy_McDonald, author="McDonald"),
          mutate(tidy_Starbucks, author= "Starbucks"),
          mutate(tidy_SUBWAY, author="SUBWAY")) %>% #closing bind_rows
          mutate(word=str_extract(word, "[a-z']+")) %>%
          count(author, word) %>%
          group_by(author) %>%
          mutate(proportion = n/sum(n))%>%
          select(-n) %>%
          spread(author, proportion) %>%
          gather(author, proportion, 'Starbucks', 'SUBWAY')


#let's plot the correlograms:
library(scales)

ggplot(frequency, aes(x=proportion, y=McDonald, 
  color = abs(McDonald- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= 'McDonald', x=NULL)

#Taking a look at correlation coefficients
cor.test(data=frequency[frequency$author == "Starbucks",],
         ~proportion + McDonald)

cor.test(data=frequency[frequency$author == "SUBWAY",],
         ~proportion + McDonald)


############################################
## Sentiment analysis 
#############################################

# COMBINE ALL THE DATASET AND ANALYZE 


library(textdata)
library(tidytext)
get_sentiments('afinn') # Show example of the table


ALL_DATA=rbind(tidy_McDonald,tidy_Starbucks,tidy_SUBWAY)
get_sentiments('afinn') # Show example of the table

# pulling in sentiment for these 3 tokenized datasets
ALL_DATA %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


#let's take a look at the most positive and most negative tokens
# in the McDonald dataset
ALL_DATA_sentiment <- ALL_DATA %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)
print(ALL_DATA_sentiment)

#Wordcloud
ALL_DATA_sentiment%>% with(wordcloud(word, n, max.words = 100))


#Contribution to sentiment: McDonalds
ALL_DATA_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment: ALL_DATA", x=NULL)+
  coord_flip()




# pulling in sentiment for these 3 tokenized datasets
tidy_McDonald %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


tidy_Starbucks%>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


tidy_SUBWAY  %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")



#let's take a look at the most positive and most negative tokens
# in the McDonald dataset
tidy_McDonald_sentiment <- tidy_McDonald %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)
print(tidy_McDonald_sentiment)

#Wordcloud
tidy_McDonald_sentiment%>% with(wordcloud(word, n, max.words = 100))


#Contribution to sentiment: McDonalds
tidy_McDonald_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment: McDonalds", x=NULL)+
  coord_flip()



#let's take a look at the most positive and most negative tokens
# in the Starbucks dataset

tidy_Starbucks_sentiment <- tidy_Starbucks %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)
print(tidy_Starbucks_sentiment)

#wordcloud
tidy_Starbucks_sentiment%>% with(wordcloud(word, n,
                                max.words = 100))

#Contribution to sentiment: Starbucks
tidy_Starbucks_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment: Starbucks", x=NULL)+
  coord_flip()


#let's take a look at the most positive and most negative tokens
# in the SUBWAY dataset

tidy_SUBWAY_sentiment <- tidy_SUBWAY %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

print(tidy_SUBWAY_sentiment)

#wordcloud
tidy_SUBWAY_sentiment%>% with(wordcloud(word, n,
                                           max.words = 100))

#Contribution to sentiment: SUBWAY
tidy_SUBWAY_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment: SUBWAY", x=NULL)+
  coord_flip()





############################################
## TF-IDF analysis
#############################################
combined_food <- bind_rows(mutate(d, make="McDonald"),
                           mutate(e, make= "Starbucks"),
                           mutate(a, make="SUBWAY")
)

McDonald_modif <- combined_food %>%
  unnest_tokens(word, text) %>%
  count(make, word, sort=TRUE) %>%
  ungroup()

McDonald_modif2 <- McDonald_modif %>%
  group_by(make) %>%
  summarize(total=sum(n))

McDonald_leftjoined <- left_join(McDonald_modif, McDonald_modif2)

tidy_McDonald_tfidf <-McDonald_leftjoined %>%
  bind_tf_idf(word, make, n)

tidy_McDonald_tfidf # we get all the zeors because we are looking at stop words ... too common

tidy_McDonald_tfidf %>%
  arrange(desc(tf_idf))

#what can we say about these words?

#############
# Looking at the graphical apprach:
tidy_McDonald_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(make) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=make))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~make, ncol=2, scales="free")+
  coord_flip()



























































