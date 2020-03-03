install.packages("twitteR")

#loading libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(twitteR)
library(tm)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key<-'2nSlqvVYhQGUxUMY8ATwOSE6v'
consumer_secret<-'RM9xtXqMZXrNyBjMDx1X1ScUvx1AEhmIO0m9FXAlKAr89IszCK'
access_token<- '2758475550-AtHCc2rolkr2ld78aEzoav2BL8efGFbJ6dfqDci'
access_secret<-'GJFJep5OnqqVpO6emhXWVIHfP6R37rupK1SmOGnfSjO9z'


# ?searchTwitteR # STOP AND EXPLAIN TO STUDENTS!

setup_twitter_oauth(consumer_key, consumer_secret,
      access_token=access_token, access_secret=access_secret)

tesla <- twitteR::searchTwitter('#Tesla', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tesla)
head(d)
ford <- twitteR::searchTwitter('#Ford', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(ford)
head(e)
mercedes <- twitteR::searchTwitter('#Mercedes', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(mercedes)
head(a)


# Cleaning the datasets
# Remove http and https elements manually
d$text <- gsub("http[^[:space:]]*","",  d$text) # For http
d$text <- gsub("http[^[:space:]]*","", d$text) # For https

e$text <- gsub("http[^[:space:]]*","",  e$text) # For http
e$text <- gsub("http[^[:space:]]*","", e$text) # For https

a$text <- gsub("http[^[:space:]]*","",  a$text) # For http
a$text <- gsub("http[^[:space:]]*","", a$text) # For https


#tokenizing all 3 datasets from twitter
tidy_tesla <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
head(tidy_tesla)

tidy_ford <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_mercedes <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#################################################
### Combining all 3 tidy data frames and creating correlograms
#################################################

library(tidyr)
frequency <- bind_rows(mutate(tidy_tesla, author="Tesla"),
                       mutate(tidy_ford, author= "Ford"),
                       mutate(tidy_mercedes, author="Mercedes")) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Ford`, `Mercedes`)

head(frequency)
#let's plot the correlograms:
library(scales)

ggplot(frequency, aes(x=proportion, y=`Tesla`, 
                      color = abs(`Tesla`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Tesla", x=NULL)

#Taking a look at correlation coefficients
cor.test(data=frequency[frequency$author == "Ford",],
         ~proportion + `Tesla`)

cor.test(data=frequency[frequency$author == "Mercedes",],
         ~proportion + `Tesla`)

############################################
## Sentiment analysis 
#############################################
install.packages("textdata")
library(textdata)
library(tidytext)
get_sentiments('afinn') # Show example of the table

# pulling in sentiment for these 3 tokenized datasets
tidy_tesla %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_ford %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_mercedes %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#let's take a look at the most positive and most negative tokens in the tesla dataset

tidy_tesla_sentiment <- tidy_tesla %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

print(tidy_tesla_sentiment)

tidy_tesla_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

############################################
## TF-IDF analysis
#############################################
combined_cars <- bind_rows(mutate(d, make="Tesla"),
                           mutate(e, make= "Ford"),
                           mutate(a, make="Mercedes")
)

tesla_modif <- combined_cars %>%
  unnest_tokens(word, text) %>%
  count(make, word, sort=TRUE) %>%
  ungroup()

tesla_modif2 <- tesla_modif %>%
  group_by(make) %>%
  summarize(total=sum(n))

tesla_leftjoined <- left_join(tesla_modif, tesla_modif2)

tidy_tesla_tfidf <- tesla_leftjoined %>%
  bind_tf_idf(word, make, n)

tidy_tesla_tfidf # we get all the zeors because we are looking at stop words ... too common

tidy_tesla_tfidf %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
tidy_tesla_tfidf %>%
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




