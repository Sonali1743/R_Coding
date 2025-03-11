#Sentiment Analysis ICE
#Understanding Sentiment

library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(udpipe)
library(textdata)

merger = read.csv('Week11 (Text Mining)/text-mining-ice-Sonali1743/data/tmobile_sprint_merger.csv',header = TRUE)
merger_tweet = merger %>%
  select(tweet) %>%
  unnest_tokens(word,tweet)

#For this assignment you will use the NRC lexicon. Please perform the following tasks:

#Generate a new variable assessing the difference between joy and sadness. (1 pt.)

nrc_joysad = get_sentiments('nrc') %>%
  filter(sentiment == 'joy' | sentiment == 'sadness')

tweet_joysad = inner_join(merger_tweet,nrc_joysad)
count1 = count(tweet_joysad,word, sentiment)
spread1 = spread(count1,sentiment,n, fill = 0)
tweet_contentment = mutate(spread1, contentment = joy-sadness,
                           overall_joysad = case_when(contentment>0~'Joy',
                                                      contentment==0~'Neutral',
                                                      contentment<0~'Sadness'))
tweet_content = arrange(tweet_contentment, desc(contentment))

#Create a plot for the top 10 values for joy and top 10 for sadness; 
#copy to your Word file. (1 pt.)

tweet_content %>%
  slice(1:10, 205:214) %>%
  ggplot(aes(x=word, y=contentment,fill=overall_joysad)) +
  coord_flip() +
  geom_col() +
  labs(x='Word',y='Contentment') +
  theme_light(base_size = 12) +
  theme(axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid = element_blank())

#Generate a new variable assessing the difference between positive and negative. (1 pt.)

bing = get_sentiments('bing')

tweet_posneg = inner_join(merger_tweet,bing)
count2 = count(tweet_posneg,word, sentiment)
spread2 = spread(count2, sentiment, n, fill = 0)

tweet_satis = spread2 %>%
  mutate(satisfaction = positive-negative, overall_satis = case_when(satisfaction>0~'positive',
                                                                     satisfaction==0~'neutral',
                                                                     satisfaction<0~'negative'))
tweet_satis = arrange(tweet_satis, desc(satisfaction))

#Create a plot for the top 10 values for positive and top 10 for negative; 
#copy this to your Word document. (1 pt.)

tweet_satis %>%
  slice(1:10,592:601) %>%
  ggplot(aes(x=word, y=satisfaction,fill=overall_satis)) +
  coord_flip() +
  geom_col() +
  labs(x='Word', y='Satisfaction') +
  theme_light(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))
  
#Perform the same thing for one other sentiment pair in NRC of your choice:
#Select the pair of sentiment scores (1 pt.)

get_sentiments('nrc') %>%
  distinct(sentiment)

nrc_trustfear = get_sentiments('nrc') %>%
  filter(sentiment == 'trust' | sentiment == 'fear')

#Generate a new variable assessing the difference between them (1 pt.)

tweet_trustfear = inner_join(merger_tweet, nrc_trustfear)
count3 = count(tweet_trustfear, word,sentiment)
spread3 = spread(count3, sentiment, n, fill= 0)

tweet_emotion = spread3 %>%
  mutate(emotion = trust-fear, overall_emo = case_when(emotion>0~'trust',
                                                       emotion==0~'neutral',
                                                       emotion<0~'fear'))
tweet_emotion=arrange(tweet_emotion,desc(emotion))

#Create a plot for the top 10 values of each emotion, copy to Word (1 pt.)

tweet_emotion %>%
  slice(1:10, 342:351) %>%
  ggplot(aes(x=word, y=emotion,fill=overall_emo)) +
  coord_flip() +
  geom_col() +
  labs(x='Word', y='Emotion') +
  theme_light(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

#Using the library udpipe for POST, generate plots assessing frequencies for 
#nouns, adjectives, and verbs; copy these plots to Word. (3 pts.)

#Preparing the merger_tweet dataset
#Removing stop words
data("stop_words")
merger_tweet2 = anti_join(merger_tweet, stop_words)

#Remove numerical values
numpattern = '\\b[0-9]+\\b'
merger_tweet2$word = str_remove_all(merger_tweet2$word,numpattern)
merger_tweet3 = filter(merger_tweet2, !(word == ''))

#Filter out the names of T-Mobile and Sprint from the data
names = c('tmobile', 'sprint')
merger_tweet4 = filter(merger_tweet3, !(word %in% names))

#Removing new lines
newlines = '\\n'
merger_tweet4$word = str_remove_all(merger_tweet4$word,newlines)
merger_tweet4 = filter(merger_tweet4, !(word == ''))

#Remove tabs and spaces
tabsnspaces = '[:blank:]'
merger_tweet4$word = str_remove_all(merger_tweet4$word,tabsnspaces)
merger_tweet4 = filter(merger_tweet4, !(word == ''))

udpipe_mod = udpipe_download_model(language = 'english')
udpipe_mod = udpipe_load_model(udpipe_mod$file_model)
data = as.data.frame(udpipe_annotate(udpipe_mod, x = merger_tweet4$word))
post_stats = txt_freq(data$upos)
post_stats$key = factor(post_stats$key, levels = rev(post_stats$key))

noun = subset(data, upos %in% 'NOUN')
noun_data = txt_freq(noun$token)
noun_data$key = factor(noun_data$key, levels = rev(noun_data$key))
summary(noun_data)

adjective = subset(data, upos %in% 'ADJ')
adj_data = txt_freq(adjective$token)
adj_data$key = factor(adj_data$key, levels = rev(adj_data$key))
summary(adj_data)

verb = subset(data, upos %in% 'VERB')
verb_data = txt_freq(verb$token)
verb_data$key = factor(verb_data$key, levels = rev(verb_data$key))
summary(verb_data)

#Plot assessing frequency for nouns (top 15)
noun_data %>%
  slice(1:15) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill = freq)) +
  coord_flip() +
  geom_col() +
  theme_light(base_size = 12) +
  labs(x='Noun', y='Freqency') +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  scale_fill_gradient(low="orange", high="orange3")
  
#Plot assessing frequency for adjectives (top 15)
adj_data %>%
  slice(1:15) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill = freq)) +
  coord_flip() +
  geom_col() +
  theme_light(base_size = 12) +
  labs(x='Adjective', y='Freqency') +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  scale_fill_gradient(low="blue", high="blue3")

#Plot assessing frequency for verbs (top 15)
verb_data %>%
  slice(1:15) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill = freq)) +
  coord_flip() +
  geom_col() +
  theme_light(base_size = 12) +
  labs(x='Verb', y='Freqency') +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  scale_fill_gradient(low="green", high="green3")


