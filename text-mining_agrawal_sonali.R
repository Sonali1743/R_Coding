#Text Mining ICE

#Prepare the Data (6 pts.)

#Using the text mining process to clean data, perform the following steps to 
#prepare your data using tidytext:

library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)

merger = read.csv('Week9 (Text Mining)/text-mining-ice-Sonali1743/data/tmobile_sprint_merger.csv')
tweettext = merger['tweet']

#Remove stop words (1 pt.)

tokens = unnest_tokens(tweettext,word,tweet)

data("stop_words")
tokens2 = anti_join(tokens, stop_words)

#Remove numerical values (1 pt.)

numpattern = '\\b[0-9]+\\b'
tokens2$word = str_remove_all(tokens2$word,numpattern)
tokens3 = filter(tokens2, !(word == ''))

#Filter out the names of T-Mobile and Sprint from the data (1 pt.)

names = c('tmobile', 'sprint')
tokens4 = filter(tokens3, !(word %in% names))

#Remove new lines (1 pt.)

newlines = '\\n'
tokens4$word = str_remove_all(tokens4$word,newlines)
tokens4 = filter(tokens4, !(word == ''))

#Remove tabs and spaces (1 pt.)

tabsnspaces = '[:blank:]'
tokens4$word = str_remove_all(tokens4$word,tabsnspaces)
tokens4 = filter(tokens4, !(word == ''))

#Stem the words (1 pt.)

tokens4$word = wordStem(tokens4$word,'en')

#Follow-Up Analysis (4 pts.)

#Please answer the following questions.

#Output the top 10 words (in terms of volume) to the console. What do you notice 
#about the top 10 words (in terms of volume) in your dataset? If you would like, 
#you can use the data visualization shown in the tutorial to answer this question (1 pt.)

tokens4 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:10)

frequency = tokens4 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  mutate(proportion = (n/sum(n))*100) %>%
  slice(1:10)

ggplot(frequency, aes(proportion,word)) +
  geom_abline(color = 'gray40', lty = 2) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.2, height = 0.2) +
  labs(x = 'Proportion', y = 'Word') +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)

#Top words include merger, tmobilesprint, mobil, https and price; ~5% tweets have merger
#in them which makes them since they are discussing about tmobile and sprint merger

#Take any 4 terms/words and explain why they are justified as being in the top 10. (2 pts.)

#1. 'price' - People are discussing about the impact on the prices (before and after merger)
#2. 'competit' - People might be talking about the available telecom operators 
#and overall competition in the market
#3. '5g' - How the merger will impact the 5g services
#4. 'fcc' - Approval of the merger by FCC (Federal Communications Commission) 

#Overall, what do the terms suggest concerning opinion of the merger? (1 pt.)

#To understand overall opinion, we are looking at top 80 tokens (or words)
tokens4 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:80)

#People are discussing about the overall impact of merger on the consumers and
#industry; they are discussing its impact on prices of the services and competition; 
#they believe that the merger would lead to increased prices as the number of 
#players in the market would decrease; they are also talking about the role of
#industry regulators such as FCC

