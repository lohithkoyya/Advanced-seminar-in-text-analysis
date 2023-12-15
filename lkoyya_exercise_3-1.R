###########################
##lkoyya_exercise3.R
##Note : Code for exercise #3
## Author : Lohithkoyya
##############################

####BEGIN Section 1###

#Necessary packages
install.packages(c("quanteda", "devtools", "stringr", "NLP", "tm", 
                   "textstem", "textreg", "ggplot2", "tidyverse", "tidytext"))
devtools::install_github("quanteda/quanteda.corpora")
library(quanteda)
library(quanteda.corpora)
library(stringr)
library(NLP)
library(tm)
library(textstem)
library(textreg)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(sentimentr)
library(reshape2) 

#Load the pre-built UKPA corpus
ukpa.corpus <-  tidytext::tidy(quanteda.corpora::data_corpus_immigrationnews)

#Inspect the corpus
ukpa.corpus$text[1] #First entry

#Turn it into a tm1 corpus
ukpa.corpus.tm1 <- VCorpus(VectorSource(ukpa.corpus$text))

#Turn it into a tm2 corpus
ukpa.corpus.tm2 <- VCorpus(VectorSource(ukpa.corpus$text))

#Section 1 & 2

#Clean it up
ukpa.corpus.tm1 <- tm_map(ukpa.corpus.tm1, removePunctuation, preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
#removeSpecialChars function from here: https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
ukpa.corpus.tm1 <- tm_map(ukpa.corpus.tm1, content_transformer(removeSpecialChars))
ukpa.corpus.tm1<- tm_map(ukpa.corpus.tm1, content_transformer(tolower))
ukpa.corpus.tm1 <- tm_map(ukpa.corpus.tm1, removeNumbers)
ukpa.corpus.tm1 <- tm_map(ukpa.corpus.tm1, removeWords, stopwords("english"))
ukpa.corpus.tm1 <- tm_map(ukpa.corpus.tm1, stripWhitespace)

#Let's not stem, but instead lemmatize (i.e, convert every word to its base
#dictionary word
ukpa.corpus.tm3 <- convert.tm.to.character(ukpa.corpus.tm1)
ukpa.corpus.tm3 <- lemmatize_strings(ukpa.corpus.tm3, 
                                     dictionary = lexicon::hash_lemmas)

#Take a look at the cleaned, lemmatized texts (first speech)
ukpa.corpus.tm3[1]

#Attach the cleaned docs back to our dataset, but separate from the "text"
#object, which contrains the uncleaned texts.
ukpa.corpus$text_clean <- ukpa.corpus.tm3

#Make the dictionary
immigr_dict <- c("immigration", "immigrant", "immigrate")

#Extract the relevant SOTUs
ukpa_immigr <- ukpa.corpus[str_detect(ukpa.corpus$text_clean, 
                                      "immigration|immigrant|immigrate"),]
ukpa_immigr$text_clean[1:2]

#First, convert to a DTM so we can count the number of times the dictionary words
#are used
ukpa.dtm <- DocumentTermMatrix(ukpa.corpus.tm1)

#Count the number of times each dictionary item shows up
immigr.count <- as.matrix(DocumentTermMatrix(ukpa.corpus.tm1, 
                                             list(dictionary = immigr_dict)))
rownames(immigr.count) <- rownames(ukpa.corpus)

#Get the row sums so you get the total number of times the dictionary words 
#are used per UKPA
immigr.sum <- rowSums(immigr.count)

#"Normalize" by total SOTU word count so you can control for differences in
#speech length, then attach it back to our dataset
total.words <- rowSums(as.matrix(ukpa.dtm))
immigr.normal <- immigr.sum/total.words
ukpa.corpus <- cbind(ukpa.corpus, immigr.normal)

# section 3
#A little cleaning
#Turn into a tm corpus
ukpa.corpus.tm2 <- VCorpus(VectorSource(ukpa.corpus$text))

#Some preprocessors (we need punctuation, so don't remove it)
ukpa.corpus.tm2 <- tm_map(ukpa.corpus.tm2, content_transformer(tolower))
ukpa.corpus.tm2 <- tm_map(ukpa.corpus.tm2, removeNumbers)
ukpa.corpus.tm2 <- tm_map(ukpa.corpus.tm2, stripWhitespace)

inspect(ukpa.corpus.tm2[[179]])

#Convert the corpus to a character vector to use for sentiment analysis
ukpa.corpus.tm2 <- convert.tm.to.character(ukpa.corpus.tm2)


#Sentiment analysis using sentimentr
#Take a look at some the available sentiment dictionaries
dplyr::sample_n(lexicon::hash_sentiment_jockers_rinker, 20)
dplyr::sample_n(lexicon::hash_sentiment_nrc, 20)

#Some valence shifters:
#1 = Negator
#2 = Amplifier
#3 = De-Amplifier
#4 = Adversative Conjunction
lexicon::hash_valence_shifters[which(lexicon::hash_valence_shifters$y==4)]

#Sentence-level sentiment
sentences <- get_sentences(ukpa.corpus.tm2)
sent.sentiment <- sentiment(sentences)

#Visualize the sentence-level sentiment
ggplot(sent.sentiment[which(sent.sentiment$element_id==179),], 
       aes(x = sentence_id, y = sentiment,
           fill = ifelse(sent.sentiment$sentiment[which(
             sent.sentiment$element_id==179)] < 0, "0", "1"))) +
  geom_histogram(stat = "identity") +
  xlab("Sequence (in Sentences)") + ylab("Sentiment Polarity") +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none")

#What are the positive and negative words in this UKPA?
extract_sentiment_terms(ukpa.corpus$text[179])

#Take a look at the sentiment-coded ukpa transcript
sentiment.byUKPA <- sentiment_by(sentences, by = NULL) 
highlight(sentiment.byUKPA)

#Classify emotions
library(syuzhet)
emotions <- get_nrc_sentiment(ukpa.corpus.tm2)

#Normalize emotion count by total word count
emotions <- emotions/sentiment.byUKPA$word_count

#Visualize emotion trends
emotions <- melt(emotions, measure.vars = colnames(emotions))

ggplot(emotions[which(emotions$variable!="positive" & emotions$variable!="negative"),], 
       aes(x = as.factor(variable), y = value*100,
           fill = as.factor(variable))) +
  geom_boxplot(aes(fill = as.factor(variable)),
               outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  labs(y = "% of UKPA Words that are Emotion Words", x = "") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = "right") +
  guides(fill = guide_legend(title = "Emotion"))

save.image("data/exercise3.RData")

### END ###
