###BEGIN###



#Necessary packages

install.packages(c("devtools", "ggplot2", "ggpubr"))

library(devtools)

devtools::install_github("trinker/entity")

library(entity)

library(ggplot2)

library(ggpubr)



#Load the data

sotu_corpus <- readRDS(file = "data/sotu_corpus.rds")



sotu_corpus$texts[[1]]



#section 1



#Apply the "Person" tagger

people.d <- person_entity(as.character(sotu_corpus[which(sotu_corpus$party=="Democratic"),]
                                       
                                       $texts))

people.r <- person_entity(as.character(sotu_corpus[which(sotu_corpus$party=="Republican"),]
                                       
                                       $texts))

democratic <- plot(people.d, min = 2)

republican <- plot(people.r, min = 2)



democratic.plot <- ggplot(democratic$data, aes(x = entity, y = frequency)) +
  
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           
           fill = "#2332A9") +
  
  xlab("President Mentioned by Democrats") + ylab("Count") +
  
  ylim(0, 60) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



republican.plot <- ggplot(republican$data, aes(x = entity, y = frequency)) +
  
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           
           fill = "#990000") +
  
  xlab("President Mentioned by republicans") + ylab("Count") +
  
  ylim(0, 60) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



ggarrange(democratic.plot, republican.plot, align = "v")





#Apply the "Organization" tagger

org.d <- organization_entity(as.character(sotu_corpus[which(sotu_corpus$party=="Democratic"),]
                                          
                                          $texts))

org.r <- organization_entity(as.character(sotu_corpus[which(sotu_corpus$party=="Republican"),]
                                          
                                          $texts))

democratic <- plot(org.d, min = 2)

republican <- plot(org.r, min = 2)



demovratic.plot <- ggplot(democratic$data, aes(x = entity, y = frequency)) +
  
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           
           fill = "#2332A9") +
  
  xlab("Organizations Mentioned by Democrats") + ylab("Count") +
  
  ylim(0, 20) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



republican.plot <- ggplot(republican$data, aes(x = entity, y = frequency)) +
  
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           
           fill = "#990000") +
  
  xlab("Organizations Mentioned by republicans") + ylab("Count") +
  
  ylim(0, 20) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



ggarrange(democratic.plot, republican.plot, align = "v")



#section 2



#installing necessary packages



devtools::install_github(c(
  
  "trinker/termco",
  
  "trinker/coreNLPsetup",       
  
  "trinker/tagger"
  
))



library(tagger)

library(ggplot2)

library(ggpubr)

library(tidyverse)

library(tidytext)

library(RCurl)

library(gmodels)

library(socviz)

library(textclean)



#Apply the POS tagger to the first blog post

tag_pos(as.character(sotu_corpus$texts))[1]



##tag_pos(as.character(sotu_corpus$texts[1])) %>% c()





#What do these tags mean?

penn_tags()



#Apply the tagger across all the blog posts

pos.d <- tag_pos(as.character(sotu_corpus[which(sotu_corpus$party=="Democratic"),]
                              
                              $texts))

pos.r <- tag_pos(as.character(sotu_corpus[which(sotu_corpus$party=="Republican"),]
                              
                              $texts))

Democratic <- plot(pos.d)

Republican <- plot(pos.r)



Democratic.plot <- ggplot(Democratic$data, aes(x = Terms, y = Prop)) +
  
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           
           fill = "#2332A9") +
  
  xlab("Democratic's Parts of Speech") + ylab("Proportion of Total SOTU Words") +
  
  ylim(0, .2) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



Republican.plot <- ggplot(Republican$data, aes(x = Terms, y = Prop)) +
  
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           
           fill = "#990000") +
  
  xlab("Republican's Parts of Speech") + ylab("Proportion of Total SOTU Words") +
  
  ylim(0, .2) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



ggarrange(Democratic.plot, Republican.plot, align = "v")



#Let's simplify the categories (code adapted from here: https://github.com/trinker/tagger)

pos.d <- tag_pos(as.character(sotu_corpus[which(sotu_corpus$party=="Democratic"),]
                              
                              $texts))%>% as_basic()

pos.r <- tag_pos(as.character(sotu_corpus[which(sotu_corpus$party=="Republican"),]
                              
                              $texts))%>% as_basic()

Democratic <- plot(pos.d)

Republican <- plot(pos.r)



Democratic.plot <- ggplot(Democratic$data, aes(x = Terms, y = Prop)) +
  
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           
           fill = "#2332A9") +
  
  xlab("Democratic's Parts of Speech") + ylab("Proportion of Total SOTU Words") +
  
  ylim(0, .3) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



Republican.plot <- ggplot(Republican$data, aes(x = Terms, y = Prop)) +
  
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           
           fill = "#990000") +
  
  xlab("Republican's Parts of Speech") + ylab("Proportion of Total SOTU Words") +
  
  ylim(0, .3) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



ggarrange(Democratic.plot, Republican.plot, align = "v")





#Count the tags

counts <- tag_pos(as.character(sotu_corpus$texts))

counts <- count_tags(counts, sotu_corpus$ranks)



counts <- tag_pos(as.character(sotu_corpus$texts)) %>% as_basic()

counts <- count_tags(counts, sotu_corpus$ranks)



# Get the frequency distribution of tags and turn it into a data frame

tag.count <- count_tags(pos.d, sotu_corpus[sotu_corpus$party=="Demoratic",]$sotu_corpus) %>% as.data.frame()



# Subset to only include some of the tags, convert the blog names into rownames,

# convert the dataframe into a matrix so that it can be used in the CrossTable()

# function, then use that function to get the crosstab

tag.count[, c("sotu_corpus","adjective", "adverb", "noun", "verb")] %>%
  
  column_to_rownames(var = "sotu_corpus") %>% as.matrix() %>%
  
  CrossTable(prop.c = F, prop.chisq = F, prop.t = F)



#Most common nouns/verbs



# Take the posts with the basic tagset, turn it into a series of character vectors

# per blog post, unlist it into a single character vector, then get rid of some

# curly quotes that, upon inspection, made their way into the tagged nouns

lib.nouns <- pos.d %>% select_tags("noun") %>%
  
  c() %>% unlist() %>%
  
  replace_curly_quote(replacement = "")



# Turn that character vector into a data frame, where we have the frequency of

# each tag. Also remove some characters that were erroneously tagged as nouns

lib.nouns <- lib.nouns %>%
  
  table() %>%
  
  as.data.frame() %>%
  
  subset(., . %nin% c("%", "\"", " "))



# Arrange the nouns by frequency in descending order, and retain the top 10

lib.nouns <- arrange(lib.nouns, -Freq) %>% head(10)



# Repeat for the conservative blogs

con.nouns <- pos.r %>% select_tags("noun") %>%
  
  c() %>% unlist() %>%
  
  replace_curly_quote(replacement = "")



con.nouns <- con.nouns %>%
  
  table() %>%
  
  as.data.frame() %>%
  
  subset(., . %nin% c("%", "\"", " "))



con.nouns <- arrange(con.nouns, -Freq) %>% head(10)



#take a look

lib.nouns

con.nouns



#Now put these in another bar plot

Democratic.plot <- ggplot(lib.nouns, aes(x = reorder(., Freq), y = Freq)) +
  
  geom_bar(aes(x = reorder(., Freq), y = Freq),
           
           stat = "identity", color = "black",
           
           fill = "#2332A9") +
  
  xlab("Top Nouns in Democratic SOTU Speeches") + ylab("Frequency") +
  
  ylim(0, 200) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



Republican.plot <- ggplot(con.nouns, aes(x = reorder(., Freq), y = Freq)) +
  
  geom_bar(aes(x = reorder(., Freq), y = Freq),
           
           stat = "identity", color = "black",
           
           fill = "#990000") +
  
  xlab("Top Nouns in Republican SOTU Speeches") + ylab("Frequency") +
  
  ylim(0, 200) +
  
  theme_bw() +
  
  theme(axis.title = element_text(face = "bold")) +
  
  coord_flip()



ggarrange(Democratic.plot, Republican.plot, align = "v")

save.image("data/exercise4.rData")

### END ###