##############################
## lkoyya_exercise2.R
## Note: Code for exercise #2 
## Author: Lohith koyya
##############################

### BEGIN ###

# Necessary packages
install.packages(c("NLP", "tm"))
library(NLP)
library(tm)
library(SnowballC)
### BEGIN ###

#Install and load packages
install.packages("devtools")
install.packages("rtweet")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("reshape2")
install.packages("rvest")
install.packages("purrr")
install.packages("stringr")
install.packages("lubridate")
library(rtweet)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)

devtools::install_github("rOpenGov/rtimes")
library(rtimes)

#Personalized scraper with rvest (influenced by this page: 
#https://stackoverflow.com/questions/36683510/r-web-scraping-across-multiple-pages)
product_id <- "B083C58VDP"
url <- paste0("https://www.amazon.com/product-reviews/", 
              product_id, 
              "/ref=acr_dpproductdetail_text?ie=UTF8&reviewerType=all_reviews&pageNumber=")

n_pages <- round(500/10, digits = 0)


uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
session <- html_session(url, httr::user_agent(uastring) )

# Create a list of all the URLs
url_list <- paste0(url, 1:n_pages)
# Did it work? Check that it is 483 long:
length(url_list)

review.text <- lapply( url_list,
                       
                       function(url){
                         
                         url %>% jump_to(x = session) %>% 
                           html_nodes(".review-text-content") %>% 
                           html_text()
                       }
)


length(review.text)

cat( trimws(review.text[[1]][1]) )

review.title <- lapply( url_list,
                        
                        function(url){
                          
                          url %>% jump_to(x = session) %>% 
                            html_nodes("#cm_cr-review_list .a-color-base") %>% 
                            html_text()
                        }
)

review.rating <- lapply( url_list,
                         
                         function(url){
                           
                           url %>% jump_to(x = session) %>% 
                             html_nodes("#cm_cr-review_list .review-rating") %>% 
                             html_text()
                         }
)

review.name <- lapply( url_list,
                       
                       function(url){
                         
                         url %>% jump_to(x = session) %>% 
                           html_nodes("#cm_cr-review_list .a-profile-name") %>% 
                           html_text()
                       }
)

review.date <- lapply( url_list,
                       
                       function(url){
                         
                         url %>% jump_to(x = session) %>% 
                           html_nodes("#cm_cr-review_list .review-date") %>% 
                           html_text()
                       }
)

df.swnh.reviews <- data.frame(unlist(review.name), 
                              unlist(review.rating),
                              unlist(review.title),
                              unlist(review.text), 
                              unlist(review.date) )

# add some column names
colnames(df.swnh.reviews) <- c("name", "rating", "title", "text", "date")

# have a quick check at our work
head(df.swnh.reviews$text, 5)
head(df.swnh.reviews$rating, 5)
head(df.swnh.reviews$name, 5)
head(df.swnh.reviews$title, 5)


ny.corpus <- Corpus(VectorSource(df.swnh.reviews$text))
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, removePunctuation)
inspect(ny.corpus[[1]])
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
#removeSpecialChars function from here: https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus

ny.corpus <- tm_map(ny.corpus, removeSpecialChars)
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, PlainTextDocument)
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, content_transformer(tolower))
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, removeNumbers)
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, removeWords, stopwords("english"))
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, stemDocument)
inspect(ny.corpus[[1]])

ny.corpus <- tm_map(ny.corpus, stripWhitespace)
inspect(ny.corpus[[1]])

Amazon.dtm <- DocumentTermMatrix(ny.corpus)


#Find the most frequent terms

freq.terms <- colSums(as.matrix(Amazon.dtm))

freq.terms <- sort(freq.terms, decreasing = T)



#Top 10 terms

head(freq.terms, 10)



#Bottom 10 terms

tail(freq.terms, 10)



#Find terms that occur at least 5 times

findFreqTerms(Amazon.dtm, lowfreq = 5)





#Visualize top 10 terms

top.terms <- as.data.frame(freq.terms[1:10])

colnames(top.terms) <- "counts"

top.terms$terms <- factor(rownames(top.terms), levels = rownames(top.terms))



ggplot(top.terms, aes(x = terms, y = counts)) +
  
  geom_bar(stat = "identity", color = "black", fill = "#a96a96") +
  
  ylab("Frequency") + xlab("") +
  
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   
                                   face = "bold"))



#Word correlations



#Find words that tend to co-occur with "consol"

findAssocs(Amazon.dtm, "consol", .5)



#Find words that tend to co-occur with "amazon"

findAssocs(Amazon.dtm, "amazon", .5)



#Find words that tend to co-occur with "game"

findAssocs(Amazon.dtm, "game", .5)





#tf-idf scores

Amazon.dtm.tfidf <- weightTfIdf(Amazon.dtm)



#Visualize some tf-idf scores

rownames(Amazon.dtm.tfidf) <- 1:nrow(Amazon.dtm.tfidf)

Amazon.dtm.tfidf2 <- melt(as.matrix(Amazon.dtm.tfidf[c("2", "4", "6", "8"),]),
                          
                          id = rownames(Amazon.dtm.tfidf))

Amazon.dtm.tfidf2 <- as.data.frame(Amazon.dtm.tfidf2)

Amazon.dtm.tfidf2 <- Amazon.dtm.tfidf2[order(Amazon.dtm.tfidf2$Docs, -Amazon.dtm.tfidf2$value),]

tfidf.doc4 <- Amazon.dtm.tfidf2[which(Amazon.dtm.tfidf2$Docs=="2"),]

tfidf.doc20 <- Amazon.dtm.tfidf2[which(Amazon.dtm.tfidf2$Docs=="4"),]

tfidf.doc24 <- Amazon.dtm.tfidf2[which(Amazon.dtm.tfidf2$Docs=="6"),]

tfidf.doc31 <- Amazon.dtm.tfidf2[which(Amazon.dtm.tfidf2$Docs=="8"),]



tfidf.doc4$Terms <- factor(tfidf.doc4$Terms, levels = tfidf.doc4$Terms)

tfidf.doc20$Terms <- factor(tfidf.doc20$Terms, levels = tfidf.doc20$Terms)

tfidf.doc24$Terms <- factor(tfidf.doc24$Terms, levels = tfidf.doc24$Terms)

tfidf.doc31$Terms <- factor(tfidf.doc31$Terms, levels = tfidf.doc31$Terms)



doc4.plot <- ggplot(tfidf.doc4[1:10,], aes(x = Terms, y = value)) +
  
  geom_bar(stat = "identity", color = "black", fill = "#d11141") +
  
  ylab("tf-idf Score") + xlab("") +
  
  ylim(0, 0.8) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   
                                   face = "bold"),
        
        plot.title = element_text(face = "bold")) +
  
  ggtitle("Article #1")



doc20.plot <- ggplot(tfidf.doc20[1:10,], aes(x = Terms, y = value)) +
  
  geom_bar(stat = "identity", color = "black", fill = "#00aedb") +
  
  ylab("tf-idf Score") + xlab("") +
  
  ylim(0, 0.8) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   
                                   face = "bold"),
        
        plot.title = element_text(face = "bold")) +
  
  ggtitle("Article #2")



doc24.plot <- ggplot(tfidf.doc24[1:10,], aes(x = Terms, y = value)) +
  
  geom_bar(stat = "identity", color = "black", fill = "#00b159") +
  
  ylab("tf-idf Score") + xlab("") +
  
  ylim(0, 0.8) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   
                                   face = "bold"),
        
        plot.title = element_text(face = "bold")) +
  
  ggtitle("Article #3")



doc31.plot <- ggplot(tfidf.doc31[1:10,], aes(x = Terms, y = value)) +
  
  geom_bar(stat = "identity", color = "black", fill = "#f37735") +
  
  ylab("tf-idf Score") + xlab("") +
  
  ylim(0, 0.8) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   
                                   face = "bold"),
        
        plot.title = element_text(face = "bold")) +
  
  ggtitle("Article #4")



ggarrange(doc4.plot, doc20.plot, doc24.plot, doc31.plot,
          
          align = "h")



save.image("data/exercise2.RData")
