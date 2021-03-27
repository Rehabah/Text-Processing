
#Text 
Sys.setlocale("LC_CTYPE", "arabic" ) #displaying arabic text
###
#libraries to be installed 
library(stringr)
library(tm)
library(arabicStemR)
library(qdap)
library(textstem)
library(SnowballC)
library(tidytext)
library(tokenizers)
library(textTinyR)
library(dplyr)
library(quanteda)
library(wordcloud)
library(ggwordcloud)
library(udpipe)
library(textutils)
library(ggraph)
library(igraph)
library(ggplot2)
library(textrank)
library(plotly)
library(tidyverse)
library(Matrix)
library(topicmodels)
library(lattice)  #barchart
########## 

#readig text file
arabic_text<-readLines("C:/Users/admin/Desktop/1 - Copy.txt", encoding = "UTF-8")
View(arabic_text)
arabic_text <- arabic_text[arabic_text != ""] #removing empty lines
length(arabic_text)
##########

#Calculate word frequencies before apllying any text normalization
words<-strsplit(arabic_text," ")
words.freq<-table(unlist(words))
word_freq=as.data.frame(words.freq)

#barplot
barplot(words.freq, las = 2, col ="light blue", main ="Frequent Words",ylab = "Word frequencies")

#barplot using ggplot
fig1<-ggplot(word_freq, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity",color="light blue")
fig1 + ggtitle("Wrds Frequency")  +ylab("Count") 
fig1_=fig1 +   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
ggplotly(fig1_)%>% layout(titlefont=list(size=24), yaxis = list(side="Left Top",  gridcolor = toRGB("Lightblue"),
                                                                gridwidth = 1, ticks="", title="Count", titlefont=list(size=10)), 
                          xaxis = list(ticks="",title="Words", tickfont=list(size=13)), 
                          #legend = list(x = 11, y = 11, font=list(size=20)),
                          margin = list(l = 30, r=55, b = 50, t = 80)) 

####################
#cleaning the text 

df=data.frame(arabic_text)

clean_norm_txt<-function(Text)
{
  #Text=sub("http[^[:space:]]*", "", Text) # remove url 1
  #Text=sub("http[[:alnum:][:punct:]]*", "", Text)# remove url 2
  #Text=sub('@\\S+', '', Text) ## Remove Mentions
  #Text=sub('\\b+RT', '', Text) ## Remove RT
  #Text=str_replace_all(Text,"#[a-z,A-Z]*","") #hashtags
  Text =str_remove_all(Text, "[\r\n]")
  Text=str_replace_all(Text,"[^[:alnum:][:blank:]&/\\-_]", "") #remove special characters
  Text=gsub('[[:cntrl:]]', '', Text) ## Remove Controls and special characters
  Text=gsub("\\d", '', Text) ## Remove Controls and special characters
  Text=str_replace_all(Text,"[a-z,A-Z]*","") #removing english words
  Text=stripWhitespace(Text) #space
  Text=str_replace_all(Text,"أ","ا") #removing hamzah
  Text=str_replace_all(Text,"إ","ا") #removing hamzah
  Text=str_replace_all(Text,"آ","ا") #removing hamzah
  Text=str_replace_all(Text,"ة","ه") 
  Text=gsub('[[:punct:]]', '', Text) ## Remove Punctuations
  Text=str_squish(Text)#, "right") #remove whitespace
  Text=removeDiacritics(Text) # removing diacritics 
  #removeStopWords(Text)$text
  Text=gsub("^[[:space:]]*","",Text) ## Remove leading whitespaces
  Text=gsub("[[:space:]]*$","",Text) ## Remove trailing whitespaces
  Text=gsub(' +',' ',Text) ## Remove extra whitespaces
  Text=gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", Text)# 
  #text= removeStopWords(text, defaultStopwordList=TRUE, customStopwordList=NULL)
  Text=removeNumbers(Text) # numbers
  Text= removeStopWords(Text, defaultStopwordList=TRUE, customStopwordList=NULL)$text#removing stop words
  #Text=replace_number(Text)# Replace numbers with words
  #Text=replace_abbreviation(Text) # Replace abbreviations
  #Text=replace_symbol(Text)   #Replace symbols with words
  return(Text)
}
#Calling the function (clean text) 
df$cleaned_text<-sapply(df$arabic_text,clean_norm_txt)

##############################

#Tokenization 
df1 <- tibble(txt = df$cleaned_text)
word_tokens <- df1 %>% unnest_tokens(word, txt)
#%>%  word_tokens %>% count(word, sort = FALSE)
word_tokens %>% count(word, sort = T) %>% head #head 
word_token_with_counts<-word_tokens %>% count(word, sort = T) #all

#bigrams 
words_bigrams <- df1 %>%unnest_tokens(bigram, txt, token = "ngrams", n = 2)

words_bigrams %>% count(bigram, sort = T) %>% head  #head 
bigrams_with_counts<-words_bigrams %>% count(bigram, sort = T) #all

#displaying word cloud 
word_count <- word_tokens %>% count(word, sort=T) %>%mutate(word=reorder(word,n))

ggplot(word_count%>% filter(n>=1) , aes(label=word, size=n)) +
  geom_text_wordcloud(eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal()
####################
#POS (Part-of-speech )
ud_model <- udpipe_download_model(language = "arabic")
x <- udpipe(x = df$cleaned_text,object = ud_model)
x[9] #token
x[10] #lemma

#Plotting POS tags

frequency_pos <- txt_freq(x$upos)
frequency_pos$key <- factor(frequency_pos$key, levels = rev(frequency_pos$key))
barchart(key ~ freq, data = frequency_pos, col = "blue", 
         main = "POS Frequency of Occurrence", 
         xlab = "Frequency")


#####
#most accuring token (noun,PROPN, VERB,ADJ, ADV ), you can specify any POS as you want

frequ <- subset(x, upos %in% c("NOUN", "PROPN", "VERB","ADJ", "ADV"))
frequ <- txt_freq(frequ$token)
frequ$key <- factor(frequ$key, levels = rev(frequ$key))
barchart(key ~ freq, data = head(frequ, 20), col = "Blue", 
         main = "Most occurring terms ", xlab = 'Frequency')

##########
#We look for how many times nouns, proper nouns, adjectives, verbs, and adverbs are used in the same sentence.
cooccur <- cooccurrence(x = subset(x, upos %in% c("NOUN", "PROPN", "VERB","ADJ", "ADV")), 
                        term = "lemma", 
                        group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooccur)

###

wordnetwork <- head(cooccur, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#ed9de9") +
  geom_node_point(aes(size = igraph::degree(wordnetwork)), shape = 1, color = "black") +
  geom_node_text(aes(label = name), col = "darkblue", size = 3) +
  labs(title = "Co-occurrences within sentence",
       subtitle = "Top 100 ")

################################
#Words which follow one another
cooccur <- cooccurrence(x$lemma,
                        relevant = x$upos %in% c("NOUN", "PROPN", "VERB", "ADV", "ADJ"),
                        skipgram = 1)
head(cooccur, 15)

wordnetwork <- head(cooccur, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#ed9de9") +
  geom_node_text(aes(label = name), col = "darkblue", size = 3, repel = TRUE) +
  labs(title = "Words following one another")
################
#extracting keywords
#RAKE for extracting keywords
extr_word <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                           relevant = x$upos %in% c("NOUN", "PROPN", "VERB", "ADJ"))
# plotting 
extr_word$key <- factor(extr_word$keyword, levels = rev(extr_word$keyword))
head(subset(extr_word, freq >= 2), 30) %>% ggplot() + 
  geom_bar(aes(x = key, y = rake), stat = "identity", fill = "#79ad95") + 
  theme_minimal() + 
  coord_flip() +
  labs(title = "Keywords identified by RAKE",
       y = "Rake",
       x = "Keywords")

############################
#Using Pointwise Mutual Information Collocations (words or terms that co-occur more often than would be expected by chance)
#x$word <- tolower(x$token)
collection <- keywords_collocation(x = x, term = "token", group = "doc_id")
collection$key <- factor(collection$keyword, levels = rev(collection$keyword))
# graphs
head(subset(collection, freq >=1), 30) %>% ggplot() + 
  geom_bar(aes(x = key, y = pmi), stat = "identity", fill = "#79ad95") + 
  theme_minimal() + 
  coord_flip() +
  labs(title = "Keywords identified by PMI Collocation",
       y = "PMI (Pointwise Mutual Information)",
       x = "Keywords")
######################################
#Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
k_phrases <- keywords_phrases(x = x$phrase_tag, term = x$token, 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                              is_regex = TRUE, detailed = FALSE)
k_phrases <- subset(k_phrases, ngram > 1 & freq > 1)
k_phrases$key <- factor(k_phrases$keyword, levels = rev(k_phrases$keyword))
# graph
head(k_phrases, 30) %>% ggplot() + 
  geom_bar(aes(x = key, y = freq), stat = "identity", fill = "#79ad95") + 
  theme_minimal() + 
  coord_flip() +
  labs(title = "Keywords - simple noun phrases",
       y = "Frequency",
       x = "Keywords")
######################
#relevance of each word
keywords <- textrank_keywords(x$lemma,relevant = x$upos %in% c("NOUN", "PROPN", "VERB", "ADJ"), 
                              ngram_max = 8, sep = " ")
subset(keywords$keywords, ngram >= 1 & freq > 1)

## plotting pagerank to see the relevance of each word
barplot(sort(keywords$pagerank$vector), horiz = TRUE,
        las = 2, cex.names = 0.5, col = "lightblue", xlab = "Pagerank")

##############################
#Dependency Parsing
depn_ <- merge(x, x, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
depn_ <- subset(depn_, dep_rel %in% "nsubj" & upos %in% c("NOUN", "PROPN") & upos_parent %in% c("VERB", "ADJ"))
depn_$term <- paste(depn_$lemma_parent, depn_$lemma, sep = " ")
depn_ <- txt_freq(depn_$term)
depn_

wordcloud(words = depn_$key, scale=c(5,.5),freq = depn_$freq, min.freq = 2, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))

# keys=as.data.frame(depn_$key)
# ggplot(keys, aes(label = word)) +geom_text_wordcloud() +theme_minimal()

###############################
#topics models
topic <- dfm(df$cleaned_text) %>% 
  dfm_trim(min_termfreq = 1, max_docfreq = 2)

con_topic <- convert(topic, to = "topicmodels") %>% 
  LDA(k = 4) #number of topics
# get top five terms per topic
get_terms(con_topic, 5)
###############

#corpus
#creating corpus 

text_corpus <- (VectorSource(df$cleaned_text))
text_corpus <- Corpus(text_corpus)
temp = text_corpus
print(lapply(temp,as.character))
summary(temp) 
inspect(temp[[1]])#documnet 1 
#######
## removes prefixes and suffixes
temp  <- tm_map(temp, content_transformer(stemDocument), language = "arabic")
print(lapply(temp,as.character)[1:4]) #1:4 number of documants

# list_<-lapply(text_corpus,as.character)[1:4]
# df1<-data.frame(Reduce(rbind, list_))
# names(df1)[1] <- "cleaned_text"
##################

#TERM-DOCUMENT MATRIX

tdm = TermDocumentMatrix(temp,control=list(minWordLength=1))
print(tdm)
inspect(tdm)
print(as.matrix(tdm))
print(as.matrix(weightTfIdf(tdm)))#word weight

#frequency after text normalization 
out = findFreqTerms(tdm,lowfreq=2) # you can specify the lower frequency using lowfreq
print(out)

#######
#word cloud

tdm2 = as.matrix(tdm)
wordcount = sort(rowSums(tdm2),decreasing=TRUE)
tdm_names = names(wordcount)
wordcloud(tdm_names,wordcount, scale=c(2,.5),min.freq=2, max.words = 50, 
          random.order = FALSE,random.color = FALSE,colors = brewer.pal(8, "Dark2"))
#####
arabic_freq<-sort(rowSums(tdm2),decreasing = TRUE) # term frequency

output_ar<-cbind(arabic_freq) # convert it to column
head(output_ar)
output_ar_df<-as.data.frame(output_ar)

#######################################

#text clustering(hierarchical relationship between words)

wsterms_tdm2 <- removeSparseTerms(tdm, sparse = 0.975)

cluster_ <- hclust(d = dist(wsterms_tdm2, method = "euclidean"), method = "complete")

# Plot the cluster
plot(cluster_,main = "Cluster")


######################3
#similarity betwen documents 
origdfm <- dfm(df$cleaned_text) #Document-Feature Matrix
dfm_wordstem (origdfm)
(test_stat <- textstat_dist(origdfm, margin = "documents"))
as.matrix(test_stat)
as.list(test_stat)
as.dist(test_stat)
distrib <- dist(origdfm,method="euclidean")
dfm_tfidf(origdfm)
dfm_tfidf(origdfm) %>%
  textstat_simil(method = "simple matching", selection = "text1") %>%
  as.matrix()

#using cosine
textstat_simil(origdfm,margin = "documents", method = "cosine")

#correlation 
textstat_simil(origdfm,margin = "documents", method = "correlation")

###################
