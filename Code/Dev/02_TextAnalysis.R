# R Script: 02_TextAnalysis.R
# Description: 
# Begin analysis of dataset using text mining, NLP and machine learning techniques where appropriate
# Author: Bree McLennan
# Date: 14/05/2018
#
# ======================================================================================================================== #

library(knitr)
library(stringr)
library(lubridate)
library(ggrepel)
library(wordcloud)
library(gridExtra)
library(tidyverse)
library(tidytext)
library(feather)
library(rprojroot)
library(purrr) #reduce and map functions
`%ni%` <- Negate(`%in%`)

# Define some colour themese to use in visualisations
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#ggplot2 custom theme settings
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 

wrk.01_DataPrep_LyricsWithSpotify <- read_feather(F("Data/Processed/wrk.01_DataPrep_LyricsWithSpotify.feather"))

# ======================= MANUAL DATA EXPLORATION ============================================================= #

# ==== PART A: Data Prep. Cleaning dataset with TM package, text pre-processing

# Create a new dataframe with one row of lyrics for each track (instead of multiple rows per verse/chorus)
wrk.02_TextAnalysis_00 <-  wrk.01_DataPrep_LyricsWithSpotify %>% 
  group_by(CATTrackName) %>% 
  mutate(TXTAllTrackLyrics = paste0(text, sep = "<br>", collapse = " ")) %>%
  mutate(NUMMaxLyricLines = max(as.numeric(NUMTrackLyricLineNumber)))

# Create a dataset with one row per song. One variable & record to hold all lyrics for a song.
wrk.02_TextAnalysis_01 <- wrk.02_TextAnalysis_00 %>%
  filter(style_name == "heading 3")

# lets now remove songs which were purely instrumental only
wrk.02_TextAnalysis_02 <- wrk.02_TextAnalysis_01 %>%
  filter(str_detect(TXTAllTrackLyrics, "Instrumental") == FALSE )

#Summary
glimpse(wrk.02_TextAnalysis_02)

#Identify any specific/customisable words we we with to eliminate
undesirable_words <- c("chorus", "lyrics", "verse")

#Create lyric verse tokens
lineToken <- wrk.02_TextAnalysis_02 %>%
  ungroup() %>%
  unnest_tokens(line, TXTAllTrackLyrics, token = stringr::str_split, pattern = '<br>') %>% #break the lyrics into verses
  mutate(lineCount = row_number())

#Create lyric word tokens and apply tidy text format
wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>%  #Break the lyrics into individual words
  #anti_join(stop_words) %>% #removing stop words
  #distinct() %>%
  filter(!word %in% undesirable_words) #removing custom configured stop words
  #mutate(wordCount = row_number())

# DESCRIPTIVE STATS
# Full word count for each song (non-distinct)
wrk.02_TextAnalysis_03_WordCount <- wordToken %>%
  group_by(CATMusicArtist,CATMusicAlbum, CATTrackName) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

# Common words
#overall (validation)
wrk.02_TextAnalysis_03_CommonWordsAll <- wordToken %>%
  count(word, sort = TRUE)
# Most common words in each song
wrk.02_TextAnalysis_03_CommonWordsSong <- wordToken %>%
  anti_join(stop_words) %>% # remove stop words, allow us to see some more meaningful results
  count(CATTrackName, word, sort = TRUE) %>%
  ungroup()

wrk.02_TextAnalysis_03_CommonWordsGrouped

# Most common words in each album
wrk.02_TextAnalysis_03_CommonWordsAlbum <- wordToken %>%
  anti_join(stop_words) %>% # remove stop words, allow us to see some more meaningful results
  count(CATMusicArtist, CATMusicAlbum, word, sort = TRUE) %>%
  ungroup()


# WORD CLOUDS - for each album
library(wordcloud2)
#==== DAFT PUNK
albums_wordcloud_DaftPunk <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Daft Punk") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_DaftPunk[1:89, ], size = .5) #wont render if rownum set to more than whats in the dataset

#==== U2
albums_wordcloud_U2 <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "U2") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_U2[1:100, ], size = .5)

#==== Elton John
albums_wordcloud_Elton <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Elton John") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_Elton[1:100, ], size = .5)

#==== Led Zeppelin
albums_wordcloud_LedZep <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Led Zeppelin") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_LedZep[1:100, ], size = .5)

#==== Killswitch Engage
albums_wordcloud_KsE <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Killswitch Engage") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_KsE[1:100, ], size = .5)


#==== Iron Maiden
albums_wordcloud_IronMaiden <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Iron Maiden") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_IronMaiden[1:100, ], size = .5)



# Find the TF-IDF within the albums
# We expect the albums to differ in terms of subject/topic, content and sentiment, we therefore expect the frequency of words to differ 
# between albums, We can use TF-IDF metric to calculate these differences.
tf_idf <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  bind_tf_idf(word, CATMusicArtist, n) %>%
  arrange(desc(tf_idf))

tf_idf

# Examining the TF-IDF. By Albums as groups
tf_idf %>%
  #filter(str_detect(CATMusicArtist, "^sci\\.")) %>%
  group_by(CATMusicArtist) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# Which albums are similar to each other in text content? We can explore this by finding
# the pairwise correlation of word frequencies within each newsgroup, using the pairwise_cor()
# function from the widyr package

library(widyr)

album_cors <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  pairwise_cor(CATMusicArtist, word, n, sort = TRUE)

album_cors

#Lets now filter on stronger correlations with the albums and visualise in a network
library(ggraph)
library(igraph)
set.seed(1234)

album_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# It seems these albums don't correlate strongly! Maybe we can try at the song level?

song_cors <- wrk.02_TextAnalysis_03_CommonWordsSong %>%
  pairwise_cor(CATTrackName, word, n, sort = TRUE)

song_cors

set.seed(4321)

song_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# Curious, Elton John's Mona Lisa and Led Zeppelin's Down by the Seaside have a connection with a correlation greater than 0.4.
# Interesting that no other songs had a high enough correlation to be considered as significant.



# Summary table for full word counts. This includes all words, non-distinct.
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function

# Number of songs by artist
wrk.02_TextAnalysis_03_SongPerArtist <- wrk.02_TextAnalysis_01 %>%
  ungroup() %>%
  mutate(BINSongIsInstrumental = as.factor(ifelse(str_detect(TXTAllTrackLyrics, "Instrumental") == TRUE, 1,0 ))) %>%
  mutate(BINSongIsInstrumental = as.numeric(BINSongIsInstrumental)) %>%
  group_by(CATMusicArtist,CATMusicAlbum) %>%
  add_tally() %>% #count the total number of rows in the by group assign to variable "n"
  rename(NUMTotalSongsForArtist = n) %>% # RENAME PARAMETERS (NEW NAME = OLD NAME).
  ungroup() 

#instrumental songs
instrumental <- filter(wrk.02_TextAnalysis_03_SongPerArtist, str_detect(TXTAllTrackLyrics, "Instrumental") == TRUE )

   
# Songs by the highest unique word count
wrk.02_TextAnalysis_03_WordCount %>%
  ungroup(num_words, CATTrackName) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(CATTrackName = color_tile("lightpink","lightpink")(CATTrackName)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)



# ==== PART B: Natural Language Processinh (NLP) - Sentiment Polarity
#we can consider stemming/root words here, incase we want to explore matching words in lexicons
#syuzhet pkg
#Calls the NRC sentiment dictionary to calculate the presence of 
#eight different emotions and their corresponding valence in a text file.


library(syuzhet)
SongSentiment <- get_nrc_sentiment(wrk.02_TextAnalysis_02$TXTAllTrackLyrics)
#SongSentimentValues <- get_nrc_values(wrk.02_TextAnalysis_02$TXTAllTrackLyrics)

wrk.02_TextAnalysis_03 <- bind_cols(wrk.02_TextAnalysis_02, SongSentiment) 

wrk.02_TextAnalysis_03_nrcsentiment <- wrk.02_TextAnalysis_03 %>%
  select(CATMusicArtist, CATMusicAlbum, CATTrackName, anger, anticipation, disgust,
         fear, joy, sadness, surprise , trust ) 

wrk.02_TextAnalysis_03_nrcsentiment_daftpunk <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Daft Punk")

wrk.02_TextAnalysis_03_nrcsentiment_U2 <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "U2")

wrk.02_TextAnalysis_03_nrcsentiment_Elton <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Elton John")

wrk.02_TextAnalysis_03_nrcsentiment_ledzep <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Led Zeppelin")

wrk.02_TextAnalysis_03_nrcsentiment_KsE <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Killswitch Engage")

wrk.02_TextAnalysis_03_nrcsentiment_IronMaiden <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Iron Maiden")


#wrk.02_TextAnalysis_03_nrc_sentiment <- wrk.02_TextAnalysis_02 %>%
#  inner_join(get_sentiments("nrc")) %>%
#  filter(!sentiment %in% c("positive", "negative"))

par(mfrow = c(3,2)) # 3 rows and 2 columns
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_daftpunk[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Daft Punk - Discovery", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_U2[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in U2 - Joshua Tree", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_Elton[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Elton John - Honky Chateau", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_ledzep[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Led Zeppelin - Physical Graffiti", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_KsE[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Killswitch Engage - Alive or Just Breathing", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_IronMaiden[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Iron Maiden - Powerslave", xlab="Percentage",
  col = rainbow(8)
)



# ====== EXPERIMENTAL CODE ========== #



#==== TODO: time duration is not available for all albums


  #mutate(CATMusicArtist = as.factor(CATMusicArtist )) %>%
  #mutate(CATMusicAlbum = as.factor(CATMusicAlbum )) %>%
 #mutate(key = as.factor(key)) #%>%
  #mutate(duration_ms = minutes(duration_ms)) 



## PART A: LYRICAL ANALYSIS
# What do the songs say?
df <- df %>%
  ungroup() %>%
  mutate(TXTAllTrackLyrics = str_replace_all(TXTAllTrackLyrics, '\'', ' ')) %>%
  mutate(nbreLines = str_count(TXTAllTrackLyrics, '<br>') + 1) %>%
  mutate(nbreWord = str_count(TXTAllTrackLyrics, ' ') + 1)

lineToken <- df %>%
  unnest_tokens(line, TXTAllTrackLyrics, token = stringr::str_split, pattern = '<br>') %>% 
  mutate(lineCount = row_number())

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number())

countWord <- count(wordToken, word, sort=TRUE)
countWord <- head(countWord, 100)
empty <- data.frame(a=character(100),b=rep('|',100),c=character(100), stringsAsFactors = FALSE)

data("stop_words")
wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%
  arrange(wordCount)
countWord2 <- count(wordToken2, word, sort=TRUE)
countWord2 <- head(countWord2, 100)

#plot setup
medianWord <- median(df$nbreWord)
uniqueWords <- wordToken2 %>% 
  select(word) %>%
  filter(!str_detect(word, '[0-9]')) %>%
  group_by(word) %>% 
  filter(row_number(word) == 1) %>%
  arrange(word)
nbreUniqWords <- nrow(uniqueWords)

#distribution plot of songs by number of words
df %>% 
  ggplot(., aes(x=nbreWord)) +
  geom_histogram(binwidth=10,
                 color='white',
                 fill='#FCCB85') +
  geom_vline(aes(xintercept=medianWord), colour="#990000", linetype="dashed") +
  coord_cartesian(ylim=c(0, 15)) + 
  scale_y_continuous(breaks=seq(0, 15, 1)) +
  scale_x_continuous(breaks=seq(0, 400, 20)) +
  theme(panel.grid.minor = element_blank()) +
  xlab('Total Number of Words') +
  ylab('Nbre of Songs') +
  labs(title='Distribution of Songs by Number of Words', 
       subtitle='Verses repeats not included - Dashed red line is median') +
  titles.format

# Unigrams  and Word clouds 
tab <- cbind(countWord, empty, countWord2)
kable(tab[1:20,], format='markdown', row.names = F,
      col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))

layout(matrix(c(1,2),1,2, byrow = TRUE)) #6 ROWS 2 COLS
wordcloud(countWord$word, countWord$n, random.order=FALSE, max.words = 100, 
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)
wordcloud(countWord2$word, countWord2$n, random.order=FALSE, max.words = 100,
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)

#Unigrams by album
l <- length(levels(as.factor(wordToken2$CATMusicAlbum)))
plotList <- list()
for(i in 1:l){
  part <- wordToken2[wordToken2$CATMusicAlbum == levels(as.factor(wordToken2$CATMusicAlbum))[i],] %>%
    filter(BINTrackIsInstrumental == 0) %>%
    group_by(CATMusicAlbum) %>%
    count(word) %>%
    top_n(20)
  p <- ggplot(part[1:20,], aes(reorder(word,n), n)) +
    geom_bar(stat = "identity", fill='#FCCB85', width=0.65) +
    #        scale_fill_discrete(drop=F) +
    labs(y=NULL, x=NULL, title=paste('Album: ', levels(as.factor(wordToken2$CATMusicAlbum))[i], sep='')) +
    coord_flip() +
    titles.format +
    theme(plot.title = element_text(size=11))
  plotList[[i]] <- p
}
do.call(grid.arrange, c(plotList, ncol=3))

## BI GRAMS & TRI GRAMS
nGram <- data_frame(text=paste(wordToken$word, collapse = ' '))
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' '))

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE)

b.G <- biGrams %>%
  mutate(ngram = reorder(ngram, n)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=ngram, y=n)) + 
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85') + 
  xlab('bi-grams') +
  #       ylab('Number of Press Clippings') +
  coord_flip() +
  labs(title='Most Frequent bi-Grams') +
  titles.format

t.G <- triGrams %>%
  mutate(ngram = reorder(ngram, n)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=ngram, y=n)) + 
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85') + 
  xlab('tri-grams') +
  #       ylab('Number of Press Clippings') +
  coord_flip() +
  labs(title='Most Frequent tri-Grams') +
  titles.format
grid.arrange(b.G, t.G, ncol=2, widths=c(1,1.2))


# SENTIMENTS FEELS...
bing <- get_sentiments('bing')
sentLyricsWord <- wordToken %>%
  inner_join(bing)

bing <- get_sentiments('bing')
sentLyricsWord <- wordToken %>%
  inner_join(bing)

mostSentWord <- sentLyricsWord %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

ggplot(mostSentWord, aes(word, n, fill = n > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title='Most Frequent Positive And Negative Sentiment Words',
       subtitle='In the 14 studio albums overall',
       y='Frequency',
       x=NULL) +
  coord_flip() +
  titles.format



# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #
