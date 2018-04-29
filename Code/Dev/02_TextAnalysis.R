
library(knitr)
library(stringr)
library(lubridate)
library(ggrepel)
library(wordcloud)
library(gridExtra)
library(tidyverse)
library(tidytext)

titles.format <- theme(plot.title = element_text(face="bold", size=13, color='grey50'),
                       plot.subtitle = element_text(color='grey50'),
                       axis.title = element_text(size=9, color='grey50'), 
                       axis.text = element_text(size=9, color='grey50'),
                       plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))



# Create a new dataframe with one row of lyrics for each track (instead of multiple rows per verse/chorus)
wrk.02_TextAnalysis_00 <-  wrk.01_DataPrep_LyricsWithSpotify %>% 
  group_by(CATTrackName) %>% 
  mutate(TXTAllTrackLyrics = paste0(text, collapse = " ")) %>%
  mutate(NUMMaxLyricLines = max(as.numeric(NUMTrackLyricLineNumber)))



#==== TODO: FIX ELTON JOHN, some songs aren't showing up as heading 3.
#==== TODO: time duration is not available for all albums

# We can use heading 3 for most analysis activities here.
df <- wrk.02_TextAnalysis_00 %>%
  filter(style_name == "heading 3") %>%
  mutate(CATMusicArtist = as.factor(CATMusicArtist )) %>%
  mutate(CATMusicAlbum = as.factor(CATMusicAlbum )) %>%
  mutate(BINTrackIsInstrumental = as.factor(BINTrackIsInstrumental)) %>%
  mutate(key = as.factor(key)) #%>%
  #mutate(duration_ms = minutes(duration_ms)) 


# Summary table: What artists, albums and tracks?
artists <- df %>% 
  group_by(CATMusicArtist, CATMusicAlbum) %>% 
  summarise(nbreSongs = n(), duration=seconds_to_period(sum(duration_ms)))
kable(artists, format='markdown')

# Songs by key
#TODO SHOW BY ARTIST / TRACK
df %>% filter(!is.na(key)) %>%
  group_by(key) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>%
  mutate(key = reorder(key, -n)) %>%
  ggplot(., aes(x=key, y=n)) +
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85',
           ) +
  xlab('Keys') +
  ylab('Number of Songs') +
  labs(title='Songs per Key', subtitle='18 songs are NAs') +
  titles.format


## PART A: LYRICAL ANALYSIS
df <- df %>%
  mutate(TXTAllTrackLyrics = str_replace_all(TXTAllTrackLyrics, '\'', ' ')) %>%
  mutate(nbreWord = str_count(TXTAllTrackLyrics, ' ') + 1)

lineToken <- df %>%
  unnest_tokens(line, TXTAllTrackLyrics, token = stringr::str_split, pattern = '.') %>% 
  mutate(lineCount = row_number())

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number())

countWord <- count(wordToken, word, sort=TRUE)
countWord <- head(countWord, 100)
empty <- data.frame(a=character(100),b=rep('|',100),c=character(100),
                    stringsAsFactors = FALSE)
data("stop_words")
wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%
  arrange(wordCount)
countWord2 <- count(wordToken2, word, sort=TRUE)
countWord2 <- head(countWord2, 100)


# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #
