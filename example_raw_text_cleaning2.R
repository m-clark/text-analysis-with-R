# basic issues, removing meta data at beginning, and elsewhere (acts, scenes
# etc.) which is best accomplished via tidy; however, basically everything else
# would do best with the text in bulk; read_lines will read the \n, while
# readtext won't; tidytext seemingly willl smash lines together when going from
# lines to sentences or paras, ignoring the previous \n


# Scrape Moby Shakespeare -------------------------------------------------


library(rvest); library(tidyverse); library(stringr)

page0 = read_html('http://shakespeare.mit.edu/')

works_urls0 = page0 %>%
  html_nodes('a') %>%
  html_attr('href')
main = works_urls0 %>%
  grep(pattern='index', value=T) %>%
  str_replace_all(pattern='index', replacement='full')

other = works_urls0[!grepl(works_urls0, pattern='index|edu|org|news')]

works_urls = c(main, other)


# rvest read_html apparently doesn't know what <br> is sometimes, which
# subsequently will screw up any analysis. The issue has been requested for
# about half a year
works0 = lapply(works_urls, function(x) read_html(paste0('http://shakespeare.mit.edu/', x)))

source('r/fix_read_html.R')

html_text_collapse(works0[[1]]) #works
works = lapply(works0, html_text_collapse)


names(works) = c("All's Well That Ends Well", "As You Like It", "The Comedy of Errors",
                 "Cymbeline", "Love's Labour's Lost", "Measure for Measure",
                 "The Merry Wives of Windsor", "The Merchant of Venice", "A Midsummer Night's Dream",
                 "Much Ado about Nothing", "Pericles Prince of Tyre", "The Taming of the Shrew",
                 "The Tempest", "Troilus and Cressida", "Twelfth Night",
                 "The Two Gentlemen of Verona", "The Winter's Tale", "King Henry IV Part 1",
                 "King Henry IV Part 2", "Henry V", "Henry VI Part 1",
                 "Henry VI Part 2", "Henry VI Part 3", "Henry VIII",
                 "King John", "Richard II", "Richard III",
                 "Antony and Cleopatra", "Coriolanus", "Hamlet",
                 "Julius Caesar", "King Lear", "Macbeth",
                 "Othello", "Romeo and Juliet", "Timon of Athens",
                 "Titus Andronicus", "Sonnets", "A Lover's Complaint",
                 "The Rape of Lucrece", "Venus and Adonis", "Elegy")


#○ Sonnets -----------------------------------------------------------------

sonnet_url = paste0('http://shakespeare.mit.edu/', grep(works_urls0, pattern='sonnet', value=T)) %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href')

sonnet_url = sonnet_url[-1]  # remove amazon link

sonnet0 = purrr::map(sonnet_url, function(x) read_html(paste0('http://shakespeare.mit.edu/Poetry/', x)))
sonnet = sapply(sonnet0, html_text_collapse)
works$Sonnets = sonnet


#○ Save and write out ------------------------------------------------------

save(works, file='data/texts_raw/shakes/moby_from_web.RData')

# This will spit the text to the console for reasons unknown
purrr::map2(works,
            paste0('data/texts_raw/shakes/moby/', str_replace_all(names(works), " |'", '_'), '.txt'),
            function(x, nam) write_lines(x, path=nam))


# Read text ---------------------------------------------------------------

# After the above is done, it's not required to redo

library(tidyverse); library(stringr)


shakes0 =
  data_frame(file = dir('data/texts_raw/shakes/moby/', full.names = TRUE)) %>%
  mutate(text = map(file, read_lines)) %>%
  transmute(id = basename(file), text) %>%
  unnest(text)

# Alternate that provides for more options
# library(readtext)
# shakes0 =
#   data_frame(file = dir('data/texts_raw/shakes/moby/', full.names = TRUE)) %>%
#   mutate(text = map(file, readtext, encoding='UTF8')) %>%
#   unnest(text)


#○ Add additional works ----------------------------------------------------

library(gutenbergr)

works_not_included = c("The Phoenix and the Turtle") # Taking out "The Two Noble Kinsmen" bc it's basically written in a different language and Pilgrim bc it's mostly not Shakes

from_gute0 = gutenberg_works(title %in% works_not_included)
from_gute = lapply(from_gute0$gutenberg_id, gutenberg_download)
from_gute = mapply(function(x, y) mutate(x, id=y) %>%
                     select(-gutenberg_id), from_gute, works_not_included, SIMPLIFY=F)

shakes = shakes0 %>%
  bind_rows(from_gute) %>%
  mutate(id = str_replace(id, '.txt', ''))

# shakes %>% split(.$id) # inspect

save(shakes, file='data/texts_raw/shakes/shakes_df.RData')


# Clean up ----------------------------------------------------------------

# After the above is done, it's not required to redo

library(tidyverse); library(stringr)

load('data/texts_raw/shakes/shakes_df.RData')


#○ Trim initial title preface etc. -----------------------------------------

source('r/detect_first_act.R')

shakes_trim = shakes %>%
  split(.$id) %>%
  lapply(detect_first_act) %>%
  bind_rows

shakes %>% filter(id=='Romeo_and_Juliet') %>% head
shakes_trim %>% filter(id=='Romeo_and_Juliet') %>% head



#○ Remove empty, scenes, titles, and other ---------------------------------

titles =  c("All's Well That Ends Well", "As You Like It", "The Comedy of Errors",
            "Cymbeline", "Love's Labour's Lost", "Measure for Measure",
            "The Merry Wives of Windsor", "The Merchant of Venice", "A Midsummer Night's Dream",
            "Much Ado about Nothing", "Pericles Prince of Tyre", "The Taming of the Shrew",
            "The Tempest", "Troilus and Cressida", "Twelfth Night",
            "The Two Gentlemen of Verona", "The Winter's Tale", "King Henry IV Part 1",
            "King Henry IV Part 2", "Henry V", "Henry VI Part 1",
            "Henry VI Part 2", "Henry VI Part 3", "Henry VIII",
            "King John", "Richard II", "Richard III",
            "Antony and Cleopatra", "Coriolanus", "Hamlet",
            "Julius Caesar", "King Lear", "Macbeth",
            "Othello", "Romeo and Juliet", "Timon of Athens",
            "Titus Andronicus", "Sonnets", "A Lover's Complaint",
            "The Rape of Lucrece", "Venus and Adonis", "Elegy", "The Phoenix and the Turtle")

# for later
shakes_types = data_frame(title=titles) %>%
  mutate(class = 'Comedy',
         class = if_else(grepl(title, pattern='Adonis|Lucrece|Complaint|Turtle|Pilgrim|Sonnet|Elegy'), 'Poem', class),
         class = if_else(grepl(title, pattern='Henry|Richard|John'), 'History', class),
         class = if_else(grepl(title, pattern='Troilus|Coriolanus|Titus|Romeo|Timon|Julius|Macbeth|Hamlet|Othello|Antony|Cymbeline|Lear'), 'Tragedy', class),
         problem = if_else(grepl(title, pattern='Measure|Merchant|^All|Troilus|Timon|Passion'), 'Problem', 'Not'),
         late_romance = if_else(grepl(title, pattern='Cymbeline|Kinsmen|Pericles|Winter|Tempest'), 'Late', 'Other'))

save(shakes_types, file='data/shakespeare_classification.RData')

shakes_trim = shakes_trim %>%
  filter(text != '',                     # empties
         !text %in% titles,              # titles
         !str_detect(text, '^ACT|^SCENE|^Enter|^Exit|^Exeunt|^Sonnet')  # acts etc.
         )

shakes_trim %>% filter(id=='Romeo_and_Juliet') # we'll get prologue later



# Stopwords -----------------------------------------------

# Stopwords will include character names, middle english and modern

#○ Character names ---------------------------------------------------------


# library(rvest)
# shakes_char_url = 'https://www.opensourceshakespeare.org/views/plays/characters/chardisplay.php'
# page0 = read_html(shakes_char_url)
# tabs = page0 %>% html_table()
# shakes_char = tabs[[2]][-(1:2), c(1,3,5)] # remove header and phantom columns
# colnames(shakes_char) = c('Nspeeches', 'Character', 'Play')
# shakes_char = shakes_char %>%
#   distinct(Character,.keep_all=T)

# save(shakes_char, file='data/shakespeare_characters.RData')
load('data/shakespeare_characters.RData')

# some characters with multiple names may be represented (typically) by the first or last or in the case of three, the middle, e.g. sir Toby Belch; Others are still difficultly named e.g. RICHARD PLANTAGENET (DUKE OF GLOUCESTER). The following should capture everything given what we're working with

# remove paren and split
chars = shakes_char$Character
chars = str_replace_all(chars, '\\(|\\)', '')
chars = str_split(chars, ' ') %>%
  unlist

# these were found after processsing
chars_other = c('enobarbus', 'marcius', 'katharina', 'clarence','pyramus',
                'andrew', 'arcite', 'perithous', 'hippolita', 'schoolmaster',
                'cressid', 'diomed', 'kate', 'titinius', 'Palamon', 'Tarquin',
                'Baptista', 'Lucrece', 'isidore', 'tom')

chars = unique(c(chars, chars_other))


chars =  chars[chars != '']
# sort(chars)


#○ Old/middle English ------------------------------------------------------

# old and me from python cltk module; em from http://earlymodernconversions.com/wp-content/uploads/2013/12/stopwords.txt; I also added some to me

old_stops0 = read_lines('data/old_english_stop_words.txt')
# sort(old_stops0)
old_stops = data_frame(word=str_conv(old_stops0, 'UTF8'),
                      lexicon = 'cltk')

me_stops0 = read_lines('data/middle_english_stop_words')
# sort(me_stops0)
me_stops = data_frame(word=str_conv(me_stops0, 'UTF8'),
                      lexicon = 'cltk')

em_stops0 = read_lines('data/early_modern_english_stop_words.txt')
# sort(em_stops0)
em_stops = data_frame(word=str_conv(em_stops0, 'UTF8'),
                      lexicon = 'emc')


library(tidytext)
library(quanteda)
# library(readtext)
# test = readtext('data/texts_raw/shakes/moby/Antony_and_Cleopatra')


#○ Remove name only lines --------------------------------------------------

shakes_words = shakes_trim %>%
  filter(!str_detect(text, paste0('^', chars, collapse='|'))) %>%
  unnest_tokens(word, text, token='words')


#○ Deal with suffixes ------------------------------------------------------

source('r/st_stem.R')

shakes_words = shakes_words %>%
  mutate(word = str_trim(word),    # remove possible whitespace
         word = str_replace(word, "'er$|'d$|'t$|'ld$|'rt$|'st$|'dst$", ''),    # remove me style endings
         word = vapply(word, me_st_stem, 'a')) %>%
  anti_join(old_stops) %>%
  anti_join(me_stops) %>%
  anti_join(em_stops) %>%
  anti_join(data_frame(word=str_to_lower(c(chars, 'prologue')))) %>%
  anti_join(data_frame(word=str_to_lower(paste0(chars, "'s")))) %>%     # remove possessive names
  anti_join(stop_words)


#○ Other fixes to do before counts -----------------------------------------

# porter should catch remaining est


add_a =  c('mongst', 'gainst')
shakes_words = shakes_words %>%
  mutate(word = if_else(word=='honour', 'honor', word),
         word = if_else(word=='durst', 'dare', word),
         word = if_else(word=='wast', 'was', word),
         word = if_else(word=='dust', 'does', word),
         word = if_else(word=='curst', 'cursed', word),
         word = if_else(word=='blest', 'blessed', word),
         word = if_else(word=='crost', 'crossed', word),
         word = if_else(word=='accurst', 'accursed', word),
         word = if_else(word %in% add_a,
                        paste0('a', word),
                        word),
         word = str_replace(word, "'s$", ''),                # strip remaining possessives
         word = if_else(str_detect(word, pattern="o'er"),
                        str_replace(word, "'", 'v'),
                        word)) %>%
  filter(str_count(word)>2)

# term counts and tests
term_counts = shakes_words %>%
  group_by(id, word) %>%
  count
# term_counts %>%
#   arrange(desc(n)) %>%
#   # filter(str_count(word)>2) %>%
#   data.frame()
# term_counts %>%
#   arrange(desc(n)) %>%
#   data.frame() %>%
#   filter(n<5)
# term_counts %>%
#   ungroup %>%
#   arrange(desc(n)) %>%
#   data.frame() %>%
#   filter(grepl(word, pattern='st$')) %>%
#   filter(!grepl(word, pattern='est$')) %>%
#   distinct(word)

save(shakes_words, term_counts, file='data/shakes_words_df.RData')

# Document term matrix ----------------------------------------------------

load('data/shakes_words_df.RData')

library(quanteda)

shakes_dtm = term_counts %>%
  cast_dfm(document=id, term=word, value=n)
ncol(shakes_dtm)
shakes_dtm = shakes_dtm %>%
  dfm_wordstem()
ncol(shakes_dtm)
summary(shakes_dtm)

# top 20 words
topfeatures(shakes_dtm, 20)

# useless!
textplot_wordcloud(shakes_dtm, min.freq = 300, random.order = T,
                   rot.per = .25,
                   colors = viridis::viridis(500))

# similarity

textstat_simil(shakes_dtm, margin = "documents", method = "cosine") %>%
  heatR::corrheat()

textstat_simil(dfm_weight(shakes_dtm, 'relFreq'), margin = "documents", method = "correlation") %>%
  heatR::corrheat()


# text model scaling

test_scale = textmodel(shakes_dtm, model = "wordfish", dir=c(2,1))

# topic model

library(topicmodels)
shakes_10 = LDA(convert(shakes_dtm, to = "topicmodels"), k = 10)
get_terms(shakes_10, 20)
topics(shakes_10, 3)

# pal = c(lazerhawk::palettes$latvian_red$latvian_red, lazerhawk::palettes$latvian_red$tetradic)

row_dend  = textstat_simil(shakes_dtm, margin = "documents", method = "cosine") %>%
  as.dist() %>%
  hclust(method="ward.D") %>%
  as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", c(.5,.5)) %>%
  ladderize

shakes_10@gamma %>%
  round(3) %>%
  heatmaply::heatmaply(Rowv=row_dend,
                       Colv=F,
                       # distfun = '',
                       # dendrogram = 'none',
                       labRow=rownames(shakes_dtm),
                       labCol=paste0('topic', 1:10),
                       colors=viridis::inferno(50),
                       k_row = 4,
                       row_side_colors = select(arrange(shakes_types, title), class),
                       fontsize_row=7)
