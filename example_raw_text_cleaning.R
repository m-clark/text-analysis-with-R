
# Setup -------------------------------------------------------------------


library(tidyverse); library(gutenbergr); library(tidytext); library(quanteda); library(stringr)

# the gutenberg organization is lazy (much like their website), and the gutenbergr metadata doesn't even have (approx) year. Practically begs a hack-only approach.
gutenberg_metadata

# thanks for the hack! Conveniently also, the names of the Henry works are
# completely inconsistent, e.g. 3 Henry IV of Henry IV, Part 1, King Henry IV,
# the First Part, King Henry IV, Second Part; Gutenberg also has evidently not
# updated that Two Noble Kinsmen is accepted as Shakespeare (and Fletcher);
# after further processing Hamlet (just Hamlet) and Othello (just Othello) had
# words like neuer for never; so I'm selecting the more usable version
gw0 = gutenberg_works(
  author=='Shakespeare, William',
  !str_detect(title, "Works|Shakes|Theatre|Folio|Quarto|;|^The Tragedy"),
  !str_detect(title, "^King Henry VI|^Henry IV|^The History|^History|^The Life"),
  languages='en') %>%
  bind_rows(gutenberg_works(gutenberg_id==1542)) %>%  # add two noble kinsmen
  bind_rows(gutenberg_works(gutenberg_id==1528)) %>%  # add back 'the history of troilus...
  bind_rows(gutenberg_works(gutenberg_id==1041)) %>%  # add back Sonnets
  filter(!title %in% c('Hamlet', 'Othello')) # remove these versions



gw0 %>% filter(grepl(x=title, 'Henry'))
gw0 %>% filter(grepl(x=title, 'Troilus'))

gw = gw0 %>%
  select(gutenberg_id, title) %>%
  mutate(class = 'Comedy',
         class = if_else(grepl(title, pattern='Adonis|Lucrece|Complaint|Turtle|Pilgrim|Sonnet'), 'Poem', class),
         class = if_else(grepl(title, pattern='Henry|Richard|John'), 'History', class),
         class = if_else(grepl(title, pattern='Troilus|Coriolanus|Titus|Romeo|Timon|Julius|Macbeth|Hamlet|Othello|Antony|Cymbeline|Lear'), 'Tragedy', class),
         problem = if_else(grepl(title, pattern='Measure|Merchant|^All|Troilus|Timon|Passion'), 'Problem', 'Not'))

gw


#○ Checks ------------------------------------------------------------------

# table(gw$class)
# table(gw$title, gw$class)
# gw$title[gw$problem=='Problem']
# gw %>% filter(class=='Comedy') %>% data.frame() %>% arrange(title)
# gw %>% filter(class=='Tragedy') %>% data.frame() %>% arrange(title)



# Download Works ----------------------------------------------------------

works = lapply(gw$gutenberg_id, gutenberg_download)
names(works) = gw$title
save(works, file='data/texts_raw/shakes/gutenberg_shakespeare.RData')

# works2 = gutenberg_download(100, meta_fields=c('title'))  # no titles!
# works2

# Clean up ----------------------------------------------------------------
load('data/texts_raw/shakes/gutenberg_shakespeare.RData')


#○ Trim initial title preface etc. -----------------------------------------

detect_first_act <- function(text_tbl) {
  act_lines = grep(text_tbl$text, pattern='^Act|^ACT|^by')
  if(length(act_lines) > 0) {
    idx = 1:act_lines[1]
    text_tbl = text_tbl %>% slice(-idx)
  }
  text_tbl
}

works_trim = lapply(works, detect_first_act)

# compare
works$`Romeo and Juliet`
works_trim$`Romeo and Juliet`
works$`Shakespeare's Sonnets`
works_trim$`Shakespeare's Sonnets`



#○ Get rid of scene and act lines ----------------------

works_trim = lapply(works_trim, filter,
                    !text == str_to_upper(text),
                    !str_detect(text, pattern='^Scene|^Act |^SCENE|^ACT |^\\[|^Enter'))
works_trim


#○ encoding ----------------------------------------------------------------

works_trim = lapply(works_trim, function(x) mutate(x, text= str_conv(text, encoding='UTF8')))



#○ Get rid of metainfo -----------------------------------------------------

# somehow Timon still has metadata since apparently there are no Acts or Scenes in gutenberg's version
meta_idx = sapply(works_trim, function(x) grepl(x[1,'text'], pattern='^Executive'))
timon = works_trim$`Timon of Athens`
timon_begin = 'The Life of Timon of Athens'
works_trim$`Timon of Athens` = slice(timon,
                                     -(1:grep(timon$text, pattern=timon_begin)))



# Create data frame -------------------------------------------------------

works_trim_df = bind_rows(works_trim, .id='work')


# Get and remove characters -----------------------------------------------


library(rvest)
shakes_char_url = 'https://www.opensourceshakespeare.org/views/plays/characters/chardisplay.php'
page0 = read_html(shakes_char_url)
tabs = page0 %>% html_table()
shakes_char = tabs[[2]][-(1:2), c(1,3,5)] # remove header and phantom columns
colnames(shakes_char) = c('Nspeeches', 'Character', 'Play')
shakes_char = shakes_char %>%
  distinct(Character,.keep_all=T)

# get dukes, earls, etc.
chars = sapply(shakes_char$Character, function(x) str_split(x, pattern=' '))
chars = sapply(chars, function(x) ifelse(length(x)>1, x[length(x)], NA))
chars = chars[!is.na(chars)]
chars = c(str_replace_all(chars, pattern='\\(|\\)', '')) # otherwise regex will freak
chars = c(chars, 'fford', 'ronicus', 'ssid', 'Palamon', 'Tarquin', 'Baptista',
          'Iohn') # some names got screwed up


works_trim_df = works_trim_df %>%
  mutate(text = str_replace_all(text,
                                pattern=paste0(shakes_char$Character, collapse='|'),
                                replacement=''),
         text = str_replace_all(text,
                                pattern=paste0(str_sub(shakes_char$Character, 1, 3), collapse='|'),
                                replacement=''),
         text = str_replace_all(text,
                                pattern=paste0(str_sub(shakes_char$Character, 1, 4), collapse='|'),
                                replacement=''))

works_trim_df = works_trim_df %>%
  mutate(text = str_replace_all(text,
                                pattern=paste0(chars, collapse='|'),
                                replacement=''),
         text = str_replace_all(text,
                                pattern=paste0(str_sub(chars, 1, 3), collapse='|'),
                                replacement=''),
         text = str_replace_all(text,
                                pattern=paste0(str_sub(chars, 1, 4), collapse='|'),
                                replacement=''))



# Get words ---------------------------------------------------------------


# good christ the clusterfuck. Names are sometimes abbreviated, punctuation is erratic at best, etc.
# works_sentences = lapply(works_trim, unnest_tokens, sentence, text, token='sentences')
# works_sentences

works_words = works_trim_df %>%
  select(-gutenberg_id) %>%
  group_by(work) %>%
  unnest_tokens(word, text, to_lower=F) %>%
  ungroup

any(works_words$word == 'Romeo') # check previous name removal
any(works_words$word == 'Hamlet')
any(works_words$word == 'Ham')



# Remove stopwords --------------------------------------------------------

# me from python cltk module; em from http://earlymodernconversions.com/wp-content/uploads/2013/12/stopwords.txt
me_stops0 = read_lines('data/middle_english_stop_words')
sort(me_stops0)
me_stops = data_frame(word=str_conv(me_stops0, 'UTF8'),
                      lexicon = 'cltk')
em_stops0 = read_lines('data/early_modern_english_stop_words.txt')
sort(em_stops0)
em_stops = data_frame(word=str_conv(em_stops0, 'UTF8'),
                      lexicon = 'emc')

# misc that came up, esp. butchered names from tidytext
other_stops = data_frame(word= c('ile', 'ite', 'nch', 'nce', 'wne', 'lord',
                                 'haue', 'neuer', 'heere', 'nes', 'dinall',
                                 'rence', "i'th", 'ard', 'euer', 'goe', 'mio',
                                 'eance', 'les', 'med', 'gar', 'erset', 'rece',
                                 'frey', 'ntio', 'pyramus', 'ert', 'lingbrooke',
                                 'signior', 'llow', 'dut', 'ducats', 'uolio',
                                 'dost', 'unt', "ha's", 'inia', 'cus', 'hmond',
                                 'tress', 'sull', 'till', 'ries', 'phin', 'vse',
                                 'ena', 'bel', 'ntague', 'scicin', 'pri', 'ghter',
                                 'res', "o'th", 'tector', 'vous', 'oyces', 'yea',
                                 "a'th", 'fidius', 'istian', 'mas', 'ster',
                                 'tista', 'dsor', 'eks', 'nley', 'ards', 'tista',
                                 'ens', 'empts', 'empt', 'emp', 'padua', 'alas',
                                 'anor', 'lish', 'sby', 'morrow', 'cian', "is't",
                                 'shalt', 'rousillon', 'rge', 'refore', 'fulvia',
                                 'inst', 'don', 'ere', 'nckle', 'jan', 'scena',
                                 'alarum', 'alan', 'ces', 'ioles', 'cesario'))

# Fixes based on counts
e_words = c('againe', 'selfe', 'himselfe', 'herselfe', 'heare', 'soule',
            'highnesse', 'latine', 'beare', 'lesse', 'liues', 'finde', 'eate',
            'dye', 'looke', 'faire', 'speake', 'thinke', 'feare', 'deepe', 'meane',
            'poore', 'owne',  'foole',  'downe', 'royall', 'flye', 'empresse')
uv_words = c('loue', 'giue', 'leaue', 'heauen', 'liue', 'euery', 'knaue', 'euen')

# My impression is that if you do a lot of this at once, dplyr will have
# problems, and some things won't be done
works_words = works_words %>%
  mutate(word = if_else(word %in% e_words,
                        str_sub(word, end=nchar(word)-1),
                        word),
         word = if_else(word %in% uv_words,
                        str_replace(word, 'u', 'v'),
                        word),
         word = if_else(str_detect(word, "'s$|'d$|farre|houres|sonne|soules"),
                        str_sub(word, end=nchar(word)-2),
                        word)) %>%
  mutate(word = if_else(word=='teares', 'tears', word),
         word = if_else(word=='honour', 'honor', word),
         word = if_else(word=='maiestie', 'majesty', word),
         word = if_else(word=='soueraigne', 'sovereign', word),
         word = if_else(word=='souldier', 'soldier', word),
         word = if_else(word=='newes', 'news', word),
         word = if_else(word=='iustice', 'justice', word),
         word = if_else(word=='yeeld', 'yield', word),
         word = if_else(word=='emptie', 'empty', word),
         word = if_else(word=='empale', 'impale', word),
         word = if_else(word=='herine', 'heroine', word),
         word = if_else(word=='employd', 'employed', word),
         word = if_else(word=='yong', 'young', word),
         word = if_else(str_detect(word, "emperour|emperour's|emperours|emperors"), 'emperor', word),
         word = if_else(str_detect(word, "emperiall|emperialls"), 'imperial', word),
         word = if_else(str_detect(word, "empyrie|emperie"), 'empire', word)
  )

works_words = works_words %>%
  mutate(word = str_to_lower(str_trim(word))) %>%
  anti_join(me_stops) %>%
  anti_join(em_stops) %>%
  anti_join(stop_words) %>%
  anti_join(other_stops) %>%
  arrange(work, word)

works_words

any(works_words$word == 'tis') # check
# any(works_words$word == 'thou')
# any(works_words$word == 'ye')

# find issues
works_trim$Coriolanus %>% filter(str_detect(text, 'ioles'))


# Word count tests --------------------------------------------------------

# Here we'll find leftover stuff

term_counts = works_words %>%
  group_by(work, word) %>%
  count
term_counts %>%
  arrange(desc(n)) %>%
  filter(str_count(word)>2) %>%
  data.frame()

term_counts %>%
  arrange(desc(n)) %>%
  filter(str_count(word)>2) %>%
  data.frame() %>%
  filter(n<21)

# it looked like any two letter word was just an abbreviated stopword
test = term_counts %>%
  filter(str_count(word)>2)
test %>%
  arrange(desc(n)) %>%
  data.frame()

# shakes_dtm =  %>%
  cast_dfm(document=work, term=word, value=n)

