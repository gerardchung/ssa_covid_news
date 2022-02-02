# Annual reports SSA
## This do file cleans news on SSA pre-COVID19


rm(list = ls())

#install.packages("tidyverse")
#install.packages("quanteda")
#install.packages("janitor")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("textreadr")
#install.packages("pacman")
#install.packages("tidytext")
#install.packages("htmlwidgets")
#install.packages("dplyr")

# LOAD PACKAGES ####
pacman::p_load(tidyverse, dplyr, janitor, quanteda, stringr, ggplot2, tidytext)

# LOAD DATA ####
getwd()
load( file = "cr_data/news_precovid_extracted.RData") 

data <- news_precovid_ssa
rm(news_precovid_ssa)


# LOOK FOR COVID PHRASES AND REPLACE ####
str_view_all(data$text, regex(pattern = "^covid*", ignore_case = T), match = T ) 
    # should not have any matches because there is pr-COVID during this time period

# REMOVE ARTICLES THAT ARE ABOUT PEOPLE WHO ARE DOING COMMUNITY SERVICE HOURS ####
str_view_all(data$text, regex(pattern = "hours of community service", ignore_case = T), match = T ) 
str_view_all(data$text, regex(pattern = "community service hours", ignore_case = T), match = T ) 

comm_hours <- str_extract_all(data$text, regex(pattern = "hours of community service|community service hours", ignore_case = T), simplify = T) 

unique(comm_hours)
tabyl(as_factor(comm_hours)) # 28 will be removed

rm(comm_hours)

data <- 
    data %>% 
    mutate(comm_service_hours = str_detect(text, regex(pattern = "hours of community service", 
                                                       ignore_case = T) )) %>% 
    filter(comm_service_hours != T)

# REMOVE ARTICLES THAT ARE ABOUT PEOPLE DOING COMMUNITY SERVICE PROJECTS ####
str_view_all(data$text, regex(pattern = "community service project", ignore_case = T), match = T ) 

comm_srv_proj <- str_extract_all(data$text, regex(pattern = "community service project", ignore_case = T), simplify = T) 

unique(comm_srv_proj)
tabyl(as_factor(comm_srv_proj)) # 28 will be removed

rm(comm_srv_proj)

data <- 
    data %>% 
    mutate(csp = str_detect(text, regex(pattern = "community service project", 
                                                       ignore_case = T) )) %>% 
    filter(csp != T)

# Remove these articles about a group of people that were moderators of an event ####
str_view_all(data$text, regex(pattern = "wei leong", ignore_case = T), match = T ) 
data <- 
    data %>% 
    mutate(weileong_paulin_lien = str_detect(text, regex(pattern = "wei leong|paulin straughan", 
                                                       ignore_case = T) )) %>% 
    filter(weileong_paulin_lien != T)



# CHECK DATASET FOR # ARTICLES WITH KEY PHRASES ####
glimpse(data)


str_view_all(data$text, 
             regex(pattern = "social service|community service|voluntary welfare", ignore_case =  T), 
             match = T)

## Create key phrases =====


#data$ssa <- str_detect(data$text, 
#                       regex(pattern = "social service|community service|voluntary welfare|social #work|counsellor|counselling", ignore_case =  T))

data$ssa <- str_detect(data$text, 
                       regex(pattern = "social service|community service|voluntary welfare", 
                             ignore_case =  T))

tabyl(data$ssa)

# REDUCE TO THE SAMPLE OF ARTICLES I WANT ####
data_ssa <- 
    data %>% 
    filter(ssa == T) %>% 
    filter(id1 != 52) 
        # this article is NDP awards -> later in sentiment analysis produces v high ratings; not relevant 

unique(data_ssa$id1)
NROW(data_ssa) # 325

# TOKEN TO SENTENCES ####
corp <- corpus(data_ssa,
               text_field = "text",
               docid_field = "id1")


summary(summary(corp))

## Tokenize to sentences ====
tok <- 
    corp %>% 
    tokens(what = "sentence")    # I can also use unnest in tidytext to tokenize to sentence
tok[1]

dfm <- dfm(tok)

## Detect setences with key phrases ====
### Convert to tidy format - easier!
tidy1 <- tidy(dfm) %>%  select(-count)

tidy1$document <- as.integer(tidy1$document)
glimpse(tidy1)

ssa_keyphrase = "social service|community service|voluntary welfare|counseling centre|counselling centre|senior activity centre|family service|crisis shelter|\\bshelter\\b|\\bshelters\\b"

# Since the articles are also related to SSAs, it is to search for agencies and organiations


#ssa_keyphrase = "social service|community service|voluntary welfare|organization|organisation|social sector|non-profit|counseling #centre|counselling centre|senior activity centre|prevention centre|family service|good shepherd centre|crisis #shelter|\\bshelter\\b|\\bshelters\\b|the centre|ppis centre"
#    # Since the articles are also related to SSAs, it is to search for agencies and organiations

tidy1 <-
    tidy1 %>% 
    mutate(ssa = str_detect(term, 
                            regex(pattern = ssa_keyphrase, 
                                  ignore_case = T)))

tabyl(tidy1$ssa)

tidy1$centre <- str_detect(tidy1$term, regex(pattern = "centre", ignore_case = T))
tidy1$shelter <- str_detect(tidy1$term, regex(pattern = "\\bshelter\\b|\\bshelters\\b", ignore_case = T))

# CLEAN DATASET ####
tidy1 <- 
    tidy1 %>% 
    filter(ssa == T) %>% 
    select(-centre, -shelter, - ssa)


NROW(tidy1) # 547 

# REMOVE DUPLICATES IN SENTENCES ####
## this is possible because some the same sentence (e.g., quotes) appear again the article for e.g. photo and tex
## Sometime the articles are published on consecutive days maybe one comes on first online

#dup <- news_covidsg %>% duplicated()
dup <- tidy1[duplicated(tidy1$term), ] 
NROW(dup) # 0 duplicates in title

tidy1 <- tidy1[!duplicated(tidy1$term), ] # 547-0 = 1446 becos no duplicates
NROW(tidy1) # 547 sentences



# Sentiment analysis ####
# https://www.tidytextmining.com/sentiment.html
# https://m-clark.github.io/text-analysis-with-R/sentiment-analysis.html#sentiment-analysis-examples
get_sentiments("nrc")

#tidy1_tok <-
#    tidy1 %>% 
#    mutate(id_sentence = row_number()) %>% # id created for each sentence 
#    unnest_tokens(word, term)

## unnest into tokens but keep the sentence columns ===
tidy1_tok <-
    tidy1 %>% 
    mutate(id_sentence = row_number()) %>%  # id created for each sentence 
    unnest_tokens(
        output = word,
        input = term,
        token = 'words',
        drop = FALSE # This drop keep the "term" column that contains the sentences
    ) %>%
    ungroup()
# unnest into tokens but keep the sentence columns

## inner join the sentiments (afinn dictionary approach) ====
tidy1_sentiment_afinn = 
    tidy1_tok %>% 
    left_join(get_sentiments("afinn")) %>% 
    mutate(value = if_else(is.na(value) , 0, value)) %>% 
    group_by(document, term, id_sentence ) %>% 
    summarise(valence = sum(value)) %>%
    ungroup() %>% 
    mutate(total_words = str_count(term, pattern = "\\w+")) # calculate # of words in each sentence

tidy1_sentiment_afinn <- 
    tidy1_sentiment_afinn %>% 
    mutate(valence_std = valence/total_words) 
    # valence variable does not consider # of words in the sentence
    # valence_std standardized by # of words in the sentence

tabyl(tidy1_sentiment_afinn$valence)

tidy1_sentiment_afinn %>% 
    ggplot(aes(x = valence)) +
    geom_bar()

tidy1_sentiment_afinn %>% 
    ggplot(aes(x = valence_std)) +
    geom_density()


## inner join the sentiments (bing dictionary approach) ====
tidy1_sentiment_bing = 
    tidy1_tok %>% 
    left_join(get_sentiments("bing")) 

### label the sentiments 
tidy1_sentiment_bing <-
    tidy1_sentiment_bing %>% 
    mutate(sentiment = if_else(is.na(sentiment), "neutral", sentiment)) 
# those with no sentiments are neutral

unique(tidy1_sentiment_bing$sentiment)

### Code values 
tidy1_sentiment_bing <-
    tidy1_sentiment_bing %>% 
    mutate(value = case_when(sentiment == "positive" ~ 1,
                             sentiment == "negative" ~ -1,
                             sentiment == "neutral" ~ 0)) 

tabyl(tidy1_sentiment_bing$value)

### Calculate valence 
tidy1_sentiment_bing <-
    tidy1_sentiment_bing %>% 
    group_by(document, term, id_sentence ) %>% 
    summarize(valence = sum(value)) %>% 
    ungroup() %>% 
    mutate(total_words = str_count(term, pattern = "\\w+"))

tidy1_sentiment_bing <- 
    tidy1_sentiment_bing %>% 
    mutate(valence_std = valence/total_words) 
    # valence variable does not consider # of words in the sentence
    # valence_std standardized by # of words in the sentence

tabyl(tidy1_sentiment_bing$valence)
mean(tidy1_sentiment_bing$valence_std)

tidy1_sentiment_bing %>% 
    ggplot(aes(x = valence)) +
    geom_bar()

tidy1_sentiment_bing %>% 
    ggplot(aes(x = valence_std)) 
    geom_density()


## inner join the sentiments (NRC dictionary approach) ====
tidy1_sentiment_nrc <-
    tidy1_tok %>% 
    left_join(get_sentiments("nrc")) %>% # keeps all the sentiments and NAs
    filter(sentiment %in% c("positive", "negative", NA)) 

### label the sentiments 
tidy1_sentiment_nrc <-
    tidy1_sentiment_nrc %>% 
    mutate(sentiment = if_else(is.na(sentiment), "neutral", sentiment)) 
# those with no sentiments are neutral

unique(tidy1_sentiment_nrc$sentiment)

### Code values 
tidy1_sentiment_nrc <-
    tidy1_sentiment_nrc %>% 
    mutate(value = case_when(sentiment == "positive" ~ 1,
                             sentiment == "negative" ~ -1,
                             sentiment == "neutral" ~ 0)) 

tabyl(tidy1_sentiment_nrc$value)

### Calculate valence 
tidy1_sentiment_nrc <-
    tidy1_sentiment_nrc %>% 
    group_by(document, term, id_sentence ) %>% 
    summarize(valence = sum(value)) %>% 
    ungroup() %>% 
    mutate(total_words = str_count(term, pattern = "\\w+"))

tidy1_sentiment_nrc <- 
    tidy1_sentiment_nrc %>% 
    mutate(valence_std = valence/total_words) 
# valence variable does not consider # of words in the sentence
# valence_std standardized by # of words in the sentence

tidy1_sentiment_nrc %>% 
    ggplot(aes(x = valence)) +
    geom_bar()

tidy1_sentiment_nrc %>% 
    ggplot(aes(x = valence_std)) +
    geom_density()

tabyl(tidy1_sentiment_afinn$valence)
mean(tidy1_sentiment_afinn$valence_std)

tabyl(tidy1_sentiment_nrc$valence)
mean(tidy1_sentiment_nrc$valence_std)

tabyl(tidy1_sentiment_bing$valence)
mean(tidy1_sentiment_bing$valence)



# CREATE VARIABLE FOR COMPARISON WITH ARTICLES IN COVID ####

tidy1_val_PRECOVID <- tidy1_sentiment_nrc
#tidy1_val_PRECOVID <- tidy1_sentiment_bing
#tidy1_val_PRECOVID <- tidy1_sentiment_afinn

rm(tidy1_sentiment_afinn, tidy1_sentiment_bing, tidy1_sentiment_nrc)

tidy1_val_PRECOVID <- 
    tidy1_val_PRECOVID %>% 
    mutate(covid = 0)

# SAVE #####
    ## document corresponds to id1
getwd()
save(tidy1_val_PRECOVID, file =  "an_data/news_precovid_valence" )



# SEANCE SENTIMENT ANALYSIS ####

## Prep data for conversion in SEANCE ======
tidy1_val_PRECOVID_seance <- 
    tidy1_val_PRECOVID %>% 
    mutate(id_seance = row_number()) %>% # this is an id for seance and will be use for merging bk
    select(id_seance, term, document, id_sentence)

## Save as txt files ====
    ## This will save each row in the dataset to an individual txt file 
    ## because seance can only analyze using text files for each text (in this case, sentence)
    ## paste0("an_data/seance/duringcovid/  --> the directory to save 
    ## tidy1_val_COVID_seance$id_seance[i], ".txt"  --> name of file + .txt

#getwd()
#for (i in 1:nrow(tidy1_val_PRECOVID_seance)) {
#    write(tidy1_val_PRECOVID_seance$term[i], 
#          paste0("an_data/seance/precovid/",
#                 tidy1_val_PRECOVID_seance$id_seance[i], ".txt"))
#}
    # https://stackoverflow.com/questions/34970128/exporting-data-frame-columns-into-separate-txt-files
## After seance finished, merge back using id_seance ======
## seance will read in each txt file and do the analysis. 
## Results will be all read into one csv file in a folder i specified
## in this case, it is in "an_data/seance/output/"

seance_precovid <- read_csv(file = "an_data/seance/output/results_precovid.csv")

seance_precovid$id_seance <- str_extract_all(seance_precovid$filename ,
                                                pattern = "[\\d]+", simplify = T)

seance_precovid$id_seance <- as.integer(seance_precovid$id_seance)

seance_precovid <- 
    seance_precovid %>% 
    relocate(id_seance, .before = filename) 

## Check for duplicates ====
unique(seance_precovid$id_seance)
    # very strange! there are duplicates

dup <- seance_precovid[duplicated(seance_precovid$id_seance), ] 
NROW(dup) # 1 duplicates in id

seance_precovid <- seance_precovid[!duplicated(seance_precovid$id_seance), ] # 548-1 = 547
NROW(seance_precovid)

## Merge
seance_precovid <- 
    seance_precovid %>% 
    select(-filename) %>% 
    right_join(tidy1_val_PRECOVID_seance) %>% 
    mutate(covid = 0) %>% 
    relocate(document, term, id_sentence, id_seance, covid, .before =nwords) 

glimpse(seance_precovid)

save(seance_precovid, file = "an_data/seance_precovid")



