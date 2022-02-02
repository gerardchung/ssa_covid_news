# Annual reports SSA
## This do file extract news on SSA COVID19


#renv::init()
rm(list = ls())


# LOAD PACKAGES ####
pacman::p_load(textreadr,stringr,htmlwidgets, dplyr, janitor) 


# EXTRACT FILE NAMES ####
getwd()
file.list = list.files("source_data/news_ssa_covid/source_data_news_covid", recursive = T)

file.list[1:10]

## Remove file names with _doclist.docx ====
str_view_all(file.list, regex(pattern = "_doclist.docx", ignore_case = T))

## need ‘htmltools’ and "htmlwidgets" 
n <- str_detect(file.list, regex(pattern = "_doclist.docx", ignore_case = T))
sum(n) # 11 because 11 folders with this file name
rm(n)

## remove _doclist ====
file.list <- str_subset(file.list, regex(pattern = "_doclist.docx", ignore_case = T), negate = T) 

n <- str_detect(file.list, regex(pattern = "_doclist.docx", ignore_case = T))
sum(n) # 0 because all removed
rm(n) 

# FOLDER NUMBER ####
str_view_all(file.list[1:2], regex(pattern = "^\\d{1,5}"))
foldernum <- str_extract(file.list, regex(pattern = "^\\d{1,5}"))
unique(foldernum)

# CREATE FULL FILES NAMES ####
file.list[1:3]
file.list2 <- paste("source_data/news_ssa_covid/source_data_news_covid", file.list, sep = "/")
file.list2[1:3]



# EXTRACT INFORMATION ####


## trial with one document ====

trial = read_docx(file.list2[1])
trial
trial = read_docx(file.list2[3])
trial
file.list2[2]
file.list2[3]


# trial = read_docx(file.list2[1420])
str_detect(trial, pattern = "Body")

##  text body ====
# see that the body of news starts at line after "Body: and end at line before "Classification"
start.text = which(trial == "Body") + 1

end.text   = which(trial == "Classification") - 1

text = paste(trial[start.text:end.text], collapse = "\n")
#text = ifelse(length(start.text) != 0,
#              paste(trial[start.text:end.text], collapse = "\n"), NA)

cat(text, "\n")

# Next, let's grab each of the options that has an explicit tag. ====

(section = gsub("Section:","",trial[grepl("Section:",trial,fixed=T)] ,fixed=T))
# grepl() will find in trial the pattern = "Section" and return a logical vector.
# If true, then trial[ ] will take out that entire string value
# gsub will remove remove from that string value "Section:" and replace with "" (blank)
(section = str_replace(trial[str_detect(trial, "Section:")], pattern = "Section:", replacement =""))
# Need to first detect because the trial is in a vector
(words = gsub("Length:","",trial[grepl("Length:",trial,fixed=T)] ,fixed=T))
(language = gsub("Language:","",trial[grepl("Language:",trial,fixed=T)],fixed=T))

(type = str_replace(trial[str_detect(trial, "Publication-Type:")], pattern = "Publication-Type:", replacement =""))
# trial[1] has a section on "Publication-Type:" 
# trial[3] does not have. So if just run codes withOUT ifelse, the type vector will have no data.
# This will become a problem in the loop later (the loop will stop because entering NO data into the dataframe)
# This ifelse code => if length is > 0, then ran the gsub. Else, input NA 
(type = ifelse(length(trial[grepl("Publication-Type:",trial,fixed=T)]) >0,
               gsub("Publication-Type:","",trial[grepl("Publication-Type:",trial,fixed=T)],fixed=T),
               NA))
(subject = gsub("Subject:","",trial[grepl("Subject:",trial,fixed=T)],fixed=T))
(industry = gsub("Industry:","",trial[grepl("Industry:",trial,fixed=T)],fixed=T))
(geographic = gsub("Geographic:","",trial[grepl("Geographic:",trial,fixed=T)],fixed=T))
# (load.date = gsub("Load-Date:","",trial[grepl("Load-Date:",trial,fixed=T)],fixed=T))

# These below are relational -> they should be the same position in every docu
(title = trial[1])
(source = trial[2])
(pub.date = trial[3])

class(file.list2)

# In the loop below, i fonud the following with problesms; so they are removed
## 1202 has no main text -> thus, removed
#file.list2[1911]
#
#length(file.list2)
#
#file.list2 <- str_subset(file.list2, 
#                         pattern = "source_data/20/106 new Covid-19 cases in Singapore, 39 linked to known #clusters at foreign worker dormitories.docx",
#                         negate = T) 

file.list2[625]

# Step 1 here is to create the empty data frame.

news <- data.frame( title = rep(NA, length(file.list2)),
                    source = rep(NA, length(file.list2)),
                    pub.date = rep(NA, length(file.list2)),
                    section = rep(NA, length(file.list2)),
                    words = rep(NA, length(file.list2)),
                    language = rep(NA,length(file.list2)),
                    type = rep(NA,length(file.list2)),
                    subject = rep(NA,length(file.list2)),
                    industry = rep(NA,length(file.list2)),
                    geographic = rep(NA,length(file.list2)),
                    text = rep(NA,length(file.list2)),
                    stringsAsFactors = F
)


# Step 2 is to create the loop by copying down the code we know extracts what we want and has it input it into our data frame.
for(i in 1:length(file.list2)) {
    print(paste("Working on document", i, "of", length(file.list2)))
    temp.doc = read_docx(file.list2[i])
    
    news$title[i] = temp.doc[1]
    news$source[i] = temp.doc[2]
    news$pub.date[i] = temp.doc[3]
    
    #news$section[i] = gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[8]
    news$section[i] = ifelse(length(temp.doc[grepl("Section:",temp.doc,fixed=T)]) >0,
                             gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T),
                             NA)
    news$words[i] = gsub("Length:","",temp.doc[grepl("Length:",temp.doc,fixed=T)] ,fixed=T)
    news$language[i] = gsub("Language:","",temp.doc[grepl("Language:",temp.doc,fixed=T)] ,fixed=T)
    
    # news$type[i] = gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[3]    
    news$type[i] = ifelse(length(temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)]) >0, 
                          gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T),
                          NA)
    
    #news$subject[i] = gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T)
    news$subject[i] = ifelse(length(temp.doc[grepl("Subject:",temp.doc,fixed=T)] >0),
                             gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T),
                             NA)
    
    #news$industry[i] = gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T)
    # at least one article 4 does not have industry
    news$industry[i] = ifelse(length(temp.doc[grepl("Industry:",temp.doc,fixed=T)] >0),
                              gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T),
                              NA)
    
    # news$geographic[i] = gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T)
    news$geographic[i] = ifelse(length(temp.doc[grepl("Geographic:",temp.doc,fixed=T)] >0),
                                gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    
    start.text = which(temp.doc == "Body") + 1
    end.text   = which(temp.doc == "Classification") - 1
    
    news$text[i] = paste(temp.doc[start.text:end.text], collapse = "\n")
}



#save(news, foldernum, file = "cr_data/news.RData") 
#rm(list = ls())
#load(file = "cr_data/news.RData")

#######
news_covid_ssa <- news
rm(news)
# Add in the vars that denote folder number
news_covid_ssa$foldernum <- foldernum

glimpse(news_covid_ssa)

news_covid_ssa <- 
    news_covid_ssa %>% 
    relocate(foldernum, .before = title )
news_covid_ssa$foldernum <- as.numeric(news_covid_ssa$foldernum)


# Duplicates 

#dup <- news_covidsg %>% duplicated()
dup <- news_covid_ssa[duplicated(news_covid_ssa$title), ] 
NROW(dup) # 195 duplicates in title

news_covid_ssa <- news_covid_ssa[!duplicated(news_covid_ssa$title), ] # 1011 - 195 = 816 

NROW(news_covid_ssa)

# Create identifier id1 
news_covid_ssa <- 
    news_covid_ssa %>% 
    mutate(id1 = row_number())

news_covid_ssa <- news_covid_ssa %>% 
    relocate(id1, .after = foldernum )

glimpse(news_covid_ssa)
tabyl(news_covid_ssa$id1)

NROW(news_covid_ssa)


# save as Rdata file
# ====================
getwd()
save(news_covid_ssa, file = "cr_data/news_covid_extracted.RData") 
