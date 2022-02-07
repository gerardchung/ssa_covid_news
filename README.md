---
output:
  pdf_document: default
  html_document: default
---
# News Media Representation of Social Service Agencies During COVID-19 pandemic in Singapore :singapore:

**README WORK IN PROGRESS**

## Data-Search
I searched for news articles in __Nexis Uni__
 
Search date was on 24th Nov 2021 

A total of 1,435 articles were downloaded in .docx format. 

**Search parameters**

 (1) Date of articles: after 31st Jan 2020 because 1st Feb 2020 is the first SG case on COVID-19

 (2) Language: English
 
 (3) Location: Singapore
 
 (4) Sources: Three news media sources in Singapore:
 
  * [Channel NewsAsia](https://www.channelnewsasia.com/news/singapore)
        
  * [Today (Singapore)](https://www.todayonline.com/)
        
  * [The Straits Times (Singapore)](https://www.straitstimes.com/global)
        
 (5) Search string: "(social service*) OR (voluntary welfare) OR (community service*)"
 
 (6) Search fields: “All fields” (i.e., including title, leading section, body etc)
 
**Results of search**: 1,435 articles across almost three years period

***Note: Nexis Uni's IP policies do not allow me to share the data***. Nonetheless, the raw text files can be downloaded with a subscription to Nexis and using the above search parameters.

## Data-extraction
see codes in cr_do

## Building the corpus
The online database of newspapers in Nexis Uni was used to search for any print and online articles based on the following inclusion criteria: (1) published up to one year before 1st February 2020 (date of the first article published about COVID-19 in Singapore) and up to December 2021 (a period of three years from 2019 to 2021), (2) contained key phrases such as “social service*”, “community service*”, or “voluntary welfare” in the title or anywhere in the article, (3) Singapore-based news (i.e., news of COVID-19 not about Singapore are excluded), and (4) published in the English language. 

The initial search gave 1,435 articles. Based on the inclusion criteria and the removal of duplicates, the total number of articles was reduced to 1,095 (325 articles in the pre-COVID-19 period defined as one year before 1st Feb 2020; 770 articles in the COVID-19 period up to December 2021). Text from each article was also tokenized at sentence-level and only those sentences that contained at least one of the key phrases were extracted. This produced a corpus comprising of 1,993 sentences out of the set of 1,095 news articles. 
 

## Preprocessing the text
The text was preprocessed before executing any analysis. Text preprocessing is important in preparing the textual data for any computations[^1]. I removed punctuation and numbers, converted all letters to lower case, and stemmed words by removing all prefixes and suffixes to reduce all the words to their root words (e.g., “talking” & “talks” become “talk”; addiction, addicted, & addicts become addict). Stop words (e.g., “and,” “the,” and “is”) were also removed from the corpus because they do not contribute to words’ contextual meanings or the identification of topics. 

In a “bag of words” approach in text-as-data analysis, words are analyzed as individual units, and their relationships to sentiments or to documents are examined. However, text-as-data analyses can also be based on the relationships between words, whether examining which words tend to follow others immediately, or that tend to co-occur within the same documents. N-grams is a technique in NLP that can be used to investigate these neighbouring sequences of words in a document or sentence. N-grams are contiguous sequences of n items from a given sample of text. In this study, I used up to 3-grams allowing the analysis to capture up to a window of 3 words in a sequence (e.g., circuit_breaker, senior_activity_centres; national_help_hotline). 

Tokenization is another technique in NLP where words or text features are represented as numbers so that quantitative computations can be done. Depending on the specific analysis, the raw text in this study is either tokenized as words/N-grams or as sentences. Parts of speech (POS) using the Universal tagset for POS in the SpacyR package in R was also used to tag words to whether it is an adjective or a verb. 

Additional preprocessing was done depending on the unique characteristics of the corpus. For instance, I converted all variations of COVID-19-related phrases (e.g., COVID, COVID19, coronavirus) to COVID19 for standardization. For certain analyses, the phrases “social service agencies”, “voluntary welfare organizations, and “community service agencies” were removed from the corpus because these phrases described a shared context for all the articles and thus would not contribute to answering the research questions. 

All text pre-processing and analyses were done in the R statistical software using the quanteda, spacyr, and tidytext packages. Visualizations were done with the ggplot package. Data cannot be publicly shared because of the intellectual property restrictions stipulated by Nexis Uni. All codes and detailed descriptions on downloading the data from Nexis Uni are provided at https://github.com/gerardchung/ssa_covid_news.

## Footnotes
[^1]: See discussion on text preprocessing in Chung, G., Rodriguez, M., Lanier, P., & Gibbs, D. (2021, May 11). Text-mining open-ended survey responses using structural topic modeling: A practical demonstration to understand parents’ coping methods during COVID-19 pandemic in Singapore. https://doi.org/10.31219/osf.io/enzst![image](https://user-images.githubusercontent.com/65118803/152710429-906f1cd8-b285-4e1c-954c-33803076a8ec.png)



