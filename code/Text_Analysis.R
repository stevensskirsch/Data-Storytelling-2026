# Patrick Chester
# Lecture: Text as Data
# Date: 3/29/2017
# Preprocessing, Weighting, and Naive Bayes
rm(list = ls())

# Load Quanteda

# Installs the latest version of Kenneth Benoit's quanteda

#install.packages("quanteda")
#install.packages("quanteda.textmodels")
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")

library(quanteda)
library(quanteda.textmodels)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)

## Part 1: Running basic text analysis

# start with a short character vector
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

# Let's tokenize (break vector into individual words)
tokens <- tokens(sampletxt)
?tokens

tokens <- tokens(sampletxt) |>
  tokens_remove(pattern = "[[:punct:]]") # removes punctuation

# Stemming examples

stems <- tokens_wordstem(tokens)
?tokens_wordstem

# Loading State of the Union corpus

data("data_corpus_sotu", package = "quanteda.corpora")

?data

# ndoc identifies the number of documents in a corpus


ndocs <- ndoc(data_corpus_sotu)

# Here, we identifiy the text of the last SOTU Speech in the corpus
# Note: a corpus is a quanteda list object that includes both docvars and text

last_speech_text <- data_corpus_sotu[ndocs]

# The DFM function creates a Document Feature Matrix from the last SOTU speech

biden_dfm <- dfm(tokens(last_speech_text))
head(biden_dfm)
?dfm

# Inspecting the components of a DFM object

str(biden_dfm)
docvars(biden_dfm)

biden_dfm[1,1:20]

# The topfeatures function by default shows the most frequently occuring terms in a given DFM

topfeatures(biden_dfm)

# Are all of these features relevant?

# Stopwords

# Stopwords are commonly used words that add little understanding to the content of the document by themselves

# The stopwords function takes a language as an input and produces a vector of stopwords compiled from that language

c(stopwords("english"), "will")

# Fun fact: Quanteda also supports stopwords for english, SMART, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

# Here we compare a DFM from the last SOTU while without English stopwords with one that has them

biden_dfm1 <- last_speech_text |>
  tokens(remove_punct = TRUE) |>
  dfm()

biden_dfm2 <- last_speech_text |>
  tokens(remove_punct = TRUE) |>
  dfm() |>
  dfm_remove(pattern = stopwords("english"))



topfeatures(biden_dfm1)
topfeatures(biden_dfm2)

## 3 Visualization and Weighted DFM

# Now we will create a DFM of all the SOTU speeches

full_dfm <- data_corpus_sotu |>
  tokens(remove_punct = TRUE) |>
  dfm() |>
  dfm_remove(pattern = stopwords("english"))

docvars(data_corpus_sotu)
topfeatures(full_dfm)

# Visualizing the text contained within the data frame
textplot_wordcloud(full_dfm)

# You can limit the number of characters to make the visualization cleaner


textplot_wordcloud(full_dfm, max_words = 100)


## You can also create comparisons between documents 
dfmat2 <- corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump")) |> # subsets a corpus based on values of 
  corpus_group(groups = President) |> # groups variables by some variable in docvars(corpus)
  tokens(remove_punct = TRUE) |>
                        dfm() |>
  dfm_remove(pattern = stopwords("english")) %>%
    dfm_trim(min_termfreq = 3)

textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 300,
                   color = c("blue", "red"))


## Exercise

# 1. Subset SOTU data to include only Republican and Democratic speeches
# 2. Group SOTU by party
# 3. Remove punctuation 
# 4. Convert to a dfm 
# 5. Remove stopwords 
# 6. Trim dfm to only include terms that occur 5 or more times
# 7. Create a textplot_wordcloud with max_words = 200, that assigns different colors to democrats and republicans


##  Collocations

# bigrams
textstat_collocations(last_speech_text)
?collocations

# trigrams

textstat_collocations(last_speech_text, size=3)

# detect collocations in overall

colloc <- textstat_collocations(last_speech_text)

str(colloc)
# remove any collocations containing a word in the stoplist

library(dplyr)

col <- colloc |>
  filter(!grepl(colloc$collocation, pattern = paste(stopwords("english"), collapse ="|")))

# Are there any other terms you all think are interesting?


## 5 Regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index <- grep(" s ", data_corpus_sotu)
?grep

# this returns every speech that contains " s "
s_texts <- grep(" s ", data_corpus_sotu, value=TRUE)

# Here we create a vector of documents with " s " removed
no_s <- gsub(" s ", "",  data_corpus_sotu[s_index])

## Exercise

# 1. Subset the data_corpus_inaugural corpus based on a keyword of interest to you using regular expressions
# 2. Assess what two word collocations exist in that data, excluding combinations that include stopwords


## 1) Supervised Learning: Naive Bayes


trainingset <- matrix(0,ncol=6,nrow=5)
trainingset[1,] <- c(1, 2, 0, 0, 0, 0)
trainingset[2,] <- c(0, 2, 0, 0, 1, 0)
trainingset[3,] <- c(0, 1, 0, 1, 0, 0)
trainingset[4,] <- c(0, 1, 1, 0, 0, 1)
trainingset[5,] <- c(0, 3, 1, 0, 0, 1)
colnames(trainingset) <- c("Beijing", "Chinese",  "Japan", "Macao", "Shanghai", "Tokyo")
rownames(trainingset) <- paste("d", 1:5, sep="")
trainingset <- as.dfm(trainingset)
trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered=TRUE)


# replicate IIR p261 prediction for test set (document 5)

nb.p261 <- textmodel_nb(x=trainingset, y=trainingclass,
                        smooth=1, prior="docfreq") # Smooth gives values of 1 for new words; NB wouldn't work very well


pr.p261 <- predict(nb.p261)
pr.p261


# 2 Classification using Naive Bayes: Manifestos


## Applying Naive Bayes and Word Scores to Amicus texts from Evans et al

# Loading data


data(data_corpus_amicus)

head(data_corpus_amicus)

# 2) Fitting NB model: with stemming, tolower, and no stopwords

amDfm <- data_corpus_amicus |>
  tokens() |>
  dfm() 

amNBmodel <- textmodel_nb(amDfm, docvars(data_corpus_amicus, "trainclass"), smooth=1) 

print(amNBmodel, 10)

amNBpredict <- predict(amNBmodel)

# "confusion matrix": NB
tab_NB <- table(amNBpredict, docvars(data_corpus_amicus, "testclass"))

# Accuracy: NB
sum(diag(tab_NB))/sum(tab_NB)

# 3) Fitting NB model: with stemming, tolower, no stopwords, and tfidf weighting
amDfm <- data_corpus_amicus |>
  tokens() |>
  dfm() |>
  dfm_tfidf()


amNBmodel <- textmodel_nb(amDfm, docvars(data_corpus_amicus, "trainclass"), smooth=1) 
print(amNBmodel, 10)
amNBpredict <- predict(amNBmodel)

# "confusion matrix": NB
tab_NB <- table(amNBpredict, docvars(data_corpus_amicus, "testclass"))

# Accuracy: NB
sum(diag(tab_NB))/sum(tab_NB)

## Exercise 3

# 1. Try preprocessing the data_corpus_amicus by removing stopwords. How does this change the model accuracy?
# 2. Now stem words using dfm_wordstem. How does this change the model accuracy?
# 3. Combine all preprocessing methods. Which approach made the biggest difference?
