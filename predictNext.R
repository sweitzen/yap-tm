################################################################################
# predictNext.R
#
# Resources used in creating this code:
# https://rpubs.com/pferriere/dscapreport (for SBO understanding)

library(data.table)
library(doParallel)
library(quanteda)
library(stringr)


source("tokenizer.R")

################################################################################
# Given an input, this function predicts the top 5 most likely next words from
# a list of ngrams, and the prediction score calculated by Stupid Backoff (SBO)
# algorithm as described in:
# “Large language models in machine translation” by T. Brants et al, in
# EMNLP/CoNLL 2007 (http://www.aclweb.org/anthology/D07-1090.pdf)
# 
# If the input has no trailing space, predictNext takes the last word as a word
# fragment of the next word and uses the first N-1 words as the input; in each
# loop through ngrams, anyhits are filtered to those starting with the next-word
# fragment.
# Inputs:
#    input
#        a string from which predictNext will predict
#    ngrams
#        a list of data.tables of size Nmax containing 1-grams, 2-grams, ...,
#        Nmax-grams and their total counts observed in the input corpus. The 
#        columns are count, X (first n-1 terms of n-gram), and y (last word of 
#        n-gram), and has a key on (X, y)
# Output:
#        a data.table of the top 5 predictions of the next word, plus their SBO
#        scores
predictNext <- function(input, ngrams) {
    
    if (str_trim(input) == "") {
        return(data.table(score=0, y=""))
    }
    
    # Maximum size of Ngrams
    Nmax <- 5
    
    # Remove repeated whitespace inside input
    input <- str_replace_all(input, "\\s+", " ")
    
    # If the input ends with a space, the last word is complete
    # If it doesn't end with a space, the last word may not be complete
    if (str_sub(input, start= -1) == " ") {
        # Leave input as is, and set beginning fragment of next word to ""
        fragment <- ""
    } else {
        # Split beginning fragment of next word off of input
        fragment <- str_to_lower(word(input, -1))
        input <- paste(word(input, 1:(str_count(input, "\\S+") - 1)),
                       collapse=" ")
    }
    
    # Parse input text into sentences and take the last sentence
    sentences <- makeSentences(input)
    input <- sentences[length(sentences)]
    
    # Count words in input
    num_words <- str_count(input, "\\S+")
    
    # With N input words, max size Ngram used to predict is (N+1)
    Nmax <- min(num_words, Nmax-1) + 1
    
    # Truncate input to no more than last (Nmax-1) words of input
    input <- paste(word(input, -(Nmax-1):-1), collapse=" ")
    
    # lambda is a hyperparameter used in the Stupid Backoff algorithm
    # 0.4 is the value suggested by T. Brants et al.
    lambda <- 0.4
    
    # Initialize output
    hits <- NULL 
    
    # Loop over Ngram size, descending order: i
    for (i in Nmax:1) {
        # Set exponent
        exp <- Nmax - i
        
        if (i == 1) {
            # If i == 1, then there are no 0-grams, so baseCount will just be
            # the sum of all word counts (matching the next word starting 
            # fragment, if present), and hitsi will just be the top 5 most 
            # common words (matchingthe next word starting fragment, if present)
            if (fragment == "") {
                baseCount <- sum(ngrams[[1]]$count)
                hitsi <- ngrams[[1]][order(-count)][1:5]
            } else {
                mask <- grep(paste0("^", fragment), ngrams[[1]]$y)
                baseCount <- sum(ngrams[[1]][mask, count])
                hitsi <- ngrams[[1]][mask, ][order(-count)][1:5]
            }
            
            if (is.na(baseCount)) next
        } else {
            # Shorten input by one word off the front
            txt <- paste(word(input, -(i-1):-1), collapse="_")
    
            # Generate search values for X and y for (N-1)-gram
            if (i == 2) {
                X_search <- ""
                y_search <- input
            } else {
                X_search <- paste(word(input, -(i-1):-2), collapse="_")
                y_search <- word(input, -1)
            }
            
            # Input (N-1)-gram count
            # NOTE: ngrams should have key (X, y)
            baseCount <- ngrams[[i-1]][.(X=X_search, y=y_search)]$count
            
            # If input (N-1)-gram count wasn't found, skip to next iteration
            if (is.na(baseCount)) next
            
            # Subset ngrams[[i]] where X matches txt
            hitsi <- ngrams[[i]][.(X=txt)]
            
            # Filter hitsi for rows that begin with fragment, if present
            if (fragment != "") {
                mask <- grep(paste0("^", fragment), hitsi$y)
                hitsi <- hitsi[mask, ]
            }
        }
        
        # If no hits were found, skip to next iteration
        if (nrow(hitsi) == 0) next
        
        # This is where Stupid Backoff (SBO) scoring is implemented
        # This simple explanation of SBO is courtesy of Phil Ferriere:
        # https://rpubs.com/pferriere/dscapreport
        #
        # if (candidateIs5gram) {
        #     score = matched5gramCount / input4gramCount
        # } else if (candidateIs4gram) {
        #     score = 0.4 * matched4gramCount / input3gramCount
        # } else if (candidateIs3gram) {
        #     score = 0.4 * 0.4 * matched3gramCount / input2gramCount
        # } else if (candidateIs2gram) {
        #     score = 0.4 * 0.4 * 0.4 * matched2gramcount / input1gramCount
        # }
        hitsi <- hitsi[, ':=' (
            baseCount=baseCount,
            exp=exp,
            score=(lambda^exp * count / baseCount),
            N=i
        ), by=y][order(-count)]
        
        # Remove hits already found
        hitsi <- hitsi[!(y %in% hits$y)]
        
        # Append hitsi to hits
        hits <- rbind(hits, hitsi[1:5][!is.na(y)])
        
        # Quit if we already have the required 5 predictions
        if (nrow(hits) >= 5) break
    }
    
    return(hits[1:5, c("score", "y")])
}