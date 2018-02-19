################################################################################
# predictNext.R
#
# Resources used in creating this code:
# https://rpubs.com/pferriere/dscapreport (for SBO understanding)
# TODO: modify predictNext to detect trailing space. If found, then we have a
# input ngram from which to predict. If no space, assume next word is still
# being typed. Truncate after last space, and save word fragment. Filter hits
# in each loop for entries starting with word fragment. Fewer than 5 predictions
# may be returned.

library(data.table)
library(doParallel)
library(quanteda)


source("tokenizer.R")

################################################################################
# Given an input, this function predicts the top 5 most likely next words from
# a list of ngrams, and the prediction score calculated by Stupid Backoff (SBO) 
# algorithm as described in:
# “Large language models in machine translation” by T. Brants et al, in 
# EMNLP/CoNLL 2007 (http://www.aclweb.org/anthology/D07-1090.pdf)
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
    
    if (trimws(input) == "") {
        return(data.table(score=0, y=""))
    }
    
    # Maximum size of Ngrams
    Nmax <- 5
    
    # Parse input text into sentences and take the last
    sentences <- makeSentences(input)
    input <- sentences[length(sentences)]
    
    # Count words in input
    # TODO: This may not be robust
    num_words <- sapply(gregexpr("\\s+", input), length) + 1
    
    # Format input with underscores instead of spaces, to match format of 
    # train data
    input <- gsub("\\s+", "_", input)
    
    # With N input words, max size Ngram used to predict is (N+1)
    Nmax <- min(num_words, Nmax-1) + 1
    
    # Truncate input to no more than last (Nmax-1) words of input
    input <- paste(
        tail(strsplit(input, split="_")[[1]], Nmax-1),
        collapse="_"
    )
    
    # lambda is a hyperparameter used in the Stupid Backoff algorithm
    # 0.4 is the value suggested by T. Brants et al.
    lambda <- 0.4
    
    # Initialize output
    hits <- NULL 
    
    # Loop over Ngram size, descending order: i
    for (i in Nmax:2) {
        # Set exponent
        exp <- Nmax - i
        
        # Shorten input by one word off the front
        txt <- paste(
            tail(strsplit(input, split="_")[[1]], i-1),
            collapse="_"
        )
        
        # Generate search values for X and y for (N-1)-gran
        X_search <- paste(
            head(strsplit(input, split="_")[[1]], i-2),
            collapse="_"
        )
        y_search <- tail(strsplit(input, split="_")[[1]], 1)
        
        # Input (N-1)-gram count
        # NOTE: ngrams should have key (X, y)
        baseCount <- ngrams[[i-1]][.(X=X_search, y=y_search)]$count
        
        # If input (N-1)-gram count wasn't found, skip to next iteration
        if (is.na(baseCount)) next
        
        # Subset ngrams[[i]] where X matches txt
        hitsi <- ngrams[[i]][.(X=txt)]
        
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
    
    # If we don't yet have 5 predictions, fill them in from most common 1-grams
    # TODO: Determine how to provide a score (if possible) for these terms
    if (is.null(hits)) {
        hits <- 
            data.table(score=0, y=ngrams[[1]][order(-count)][1:5, y])
    } else {
        hits <- hits[1:5, .(score, y)][!is.na(y)][order(-score)]
        
        if (nrow(hits) < 5) {
            m <- 5 - nrow(hits)
            default_hits <- 
                data.table(score=0, y=ngrams[[1]][order(-count)][1:m, y])
            hits <- rbind(hits, default_hits)
        }
    }
    
    return(hits)
}