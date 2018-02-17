# exploreData.R
#
# Resources used in creating this code:
# https://rpubs.com/pferriere/dscapreport (for SBO understanding)

library(data.table)
library(doParallel)
library(quanteda)

# predictNext uses:
#   * makeSentences
#   * makeTokens
source("prepareData.R")

# Given an input 
predictNext <- function(input) {

    # Maximum size of Ngrams
    Nmax <- 5
    
    # Parse input text into an (N-1)-gram
    input <- makeSentences(input)
    
    # Counting words may be tricky...
#    num_words <- sapply(gregexpr("[[:alpha:]]+", input),
#                        function(x) sum(x > 0))
    
    num_words <- sapply(gregexpr("\\s+", input), length) + 1
    
    input <- gsub("\\s+", "_", input)
    
    # Max size Ngram to use to predict
    Nmax <- min(num_words, Nmax-1) + 1
    
    #input <- getNgram(input, Nmax-1, "_")
    input <- paste(
        tail(strsplit(input, split="_")[[1]], Nmax-1),
        collapse="_"
    )
    
    # lambda is a hyperparameter used in the Stupid Backoff algorithm as 
    # described in:
    # “Large language models in machine translation” by T. Brants et al, in 
    # EMNLP/CoNLL 2007 (http://www.aclweb.org/anthology/D07-1090.pdf)
    lambda <- 0.4
    
    # Initialize output
    hits <- NULL 
    
    # Stupid backoff explained in
    # https://rpubs.com/pferriere/dscapreport
    
    for (i in Nmax:2) {
        # Set exponent
        exp <- Nmax - i
        
        #txt <- getNgram(input, i-1, "_")
        txt <- paste(
            tail(strsplit(input, split="_")[[1]], i-1),
            collapse="_"
        )
        
        # Input (N-1)gram count
        baseCount <- dts[[i-1]][.(txt)]$count
        
        # If input (N-1)gram count wasn't found, skip to next iteration
        if (is.na(baseCount)) next
        
        hitsi <- dts[[i]][ngram %like% paste0("^", txt, "_")]
        
        if (nrow(hitsi) == 0) next
        
        hitsi <- hitsi[, ':=' (
            baseCount=baseCount,
            exp=exp,
            score=(lambda^exp * count / baseCount), # Implement Stupid Backoff
            N=i,
            X=txt,
            y=tail(strsplit(ngram, split="_")[[1]], 1)
        ), by=ngram][order(-count)]
        
        # Remove hits already found
        hitsi <- hitsi[!(y %in% hits$y)]
        
        hits <- rbind(hits, hitsi[1:5][!is.na(y)])
        
        if (nrow(hits) >= 5) break
    }
    
    # If we don't yet have 5 predictions, fill them in from most common 1-grams
    if (is.null(hits)) {
        hits <- 
            data.table(score=0, y=dts[[1]][order(-count)][1:5, ngram])
    } else {
        hits <- hits[1:5, .(score, y)][!is.na(y)][order(-score)]
        
        if (nrow(hits) < 5) {
            m <- 5 - nrow(hits)
            default_hits <- 
                data.table(score=0, y=dts[[1]][order(-count)][1:m, ngram])
            hits <- rbind(hits, default_hits)
        }
    }
    
    return(hits)
}