################################################################################
# tokenizer.R
#
# Resources used in creating this code:
# https://github.com/rstudio/cheatsheets/raw/master/quanteda.pdf
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html

library(quanteda)


# TODO: perhaps include qdap and tm to do initial cleaning, since doing it in
# quanteda::tokens doesn't seem to be working

################################################################################
# Given a corpus, this function tokenizes each document in it into individual
# sentences, to prevent the makeTokens function from creating Ngrams that span
# sentences.
# Input:
#    input
#        a corpus
# Output:
#        a named character vector with one sentence per element. The output will
#        have length >= length of the input corpus
makeSentences <- function(input) {
    
    # TODO: Figure out why the remove_* arguments seem to do nothing
    output <- tokens(
        input,
        what="sentence",
        remove_numbers=TRUE,
        remove_punct=TRUE,
        remove_separators=TRUE,
        remove_twitter=TRUE,
        remove_url=TRUE,
        remove_hyphens=FALSE
    )
    
    output <-unlist(
        lapply(
            output,
            tolower
        )
    )
    
    return(output)
}

################################################################################
# Given an input character vector and ngram size, splits the input into Ngrams.
# Input:
#    input
#        a character vector of individual sentences
#    ngram_size
#        size of Ngrams to generate, default=1
#    concatenator
#        character to use in concatenating n-grams, default="_"
# Output:
#        a tokens vector consisting of Ngrams for each sentence
makeTokens <- function(input, ngram_size=1L, concatenator="_") {
    
    # TODO: Figure out why the remove_* arguments seem to do nothing
    output <- tokens(
        input,
        what="word",
        remove_numbers=TRUE,
        remove_punct=TRUE,
        remove_separators=TRUE,
        remove_twitter=FALSE,
        remove_hyphens=FALSE,
        ngrams=as.integer(ngram_size),
        concatenator=concatenator
    )
    
    return(output)
}