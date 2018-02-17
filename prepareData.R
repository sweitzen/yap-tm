# prepareData.R
#
# Resources used in creating this code:
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md
# https://www.coursera.org/learn/data-science-project/discussions/forums/bXKqKZfYEeaRew5BAmrkbw/threads/dW1Z5sKMEeeTyArhA0ZGig
# https://github.com/rstudio/cheatsheets/raw/master/quanteda.pdf
# https://s3.amazonaws.com/assets.datacamp.com/blog_assets/datatable_Cheat_Sheet_R.pdf

library(data.table)
library(doParallel)
library(quanteda)


# Set seed for reproducability
set.seed(222)

# This function takes Sys.time() objects tic and toc (toc >= tic) and returns a
# string of format"delta_t= 00h:00m:00.00s"
# Input:
#        tic - a date-time or date object
#        toc - a date-time or date object (toc >= tic)
# Output:
#        a string of format "delta_t= 00h:00m:00.00s"
delta_t <- function(tic, toc) {
    delta_t <- toc - tic
    t_h <- as.integer(as.numeric(delta_t, units="hours"))
    t_m <- as.integer(as.numeric(delta_t, units="mins")) - 60L*t_h
    t_s <- round(as.numeric(delta_t, units="secs") - 60L*t_m  - 3600L*t_h, 2)
    
    return(paste0("delta_t= ", t_h, "h:", t_m, "m:", t_s, "s"))
    
}

# Generic function for parallelizing any task (when possible)
# 
# Credit for this function goes to Eric Rodriguez
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
parallelizeTask <- function(task, ...) {
    
    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    #print("Starting task")
    r <- task(...)
    #print("Task done")
    stopCluster(cl)
    
    return(r)
}

# Given a vector of paths to input data text files, this function reads them all
# and concatenates them. It then shuffles the data, and splits it into train and
# test sets (98% of data to train).
# Input:
#        file_list - a vector of paths to input files relative to the directory
#                    of prepareData.R
# Output:
#        a list containing training data (element 1) and test data (element 2)
getData <- function(file_list) {
    
    dat <- NULL
    for(f in file_list) {
        txt <- readLines(con=f, encoding="UTF-8", skipNul=TRUE)
        
        # Concatenate data
        dat <- c(dat, txt)
    }
    
    # Shuffle data
    dat <- dat[sample(seq(length(dat)))]
    
    # Split data into train and train sets
    smp_size <- floor(0.98 * length(dat))
    
    inTrain <- sample(seq_len(length(dat)), size=smp_size)
    
    train <- dat[inTrain]
    test <- dat[-inTrain]
    
    return(list(train=train, test=test))
}

# Given a number of lines in an input text file, this function creates an
# index to split the file into chunks, to aid in processing large files. The
# chunk size is hard-coded here.
# Input:
#        max_idx - the number of lines in the fule (i.e., the max index number)
# Output:
#        a sequence (integer vector) from 0 to max_idx, by chunk_size
getIdx <- function(max_idx) {

    chunk_size <- min(100000L, max_idx)
    idx <- seq(0L, max_idx, by=chunk_size)
    idx[length(idx)] <- as.integer(max_idx)
    
    return(idx)
}

# Given a corpus, this function tokenizes each document in it into individual
# sentences, to prevent the makeTokens function from creating Ngrams that span
# sentences.
# Input:
#        input - a corpus
# Output:
#        a named character vector with one sentence per element. The output will
#        have length >= length of the input corpus
#
# Inspiration for this function from Eric Rodriguez
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
makeSentences <- function(input) {

    output <- tokens(
        input,
        what = "sentence",
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

# Given an input character vector and integer n, splits the input into Ngrams
# Input:
#        input - a character vector of individual sentences
#        n - size of Ngrams to generate, default=1
#        concatenator - character to use in concatenating n-grams, 
#                       default="_"
# Output:
#        a tokens vector consisting of Ngrams for each sentence
#
# Inspiration for this function from Eric Rodriguez
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
makeTokens <- function(input, n=1L, concatenator="_") {
    
    output <- tokens(
        input,
        what="word",
        remove_numbers=TRUE,
        remove_punct=TRUE,
        remove_separators=TRUE,
        remove_twitter=FALSE,
        remove_hyphens=FALSE,
        ngrams=as.integer(n),
        concatenator=concatenator
    )
    
    return(output)
}

# This function breaks input raw text data dat into chunks as specified in idx,
# generates 1-grams, 2-grams, ..., Nmax-grams from each chunk, and then saves
# the chunks to disk.
# Input:
#        dat - character vector of raw input data. Each element may contain
#              multiple sentences.
#        idx - an integer vector from 0 to length(dat)
#        Nmax - maximum size of Ngrams to create
#        train - boolean value - TRUE for train data, FALSE for test
# Output:
#        none (data saved to disk and status messages printed to console)
analyzeChunks <- function(dat, idx, Nmax, train) {
    
    if (train == TRUE) {
        # If train, set concatenator to "_" and folder fname to 'train'
        concatenator <- "_"
        fname <- "train"
    } else {
        # If test, set set concatenator to " " and folder fname to 'test'
        concatenator <- " "
        fname <- "test"
    }
    
    # Loop over the values of idx: i
    for(i in 1:(length(idx)-1)) {
        print(paste0("Analysing chunk ", i, " of ", length(idx)-1))
        
        # Chunk data:
        # Each chuck i starts at index idx[i]+1 and ends at index idx[i+1]
        qcorpus <- corpus(dat[(idx[i]+1):idx[i+1]])
        sentences <- parallelizeTask(makeSentences, qcorpus)
        
        # Loop over Ngram size: j
        # Make sure directories ../data, ../data/fname/chunks, 
        # ../data/fname/pruned, and ../data/fname/total all exist
        for (j in 1:Nmax) {
            tic <- Sys.time()
            
            # Construct j-grams
            ngram <- parallelizeTask(makeTokens, sentences, j, concatenator)
            
            # Construct document-feature matrix from ngrams
            ngram_dfm <- parallelizeTask(dfm, ngram)
            
            # Collapse ngram_dfm into a data.table with columns ngram and count,
            # keyed on ngram, creating chunk i of j-grams
            dts <- data.table(
                ngram=featnames(ngram_dfm),
                count=as.integer(colSums(ngram_dfm)),
                key="ngram"
            )
            
            # Save this chunk to disk as '../data/fname/chunks/dts_j_i.rda'
            file_name <-
                paste0("../data/", fname, "/chunks/dts_", j, "_", i, ".rda")
            
            save(dts, file=file_name)
            
            # Remove objects created in this iteration to release memory
            # TODO: Research: Is this step neccessary or effectual?
            rm(list=c("ngram", "ngram_dfm", "dts"))
            
            toc <- Sys.time()

            print(paste0("Constructed ", j, "-gram; Saved at: ", file_name,
                         "; delta_t= ", delta_t(tic, toc))
            )
        }
    }
}

# This function combines the previously-generated Ngram chunks into total
# Ngrams, and then saves the chunks to disk. It then prunes very low-frequency
# terms from the Ngrams, and saves the pruned Ngrams to disk.
# Input:
#        idx - an integer vector from 0 to length(dat)
#        Nmax - maximum size of Ngrams to create
#        train - boolean value - TRUE for train data, FALSE for test
# Output:
#        none (data saved to disk and status messages printed to console)
combineChunks <- function(idx, Nmax, train) {
    
    if (train == TRUE) {
        # If train, set folder fname to 'train'
        fname <- "train"
    } else {
        # If test, set folder fname to 'test'
        fname <- "test"
    }
    
    # Loop over Ngram size: j
    for (j in 1:Nmax) {
        print(paste0("Combining ", j, "-grams"))
        
        # Initialize output
        out_dts <- NULL
        
        # Loop over the values of idx: i
        for(i in 1:(length(idx)-1)) {
            print(paste0("Combining chunk ", i, " of ", length(idx)-1))
            
            # Load 'dts_j_i.rda' (j-grams, chunk i) from disk
            load(file=paste0("../data/", fname, "/chunks/dts_", j, "_", i, ".rda"))
            
            # Combine dts for (j, i) with output, and sum any identical ngrams
            out_dts <- 
                rbindlist(
                    list(out_dts, dts)
                )[, lapply(.SD, sum, na.rm=TRUE), by=ngram]
        }
        
        # Rename output
        dts <- out_dts
        rm(list=c("out_dts"))
        
        # Save 'dts_total_j.rda' to disk
        file_name <-
            paste0("../data/", fname, "/total/dts_total_", j, ".rda")
        
        save(dts, file=file_name)
        
        print(paste0("Saved ", file_name))
        
        # Prune Ngrams
        # Ngram frequencies follow Zipf's Law, so there's a relatively small
        # number of entries with large counts. Most of the rows consist of very
        # small counts, so we can achieve significant memory savings by
        # truncating our Ngram tables to include only those entries with a count
        # larger than 2.
        dts <- dts[count > 2]
        
        # Save 'dts_pruned_j.rda' to disk
        file_name <-
            paste0("../data/", fname, "/pruned/dts_pruned_", j, ".rda")
        
        save(dts, file=file_name)

        print(paste0("Saved ", file_name))
        
        # Remove unneeded object to reclaim memory
        rm(list=c("dts"))
    }
}

# This is the main function that calls the other functions to prepare the data.
# Input:
#        train - boolean value - TRUE for train data, FALSE for test
# Output:
#        none (data saved to disk and status messages printed to console)
prepareData <- function(train=TRUE) {
    
    tic <- Sys.time()
    
    # Maximum size of Ngrams
    Nmax <- 5
    
    # To save time, only call getData the first time we attempt analysis, and
    # then save the separated train and test sets to disk.
    if(file.exists("../data/dat.rda")) {
        print("Loading dat.rda")
        load("../data/dat.rda")
    } else {
        print("Loading data from initial text files")
        file_list <- c(
            "../final/en_US/en_US.blogs.txt",
            "../final/en_US/en_US.news.txt",
            "../final/en_US/en_US.twitter.txt"
        )
        
        dat <- getData(file_list)
        save(dat, file="../data/dat.rda")
    }
    
    if (train == TRUE) {
        # If train, set dat to train data and folder fname to 'train'
        dat <- dat$train
        fname <- "train"
    } else {
        # If test, set dat to test data and folder fname to 'test'
        dat <- dat$test
        fname <- "test"
    }
    
    print(paste0("Set output directory to '", fname, "'"))
    
    idx <- getIdx(length(dat))
    
    analyzeChunks(dat, idx, Nmax, train)
    
    combineChunks(idx, Nmax, train)
    
    # Package our Ngrams into a single list to make loading simpler
    print("Packaging Ngrams into single list")
    dts_list <- vector("list", Nmax)
    for (i in 1:Nmax) {
        load(paste0("../data/", fname, "/pruned/dts_pruned_", i, ".rda"))
        
        # Set the key to ngram
        setkey(dts, ngram)
        
        dts_list[[i]] <- dts
        rm(list=c("dts"))
    }
    
    # Rename output
    dts <- dts_list
    rm(list=c("dts_list"))
    
    file_name <- paste0("../data/", fname, "/dts.rda")
    save(dts, file=file_name)
    
    print(paste0("Saved ", file_name))
    
    toc <- Sys.time()

    print(paste0("Done! delta_t= ", delta_t(tic, toc)))
}
