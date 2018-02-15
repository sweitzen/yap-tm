# Much of the quanteda-related work is based on
# http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md
# https://www.coursera.org/learn/data-science-project/discussions/forums/bXKqKZfYEeaRew5BAmrkbw/threads/dW1Z5sKMEeeTyArhA0ZGig

library(data.table)
library(doParallel)
library(ggplot2)
library(quanteda)

# Set seed for reproducability
set.seed(222)

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
    smp_size <- floor(0.8 * length(dat))
    
    inTrain <- sample(seq_len(length(dat)), size=smp_size)
    
    train <- dat[inTrain]
    test <- dat[-inTrain]
    
    return(list(train=train, test=test))
}

getIdx <- function(max_idx) {
    idx <- seq(0, max_idx, by=1e5)
    idx[length(idx)] <- max_idx
    
    return(idx)
}

makePlot <- function(data, title) {
    data[1:30, ] %>%
        ggplot(aes(reorder(ngram, -count), count)) +
        geom_bar(stat="identity", fill=I("darkred")) +
        theme(axis.text.x=element_text(angle=45, size=10, hjust=1)) +
        labs(x="Ngrams", y="Count") +
        ggtitle(title)
}

# Generic function for parallelizing any task (when possible)
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

# Returns a vector of profanity words
# TODO: Fix this function
getProfanityWords <- function(corpus) {
    profanityFileName <- "profanity.txt"
    if (!file.exists(profanityFileName)) {
        profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
        download.file(profanity.url, destfile = profanityFileName, method = "curl")
    }
    
    if (sum(ls() == "profanity") < 1) {
        profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
        profanity <- profanity$V1
        profanity <- profanity[1:length(profanity)-1]
    }
    
    return(profanity)
}

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

makeTokens <- function(input, n=1L) {
    output <- tokens(
        input,
        what="word",
        remove_numbers=TRUE,
        remove_punct=TRUE,
        remove_separators=TRUE,
        remove_twitter=FALSE,
        remove_hyphens=FALSE,
        ngrams=n
    )
    
    return(output)
}

analyzeChunks <- function(dat, idx, N) {
    for(i in 1:(length(idx)-1)) {
        print(paste0("Analysing chunk ", i, " of ", length(idx)-1))
        
        # Chunk data
        qcorpus <- corpus(dat[(idx[i]+1):idx[i+1]])
        sentences <- parallelizeTask(makeSentences, qcorpus)
        
        dts <- vector("list", N)
        for (j in 1:N) {
            ngram <- parallelizeTask(makeTokens, sentences, j)
            ngram_dfm <- parallelizeTask(dfm, ngram)
            
            dts[[j]] <- data.table(
                ngram=featnames(ngram_dfm),
                count=colSums(ngram_dfm),
                key="ngram"
            )[order(-count)]
            
            # Remove unneeded object to reclaim memory
            rm(list=c("ngram", "ngram_dfm"))
        }
        
        save(dts, file=paste0("../data/dts_", i, ".rda"))
    }
}

combineChunks <- function(idx, N) {
    out_dts <- vector("list", N)
    for(i in 1:(length(idx)-1)) {
        print(paste0("Combining chunk ", i, " of ", length(idx)-1))
        
        load(paste0("../data/dts_", i, ".rda"))
        
        for (j in 1:N) {
            out_dts[[j]] <- 
                rbindlist(
                    list(out_dts[[j]], dts[[j]])
                )[, lapply(.SD, sum, na.rm=TRUE), by=ngram]
        }
    }
    
    for (j in 1:N) {
        setkey(out_dts[[j]], ngram)
    }
    
    return(out_dts)
}

# Good Turing
createFreqs <- function(index) {
    DT <- dts[[index]]
    l <- rep(1, times = DT[, max(count)])
    for (n in unique(DT$count)) {
        l[n] <- DT[count == n, length(count)]
    }
    l[length(l) + 1] <- 1
    
    return(l)
}

getNgram <- function(text, i, sep = "_") {
    regex <- paste0(
        "^(([^", sep, "]*", sep, "){", 
        i, 
        "}[^", sep, "]*).*"
    )
    
    return(paste0("^", sub(regex, "\\1", text)))
}

# Stupid Backoff
stupidBO <- function(text, n) {
    if (n == 0) {
        return(0)
    } 
    l <- rep(1, times=n)
    exp <- n
    for (i in n:1) {
        regex <- getNgram(text, i, sep = "_")
        l[i] <- .4^exp * dts[[i]][.(regex)]$count
        #l[i] <- .4^exp * sum(dts[[i]][ngram %like% paste0("^", regex, "_")]$count)
        exp <- exp + 1
    }
    return(sum(l))
}


# Main code ====================================================================

N <- 4

if(file.exists("../data/dts_total.rda")) {
    load("../data/dts_total.rda")
} else {
    if(file.exists("../data/dat.rda")) {
        load("../data/dat.rda")
    } else {
        file_list <- c(
            "../final/en_US/en_US.blogs.txt",
            "../final/en_US/en_US.news.txt",
            "../final/en_US/en_US.twitter.txt"
        )
        
        dat <- getData(file_list)
        save(dat, file="../data/dat.rda")
    }
    
    idx <- getIdx(length(dat$train))
    
    analyzeChunks(dat$train, idx, N)
    
    dts <- combineChunks(idx, N)
    
    save(dts, file="../data/dts_total.rda")
}

nfeats <- vector("list", N)
for (i in 1:N) {
    nfeats[[i]] <- nrow(dts[[i]])
}
# ==============================================================================

# Plot common N-grams
makePlot(dts[[1]], "30 Most Common Unigrams")
makePlot(dts[[2]], "30 Most Common Bigrams")
makePlot(dts[[3]], "30 Most Common Trigrams")
makePlot(dts[[4]], "30 Most Common Quadgrams")

countDFS <- list(
    parallelizeTask(createFreqs, 1),
    parallelizeTask(createFreqs, 2),
    parallelizeTask(createFreqs, 3),
    parallelizeTask(createFreqs, 4)
)

regex <- "how_is_it"
n0 <- 4
DT <- dts[[n0]]
# add ^ to make sure we are at the beginning of an ngram
#hits <- DT[ngram %like% paste0("^", regex, "_"), ngram]
hits <- DT[ngram %like% paste0("^", regex, "_"), ngram]

if (length(hits) > 0) {
    print("Hit!")
    baseCount <- dts[[n0]][.(regex)]$count
    for (hit in hits) {
        DT[.(hit), ':=' (
            mle = count / baseCount, 
            lap = (count + 1) / (baseCount + nfeats[[n0]]),
            gt = (count + 1) * (countDFS[[n0]][count + 1] / countDFS[[n0]][count]),
            sbo = count + 0.4 * baseCount + stupidBO(ngram, n0 - 1)
        )]
    }
    DT[hits][order(-gt)]
}