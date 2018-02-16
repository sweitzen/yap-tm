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
        
        for (j in 1:N) {
            tic <- Sys.time()
            
            ngram <- parallelizeTask(makeTokens, sentences, j)
            ngram_dfm <- parallelizeTask(dfm, ngram)
            
            dts <- data.table(
                ngram=featnames(ngram_dfm),
                count=colSums(ngram_dfm),
                key="ngram"
            )[order(-count)]
            
            save(dts, file=paste0("../data/dts_", j, "_", i, ".rda"))
            
            rm(list=c("ngram", "ngram_dfm", "dts"))
            
            toc <- Sys.time()
            print(paste0("Constructed ", j, "-gram; delta_t=", toc - tic))
        }
    }
}

combineChunks <- function(idx, N) {
    for (j in 1:N) {
        print(paste0("Combining ", j, "-grams"))
        
        out_dts <- NULL
        for(i in 1:(length(idx)-1)) {
            print(paste0("Combining chunk ", i, " of ", length(idx)-1))
            
            load(file=paste0("../data/dts_", j, "_", i, ".rda"))
            out_dts <- 
                rbindlist(
                    list(out_dts, dts)
                )[, lapply(.SD, sum, na.rm=TRUE), by=ngram]
        }
        
        dts <- out_dts
        rm(list=c("out_dts"))
        save(dts, file=paste0("../data/dts_total_", j, ".rda"))
        
        dts <- dts[count > 2]
        save(dts, file=paste0("../data/dts_pruned_", j, ".rda"))
        
        # Remove unneeded object to reclaim memory
        rm(list=c("dts"))
    }
}

getNgram <- function(text, i, sep = "_") {
    toks <- unlist(strsplit(text, "_"))
    n <- length(toks)
    m <- n - i + 1
    out <- toks[m]
    if (m < n) {
        for (j in (m+1):n) out <- paste0(out, "_", toks[j])
    }
    return(out)
}

predictNext <- function(input) {
    # Parse input text into an (N-1)-gram
    input <- makeSentences(input)
    
    num_words <- sapply(gregexpr("[[:alpha:]]+", input),
                        function(x) sum(x > 0))
    
    input <- makeTokens(input, num_words)$text1
    
    # Max size Ngram to use to predict
    Nmax <- min(num_words, N-1) + 1
    
    input <- getNgram(input, Nmax-1, "_")
    
    
    lambda <- 0.4
    hits <- NULL 
    
    # Stupid backoff explained in
    # https://rpubs.com/pferriere/dscapreport
    
    for (i in Nmax:2) {
        exp <- Nmax - i
        txt <- getNgram(input, i-1, "_")
        
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
            y=tail(strsplit(ngram, split="_")[[1]],1)
        ), by=ngram][order(-count)]
        
        # Remove hits already found
        hitsi <- hitsi[!(y %in% hits$y)]
        
        hits <- rbind(hits, hitsi[1:5][!is.na(y)])
        
        #    num_found <- num_found + nrow(hits[[i]])
        if (nrow(hits) >= 5) break
    }
    
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

# Main code ====================================================================

N <- 5

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
    
    combineChunks(idx, N)
}

for (i in 1:N) {
    load(paste0("../data/dts_total_", i, ".rda"))
    
    dts <- dts[count > 2]
    save(dts, file=paste0("../data/dts_pruned_", i, ".rda"))
    
    # Remove unneeded object to reclaim memory
    rm(list=c("dts"))
}

dts_list <- vector("list", N)
for (i in 1:N) {
    load(paste0("../data/dts_pruned_", i, ".rda"))
    
    dts_list[[i]] <- dts
    rm(list=c("dts"))
}

dts <- dts_list
rm(list=c("dts_list"))


# ==============================================================================

# Plot common N-grams
for (j in 1:N) {
    dts[[j]] <- dts[[j]][order(-count)]
}
makePlot(dts[[1]], "30 Most Common Unigrams")
makePlot(dts[[2]], "30 Most Common Bigrams")
makePlot(dts[[3]], "30 Most Common Trigrams")
makePlot(dts[[4]], "30 Most Common Quadgrams")
makePlot(dts[[5]], "30 Most Common Pentagrams")


for (j in 1:N) {
    setkey(dts[[j]], ngram)
}

# Input text
predictNext("he said, 'Hello")

predictNext("bxxt gfff nazgrapo")