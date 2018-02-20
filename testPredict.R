################################################################################
# testPredict.R

library(data.table)


source("delta_t.R")
source("predictNext.R")

################################################################################
# If dts_prepped.rda exists, reads it from disk. Otherwise, reads test/dts.rda
# loops over Ngrams, adds columns for the predictions (pred1..5), boolean 
# columns (correct1..5), such that columnN indicated cumulatively whether the 
# actual next word y is in pred1..N,  and then saves this as 
# test/dts_prepped.rda.
# Input:
#    none
# Output:
#        a list of Ngram data.tables, with columns ngram (key), count, X, y,
#        pred1..5, and correct1..5
getPreppedTestData <- function() {
    
    tic <- Sys.time()
    
    prepped_name <- "../data/test/dts_prepped.rda"
    
    if(file.exists(prepped_name)) {
        print(paste0("Loading ", prepped_name))
        load(prepped_name)
    } else {
        # Load test Ngrams
        file_name <- "../data/test/dts.rda"
        print(paste0("Loading ", file_name))
        load(file_name)
        
        # Maximum size of Ngrams
        Nmax <- 5
        
        # Loop over Ngram size: i
        for (i in 2:Nmax) {
            print(paste0(
                "Adding pred1..5, correct1..5 to ", i, "-grams")
            )
            
            dts[[i]] <- dts[[i]][, ':=' (
                pred1="",
                pred2="",
                pred3="",
                pred4="",
                pred5="",
                correct1=FALSE,
                correct2=FALSE,
                correct3=FALSE,
                correct4=FALSE,
                correct5=FALSE
            )]
        }
        
        print(paste0("Saving ", prepped_name))
        save(dts, file=prepped_name)
    }
    
    toc <- Sys.time()

    print(paste0("Done! ", delta_t(tic, toc)))
    
    return(dts)
}

################################################################################
# Given input train set dts and test set dts_test, this function take a random
# sample smp_size from dts_test, feeds each test X into predictNext and stores
# the top 5 predictions in the test data.table. It also scores (TRUE or FALSE)
# whether the predictions is correct. The function also saves a benchmark file
# with a data.table of Ngram size (ngram), smp_size (nrows), accuracy, total 
# time in seconds  to calculate smp_size predictions for Ngram size ngram, and
# time in milliseconds for each prediction.
# Inputs: 
#    smp_size
#        number of rows to sample from dts_test; if -1, use all rows
#    bare_benchmark
#        if TRUE, runs testPredict with minimal overhead in the prediction loop,
#        to gauge the predictNext runtime as accurately as possible; if FALSE,
#        calculates pred, score, correct, and accuracy
#    dts (not passed, but present in calling environment)
#        a list of data.tables of size Nmax containing 1-grams, 2-grams, ...,
#        Nmax-grams and their total counts observed in the input corpus. The
#        columns are count, X (first n-1 terms of n-gram), and y (last word of
#        n-gram), and has a key on (X, y)
#    dts_test (not passed, but present in calling environment)
#        similar to dts, but ngrams are space-delimited, rather than underscore-
#        delimited. dts_test also includes blank columns for the top prediction 
#        (pred), its SBO score (score), and whether the prediction was correct
#        (0, 1 - column 'correct').
# Outputs:
#        benchmark data.table (other outputs are saved to disk)
testPredict <- function(smp_size, bare_benchmark=FALSE) {
    
    tic0 <- Sys.time()
    
    # Maximum size of Ngrams
    Nmax <- 5
    
    # Set seed for reproducability
    set.seed(222)
    
    bmark <- NULL
    
    # Loop over Ngram size: j
    for (j in 2:Nmax) {
        
        if (smp_size == -1) {
            test <- dts_test[[j]]
        } else {
            inSmp <- sample(seq_len(nrow(dts_test[[j]])), size=smp_size)
            test <- dts_test[[j]][inSmp]
        }
        
        tic <- Sys.time()
        
        if (bare_benchmark == TRUE) {
            # Loop over sample rows: i
            for (i in 1:smp_size) {
                mypred <- predictNext(test[i]$X)[1]
            }
        } else {
            # Loop over sample rows: i
            for (i in 1:smp_size) {
                if (i %% 100 == 0) {
                    print(paste0(
                        "Working on ", j, "-grams, row ", i, " of ", smp_size, 
                        "; elapsed time ", delta_t(tic, Sys.time()))
                    )
                }
                
                # Calculate prediction
                mypred <- predictNext(test[i]$X, dts)
                
                # Update table
                test[i]$pred1 <- mypred$y[1]
                test[i]$pred2 <- mypred$y[2]
                test[i]$pred3 <- mypred$y[3]
                test[i]$pred4 <- mypred$y[4]
                test[i]$pred5 <- mypred$y[5]
            }
        }
        
        toc <- Sys.time()
        
        if (bare_benchmark == TRUE) {
            accuracy1 <- 0
            accuracy2 <- 0
            accuracy3 <- 0
            accuracy4 <- 0
            accuracy5 <- 0
        } else {
            # correctN is cumulative for pred1..N
            test[, correct1 := (y == pred1)]
            test[, correct2 := (y == pred2) | correct1]
            test[, correct3 := (y == pred3) | correct2]
            test[, correct4 := (y == pred4) | correct3]
            test[, correct5 := (y == pred5) | correct4]

            file_name <- paste0("../data/validation/test_", j, ".rda")
            
            print(paste0("Saving ", file_name))
            save(test, file=file_name)
            
            # Calculate prediction accuracy: accuracy0N is accuracy including
            # all samples in test; accuracy1N includes samples with count > 1.
            
            accuracy01 <- sum(test$correct1 * test$count) / sum(test$count)
            accuracy02 <- sum(test$correct2 * test$count) / sum(test$count)
            accuracy03 <- sum(test$correct3 * test$count) / sum(test$count)
            accuracy04 <- sum(test$correct4 * test$count) / sum(test$count)
            accuracy05 <- sum(test$correct5 * test$count) / sum(test$count)
            
            test <- test[count > 1]
            accuracy11 <- sum(test$correct1 * test$count) / sum(test$count)
            accuracy12 <- sum(test$correct2 * test$count) / sum(test$count)
            accuracy13 <- sum(test$correct3 * test$count) / sum(test$count)
            accuracy14 <- sum(test$correct4 * test$count) / sum(test$count)
            accuracy15 <- sum(test$correct5 * test$count) / sum(test$count)
        }
        
        # Remove unneeded object to reclaim memory
        rm(list=c("test"))
        
        t <- difftime(toc, tic, units="secs")
        
        dt <- data.table(
            ngram=j,
            nrows=smp_size,
            accuracy01=accuracy01,
            accuracy02=accuracy02,
            accuracy03=accuracy03,
            accuracy04=accuracy04,
            accuracy05=accuracy05,
            accuracy11=accuracy11,
            accuracy12=accuracy12,
            accuracy13=accuracy13,
            accuracy14=accuracy14,
            accuracy15=accuracy15,
            t_tot_s=t,
            t_row_ms=(as.numeric(t) * 1000 / smp_size)
        )
        
        bmark <- rbindlist(list(bmark, dt))
        
        print(dt)
    }
    
    if (bare_benchmark == TRUE) {
        file_name <- "../data/validation/benchmark_time.rda"
    } else {
        file_name <- "../data/validation/benchmark_accuracy.rda"
    }
    
    print(paste0("Saving ", file_name))
    save(bmark, file=file_name)
    
    toc0 <- Sys.time()
    
    print(paste0(
        "Done!; total elapsed time ", delta_t(tic0, toc0))
    )
    
    return(bmark)
}

# Main code ====================================================================

# Get prepped test Ngrams
dts_test <- getPreppedTestData()

# Load train Ngrams
load("../data/train/dts_pruned_8.rda")

bmark <- testPredict(2000, TRUE)

bmark <- testPredict(100000, FALSE)