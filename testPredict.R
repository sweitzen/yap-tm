################################################################################
# testPredict.R

library(data.table)


source("delta_t.R")
source("predictNext.R")

################################################################################
# If dts_prepped.rda exists, reads it from disk. Otherwise, reads test/dts.rda
# loops over Ngrams, adds a column for the prediction (pred), the score (score),
# correct, and correct_wt, and then saves this as test/dts_prepped.rda.
# Input:
#    none
# Output:
#        a list of Ngram data.tables, with columns ngram (key), count, X, y,
#        pred, and score
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
                "Adding pred, score, correct, correct_wt to ", i, "-grams")
            )
            
            dts[[i]] <- dts[[i]][, ':=' (
                pred="",
                score=0.0,
                correct=0L,
                correct_wt=0L
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
# the tope prediction and score in the test data.table. It also scores (1 or 0)
# whether the prediction is correct. The function also saves a benchmark file
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
                mypred <- predictNext(test[i]$X, dts)[1]
                
                # Update table
                test[i]$pred <- mypred$y
                test[i]$score <- mypred$score
            }
        }
        
        toc <- Sys.time()
        
        if (bare_benchmark == TRUE) {
            accuracy0 <- 0
            accuracy1 <- 0
        } else {
            # correct_wt is the weighted correct predictions
            test[, correct := as.integer(y == pred)]
            test[, correct_wt := (correct * count)] 

            file_name <- paste0("../data/validation/test_", j, ".rda")
            
            print(paste0("Saving ", file_name))
            save(test, file=file_name)
            
            # Calculate prediction accuracy: accuracy0 is accuracy including all
            # samples in test; accuracy1 excludes samples with count < 2.
            accuracy0 <-
                sum(test[, correct_wt]) / sum(test[, count])
            accuracy1 <-
                sum(test[count > 1, correct_wt]) / sum(test[count > 1, count])
        }
        
        # Remove unneeded object to reclaim memory
        rm(list=c("test"))
        
        t <- difftime(toc, tic, units="secs")
        
        dt <- data.table(
            ngram=j,
            nrows=smp_size,
            accuracy0=accuracy0,
            accuracy1=accuracy1,
            t_tot_s=t,
            t_row_ms=(as.numeric(t) * 1000 / smp_size)
        )
        
        bmark <- rbindlist(list(bmark, dt))
        
        print(dt)
    }
    
    if (bare_benchmark == TRUE) {
        file_name <- paste0("../data/validation/benchmark_time.rda")
    } else {
        file_name <- paste0("../data/validation/benchmark_accuracy.rda")
    }
    
    print(paste0("Saving ", file_name))
    save(bmark, file=file_name)
    
    return(bmark)
}

# Main code ====================================================================

# Get prepped test Ngrams
dts_test <- getPreppedTestData()

# Load train Ngrams
load("../data/train/dts_pruned_8.rda")

bmark <- testPredict(2000, TRUE)

bmark <- testPredict(100000, FALSE)