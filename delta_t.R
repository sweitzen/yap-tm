################################################################################
# delta_t.R

################################################################################
# This function takes Sys.time() objects tic and toc (toc >= tic) and returns
# a string of format"delta_t= 00h:00m:00.00s"
# Input:
#    tic
#        a date-time or date object
#    toc
#        a date-time or date object (toc >= tic)
# Output:
#        a string of format "delta_t= 00h:00m:00.00s"
delta_t <- function(tic, toc) {
    delta_t <- toc - tic
    t_h <- as.integer(as.numeric(delta_t, units="hours"))
    t_m <- as.integer(as.numeric(delta_t, units="mins")) - 60L*t_h
    t_s <- round(as.numeric(delta_t, units="secs") - 60L*t_m - 3600L*t_h, 2)
    
    return(paste0("delta_t= ", t_h, "h:", t_m, "m:", t_s, "s"))
    
}
