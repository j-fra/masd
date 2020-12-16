rnd_p <- function(x, withEqualSign = FALSE) {
    
    round2 = function(x, n) {
        # Credit: https://stackoverflow.com/a/12688836
        posneg = sign(x)
        z = abs(x)*10^n
        z = z + 0.5
        z = trunc(z)
        z = z/10^n
        z*posneg
    }
    
    x_dup <- as.character(x)
    if(withEqualSign){
        x_dup <- paste("=", gsub("^.*?.","", sprintf("%.3f", round2(x, 3))))
    } else{
        x_dup <- paste(gsub("^.*?.","", sprintf("%.3f", round2(x, 3))))
    }
    x_dup[x < 0.001] <- "< .001"
    x_dup[is.na(x)] <- "NA"
    x_dup
    # For testing: rnd_p(c(NA, 0.002, 0.0009), F)
    # For testing: rnd_p(c(NA, 0.002, 0.0009), T)
}
