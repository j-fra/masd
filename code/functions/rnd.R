rnd <- function(x, dec){
    
    round2 = function(x, n) {
        # Credit: https://stackoverflow.com/a/12688836
        posneg = sign(x)
        z = abs(x)*10^n
        z = z + 0.5
        z = trunc(z)
        z = z/10^n
        z*posneg
    }
    
    sprintf(paste0("%.", dec, "f"), round2(x, dec))
}
