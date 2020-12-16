rnd_3 <- function(x){
    
    round2 = function(x, n) {
        # Credit: https://stackoverflow.com/a/12688836
        posneg = sign(x)
        z = abs(x)*10^n
        z = z + 0.5
        z = trunc(z)
        z = z/10^n
        z*posneg
    }
    
    sprintf("%.3f", round2(x, 3))
}