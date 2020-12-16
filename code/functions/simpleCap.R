simpleCap <- function(x) {
    simpleCapOne <- function(x){
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    unname(sapply(x, simpleCapOne))
    # For testing: simpleCap(c("make", "me", "cap"))
}