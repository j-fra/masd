get_dist <- function(x){
    if(class(x) == "numeric"){
        quarts <- x %>% quantile(seq(0, 1, 0.25), na.rm = T) %>% rnd_2
        paste0("Q = [", paste(quarts, collapse = ", "), 
               "], M = ", mean(x, na.rm = T) %>% rnd_2, 
               ", SD = ", sd(x, na.rm = T) %>% rnd_2)
    }else if(class(x) == "character"){
        paste0(x %>% table(useNA = "ifany") %>% names %>% unname, " (m = ",
               x %>% table(useNA = "ifany") %>% unname %>% c, ")", collapse = ", ")
    }
}