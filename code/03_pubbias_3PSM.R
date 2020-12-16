{
    rm(list = ls())
    
    library(tidyverse)
    library(magrittr)
    library(weightr)
    library(robumeta)
    library(officer)
    library(flextable)
    
    source("code/functions/preview_ft_piped.R")
    source("code/functions/rnd_2.R")
    source("code/functions/rnd_3.R")
    source("code/functions/rnd_p.R")
    
    load("results/es_prepared2.Rda")
    
    get_lrtest_pval <- function(x){
        # Taken from edit(getAnywhere("weightfunct")).
        df <- length(x[[2]]$par) - length(x[[1]]$par)
        lrchisq <- 2 * (abs(x[[1]]$value - x[[2]]$value))
        pvalue <- 1 - pchisq(lrchisq, df)
        list(pvalue=pvalue, lrchisq=lrchisq, df=df)
    }
    
    n_boot <- 100
}

res_3PSM <- d2[c("rs.df", "rs.cf", "rs.bf")] %>% 
    map_dfr(.id = "cluster", function(dat){
        1:n_boot %>% 
            map(~ sample_n(group_by(dat, id.full), size = 1)) %>% 
            map(~ weightfunct(effect = .x$g, v = .x$var.g, steps = c(0.025, 1))) %>% 
            map_dfr(~ get_lrtest_pval(.x))
    }) %>% 
    group_by(cluster) %>% 
    summarize(mean_lrchisq_3psm = mean(lrchisq) %>% rnd_2,
              df_3psm = mean(df),
              mean_p_3psm = mean(pvalue) %>% rnd_p, 
              perc_below_05_3psm = sum(pvalue < .05) / n_boot * 100)

res_egger <- d2[c("rs.df", "rs.cf", "rs.bf")] %>% 
    map_dfr(.id = "cluster", ~ robu(g~se.g, .x, id.full, var.g)$reg_table[2,]) %>% 
    transmute(cluster = cluster, p_egger = prob %>% rnd_p) 

res_pubbias <- res_egger %>% 
    inner_join(res_3PSM, by = "cluster")

save(res_pubbias, file = "results/res_pubbias.Rda")

