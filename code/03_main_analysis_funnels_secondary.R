{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    load("results/es_prepared2.Rda")
}

robu_second <- robu(g ~ cluster-1, d2$rs.second, id.full, var.g, rho = 0.8)

XLIM <- c(min(d2$rs_with_outlier$g), max(d2$rs_with_outlier$g))
YLIM <- c(min(d2$rs_with_outlier$se.g), max(d2$rs_with_outlier$se.g))

g.list <- robu_second %>% 
    extract2("reg_table") %>% 
    mutate(labels = recode(labels, 
                           "clusterAffect.Intensity"="DI", 
                           "clusterSelf.Rated.Sex.Drive" = "SD")) %>% 
    {setNames(object = .$b.r, nm = .$labels)}


png(filename = "results/figures and tables/main_analysis_funnels_secondary.png", width = 3.5*2*1.5, height = 3.5*1.5, units = 'in', res = 300)
par(mfrow = c(1, 2))
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.di_with_outlier$g, vi = d2$rs.di_with_outlier$var.g, refline = g.list["DI"], main = "Affect Intensity")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.sd$g, vi = d2$rs.sd$var.g, refline = g.list["SD"], main = "Self-Rated Sex Drive")
par(mfrow = c(1,1))
dev.off()

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_funnels_secondary.png"))
