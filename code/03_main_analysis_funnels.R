{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    load("results/es_prepared2.Rda")
}

robu_core = robu(g ~ cluster-1, d2$rs.core_with_outlier, id.full, var.g, rho = 0.8)

XLIM = c(min(d2$rs_with_outlier$g), max(d2$rs_with_outlier$g))
YLIM = c(min(d2$rs_with_outlier$se.g), max(d2$rs_with_outlier$se.g))

g.list <- robu_core %>% 
    extract2("reg_table") %>% 
    mutate(labels = recode(labels, 
                           "clusterAffect.Frequency" = "DF", 
                           "clusterCognition.Frequency" = "CF", 
                           "clusterBehavior.Frequency" = "BF")) %>% 
    {setNames(object = .$b.r, nm = .$labels)}


png(filename = "results/figures and tables/main_analysis_funnels.png", width = 3.5*3, height = 3.5*1, units = 'in', res = 300)
par(mfrow = c(1, 3))
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.cf$g, vi = d2$rs.cf$var.g, refline = g.list["CF"], main = "Cognition Frequency")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.df_with_outlier$g, vi = d2$rs.df_with_outlier$var.g, refline = g.list["DF"], main = "Affect Frequency")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.bf$g, vi = d2$rs.bf$var.g, refline = g.list["BF"], main = "Behavior Frequency")
par(mfrow = c(1,1))
dev.off()

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_funnels.png"))
