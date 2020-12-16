{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    load("results/es_prepared2.Rda")
}

robu_control = robu(g ~ cluster-1, d2$rs.control, id.full, var.g, rho = 0.8)

XLIM = c(min(d2$rs_with_outlier$g), max(d2$rs_with_outlier$g))
YLIM = c(min(d2$rs_with_outlier$se.g), max(d2$rs_with_outlier$se.g))

g.list <- robu_control %>% 
    extract2("reg_table") %>% 
    mutate(labels = recode(labels, 
                           "clusterIntercourse.Frequency"="SF", 
                           "clusterTotal.One.Night.Stand.Partners"="ONS", 
                           "clusterTotal.Sex.Partners"="TP", 
                           "clusterSex.Partners.in.Last.Year" = "P12")) %>% 
    {setNames(object = .$b.r, nm = .$labels)}


png(filename = "results/figures and tables/main_analysis_funnels_control.png", width = 3.5*2.5, height = 3.5*2.5, units = 'in', res = 300)
par(mfrow = c(2, 2))
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.sf_with_outlier$g, vi = d2$rs.sf_with_outlier$var.g,   refline = g.list["SF"],  main = "Sexual Intercourse Frequency")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.ons$g, vi = d2$rs.ons$var.g, refline = g.list["ONS"], main = "Total One-Night Stands")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.tp$g, vi = d2$rs.tp$var.g,   refline = g.list["TP"],  main = "Total Sex Partners")
funnel(xlab = substitute(paste("Hedges' ", italic("g"))), ylim = YLIM, xlim = XLIM, x = d2$rs.p12$g, vi = d2$rs.p12$var.g, refline = g.list["P12"], main = "Total Sex Partners in Last Year")
par(mfrow = c(1,1))
dev.off()

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_funnels_control.png"))
