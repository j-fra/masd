{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    library(clubSandwich)
    library(gridExtra)
    library(jtools)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    walk(list.files("code/functions/", full.names = T), source)
    
    load("results/es_prepared2.Rda")
}

Wald_test_wrapper <- function(model){
    # This is just a short wrapper for the usual wald test of all coefficients.
    Wald_test(model, constrain_zero(2:length(model$reg_table$labels)), vcov = "CR2")
}

models <- list(core = robu(g ~ cluster, d2$rs.core, id.full, var.g, rho = 0.8),
               second = robu(g ~ cluster, d2$rs.second, id.full, var.g, rho = 0.8),
               control = robu(g ~ cluster, d2$rs.control %>% mutate(cluster = recode(cluster, "Intercourse Frequency" = "Sexual Intercourse Frequency",
                                                                                     "Total One Night Stand Partners" = "Total One-Night Stands",
                                                                                     "Sex Partners in Last Year" = "Total Sex Partners in Last Year")), id.full, var.g, rho = 0.8))
models2 <- list(core = robu(g ~ cluster-1, d2$rs.core, id.full, var.g, rho = 0.8),
                second = robu(g ~ cluster-1, d2$rs.second, id.full, var.g, rho = 0.8),
                control = robu(g ~ cluster-1, d2$rs.control %>% mutate(cluster = recode(cluster, "Intercourse Frequency" = "Sexual Intercourse Frequency",
                                                                                        "Total One Night Stand Partners" = "Total One-Night Stands",
                                                                                        "Sex Partners in Last Year" = "Total Sex Partners in Last Year")), 
                               id.full, var.g, rho = 0.8))

walds <- lapply(models, Wald_test_wrapper)



grid.arrange(plot.rve.cat(modvar = 'cluster', titlename = "Sex Drive Manifestations", levelorder = c(3,1,2),
                          model = models[["core"]], model2 = models2[["core"]], wald = walds[["core"]]) + ylim(c(min(d2$rs$g)-0.1, max(d2$rs$g)-0.1)),
             plot.rve.cat(modvar = 'cluster', titlename = "Indicators of Latent Sex Drive",
                          model = models[["second"]], model2 = models2[["second"]], wald = walds[["second"]]) + ylim(c(min(d2$rs$g)-0.1, max(d2$rs$g)-0.1)),
             plot.rve.cat(modvar = 'cluster', titlename = "Bias Indicators",
                          model = models[["control"]], model2 = models2[["control"]], wald = walds[["control"]]) + ylim(c(min(d2$rs$g)-0.1, max(d2$rs$g)-0.1)), 
             nrow = 3) %>% 
    ggsave(filename = "results/figures and tables/main_analysis_figure.png", 
           width = 9, height = 12, units = "in")

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_figure.png"))
