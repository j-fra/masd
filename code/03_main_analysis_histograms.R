{
    library(tidyverse)
    library(magrittr)
    library(cowplot)
    library(jtools)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    load("results/es_prepared2.Rda")
}

get_hist <- function(dat, .title, .ylab = "", .xlab = "", hide_y_ticks = T){
    dat %>% 
        {ggplot(., aes(x = g)) +
                geom_histogram(aes(y =..density..), breaks = seq(-3, 3, 0.1)) +
                stat_function(fun = dnorm, args = list(mean = mean(.$g), sd = sd(.$g)), size = 1, color = "red", alpha = 0.5) +
                ggtitle(.title) + 
                ylab(.ylab) + 
                xlab(.xlab) + 
                scale_x_continuous(breaks = seq(-3, 3, 0.5), limits = c(min(d2$rs$g)-0.05, max(d2$rs$g)+0.05))+
                scale_y_continuous(limits = c(0, 2.5)) +
                theme_apa() + 
                theme(plot.title = element_text(size=12)) +
                if(hide_y_ticks){
                    theme(axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
                } else{
                    
                }
        }
}

plot_grid(d2$rs.core %>% get_hist("Sex Drive Manifestations", hide_y_ticks = F, .ylab = "Density"),
             d2$rs.second %>% get_hist("Indicators of Latent Sex Drive", .xlab = expression(paste("Hedges' ", italic("g")))),
             d2$rs.control %>% get_hist("Bias Indicators")
             # , d2$rs.cf %>% get_hist("Cognition Frequency", hide_y_ticks = F, .ylab = "Density"),
             # d2$rs.df %>% get_hist("Affect Frequency", .xlab = expression(paste("Hedges' ", italic("g")))),
             # d2$rs.bf %>% get_hist("Behavior Frequency"), 
             # d2$rs.di %>% get_hist("Affect Intensity", hide_y_ticks = F, .ylab = "Density"),
             # d2$rs.sd %>% get_hist("Self-Rated Sex Drive", .xlab = expression(paste("Hedges' ", italic("g")))),
             # d2$rs.ons %>% get_hist("One-Night Stands"),
             # d2$rs.p12 %>% get_hist("Partners Last Year", hide_y_ticks = F, .ylab = "Density"),
             # d2$rs.tp %>% get_hist("Total Partners", .xlab = expression(paste("Hedges' ", italic("g")))),
             # d2$rs.sf %>% get_hist("Sex Frequency")
                                   ,
             nrow = 1, ncol = 3, rel_widths = c(1/3.45, 1/4, 1/4)) %>% 
    ggsave(filename = "results/figures and tables/main_analysis_histograms.png", 
       width = 9, height = 3*1, units = "in", dpi = 300)

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_histograms.png"))
