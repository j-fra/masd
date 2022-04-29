{
    library(tidyverse)
    library(magrittr)
    library(cowplot)
    
    rm(list = ls())
    
    load("results/mod_results.Rda")
    load("results/mod_results_secondary.Rda")
    
    blank_plot <- function(mytext, header){
        ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = paste0(mytext, "\n", header)) + 
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 axis.title.x=element_blank(),
                                 axis.title.y=element_blank(),legend.position="none",
                                 panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank(),plot.background=element_blank())
    }
}



xx <- mod_results %>% 
    filter(modvarname %in% c("year", "age.mean", "gii")) %>% 
    transmute(label, class,
              cf_regplot = map(cf_regplot, ~ extract2(.x, 1)),
              df_regplot = map(df_regplot, ~ extract2(.x, 1)),
              bf_regplot = map(bf_regplot, ~ extract2(.x, 1))) %>% 
    pivot_longer(cols = contains("regplot"), names_to = "cluster", values_to = "regplot") %>% 
    mutate(regplot = map(regplot, ~ .x + 
                             ggtitle(NULL) +        
                             scale_y_continuous(breaks = seq(-0.25, 1.75, 0.25), 
                                                limits = c(-0.4, 1.7)))) %>% 
    extract2("regplot")

plot_grid(plotlist = list(xx[[1]] + xlim(15, 55) + ggtitle("Cognition Frequency") + theme(plot.title = element_text(hjust = 0.5)) + xlab(" "), 
                          xx[[2]] + xlim(15, 55) + ggtitle("Affect Frequency") + theme(plot.title = element_text(hjust = 0.5))+ ylab(" ") + xlab("Mean Sample Age") +theme(axis.title=element_text(size=14,face="bold")), 
                          xx[[3]] + xlim(15, 55) + ggtitle("Behavior Frequency") + theme(plot.title = element_text(hjust = 0.5))+ ylab(" ") + xlab(" "),
                          xx[[4]] + xlim(0, 0.7) + ggtitle(" ") + xlab(" ") + xlab(" "), 
                          xx[[5]] + xlim(0, 0.7) + ggtitle(" ") + ylab(" ") + xlab("Gender Inequality Index") +theme(axis.title=element_text(size=14,face="bold")), 
                          xx[[6]] + xlim(0, 0.7) + ggtitle(" ") + ylab(" ") + xlab(" "),
                          xx[[7]] + xlim(1994, 2022) + ggtitle(" ") + xlab(" "), 
                          xx[[8]] + xlim(1994, 2022) + ggtitle(" ") + ylab(" ") + xlab("Year of Study") +theme(axis.title=element_text(size=14,face="bold")), 
                          xx[[9]] + xlim(1994, 2022) + ggtitle(" ") + ylab(" ") + xlab(" ")), ncol = 3) %>% 
    ggsave(filename = "results/figures and tables/moderation_plots_age_year_gii.png", 
           width = 10, height = 10, units = "in", dpi = 300, limitsize = FALSE)


# mutate(cluster = cluster %>% str_remove("_regplot") %>% toupper) %>% {
#     plot_grid(plotlist = c(list(blank_plot("CF", header = ""), blank_plot("DF", header = ""), blank_plot("BF", header = "")), .$regplot), ncol = 3, rel_heights = c(2, 8, 8, 8))
# }
# 
# plot_grid(blank_plot(.y, "\nCognition Frequency (Controlled) - Affect Frequency - Behavior Frequency"), .x,  ncol = 1, rel_heights = c(1, 10))
#     imap(~ plot_grid(blank_plot(.y, "\nCognition Frequency (Controlled) - Affect Frequency - Behavior Frequency"), .x,  ncol = 1, rel_heights = c(1, 1))) %>%
#     {plot_grid(plotlist = ., ncol = 1,
#                rel_heights = mod_results %>% group_by(class) %>% summarize(cnt = n()) %>% select(cnt) %>% extract2(1))}
# 
# 
#     ggsave(filename = "manuscript/all_moderation_plots.pdf",
#            width = 18, height = nrow(mod_results) * 4.5, units = "in", dpi = 300, limitsize = FALSE)

# mod_results %>%
#     transmute(label, class,
#               cf_regplot = map(cf_regplot, ~ extract2(.x, 1)),
#               df_regplot = map(df_regplot, ~ extract2(.x, 1)),
#               bf_regplot = map(bf_regplot, ~ extract2(.x, 1))) %>%
#     pivot_longer(cols = contains("regplot"), names_to = "cluster", values_to = "regplot") %>%
#     mutate(cluster = cluster %>% str_remove("_regplot") %>% toupper) %>%
#     split(.$class) %>%
#     map(~ plot_grid(plotlist = .x$regplot, ncol = 3)) %>%
#     imap(~ plot_grid(blank_plot(.y, "\nCognition Frequency (Controlled) - Affect Frequency - Behavior Frequency"), .x,  ncol = 1, rel_heights = c(1, 10))) %>%
#     {plot_grid(plotlist = ., ncol = 1,
#                rel_heights = mod_results %>% group_by(class) %>% summarize(cnt = n()) %>% select(cnt) %>% extract2(1))} %>%
#     ggsave(filename = "manuscript/all_moderation_plots.pdf",
#            width = 18, height = nrow(mod_results) * 4.5, units = "in", dpi = 300, limitsize = FALSE)
#
# mod_results_secondary %>%
#     transmute(label, class,
#               cf_regplot = map(cf_regplot, ~ extract2(.x, 1)),
#               di_regplot = map(di_regplot, ~ extract2(.x, 1)),
#               sd_regplot = map(sd_regplot, ~ extract2(.x, 1))) %>%
#     pivot_longer(cols = contains("regplot"), names_to = "cluster", values_to = "regplot") %>%
#     mutate(cluster = cluster %>% str_remove("_regplot") %>% toupper) %>%
#     split(.$class) %>%
#     map(~ plot_grid(plotlist = .x$regplot, ncol = 3)) %>%
#     imap(~ plot_grid(blank_plot(.y, "\nCognition Frequency - Affect Intensity - Self-Rated Sex Drive"), .x,  ncol = 1, rel_heights = c(1, 10))) %>%
#     {plot_grid(plotlist = ., ncol = 1,
#                rel_heights = mod_results %>% group_by(class) %>% summarize(cnt = n()) %>% select(cnt) %>% extract2(1))} %>%
#     ggsave(filename = "manuscript/all_moderation_plots_secondary.pdf",
#            width = 18, height = nrow(mod_results_secondary) * 4.5, units = "in", dpi = 300, limitsize = FALSE)
#
# shell.exec(paste0(getwd(), "/manuscript/all_moderation_plots.pdf"))
# shell.exec(paste0(getwd(), "/manuscript/all_moderation_plots_secondary.pdf"))
