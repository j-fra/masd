{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    library(RColorBrewer)
    library(gridExtra)
    
    rm(list = ls())
    
    source("code/functions/rnd_2.R")
    
    load("results/es_prepared2.Rda")
}

g_adj <- d2[c("rs.core", "rs.control")] %>% 
    map(~{robu(g ~ cluster -1, data = .x, studynum = id.full, var.eff.size = var.g, rho = 0.8) %>% 
            extract2("reg_table") %>% 
            select(b.r, SE) %>% {
                rma(yi = .$b.r, 
                    sei = .$SE, 
                    weights = 1)} %>% 
            extract2("b") %>% 
            c}) %>% 
    {round(.$rs.core,2) - round(.$rs.control, 2)}

g_unadj <- d2[c("rs.core", "rs.control")] %>% 
    map(~{robu(g ~ cluster -1, data = .x, studynum = id.full, var.eff.size = var.g, rho = 0.8) %>% 
            extract2("reg_table") %>% 
            select(b.r, SE) %>% {
                rma(yi = .$b.r, 
                    sei = .$SE, 
                    weights = 1)} %>% 
            extract2("b") %>% 
            c}) %>% 
    {round(.$rs.core,2)}



men_col <- brewer.pal(n = 11, name = "RdYlGn")[2]
fem_col <- brewer.pal(n = 11, name = "RdYlGn")[10]
ovl_col <- brewer.pal(n = 11, name = "RdYlGn")[6]

# RColorBrewer::display.brewer.all()

alp <- 0.5
custom_ovl <- F


arrangeGrob(layout_matrix = matrix(1:2, ncol = 2),
            ggplot(NULL, aes(c(-3.5, 3.5 + g_unadj ))) +
                geom_area(stat = "function", fun = dnorm, fill = fem_col, alpha = alp) +
                geom_area(stat = "function", fun = dnorm, args = list(mean = g_unadj, sd = 1), fill = men_col, alpha = alp) + 
                {if(custom_ovl)geom_area(stat = "function", fun = dnorm, args = list(mean = g_unadj, sd = 1), fill = ovl_col, xlim = c(-3.5, g_unadj / 2 ))} +
                {if(custom_ovl)geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), fill = ovl_col, xlim = c(g_unadj / 2, 3.5 + g_unadj))} +
                theme_bw() +
                ggtitle("A. Unadjusted Mean Gender Difference") +
                xlab("Sex Drive") +
                ylab("Density") + 
                theme(axis.line=element_blank(),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks=element_blank(),
                      # plot.title = element_text(hjust = 0.5),
                      legend.position="none",
                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),plot.background=element_blank()) + 
                geom_segment(aes(y = dnorm(0), x = 0, yend = dnorm(0), xend = g_unadj),  
                             arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")) + 
                annotate(geom="text", x=g_unadj/2, y=dnorm(0) + 0.015, label= paste0("italic('g =')~", rnd_2(g_unadj)), parse = T, size = 3),
            ggplot(NULL, aes(c(-3.5, 3.5 + g_adj ))) +
        geom_area(stat = "function", fun = dnorm, fill = fem_col, alpha = alp) +
        geom_area(stat = "function", fun = dnorm, args = list(mean = g_adj, sd = 1), fill = men_col, alpha = alp) + 
        {if(custom_ovl)geom_area(stat = "function", fun = dnorm, args = list(mean = g_adj, sd = 1), fill = ovl_col, xlim = c(-3.5, g_adj / 2 ))} +
        {if(custom_ovl)geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), fill = ovl_col, xlim = c(g_adj / 2, 3.5 + g_adj))} +
        theme_bw() +
        ggtitle("B: Adjusted Mean Gender Difference") +
        xlab("Sex Drive") +
        ylab("Density") + 
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              # plot.title = element_text(hjust = 0.5),
              legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank()) + 
        geom_segment(aes(y = dnorm(0), x = 0, yend = dnorm(0), xend = g_adj),  
                     arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")) + 
        annotate(geom="text", x=g_adj/2, y=dnorm(0) + 0.015, label= paste0("italic('g =')~", rnd_2(g_adj)), parse = T, size = 3)
    
) %>% ggsave(filename = "results/figures and tables/distribution_overlap.png",
           width = 10, height = 4, units = "in")

shell.exec(paste0(getwd(), "/results/figures and tables/distribution_overlap.png"))


