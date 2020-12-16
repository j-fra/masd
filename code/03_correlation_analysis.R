{
    rm(list = ls())
    
    library(tidyverse)
    library(grid)
    library(gridExtra)
    library(pBrackets)
    
    source("code/functions/rnd.R")
    
    load("results/es_cor.Rda")
}

plt <- es_cor %>% 
    select(label, r, k, m) %>% 
    mutate(label = ifelse(label == "BF_CF", "CF_BF", label)) %>% 
    mutate(label = ifelse(label == "BF_DF", "DF_BF", label)) %>%
    mutate(label = ifelse(label == "SF_TP", "TP_SF", label)) %>%
    mutate(label = ifelse(label == "ONS_P12", "P12_ONS", label)) %>%
    mutate(label = ifelse(label == "ONS_TP", "TP_ONS", label)) %>%
    mutate(label = ifelse(label == "P12_TP", "TP_P12", label)) %>%
    mutate(label = ifelse(label == "P12_SD", "SD_P12", label)) %>%
    mutate(label = ifelse(label == "ONS_SD", "SD_ONS", label)) %>%
    separate(label, c("V1", "V2"), "_", remove = F) %>% 
    mutate(r = round(r, 2)) %>% {
        
        right_join(., unique(c(as.character(.[["V1"]]), as.character(.[["V2"]]))) %>% {
            expand.grid(., .)   
        } %>% {
            data.frame(V1 = .[[1]],
                       V2 = .[[2]],
                       label = paste0(.[[1]], "_",  .[[2]]), stringsAsFactors = F)
        }, by = "label")
        
    } %>%  
    mutate(V1 = as.character(V1.y), V2 = as.character(V2.y)) %>% 
    mutate(across(c(V1, V2), ~ recode(.x, "DF" = "AF", "DI" = "AI"))) %>% 
    mutate(across(c(V1, V2), ~ recode(.x, "SD" = "SRSD", "TP" = "TSP", "P12" = "TSPY", "SF" = "SIF"))) %>% 
    mutate(across(c(V1, V2), ~ factor(as.factor(.x), levels = c("SIF", "ONS", "TSPY", "TSP", "SRSD", "AI", "BF", "AF", "CF")))) %>% 
    mutate(r = ifelse(V1 == V2, 0, r)) %>% 
    mutate(r = ifelse(is.na(r), 0, r)) %>% 
    { ggplot(., aes(V2, V1, fill = as.numeric(r)))+
            geom_tile(color = "white") +
            scale_x_discrete(position = "bottom", limits = rev(levels(.$V2))) +
            scale_y_discrete(position = "left", limits = rev(levels(.$V1))) +
            scale_fill_gradient2(low = "#8CBCB9", high = "#BB513B", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Summary Effect for\nPearson Correlation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(angle = 0, vjust = 1,
                                             size = 12, hjust = 1))+
            coord_fixed()+ 
            geom_text(aes(V2, V1, label = ifelse(r == 0, "", paste0(rnd(r, 2), "\n"))), color = "black", size = 4) +
            geom_text(aes(V2, V1, label = ifelse(r == 0, "", paste0("\n(", k, ", ", m, ")"))), color = "black", size = 3) +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.4, 0.8),
                legend.direction = "horizontal")+
            guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                         title.position = "top", title.hjust = 0.5)) +
            geom_rect(aes(xmin = 1.5, xmax = 5.5, ymin = 0.5, ymax = 4.5),
                      fill = "transparent", colour = "black", linetype = 1 , size = 1.5) +
            # geom_rect(aes(xmin = -7, xmax = -5, ymin = 1+4 - 0.5, ymax = 5+4 + 0.5),
            #           fill = "transparent", colour = "black", linetype = 2 , size = 1.5) +
            theme(plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "in"))
        # geom_segment(aes(x = -5, y = -5, xend = 4, yend = 4))
    }

ggsave(plt, filename = "results/figures and tables/correlation_analysis.png", width = 7, height = 7, units = 'in', dpi = 300)

shell.exec(paste0(getwd(), "/results/figures and tables/correlation_analysis.png"))

