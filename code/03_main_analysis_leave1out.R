{
    library(tidyverse)
    library(magrittr)
    library(robumeta)
    library(clubSandwich)
    library(flextable)
    library(officer)
    library(jtools)
    
    rm(list = ls())
    
    source("code/functions/rnd.R")
    
    load("results/es_prepared2.Rda")
}

# this takes about two minutes
loo_dat_core <- d2$rs.core_with_outlier %>% 
    {set_names(map(1:nrow(.), function(id)slice(., -id)), .$id.full.out)} %>% 
    map_dfr(.id = "id.full.out", ~ robu(g ~ cluster -1, .x, id.full, var.g)$reg_table) %>% 
    full_join(robu(g ~ cluster -1, d2$rs.core_with_outlier, id.full, var.g)$reg_table %>% 
                  transmute(labels, b.r_true = b.r, SE_true = SE), 
              by = "labels") %>% 
    mutate(g_diff = b.r_true - b.r, se.g_diff = SE_true - SE) 

loo_dat_second <- d2$rs.second %>% 
    {set_names(map(1:nrow(.), function(id)slice(., -id)), .$id.full.out)} %>% 
    map_dfr(.id = "id.full.out", ~ robu(g ~ cluster -1, .x, id.full, var.g)$reg_table) %>% 
    full_join(robu(g ~ cluster -1, d2$rs.second, id.full, var.g)$reg_table %>% 
                  transmute(labels, b.r_true = b.r, SE_true = SE), 
              by = "labels") %>% 
    mutate(g_diff = b.r_true - b.r, se.g_diff = SE_true - SE) 

loo_dat_control <- d2$rs.control %>% 
    {set_names(map(1:nrow(.), function(id)slice(., -id)), .$id.full.out)} %>% 
    map_dfr(.id = "id.full.out", ~ robu(g ~ cluster -1, .x, id.full, var.g)$reg_table) %>% 
    full_join(robu(g ~ cluster -1, d2$rs.control, id.full, var.g)$reg_table %>% 
                  transmute(labels, b.r_true = b.r, SE_true = SE), 
              by = "labels") %>% 
    mutate(g_diff = b.r_true - b.r, se.g_diff = SE_true - SE) 

loo_dat <- rbind(loo_dat_core, loo_dat_second, loo_dat_control)

loo_res <- loo_dat %>%
    group_by(labels) %>%
    summarise(min(g_diff), max(g_diff), min(se.g_diff), max(se.g_diff)) %>%
    mutate_at(vars(-labels), ~ rnd(.x, dec = 4))

# loo_dat %>%
#     inner_join(d2$rs.core_with_outlier %>% select(id.full.out, item), by = "id.full.out") %>%
#     select(id.full.out, labels, item, g_diff, se.g_diff) %>%
#     pivot_wider(id_cols = c("id.full.out", "item"),
#                 names_from = labels,
#                 values_from = c("g_diff", "se.g_diff")) %>%
#     View

(loo_dat %>% 
        mutate(removed = ifelse(id.full.out == "Trudel_Dargis_Villeneuve_2014_1_1" & labels == "clusterAffect.Frequency" |
                                    id.full.out == "Maxwell_Muise_MacDonald_2016_5_12" & labels == "clusterIntercourse.Frequency" |
                                    id.full.out == "McIntyre_Barlow_Hayward_2015_1_1" & labels == "clusterAffect.Intensity",
                                "TRUE", NA)) %>% 
        mutate(labels = recode(labels, "clusterBehavior.Frequency" = "BF", "clusterCognition.Frequency" = "CF", "clusterAffect.Frequency" = "AF",
                               "clusterAffect.Intensity" = "AI", "clusterSelf.Rated.Sex.Drive" = "SRSD",
                               "clusterIntercourse.Frequency" = "SIF", "clusterSex.Partners.in.Last.Year" = "TSPY", 
                               "clusterTotal.One.Night.Stand.Partners" = "ONS", "clusterTotal.Sex.Partners" = "TSP")) %>% 
        mutate(labels = factor(labels, levels = c("CF", "AF", "BF", "AI", "SRSD", "SIF", "ONS", "TSP", "TSPY"))) %>% 
        pivot_longer(cols = c("g_diff", "se.g_diff"), names_to = "type_of_value", values_to = "value") %>% 
        mutate(type_of_value = recode(type_of_value, "g_diff" = "Δg", "se.g_diff" = "ΔSE")) %>% 
        ggplot(aes(y = value, x = labels)) + 
        geom_point() +
        geom_point(aes(y = value, x = labels, shape = removed), size = 3) +
        scale_shape_manual(values = c(4)) +
        guides(shape = F) +
        ylab("Change in value") +
        xlab("Type of Indicator") +
        facet_wrap(~type_of_value, scale = "free") +
        theme(strip.text = element_text(face = "italic")) + 
        theme_apa()) %>%  
    ggsave(filename = "results/figures and tables/leave1out.png", width = 8, height = 5, dpi = 300)

.open_pdf("results/figures and tables/leave1out.png")

save(loo_res, file = "results/loo_res.Rda")
