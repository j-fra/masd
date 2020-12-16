{
    rm(list = ls())
    library(tidyverse)
    library(magrittr)
    library(flextable)
    library(officer)
    options(stringsAsFactors=FALSE)
    
    walk(list.files("code/functions/", full.names = T), source)
    
    load("results/es_prepared2.Rda")
}

all_items<- d2$rs %>% 
    mutate(Facet = cluster,
           scale = str_replace_all(scale, "\n", ", "),
           item_scale = paste0(inventory.abbr, ": ", item, " [", scale, "]")) %>% 
    arrange(item_scale) %>% 
    group_by(Facet) %>% 
    summarize(Items = paste0(paste0("---", unique(item_scale)), collapse = "\n")) %>% 
    regulartable %>% 
    theme_booktabs(fontsize = 8) %>% 
    fix_border_issues %>% 
    align(align = "left") %>% 
    align(align = "center", part = "header") %>% 
    valign(valign = "top") %>% 
    width("Facet", width = 1.2) %>% 
    width("Items", width = 9) 

read_docx() %>% 
    body_add_flextable(value = all_items,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/all_items_table.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/all_items_table.docx"))
