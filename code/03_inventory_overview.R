{
    library(tidyverse)
    library(magrittr)
    library(flextable)
    library(officer)
    
    rm(list = ls())

    load("results/es_prepared2.Rda")
    
    source("code/functions/preview_ft_piped.R")
}

inventory_overview_data <- d2$rs %>% 
    mutate(cluster = recode(cluster, "Intercourse Frequency" = "Sexual Intercourse Frequency",
                            "Total One Night Stand Partners" = "Total One-Night Stands",
                            "Sex Partners in Last Year" = "Total Sex Partners in Last Year")) %>% 
    group_by(cluster) %>% 
    mutate(inventory = recode(inventory, "Item not part of an inventory" = NA_character_)) %>% 
    filter(!is.na(inventory)) %>% 
    summarise(Inventories = paste0(unique(inventory), collapse = ", ")) %>% 
    rename("Indicator" = "cluster")


inventory_overview <- regulartable(inventory_overview_data) %>% 
    theme_booktabs(fontsize = 9) %>% 
    align(align = "left", part = "body") %>% 
    valign(valign = "top", part = "body") %>% 
    align(align = "center", part = "header") %>% 
    width("Inventories", 		4.5) %>% 
    width("Indicator", 	1.5) %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(as_i("Note. "), 
                                 'See the supplementary online materials for a complete list of the inventories with references.'), 
            part = "footer") %>% 
    add_header_lines("Overview of Psychometric Inventories") %>% 
    add_header_lines("Table S2") %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = 1, part = "header") %>% 
    line_spacing(i = 1:2, space = 2, part = "header") %>% 
    fontsize(size = 9, part = "all")


read_docx() %>% 
    body_add_flextable(value = inventory_overview,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/inventory_overview.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/inventory_overview.docx"))
    
