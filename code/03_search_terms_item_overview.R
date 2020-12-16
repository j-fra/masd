{
    library(tidyverse)
    library(flextable)
    library(officer)
    
    rm(list = ls())
    
    source("code/functions/preview_ft_piped.R")
}

# Table 1. Overview of included items. ---------------------------------------------------------------------------

item_overview_data <- readxl::read_xlsx("results/figures and tables/Template_item_overview.xlsx")
item_overview <- regulartable(item_overview_data %>% rename("Abbr." = "Abbreviation")) %>% 
    align(align = "left", part = "body") %>% 
    valign(valign = "top", part = "body") %>% 
    align(align = "center", part = "header") %>% 
    width("Item", width = 2) %>% 
    width("Abbr.", width = 0.5) %>% 
    width("Role", width = 1.7) %>% 
    width("Example", width = 2.4) %>% 
    fontsize(size = 8, part = "all") %>% 
    padding(padding = 0) %>% 
    hrule(rule = "exact") %>% 
    height(height = 0.3) 

read_docx() %>% 
    body_add_flextable(value = item_overview,
                       split = TRUE) %>%
    print(target = "results/figures and tables/item_overview.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/item_overview.docx"))

# Table 2. Search terms. -----------------------------------------------------------------------------------------

search_terms_data <- readxl::read_xlsx("results/figures and tables/Template_search_terms.xlsx")  %>% 
    mutate_all(~ ifelse(is.na(.x), "", .x))

search_terms <- regulartable(search_terms_data) %>% 
    theme_booktabs(fontsize = 9) %>% 
    hrule(rule = "exact") %>% 
    height(height = 0.2) %>% 
    align(align = "left", part = "body") %>% 
    align(align = "left", part = "header") %>% 
    width("Domain", width = 0.7) %>% 

    # padding(padding = 0) %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(as_i("Note. "), 'Search terms for each domain were composed as "One of term 1 AND one of term 2 NOT one of term 3". '), part = "footer") %>% 
    fontsize(size = 9, part = "all")

read_docx() %>% 
    body_add_flextable(value = search_terms,
                       split = TRUE) %>%
    print(target = "results/figures and tables/search_terms.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/search_terms.docx"))
