{
    library(tidyverse)
    library(flextable)
    library(officer)
    
    rm(list = ls())
    
    mlist <- readxl::read_xlsx("data/coding/mods_info.xlsx")
    
    load("results/es_prepared2.Rda")
    
    walk(list.files("code/functions/", full.names = T), source)
}

get_overview <- function(mod, dat){
    
    # mod <- mlist$modvarname[1]
    # mod <- mlist$modvarname[2]
    # dat <- d2$rs
    
    dat %>% 
        select(mod) %>% 
        summarise_all(list(m = ~ sum(!is.na(.)),
                           Complete = ~ (sum(!is.na(.)) / length(.) * 100) %>% rnd(0) %>% paste0("%"),
                           Distribution = ~ get_dist(.)))
}


moderator_overview_data <- mlist %>% 
    filter(is_moderator) %>% 
    transmute(label, 
              modvarname, 
              class,
              Total = map(modvarname, get_overview, dat = d2$rs),
              cf = map(modvarname, get_overview, dat = d2$rs.cf),
              df = map(modvarname, get_overview, dat = d2$rs.df),
              bf = map(modvarname, get_overview, dat = d2$rs.bf)) %>%
    unnest_wider(Total, names_sep = "_") %>% 
    unnest_wider(cf, names_sep = "_") %>%
    unnest_wider(df, names_sep = "_") %>%
    unnest_wider(bf, names_sep = "_") %>%
    mutate(row_role = "B") %>% 
    full_join(tibble(class = unique(.$class)) %>% mutate(row_role = "A"), by = c("class", "row_role")) %>% 
    arrange(class, row_role) %>% 
    mutate(Moderator = coalesce(label, class),
           break1 = "", break2 = "", break3 = "", break4 = "") %>% 
    select(Moderator, break1, starts_with("Total_"), break2, starts_with("cf_"), break3, starts_with("df_"), break4, everything(), 
           -modvarname, -class, -label, -Total_Distribution) %>% 
    mutate_all(replace_na, "")



typology <- tibble(
    col_keys = names(moderator_overview_data),
    what = c("Moderator", "", "Total", "Total", "",
             rep("Cognition Frequency", 3),"",
             rep("Affect Frequency", 3),"",
             rep("Behavior Frequency", 3),
             ""),
    value = c("Moderator", "", "m", "Compl.", rep(c("", "m", "Compl.", "Distribution"), 3), ""))

moderator_overview <- regulartable(moderator_overview_data,
                       col_keys = moderator_overview_data %>% select(-row_role) %>% names) %>% 
    set_header_df(mapping = typology, key = "col_keys" ) %>%
    align(align = "center", part = "header") %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs %>% 
    padding(padding = 0) %>%
    align(align = "right") %>% 
    align(align = "center", part = "header") %>% 
    align(j = 1:2, align = "left", part = "header") %>% 
    align(align = "left", part = "body") %>% 
    align(align = "center", j = c("Total_m", "cf_m", "df_m", "bf_m",
                                 "Total_Complete", "cf_Complete", "df_Complete", "bf_Complete")) %>% 
    valign(valign = "top", part = "body") %>% 
    width("Moderator", 		1.2) %>% 
    width("Total_Complete", 0.5) %>% 
    width("Total_m", 		0.35) %>% 
    width("break1", 		0.1) %>% 
    width("cf_Complete", 	0.5) %>% 
    width("cf_m", 		 	0.35) %>% 
    width("cf_Distribution",1.7) %>% 
    width("break2", 		0.1) %>% 
    width("df_Complete", 	0.5) %>% 
    width("df_m", 		 	0.35) %>% 
    width("df_Distribution",1.7) %>% 
    width("break3", 		0.1) %>% 
    width("bf_Complete", 	0.5) %>% 
    width("bf_m", 		 	0.35) %>% 
    width("bf_Distribution",1.7)  %>%
    width("break4", 0.1)  %>%
    height(i = ~ row_role == "A", height = 0.4) %>% 
    hrule(rule = "atleast") %>% 
    bold(i = ~ row_role == "A", part = "body") %>% 
    align(i = ~ row_role == "A", part = "body", align = "center")%>% 
    valign(i = ~ row_role == "A", part = "body", valign = "center")%>% 
    italic(i = 2, c("Total_m", "cf_m", "df_m", "bf_m"), part = "header") %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note. "),
        as_i("m"), ": Absolute number of effect sizes for which the corresponding characteristic could be coded. ",
        "Compl.: Percentage of effect sizes for which the corresponding characteristic could be coded. ", 
        "Distribution: Information about the distribution of the coded characteristics. ", 
        "For categorical characteristics, the number of effect sizes per subgroup is reported. ",
        "For continuous characteristics, Q are quartiles (minimum, 25% quartile, median, 75% quartile, maximum), M is the mean, and SD is the standard deviation. ",
        "Note that summaries for continous moderators are computed on the effect size level for this table. In the results section, some of this information was presented on the level of individual participants (i.e., as summaries weighted by sample size). Some values may therefore differ."
        ), part = "footer" ) %>% 
    fontsize(size = 7, part = "all")
    
read_docx() %>% 
    body_add_flextable(value = moderator_overview,
                       split = TRUE) %>%
    body_end_section_landscape() %>% 
    print(target = "results/figures and tables/moderator_overview.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/moderator_overview.docx"))
