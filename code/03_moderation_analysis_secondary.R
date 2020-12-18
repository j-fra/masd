{
    rm(list = ls())
    library(tidyverse)
    library(magrittr)
    library(robumeta)
    library(clubSandwich)
    library(flextable)
    library(officer)
    library(jtools)
    
    walk(list.files("code/functions/", full.names = T), source)
    
    load("results/es_prepared2.Rda")
    
    mods_info <- readxl::read_xlsx("data/coding/mods_info.xlsx", col_names = T) %>%
        mutate(type_check = map(modvarname, ~ class(d2$rs[[.x]])) %>% flatten_chr,
               type_missmatch = !is.na(type) & !(type == "cat" & type_check == "character" | type == "con" & type_check == "numeric")) 
}

# when(any(mods_info$type_missmatch), . == TRUE ~ stop("There are type miss-matches."))

mod_results_secondary <- mods_info %>% 
    filter(is_moderator) %>% 
    # slice(1:5) %>%
    mutate(cf = map2(modvarname, list(d2$rs.cf), safe_robu, mods_info_dat = mods_info),
           di = map2(modvarname, list(d2$rs.di), safe_robu, mods_info_dat = mods_info),
           sd = map2(modvarname, list(d2$rs.sd), safe_robu, mods_info_dat = mods_info))  %>% 
    select(-contains("regplot")) %>% 
    unnest_wider(cf, names_sep = "_") %>% 
    unnest_wider(di, names_sep = "_") %>% 
    unnest_wider(sd, names_sep = "_") 

mod_results_secondary_longer <- mod_results_secondary %>% {
    list(., 
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, cf_regtable), cf_regtable)), labels = "cf_regtable.labels"),
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, di_regtable), di_regtable)), labels = "di_regtable.labels"),
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, sd_regtable), sd_regtable)), labels = "sd_regtable.labels"))
}  %>% {
    full_join(.[[1]], reduce(.[2:length(.)], full_join, by = c("modvarname", "labels")), by = "modvarname")
} %>% 
    select(-contains("regplot")) %>% 
    filter(labels != "DROP_THIS")



data_for_table <- mod_results_secondary_longer %>% 
    select(-contains("regtable"), -labels) %>% 
    mutate(row_role = "B_mod_results_secondary") %>% 
    group_by(modvarname) %>% 
    summarise_all(first) %>% 
    full_join(mod_results_secondary_longer %>% 
                  select(modvarname, class, labels, contains("regtable")) %>% 
                  mutate(row_role = "C_Reg_Results"), by = c("modvarname", "row_role")) %>% 
    arrange(modvarname, desc(row_role)) %>% 
    mutate(break_cf = " ", 
           break_di = " ", 
           break_sd = " ") %>% 
    select(modvarname, row_role, labels, starts_with("class"),
           break_cf, g_cf_reg = cf_regtable.g, SE_cf_reg = cf_regtable.SE, k_cf_reg = cf_regtable.k, m_cf_reg = cf_regtable.m, t_cf_reg = cf_regtable.t, df_cf_reg = cf_regtable.df, p_cf_reg = cf_regtable.p, HTZ_cf_mod = cf_HTZ, df_cf_mod = cf_df, p_cf_mod = cf_p,  i.sq_cf_mod = cf_i.sq,  tau_cf_mod = cf_tau,
           break_di, g_di_reg = di_regtable.g, SE_di_reg = di_regtable.SE, k_di_reg = di_regtable.k, m_di_reg = di_regtable.m, t_di_reg = di_regtable.t, df_di_reg = di_regtable.df, p_di_reg = di_regtable.p, HTZ_di_mod = di_HTZ, df_di_mod = di_df, p_di_mod = di_p,  i.sq_di_mod = di_i.sq,  tau_di_mod = di_tau,
           break_sd, g_sd_reg = sd_regtable.g, SE_sd_reg = sd_regtable.SE, k_sd_reg = sd_regtable.k, m_sd_reg = sd_regtable.m, t_sd_reg = sd_regtable.t, df_sd_reg = sd_regtable.df, p_sd_reg = sd_regtable.p, HTZ_sd_mod = sd_HTZ, df_sd_mod = sd_df, p_sd_mod = sd_p,  i.sq_sd_mod = sd_i.sq,  tau_sd_mod = sd_tau) %>% 
    mutate_if(is.numeric, round, 3) %>% 
    mutate_all(as.character) %>% 
    full_join(mod_results_secondary_longer %>% 
                  select(-contains("regtable"), -labels) %>% 
                  mutate(row_role = "header") %>% 
                  group_by(class) %>% 
                  summarise_all(~ first(.x, order_by = .x)) %>% 
                  select(class, modvarname) %>% 
                  mutate(row_role = "A_Group_Header"),
              by = c("row_role", "modvarname"))%>% 
    arrange(modvarname, row_role) %>%
    mutate(Moderator = coalesce(class, labels, modvarname)) %>% 
    mutate(group = coalesce(class.x, class.y, class)) %>% 
    select(-starts_with("class")) %>% 
    mutate_all(replace_na, "") %>% 
    select(modvarname, group, row_role, Moderator, everything(), -labels) %>% 
    arrange(group, modvarname, row_role) %>% 
    mutate(Moderator = ifelse(row_role == "C_Reg_Results", 
                              paste0("    ", Moderator %>% simpleCap %>% str_replace_all("\\.", "\\\ ")), 
                              Moderator)) %>% 
    mutate(Moderator = recode(Moderator, !!! setNames(mods_info$label, mods_info$modvarname)), 
           is_significant_cf = ifelse(p_cf_mod == "", F, p_cf_mod %>% paste0("0", .) %>% str_remove_all("<|\\\ ")%>% as.numeric %>% is_less_than(0.05)),
           is_significant_di = ifelse(p_di_mod == "", F, p_di_mod %>% paste0("0", .) %>% str_remove_all("<|\\\ ")%>% as.numeric %>% is_less_than(0.05)),
           is_significant_sd = ifelse(p_sd_mod == "", F, p_sd_mod %>% paste0("0", .) %>% str_remove_all("<|\\\ ")%>% as.numeric %>% is_less_than(0.05)),
           df_less_than_4_cf_param = ifelse(df_cf_reg == "", F, df_cf_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_di_param = ifelse(df_di_reg == "", F, df_di_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_sd_param = ifelse(df_sd_reg == "", F, df_sd_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_cf_test = ifelse(df_cf_mod == "", F, df_cf_mod %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_di_test = ifelse(df_di_mod == "", F, df_di_mod %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_sd_test = ifelse(df_sd_mod == "", F, df_sd_mod %>% as.numeric %>% is_less_than(4)))

typology_for_table <- tibble(col_keys = names(data_for_table)) %>% 
    mutate(cluster = case_when(str_detect(col_keys, "break") ~ "",
                               str_detect(col_keys, "_cf") ~ "Cognition Frequency (uncontrolled)",
                               str_detect(col_keys, "_di") ~ "Affect Intensity",
                               str_detect(col_keys, "_sd") ~ "Self-Rated Sex Drive",
                               T ~ col_keys),
           value = col_keys %>% str_remove_all("_cf|_di|_sd|_reg|_mod|break"))

moderation_analysis_secondary_regtable <- regulartable(data_for_table, 
                                             col_keys =  
                                                 names(data_for_table %>% select(Moderator, 
                                                                                 "break_cf", contains("_cf_reg"), 
                                                                                 "break_di", contains("_di_reg"), 
                                                                                 "break_sd", contains("_sd_reg"))))  %>% 
    set_header_df(mapping = typology_for_table, key = "col_keys" ) %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs() %>% 
    padding(padding.left = 0, padding.right = 0, padding.top = 0, padding.bottom = 0, part = "body") %>%
    height(height = 0.12, part = "body") %>% 
    hrule(rule = "exact") %>% 
    align(align = "right") %>% 
    align(j = 1, align = "left") %>% 
    align(align = "center", part = "header") %>% 
    align(j = 1, align = "left", part = "header") %>% 
    width(width = 0.25) %>% 
    width(width = 0.2, j = c("k_cf_reg", 
                             "k_di_reg", 
                             "k_sd_reg", 
                             "m_cf_reg", 
                             "m_di_reg", 
                             "m_sd_reg")) %>% 
    width(width = 0.3, j = c("g_cf_reg", 
                             "g_di_reg", 
                             "g_sd_reg", 
                             "SE_cf_reg", 
                             "SE_di_reg", 
                             "SE_sd_reg", 
                             "p_cf_reg", 
                             "p_di_reg", 
                             "p_sd_reg")) %>% 
    width(c("break_cf", "break_di", "break_sd"), width = 0.1) %>% 
    width("Moderator", width = 1.7) %>% 
    bold(i = ~ row_role == "A_Group_Header", part = "body") %>% 
    align(i = ~ row_role == "A_Group_Header", part = "body", align = "center") %>% 
    height(i = ~ row_role == "A_Group_Header", part = "body", height = 0.3) %>% 
    italic(i = 2, part = "header") %>% 
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_cf_param == TRUE, j = "p_cf_reg") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_di_param == TRUE, j = "p_di_reg") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_sd_param == TRUE, j = "p_sd_reg") %>%
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note. "), 
        "Meta-regression tables for moderation of the secondary sex drive indicators and cognition frequency (not controlled for item content). ",
        "For categorical moderators, point estimates for subgroups and corresponding significance tests are presented. ",
        "For continuous moderators, values are presented for the intercept and slope. ",
        "Some models could not be fitted because the number of available codings was insufficient. These are left blank. ",
        as_i("g"), " = Hedges' ",  as_i("g"), " effect size. ", 
        as_i("SE"), " = Standard Error for Hedges' ",  as_i("g"), " effect size. ", 
        as_i("k"), " = number of studies per subgroup. ", 
        as_i("m"), " = number of effect sizes per subgroup. ",  
        as_i("t"), "-value from ", as_i("t"), "-test testing the parameter against zero. ",
        as_i("df"), " = small-sample-corrected degrees of freedom. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the ", as_i("t"), "-value and ", as_i("df"), " in the same row. ",
        "Note that if degrees of freedom fall below 4, significance tests are unreliable. ", as_i("p"), "-values for unreliable tests are not reported (N/A). "
    ), part = "footer" ) %>% 
    fontsize(size = 6, part = "all")

moderation_analysis_secondary_tests <- regulartable(data_for_table %>% filter(row_role %in% c("A_Group_Header", "B_mod_results_secondary")), 
                                          col_keys =  
                                              names(data_for_table %>% select(Moderator, 
                                                                              "break_cf", contains("_cf_mod"), 
                                                                              "break_di", contains("_di_mod"), 
                                                                              "break_sd", contains("_sd_mod"))))  %>% 
    set_header_df(mapping = typology_for_table, key = "col_keys" ) %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs() %>% 
    padding(padding = 0.01, part = "body") %>%
    align(align = "right") %>% 
    align(j = 1, align = "left") %>% 
    align(align = "center", part = "header") %>% 
    width(width = 0.3) %>% 
    align(j = 1, align = "left", part = "header") %>% 
    height(height = 0.12, part = "body") %>% 
    hrule(rule = "exact") %>% 
    width(c("df_cf_mod", 
            "df_di_mod", 
            "df_sd_mod"), width = 0.35) %>%
    width(c("i.sq_cf_mod", 
            "i.sq_di_mod", 
            "i.sq_sd_mod"), width = 0.3) %>%
    width(c("HTZ_cf_mod", 
            "HTZ_di_mod", 
            "HTZ_sd_mod"), width = 0.35) %>%
    width(c("break_cf", 
            "break_di", 
            "break_sd"), width = 0.1) %>%
    width("Moderator", width = 1.7) %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = ~ row_role == "A_Group_Header", part = "body") %>% 
    align(i = ~ row_role == "A_Group_Header", part = "body", align = "center") %>% 
    height(i = ~ row_role == "A_Group_Header", part = "body", height = 0.3) %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note."), 
        "Tests for moderation of the secondary sex drive indicators and cognition frequency (not controlled for item content). The tests indicate the significance of the slope for continuous moderator or differences between subgroups for categorical moderators. ",
        "Some models could not be fitted because the number of available codings was insufficient. These are left blank. ",
        as_i("HTZ"), " = Hotelling-", as_i("T"), "-approximated test statistic. ", 
        as_i("df"), " = small-sample-corrected degrees of freedom. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the test statistic and ", as_i("df"), " in the same row. ",
        as_i("I"), as_sup("2"), " = proportion of the variation in observed effects that is due to variation in true effects. ",
        as_chunk("t", fp_text(font.size = 6, italic = T, font.family = "Symbol")), " = estimated standard deviation of the true effects. ",
        "Note that if degrees of freedom fall below 4, significance tests are unreliable. ", as_i("p"), "-values for unreliable tests are not reported (N/A). "
    ), part = "footer" ) %>% 
    compose(value = as_paragraph("I", as_sup("2")), i = 2, j = c("i.sq_cf_mod",
                                                                 "i.sq_di_mod",
                                                                 "i.sq_sd_mod"), part = "header") %>%
    compose(value = as_paragraph("t"), i = 2, j = c("tau_cf_mod",
                                                    "tau_di_mod",
                                                    "tau_sd_mod"), part = "header") %>%
    font(j = c("tau_cf_mod",
               "tau_di_mod",
               "tau_sd_mod"), part = "header", fontname = "Symbol") %>% 
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_cf_test == TRUE, j = "p_cf_mod") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_di_test == TRUE, j = "p_di_mod") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_sd_test == TRUE, j = "p_sd_mod") %>%
    add_header_lines("Tests for Moderation (Secondary Indicators)") %>% 
    add_header_lines("Table S3") %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = 1, part = "header") %>% 
    line_spacing(i = 1:2, space = 2, part = "header") %>% 
    fontsize(size = 6, part = "all") 

title_table_S4 <- flextable(data.frame(" " = NA)) %>% 
    compose(value = as_paragraph(as_i("Regression Tables for Moderation Analyses (Secondary Indicators)")), i = 1, part = "body") %>% 
    compose(value = as_paragraph(as_b("Table S4")), i = 1, part = "header") %>% 
    align(align = "left", part = "all") %>% 
    width(j = 1, width = sum(dim(moderation_analysis_secondary_regtable)$widths)) %>%
    border_remove() %>% 
    line_spacing(i = 1, space = 2, part = "header") %>% 
    fontsize(size = 6, part = "all") 

read_docx() %>% 
    body_add_flextable(value = title_table_S4) %>% 
    body_add_par("") %>% 
    body_add_flextable(value = moderation_analysis_secondary_regtable,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/moderation_analysis_secondary_regtables.docx")
shell.exec(paste0(getwd(), "/results/figures and tables/moderation_analysis_secondary_regtables.docx"))

read_docx() %>% 
    body_add_flextable(value = moderation_analysis_secondary_tests,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/moderation_analysis_tests_secondary.docx")
shell.exec(paste0(getwd(), "/results/figures and tables/moderation_analysis_tests_secondary.docx"))


save(mod_results_secondary, file = "results/mod_results_secondary.Rda")
