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

mod_results <- mods_info %>% 
    filter(is_moderator) %>% 
    mutate(cf = map2(modvarname, list(d2$rs.cf), safe_robu, mods_info_dat = mods_info, cntrl_extrapair = T),
           df = map2(modvarname, list(d2$rs.df), safe_robu, mods_info_dat = mods_info),
           bf = map2(modvarname, list(d2$rs.bf), safe_robu, mods_info_dat = mods_info))  %>% 
    select(-contains("regplot")) %>% 
    unnest_wider(cf, names_sep = "_") %>% 
    unnest_wider(df, names_sep = "_") %>% 
    unnest_wider(bf, names_sep = "_") 

mod_results_longer <- mod_results %>% {
    list(., 
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, cf_regtable), cf_regtable)), labels = "cf_regtable.labels"),
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, df_regtable), df_regtable)), labels = "df_regtable.labels"),
         rename(do.call(what = data.frame, unnest_longer(select(., modvarname, bf_regtable), bf_regtable)), labels = "bf_regtable.labels"))
}  %>% {
    full_join(.[[1]], reduce(.[2:length(.)], full_join, by = c("modvarname", "labels")), by = "modvarname")
} %>% 
    select(-contains("regplot")) %>% 
    filter(labels != "DROP_THIS")


data_for_table <- mod_results_longer %>% 
    select(-contains("regtable"), -labels) %>% 
    mutate(row_role = "B_Mod_Results") %>% 
    group_by(modvarname) %>% 
    summarise_all(first) %>% 
    full_join(mod_results_longer %>% 
                  select(modvarname, class, labels, contains("regtable")) %>% 
                  mutate(row_role = "C_Reg_Results"), by = c("modvarname", "row_role")) %>% 
    arrange(modvarname, desc(row_role)) %>% 
    mutate(break_cf = " ", 
           break_df = " ", 
           break_bf = " ") %>% 
    select(modvarname, row_role, labels, starts_with("class"),
           break_cf, g_cf_reg = cf_regtable.g, SE_cf_reg = cf_regtable.SE, k_cf_reg = cf_regtable.k, m_cf_reg = cf_regtable.m, t_cf_reg = cf_regtable.t, df_cf_reg = cf_regtable.df, p_cf_reg = cf_regtable.p, HTZ_cf_mod = cf_HTZ, df_cf_mod = cf_df, p_cf_mod = cf_p,  i.sq_cf_mod = cf_i.sq,  tau_cf_mod = cf_tau,
           break_df, g_df_reg = df_regtable.g, SE_df_reg = df_regtable.SE, k_df_reg = df_regtable.k, m_df_reg = df_regtable.m, t_df_reg = df_regtable.t, df_df_reg = df_regtable.df, p_df_reg = df_regtable.p, HTZ_df_mod = df_HTZ, df_df_mod = df_df, p_df_mod = df_p,  i.sq_df_mod = df_i.sq,  tau_df_mod = df_tau,
           break_bf, g_bf_reg = bf_regtable.g, SE_bf_reg = bf_regtable.SE, k_bf_reg = bf_regtable.k, m_bf_reg = bf_regtable.m, t_bf_reg = bf_regtable.t, df_bf_reg = bf_regtable.df, p_bf_reg = bf_regtable.p, HTZ_bf_mod = bf_HTZ, df_bf_mod = bf_df, p_bf_mod = bf_p,  i.sq_bf_mod = bf_i.sq,  tau_bf_mod = bf_tau) %>% 
    mutate_if(is.numeric, round, 3) %>% 
    mutate_all(as.character) %>% 
    full_join(mod_results_longer %>% 
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
           is_significant_df = ifelse(p_df_mod == "", F, p_df_mod %>% paste0("0", .) %>% str_remove_all("<|\\\ ")%>% as.numeric %>% is_less_than(0.05)),
           is_significant_bf = ifelse(p_bf_mod == "", F, p_bf_mod %>% paste0("0", .) %>% str_remove_all("<|\\\ ")%>% as.numeric %>% is_less_than(0.05)),
           df_less_than_4_cf_param = ifelse(df_cf_reg == "", F, df_cf_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_df_param = ifelse(df_df_reg == "", F, df_df_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_bf_param = ifelse(df_bf_reg == "", F, df_bf_reg %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_cf_test = ifelse(df_cf_mod == "", F, df_cf_mod %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_df_test = ifelse(df_df_mod == "", F, df_df_mod %>% as.numeric %>% is_less_than(4)),
           df_less_than_4_bf_test = ifelse(df_bf_mod == "", F, df_bf_mod %>% as.numeric %>% is_less_than(4)))

typology_for_table <- tibble(col_keys = names(data_for_table)) %>% 
    mutate(cluster = case_when(str_detect(col_keys, "break") ~ "",
                               str_detect(col_keys, "_df") ~ "Affect Frequency",
                               str_detect(col_keys, "_cf") ~ "Cognition Frequency",
                               str_detect(col_keys, "_bf") ~ "Behavior Frequency",
                               T ~ col_keys),
           value = col_keys %>% str_remove_all("_cf|_df|_bf|_reg|_mod|break"))

moderation_analysis_regtable <- regulartable(data_for_table, 
                       col_keys =  
                           names(data_for_table %>% select(Moderator, 
                                                           "break_cf", contains("_cf_reg"), 
                                                           "break_df", contains("_df_reg"), 
                                                           "break_bf", contains("_bf_reg"))))  %>% 
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
    width(width = 0.2, j = c("k_cf_reg", "k_df_reg", "k_bf_reg", "m_cf_reg", "m_df_reg", "m_bf_reg")) %>% 
    width(width = 0.3, j = c("g_cf_reg", "g_df_reg", "g_bf_reg", "SE_cf_reg", "SE_df_reg", "SE_bf_reg", "p_cf_reg", "p_df_reg", "p_bf_reg")) %>% 
    width(c("break_cf", "break_df", "break_bf"), width = 0.1) %>% 
    width("Moderator", width = 1.7) %>% 
    bold(i = ~ row_role == "A_Group_Header", part = "body") %>% 
    align(i = ~ row_role == "A_Group_Header", part = "body", align = "center") %>% 
    height(i = ~ row_role == "A_Group_Header", part = "body", height = 0.3) %>% 
    italic(i = 2, part = "header") %>% 
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_cf_param == TRUE, j = "p_cf_reg") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_df_param == TRUE, j = "p_df_reg") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_bf_param == TRUE, j = "p_bf_reg") %>%
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note. "), 
        "Meta-regression tables for moderation of the primary sex drive indicators. ",
        "For categorical moderators, point estimates for subgroups and corresponding significance tests are presented. ",
        "For continuous moderators, values are presented for the intercept and slope. ",
        "For cognition frequency, results are statistically controlled for item content (extra-pair partner vs. any partner/not specified). ",
        "Results for the control variable are not reported. ",
        "Some models could not be fitted because the number of available codings was insufficient. These are left blank. ",
        as_i("g"), " = Hedges' ",  as_i("g"), " effect size. ", 
        as_i("SE"), " = Standard error for Hedges' ",  as_i("g"), " effect size. ", 
        as_i("k"), " = number of studies per subgroup. ", 
        as_i("m"), " = number of effect sizes per subgroup. ",  
        as_i("t"), "-value from ", as_i("t"), "-test testing the parameter against zero. ",
        as_i("df"), " = small sample corrected degrees of freedom. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the ", as_i("t"), "-value and ", as_i("df"), " in the same row. ",
        "Note that if degrees-of-freedom fall below 4, significance tests are unreliable. ", as_i("p"), "-values for unreliable tests are not reported (N/A). "
    ), part = "footer" ) %>% 
    fontsize(size = 6, part = "all") 

moderation_analysis_tests <- regulartable(data_for_table %>% filter(row_role %in% c("A_Group_Header", "B_Mod_Results")), 
                       col_keys =  
                           names(data_for_table %>% select(Moderator, 
                                                           "break_cf", contains("_cf_mod"), 
                                                           "break_df", contains("_df_mod"), 
                                                           "break_bf", contains("_bf_mod"))))  %>% 
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
    width(c("df_cf_mod", "df_df_mod", "df_bf_mod"), width = 0.35) %>%
    width(c("i.sq_cf_mod", "i.sq_df_mod", "i.sq_bf_mod"), width = 0.3) %>%
    width(c("HTZ_cf_mod", "HTZ_df_mod", "HTZ_bf_mod"), width = 0.35) %>%
    width(c("break_cf", "break_df", "break_bf"), width = 0.1) %>%
    width("Moderator", width = 1.7) %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = ~ row_role == "A_Group_Header", part = "body") %>% 
    align(i = ~ row_role == "A_Group_Header", part = "body", align = "center") %>% 
    height(i = ~ row_role == "A_Group_Header", part = "body", height = 0.3) %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note."), 
        "Tests for moderation of the primary sex drive indicators. The tests indicate significance of the slope for continuous moderators or differences between subgroups for categorical moderators. ",
        "For cognition frequency, the results are statistically controlled for item content (extra-pair partner vs. any partner/not specified). ",
        "Results for the control variable are not reported. ",
        "Some models could not be fitted because the number of available codings was insufficient. These are left blank. ",
        as_i("HTZ"), " = Hotelling-", as_i("T"), "-approximated test statistic. ", 
        as_i("df"), " = small-sample-corrected degrees of freedom. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the test statistic and ", as_i("df"), " in the same row. ",
        as_i("I"), as_sup("2"), " = proportion of the variation in observed effects that is due to variation in true effects. ",
        as_chunk("t", fp_text(font.size = 6, italic = T, font.family = "Symbol")), " = estimated standard deviation of the true effects. ",
        "Note that if degrees of freedom fall below 4, significance tests are unreliable. ", as_i("p"), "-values for unreliable tests are not reported (N/A). "
    ), part = "footer" ) %>% 
    compose(value = as_paragraph("I", as_sup("2")), i = 2, j = c("i.sq_cf_mod","i.sq_df_mod","i.sq_bf_mod"), part = "header") %>%
    compose(value = as_paragraph("t"), i = 2, j = c("tau_cf_mod","tau_df_mod","tau_bf_mod"), part = "header") %>%
    font(j = c("tau_cf_mod","tau_df_mod","tau_bf_mod"), part = "header", fontname = "Symbol") %>% 
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_cf_test == TRUE, j = "p_cf_mod") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_df_test == TRUE, j = "p_df_mod") %>%
    compose(value = as_paragraph("N/A"), i = ~ df_less_than_4_bf_test == TRUE, j = "p_bf_mod") %>%
    add_header_lines("Tests for Moderation (Primary Indicators)") %>% 
    add_header_lines("Table 3") %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = 1, part = "header") %>% 
    line_spacing(i = 1:2, space = 2, part = "header") %>% 
    fontsize(size = 6, part = "all")

title_table_4 <- flextable(data.frame(" " = NA)) %>% 
    compose(value = as_paragraph(as_i("Regression Tables for Moderation Analyses (Primary Indicators)")), i = 1, part = "body") %>% 
    compose(value = as_paragraph(as_b("Table 4")), i = 1, part = "header") %>% 
    align(align = "left", part = "all") %>% 
    width(j = 1, width = 7.4) %>%
    border_remove() %>% 
    line_spacing(i = 1, space = 2, part = "header") %>% 
    fontsize(size = 6, part = "all") 

read_docx() %>% 
    body_add_flextable(value = title_table_4) %>% 
    body_add_par("") %>% 
    body_add_flextable(value = moderation_analysis_regtable,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/moderation_analysis_regtable.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/moderation_analysis_regtable.docx"))


read_docx() %>% 
    body_add_flextable(value = moderation_analysis_tests,
                       split = TRUE) %>% 
    print(target = "results/figures and tables/moderation_analysis_tests.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/moderation_analysis_tests.docx"))

save(mod_results, file = "results/mod_results.Rda")
