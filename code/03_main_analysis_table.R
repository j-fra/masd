{
    library(tidyverse)
    library(magrittr)
    library(metafor)
    library(robumeta)
    library(clubSandwich)
    library(flextable)
    library(officer)
    
    rm(list = ls())
    
    walk(list.files("code/functions/", full.names = T), source)
    
    load("results/es_prepared2.Rda")
}

safe_robu_main <- function(mod, dat){
    tryCatch(
        {
            # mod = "publication.status"
            # mod = "scale.range"
            # dat = d2$rs.bf
            model1 <- robu(formula(paste0("g~", mod)), data = dat, studynum = id.full, var.eff.size = var.g, rho = 0.8)
            model2 <- robu(formula(paste0("g~", mod, "-1")), data = dat, studynum = id.full, var.eff.size = var.g, rho = 0.8)
            wald <- Wald_test(model1, 2:length(model1$reg_table$labels), vcov = "CR2")
            
            list(mod.i.sq = model1$mod_info$I.2 %>% extract2(1) %>% rnd_2,
                 mod.tau = model1$mod_info$tau %>% extract2(1) %>% sqrt %>% rnd_2,
                 mod.Statistic = wald$Fstat %>% rnd_2,
                 mod.df = wald$df %>% rnd_2,
                 mod.p = wald$p_val %>% rnd_p,
                 reg_table = model2$reg_table %>% 
                     mutate(labels = str_replace(labels, mod, "")) %>% 
                     full_join(model2$data %>% 
                                   filter(!is.na(get(mod))) %>% 
                                   group_by(get(mod)) %>% 
                                   dplyr::summarize(m = n(), k = n_distinct(id.full)) %>% 
                                   rename("labels" = "get(mod)")%>% 
                                   mutate(labels = str_replace_all(labels, " ", ".")) %>% 
                                   mutate(labels = str_replace_all(labels, "-", ".")), by = "labels") %>%
                     select(-sig) %>% 
                     transmute(labels = labels, 
                               g = b.r %>% rnd_2, 
                               SE = SE %>% rnd_2, 
                               t = t %>% rnd_2, 
                               df = dfs %>% rnd_2, 
                               p = prob %>% rnd_p, 
                               CI95 = paste0("[", rnd_2(CI.L), ", ", rnd_2(CI.U), "]"), 
                               U3 = b.r %>% pnorm %>% rnd_2, 
                               OVL = (2 * pnorm((-abs(b.r)) / 2)) %>% rnd_2,
                               CL = pnorm(b.r / sqrt(2)) %>% rnd_2,
                               m = m %>% as.character, 
                               k = k %>% as.character),
                 err = NA)
        },
        error=function(cond)list(mod.i.sq = NA_character_, mod.tau = NA_character_, mod.Statistic = NA_character_, mod.df = NA_character_, mod.p = NA_character_, 
                                 reg_table = data.frame(labels=NA_character_,g=NA_character_,SE=NA_character_,t=NA_character_,df=NA_character_,p=NA_character_,CI95=NA_character_,U3 = NA_character_, OVL = NA_character_, CL = NA_character_, m=NA_character_,k=NA_character_), 
                                 err = paste0("Error fitting model for ", mod, ". ", cond)))
}

single_es_data_adj <- list(robu(g ~ cluster -1, data = d2$rs.core, studynum = id.full, var.eff.size = var.g, rho = 0.8),
     robu(g ~ cluster -1, data = d2$rs.control, studynum = id.full, var.eff.size = var.g, rho = 0.8)) %>% 
    map(function(x){x %>% extract2("reg_table") %>% 
            select(b.r, SE) %>% {
                rma(yi = .$b.r, 
                    sei = .$SE, 
                    weights = 1)}}) %>% {
                        rbind(c("Primary Sex Drive Indicators\n(Adjusted Global Summary Effect)", 
                                rep("", 17)), 
                                # .[[1]]$tau2 %>% rnd_2,
                                # .[[1]]$I2 %>% rnd_2), 
                              data.frame(
                                  Role = "",
                                  Indicator = "Primary Indicators (Adjusted)",
                                  g = (round(.[[1]]$b, 2) - round(.[[2]]$b, 2)) %>% rnd_2,
                                  SE = "", 
                                  t = "",
                                  df = "",
                                  p = "",
                                  CI95 = "",
                                  U3 = (.[[1]]$b - .[[2]]$b) %>% pnorm %>% rnd_2,
                                  OVL = (2 * pnorm((-abs(.[[1]]$b - .[[2]]$b)) / 2)) %>% rnd_2,
                                  CL = pnorm((.[[1]]$b - .[[2]]$b) / sqrt(2)) %>% rnd_2,
                                  k = length(unique(d2$rs.core$id.full)),
                                  m = nrow(d2$rs.core),
                                  F = "", 
                                  df.F = "",
                                  p.F = "", 
                                  tau = "",  
                                  i.sq = "", stringsAsFactors = F))
                    }


single_es_data <- robu(g ~ cluster -1, data = d2$rs.core, studynum = id.full, var.eff.size = var.g, rho = 0.8) %>% 
    extract2("reg_table") %>% 
    select(b.r, SE) %>% {
        rma(yi = .$b.r, 
            sei = .$SE, 
            weights = c(1,1,1))} %>% {
                rbind(c("Primary Sex Drive Indicators\n(Global Summary Effect)", 
                        rep("", 15), 
                        .$tau2 %>% sqrt %>% rnd_2,
                        .$I2 %>% rnd_2), 
                      data.frame(
                          Role = "",
                          Indicator = "Primary Indicators",
                          g = .$b %>% rnd_2,
                          SE = .$se %>% rnd_2, 
                          t = .$zval %>% rnd_2,
                          df = "",
                          p = .$pval %>% rnd_p,
                          CI95 = paste0("[", rnd_2(.$ci.lb), ", ", rnd_2(.$ci.ub), "]"),
                          U3 = .$b %>% pnorm %>% rnd_2,
                          OVL = (2 * pnorm((-abs(.$b)) / 2)) %>% rnd_2,
                          CL = pnorm(.$b / sqrt(2)) %>% rnd_2,
                          k = length(unique(d2$rs.core$id.full)),
                          m = nrow(d2$rs.core),
                          F = "", 
                          df.F = "",
                          p.F = "", 
                          tau = "",  
                          i.sq = "", stringsAsFactors = F))
            }

single_es_data_control <- robu(g ~ cluster -1, data = d2$rs.control, studynum = id.full, var.eff.size = var.g, rho = 0.8) %>% 
    extract2("reg_table") %>% 
    select(b.r, SE) %>% {
        rma(yi = .$b.r, 
            sei = .$SE, 
            weights = c(1,1,1,1))} %>% {
                rbind(c("Bias Indicators\n(Global Summary Effect)", 
                        rep("", 15), 
                        .$tau2 %>% sqrt %>% rnd_2,
                        .$I2 %>% rnd_2), 
                      data.frame(
                          Role = "",
                          Indicator = "Bias Indicators",
                          g = .$b %>% rnd_2,
                          SE = .$se %>% rnd_2, 
                          t = .$zval %>% rnd_2,
                          df = "",
                          p = .$pval %>% rnd_p,
                          CI95 = paste0("[", rnd_2(.$ci.lb), ", ", rnd_2(.$ci.ub), "]"),
                          U3 = .$b %>% pnorm %>% rnd_2,
                          OVL = (2 * pnorm((-abs(.$b)) / 2)) %>% rnd_2,
                          CL = pnorm(.$b / sqrt(2)) %>% rnd_2,
                          k = length(unique(d2$rs.control$id.full)),
                          m = nrow(d2$rs.control),
                          F = "", 
                          df.F = "",
                          p.F = "", 
                          tau = "",  
                          i.sq = "", stringsAsFactors = F))}

cluster_es_data <- tibble(
    cluster = c("Primary Sex Drive Indicators", "Secondary Sex Drive Indicators", "Bias Indicators"),
    es_data = list(d2$rs.core, d2$rs.second, d2$rs.control)) %>% 
    mutate(out = map(es_data, safe_robu_main, mod = "cluster")) %>% 
    select(-es_data) %>% 
    unnest_wider(out) %>% 
    mutate(reg_table = map(reg_table, ~ split(.x, seq(nrow(.x))) %>% unname)) %>%
    unnest_longer(reg_table) %>% 
    unnest_wider(reg_table) %>% 
    mutate(row_role = "B_Reg_Results") %>% 
    full_join(group_by(., cluster) %>% 
                  summarize_all(first) %>% 
                  select(cluster, starts_with("mod.")) %>% 
                  mutate(row_role = "A_Header"), 
              by = c("cluster", "row_role")) %>% 
    mutate(order_id = case_when(str_detect(cluster, "Primary") ~ 1,
                                str_detect(cluster, "Second") ~ 2,
                                str_detect(cluster, "Bias") ~ 3)) %>% 
    arrange(order_id, row_role) %>% 
    mutate(Role = cluster, Indicator = str_replace_all(labels, "\\.", " ")) %>% 
    select(Role, Indicator, g, SE, t, df, p, CI95, U3, OVL, CL, k, m, F = mod.Statistic.y, 
           df.F = mod.df.y, p.F = mod.p.y, tau = mod.tau.y, i.sq = mod.i.sq.y) %>% 
    mutate(Role = ifelse(is.na(Indicator), Role, "")) %>% 
    mutate_all(replace_na, "")

main_analysis_data <- rbind(single_es_data_adj, single_es_data, single_es_data_control, cluster_es_data)


main_results <- main_analysis_data %>% 
    mutate(Role = ifelse(Role == "", lag(Role), Role)) %>% 
    mutate(Role = ifelse(Role == "", lag(Role), Role)) %>% 
    mutate(Role = ifelse(Role == "", lag(Role), Role))

save(main_results, file = "results/main_results.Rda")

typology <- data.frame(
    col_keys = names(main_analysis_data),
    what = c("Role", "Indicator", rep("Summary Effect", 9), "k", "m", 
             rep("Test of Moderation", 3),  "tau", "i.sq"),
    value = c( "Role", "Indicator", "g", "SE", "t", "df", "p", "CI95", "U₃", "OVL","CL",
               "k", "m", "HTZ", "df", "p",  "tau", "i.sq"),
    stringsAsFactors = FALSE)

main_analysis_table <- regulartable(main_analysis_data) %>% 
    set_header_df(mapping = typology, key = "col_keys" ) %>%
    align(align = "left", part = "body") %>% 
    align(align = "center", part = "header") %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs %>% 
    fix_border_issues %>% 
    padding(padding = 0) %>% 
    align(align = "right") %>% 
    align(j = 1:2, align = "left") %>% 
    align(align = "center", part = "header") %>% 
    align(j = 1:2, align = "left", part = "header") %>% 
    width(width = 0.42) %>% 
    width("Role", 		1.8) %>% 
    width("Indicator", 	1.5) %>%
    # width("g", 			0.5) %>% 
    # width("SE", 		0.5) %>% 
    # width("t", 			0.5) %>% 
    # width("df", 		0.7) %>% 
    # width("p", 			0.5) %>%
    width("CI95", 		0.8) %>%
    width("U3", 		0.35) %>%
    width("OVL", 		0.4) %>%
    width("CL", 		0.35) %>%
    # width("k", 			0.5) %>% 
    # width("m", 			0.5) %>% 
    # width("F", 			0.7) %>% 
    # width("df.F",		0.45) %>% 
    # width("p.F",		0.5) %>%
    # width("tau",		0.5) %>% 
    # width("i.sq",		0.5) %>% 
    height(height = 0.15) %>% 
    height(i = ~ Role != "", height = 0.3) %>% 
    hrule(rule = "exact") %>% 
    italic(i = 1, j = c("k", "m", "tau", "i.sq"), part = "header") %>% 
    italic(i = 2, j = c("g","SE","t","df","p", "CI95", "U3", "OVL", "CL", "k","m","F","df.F","p.F","tau","i.sq"), part = "header") %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note. "), "Global and group-wise summary results for gender differences in primary sex drive indicators, secondary sex drive indicators, and bias indicators. ", 
        as_i("g"), " = Hedges' ",  as_i("g"), " effect size. ", 
        as_i("SE"), " = standard error associated with the ", as_i("g"), "-value in the same row. ", 
        as_i("t"), " = ", as_i("t"), "-value associated with the ", as_i("g"), "-value in the same row. ", 
        as_i("df"), " = degrees-of-freedom associated with the ", as_i("g"), "-value in the same row. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the ", as_i("g"), "-value in the same row. ", 
        as_i("CI95"), " = 95% confidence interval. ",
        as_i("U"), as_sub("3"), " = Cohen's ", as_i("U"), as_sub("3"), " effect size of non-overlap. ", 
        as_i("OVL"), " = overlap effect size. ", 
        as_i("CL"), " = Common-language effect size, or probability of superiority. ", 
        as_i("k"), " = number of studies per subgroup/total. ", 
        as_i("m"), " = number of effect sizes per subgroup/total. ",
        as_i("HTZ"), " = Hotelling-", as_i("T"), "-approximated test statistic. ",  
        as_i("df"), " = small sample corrected degrees of freedom. ", 
        as_i("p"), " = ", as_i("p"), "-value associated with the test statistic and ", as_i("df"), " in the same row. ",  
        as_i("I"), as_sup("2"), " = proportion of the variation in observed effects that is due to variation in true effects. ",
        as_chunk("t", fp_text(font.size = 6, italic = T, font.family = "Symbol")), " = estimated standard deviation of the true effects. "
    ), part = "footer" ) %>% 
    compose(value = as_paragraph("U", as_sub("3")), i = 1:2, j = "U3", part = "header") %>% 
    compose(value = as_paragraph("CI", as_sub("95")), i = 1:2, j = "CI95", part = "header") %>% 
    compose(value = as_paragraph("I", as_sup("2")), i = 1:2, j = "i.sq", part = "header") %>%
    compose(value = as_paragraph("t"), i = 1:2, j = "tau", part = "header") %>%
    font(j = "tau", part = "header", fontname = "Symbol") %>% 
    add_header_lines("Main Results") %>% 
    add_header_lines("Table 2") %>% 
    italic(i = 2, part = "header") %>% 
    bold(i = 1, part = "header") %>% 
    line_spacing(i = 1:2, space = 2, part = "header") %>% 
    fontsize(size = 8, part = "all") 

read_docx() %>% 
    body_add_flextable(value = main_analysis_table,
                       split = TRUE) %>% 
    body_end_section_landscape() %>%
    print(target = "results/figures and tables/main_analysis_table.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/main_analysis_table.docx"))

save(main_analysis_table, file = "results/figures and tables/main_analysis_table.Rda")
