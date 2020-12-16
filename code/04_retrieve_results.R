{
  rm(list = ls())
  
  library(tidyverse)
  library(magrittr)
  library(flextable)
  library(officedown)
  library(openxlsx)
  library(readxl)
  
  source("code/functions/rnd.R")
  source("code/functions/get_n_per_nation.R")
  
  
  lapply(paste0("results/", 
                list.files("results/", pattern = "Rda")), 
         load, envir = globalenv())
  
  mnn <- main_results
  
  lapply(paste0("results/figures and tables/", 
                list.files("results/figures and tables/", pattern = "Rda")), 
         load, envir = globalenv())
  
  png_list <- list.files("results/figures and tables/", pattern = "png") %>% {
    set_names(as.list(paste0(getwd(), "/results/figures and tables/", .)), str_remove(., ".png"))
  }
  
}


l <- list(
  total.n = d2$rs %>% 
    group_by(id.full) %>% 
    summarize(n = max(n.total)) %>%
    summarize(n = sum(n)) %>% 
    {format(round(as.numeric(.), 1), nsmall=0, big.mark=",")},
  total.m = d2$rs %>% nrow,
  total.k = d2$rs %>% summarize(n_distinct(id.full)),
  core.m = d2$rs.core %>% nrow,
  core.k = d2$rs.core %>% summarize(n_distinct(id.full)) %>% as.character,
  core.n = d2$rs.core %>% 
    group_by(id.full) %>% 
    summarize(n = max(n.total)) %>%
    summarize(n = sum(n)) %>% 
    {format(round(as.numeric(.), 1), nsmall=0, big.mark=",")},
  secondary.m = d2$rs.second %>% nrow,
  secondary.k = d2$rs.second %>% summarize(n_distinct(id.full)) %>% as.character,
  secondary.n = d2$rs.second %>% 
    group_by(id.full) %>% 
    summarize(n = max(n.total)) %>%
    summarize(n = sum(n)) %>% 
    {format(round(as.numeric(.), 1), nsmall=0, big.mark=",")},
  control.m = d2$rs.control %>% nrow,
  control.k = d2$rs.control %>% summarize(n_distinct(id.full)) %>% as.character,
  control.n = d2$rs.control %>% 
    group_by(id.full) %>% 
    summarize(n = max(n.total)) %>%
    summarize(n = sum(n)) %>% 
    {format(round(as.numeric(.), 1), nsmall=0, big.mark=",")},
  mean.year.core = mean(d2$rs.core$year, na.rm = T) %>% rnd(0),
  median.year.core = median(d2$rs.core$year, na.rm = T) %>% rnd(0),
  min.year.core = min(d2$rs.core$year, na.rm = T) %>% rnd(0),
  max.year.core = max(d2$rs.core$year, na.rm = T) %>% rnd(0),
  sex.journal.perc = mean(d2$rs.core$sex.journal == "yes", na.rm = T),
  core.global.g.adj = ((mnn$g[mnn$Indicator == "Primary Indicators"] %>% as.numeric) - 
                         (mnn$g[mnn$Indicator == "Bias Indicators"] %>% as.numeric)) %>% rnd(2),
  core.global.g = mnn$g[mnn$Indicator == "Primary Indicators"],
  core.global.CI95 = mnn$CI95[mnn$Indicator == "Primary Indicators"],
  control.global.g = mnn$g[mnn$Indicator == "Bias Indicators"],
  control.global.CI95 = mnn$CI95[mnn$Indicator == "Bias Indicators"],
  core.bf.g = mnn$g[mnn$Indicator == "Behavior Frequency"],
  core.df.g = mnn$g[mnn$Indicator == "Affect Frequency"],
  core.cf.g = mnn$g[mnn$Indicator == "Cognition Frequency"],
  core.isq = mnn$i.sq[mnn$Role == "Primary Sex Drive Indicators" & mnn$Indicator == ""],
  core.tau = mnn$tau[mnn$Role == "Primary Sex Drive Indicators" & mnn$Indicator == ""],
  second.di.g = mnn$g[mnn$Indicator == "Affect Intensity"],
  second.sd.g = mnn$g[mnn$Indicator == "Self Rated Sex Drive"],
  second.isq = mnn$i.sq[mnn$Role == "Secondary Sex Drive Indicators" & mnn$Indicator == ""],
  second.tau = mnn$tau[mnn$Role == "Secondary Sex Drive Indicators" & mnn$Indicator == ""],
  core.bf.CI95 = mnn$CI95[mnn$Indicator == "Behavior Frequency"],
  core.df.CI95 = mnn$CI95[mnn$Indicator == "Affect Frequency"],
  core.cf.CI95 = mnn$CI95[mnn$Indicator == "Cognition Frequency"],
  second.di.CI95 = mnn$CI95[mnn$Indicator == "Affect Intensity"],
  second.sd.CI95 = mnn$CI95[mnn$Indicator == "Self Rated Sex Drive"],
  control.sf.g = mnn$g[mnn$Indicator == "Intercourse Frequency"],
  control.sf.CI95 = mnn$CI95[mnn$Indicator == "Intercourse Frequency"],
  control.p12.g = mnn$g[mnn$Indicator == "Sex Partners in Last Year"],
  control.p12.CI95 = mnn$CI95[mnn$Indicator == "Sex Partners in Last Year"],
  control.ons.g = mnn$g[mnn$Indicator == "Total One Night Stand Partners"],
  control.ons.CI95 = mnn$CI95[mnn$Indicator == "Total One Night Stand Partners"],
  control.tp.g = mnn$g[mnn$Indicator == "Total Sex Partners"],
  control.tp.CI95 = mnn$CI95[mnn$Indicator == "Total Sex Partners"],
  control.isq = mnn$i.sq[mnn$Role == "Bias Indicators" & mnn$Indicator == ""],
  control.tau = mnn$tau[mnn$Role == "Bias Indicators" & mnn$Indicator == ""],
  core.F = mnn$F[mnn$Role == "Primary Sex Drive Indicators" & mnn$Indicator == ""],
  core.df.F = mnn$df.F[mnn$Role == "Primary Sex Drive Indicators" & mnn$Indicator == ""],
  core.p.F = mnn$p.F[mnn$Role == "Primary Sex Drive Indicators" & mnn$Indicator == ""],
  second.F = mnn$F[mnn$Role == "Secondary Sex Drive Indicators" & mnn$Indicator == ""],
  second.df.F = mnn$df.F[mnn$Role == "Secondary Sex Drive Indicators" & mnn$Indicator == ""],
  second.p.F = mnn$p.F[mnn$Role == "Secondary Sex Drive Indicators" & mnn$Indicator == ""],
  control.F = mnn$F[mnn$Role == "Bias Indicators" & mnn$Indicator == ""],
  control.df.F = mnn$df.F[mnn$Role == "Bias Indicators" & mnn$Indicator == ""],
  control.p.F = mnn$p.F[mnn$Role == "Bias Indicators" & mnn$Indicator == ""],
  k.less.than.0 = rbind(d2$rs.core, d2$rs.second) %>% {.$g < 0} %>% sum,
  perc.less.than.0 = ((rbind(d2$rs.core, d2$rs.second) %>% {.$g < 0} %>% sum) / (nrow(d2$rs.core) + nrow(d2$rs.second)) * 100) %>% rnd(1),
  min_r_convergent = es_cor %>% 
    filter(x1 %in% c("CF", "AF", "BF", "SD", "DI")) %>% 
    filter(x2 %in% c("CF", "AF", "BF", "SD", "DI")) %>% 
    select(r) %>% min %>% rnd(2),
  max_r_convergent = es_cor %>% 
    filter(x1 %in% c("CF", "AF", "BF", "SD", "DI")) %>% 
    filter(x2 %in% c("CF", "AF", "BF", "SD", "DI")) %>% 
    select(r) %>% max %>% rnd(2),
  loo_bf_min = loo_res %>% filter(labels == "clusterBehavior.Frequency") %>% select("min(g_diff)") %>% extract2(1),
  loo_bf_max = loo_res %>% filter(labels == "clusterBehavior.Frequency") %>% select("max(g_diff)") %>% extract2(1),
  loo_cf_min = loo_res %>% filter(labels == "clusterCognition.Frequency") %>% select("min(g_diff)") %>% extract2(1),
  loo_cf_max = loo_res %>% filter(labels == "clusterCognition.Frequency") %>% select("max(g_diff)") %>% extract2(1),
  loo_af_min = loo_res %>% filter(labels == "clusterAffect.Frequency") %>% select("min(g_diff)") %>% extract2(1),
  loo_af_max = loo_res %>% filter(labels == "clusterAffect.Frequency") %>% select("max(g_diff)") %>% extract2(1),
  loo_af_se_max = loo_res %>% filter(labels == "clusterAffect.Frequency") %>% select("max(se.g_diff)") %>% extract2(1),
  trudel_age = d2$rs.core_with_outlier %>% filter(str_detect(id.full, "Trudel")) %>% select(age.mean) %>% rnd(2),
  trudel_g = d2$rs.core_with_outlier %>% filter(str_detect(id.full, "Trudel")) %>% select(g) %>% rnd(2),
  NULL
)

wm = d2$rs.core %>% 
  transmute(id.full, n.total, 
            anonymity.participant = (anonymity.participant == "yes")  %>% multiply_by(100),
            sex.journal = (sex.journal == "Yes")  %>% multiply_by(100),
            electronic = (electronic == "yes")  %>% multiply_by(100),
            publication.status = (publication.status == "published")  %>% multiply_by(100),
            first.auth.female = (ga.first.cat == "female")  %>% multiply_by(100),
            comp.mat = (compensation == "material")  %>% multiply_by(100),
            comp.credit = (compensation == "coursecredit")  %>% multiply_by(100),
            comp.mixed = (compensation == "mixed")  %>% multiply_by(100),
            comp.none = (compensation == "none")  %>% multiply_by(100),
            sexuality.study = (sexuality.study == "yes")  %>% multiply_by(100),
            focus.gender.sexdrive = (focus.gender.sexdrive == "yes")  %>% multiply_by(100),
            age.mean, 
            age.diff,
            ethn.perc.white, 
            perc.religious, 
            university.student.perc,
            singles.perc, 
            singles.diff,
            hetero.perc, 
            hetero.diff) %>% 
  group_by(id.full) %>% 
  summarise_all(max) %>% 
  select(-id.full) %>% 
  summarise_at(vars(-n.total), 
               list(m = ~ weighted.mean(.x, n.total, na.rm = T),
                    cmpl = ~ sum(!is.na(.x)) / length(.x) * 100)) %>% 
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>% 
  arrange(variable) %>% 
  mutate(log_test = (stat == "m") & !grepl("diff", variable)) %>% 
  mutate(value = ifelse((stat == "m") & grepl("diff", variable), rnd(value, 2), rnd(value, 0))) %>% 
  pivot_wider(id_cols = "variable", names_from = stat, values_from = value)

l <- c(l, wm = list(split(wm, wm$variable)))

mr <- split(mod_results, mod_results$modvarname)%>% 
  map(function(x){
    as.list(x) %>% 
      append(list(cf_modresults = paste0("*HTZ*(", x$cf_df, ") = ", x$cf_HTZ, ", *p* ", if(str_detect(x$cf_p, "<") | is.na(x$cf_p)) x$cf_p else paste0("= ", x$cf_p)),
                  df_modresults = paste0("*HTZ*(", x$df_df, ") = ", x$df_HTZ, ", *p* ", if(str_detect(x$df_p, "<") | is.na(x$df_p)) x$df_p else paste0("= ", x$df_p)),
                  bf_modresults = paste0("*HTZ*(", x$bf_df, ") = ", x$bf_HTZ, ", *p* ", if(str_detect(x$bf_p, "<") | is.na(x$bf_p)) x$bf_p else paste0("= ", x$bf_p))))
  }) %>% 
  map(function(x){
    # x <- mr$content
    if(x$cf_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(cf_regtable2 = split(x$cf_regtable[[1]], x$cf_regtable[[1]]$labels)))
    } 
    if(x$df_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(df_regtable2 = split(x$df_regtable[[1]], x$df_regtable[[1]]$labels)))
    } 
    if(x$bf_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(bf_regtable2 = split(x$bf_regtable[[1]], x$bf_regtable[[1]]$labels)))
    }
    x
    
  }) 

mr2 <- split(mod_results_secondary, mod_results_secondary$modvarname)%>% 
  map(function(x){
    as.list(x) %>% 
      append(list(cf_modresults = paste0("*HTZ*(", x$cf_df, ") = ", x$cf_HTZ, ", *p* ", if(str_detect(x$cf_p, "<") | is.na(x$cf_p)) x$cf_p else paste0("= ", x$cf_p)),
                  di_modresults = paste0("*HTZ*(", x$di_df, ") = ", x$di_HTZ, ", *p* ", if(str_detect(x$di_p, "<") | is.na(x$di_p)) x$di_p else paste0("= ", x$di_p)),
                  sd_modresults = paste0("*HTZ*(", x$sd_df, ") = ", x$sd_HTZ, ", *p* ", if(str_detect(x$sd_p, "<") | is.na(x$sd_p)) x$sd_p else paste0("= ", x$sd_p))))
  }) %>% 
  map(function(x){
    # x <- mr$content
    if(x$cf_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(cf_regtable2 = split(x$cf_regtable[[1]], x$cf_regtable[[1]]$labels)))
    } 
    if(x$di_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(di_regtable2 = split(x$di_regtable[[1]], x$di_regtable[[1]]$labels)))
    } 
    if(x$sd_regtable[[1]]$labels[1] != "DROP_THIS"){
      x <- as.list(x) %>% append(list(sd_regtable2 = split(x$sd_regtable[[1]], x$sd_regtable[[1]]$labels)))
    }
    x
    
  }) 
l <- c(l, mr = list(mr), mr2 = list(mr2))


l <- c(l, 
       u3_global_adj = mnn$U3[mnn$Indicator == "Primary Indicators (Adjusted)"],
       ovl_global_adj = mnn$OVL[mnn$Indicator == "Primary Indicators (Adjusted)"],
       cl_global_adj = mnn$CL[mnn$Indicator == "Primary Indicators (Adjusted)"],
       u3_global_adj_reversed = ((1-as.numeric(mnn$U3[mnn$Indicator == "Primary Indicators (Adjusted)"]))*100) %>% rnd(0),
       ovl_global_adj_reversed = ((1-as.numeric(mnn$OVL[mnn$Indicator == "Primary Indicators (Adjusted)"]))*100) %>% rnd(0),
       cl_global_adj_reversed = ((1-as.numeric(mnn$CL[mnn$Indicator == "Primary Indicators (Adjusted)"]))*100) %>% rnd(0),
       egger_cf_p = res_pubbias$p_egger[res_pubbias$cluster == "rs.cf"],
       egger_df_p = res_pubbias$p_egger[res_pubbias$cluster == "rs.df"],
       egger_bf_p = res_pubbias$p_egger[res_pubbias$cluster == "rs.bf"],
       threepsm_cf_p = res_pubbias$mean_p_3psm[res_pubbias$cluster == "rs.cf"],
       threepsm_df_p = res_pubbias$mean_p_3psm[res_pubbias$cluster == "rs.df"],
       threepsm_bf_p = res_pubbias$mean_p_3psm[res_pubbias$cluster == "rs.bf"],
       outlier_age_k_removed = d2$rs %>% filter(age.mean > 70) %>% select(id.full, age.mean) %>% summarize(n_distinct(id.full)) %>% extract2(1) %>% rnd(0),
       outlier_age_m_removed = d2$rs %>% filter(age.mean > 70) %>% select(id.full, age.mean) %>% summarize(n()) %>% extract2(1) %>% rnd(0),
       outlier_age_next_closest = d2$rs %>% filter(age.mean <= 70) %>% summarize(max(age.mean, na.rm = T)) %>% extract2(1) %>% rnd(2), 
       outlier_gdi_k_removed = d2$rs %>% filter(gdi < 0.9) %>% select(id.full, gdi) %>% summarize(n_distinct(id.full)) %>% extract2(1) %>% rnd(0),
       outlier_gdi_m_removed = d2$rs %>% filter(gdi < 0.9) %>% select(id.full, gdi) %>% summarize(n()) %>% extract2(1) %>% rnd(0),
       outlier_gdi_next_closest = d2$rs %>% filter(gdi >= 0.9) %>% summarize(min(gdi, na.rm = T)) %>% extract2(1) %>% rnd(2),
       outlier_aggregation.span_k_removed = d2$rs %>% filter(aggregation.span > 60) %>% select(id.full, aggregation.span) %>% summarize(n_distinct(id.full)) %>% extract2(1) %>% rnd(0),
       outlier_aggregation.span_m_removed = d2$rs %>% filter(aggregation.span > 60) %>% select(id.full, aggregation.span) %>% summarize(n()) %>% extract2(1) %>% rnd(0),
       outlier_aggregation.span_next_closest = d2$rs %>% filter(aggregation.span <= 60) %>% summarize(max(aggregation.span, na.rm = T)) %>% extract2(1) %>% rnd(0),
       outlier_scale.range_k_removed = d2$rs %>% filter(scale.range > 90) %>% select(id.full, scale.range) %>% summarize(n_distinct(id.full)) %>% extract2(1) %>% rnd(0),
       outlier_scale.range_m_removed = d2$rs %>% filter(scale.range > 90) %>% select(id.full, scale.range) %>% summarize(n()) %>% extract2(1) %>% rnd(0),
       outlier_scale.range_next_closest = d2$rs %>% filter(scale.range <= 90) %>% summarize(max(scale.range, na.rm = T)) %>% extract2(1) %>% rnd(0),
       
       NULL)

n_per_nat <- get_n_per_nation(d2$rs.core) %>% 
  filter(country != "NA") %>% 
  mutate(n.perc = ((n.part / sum(n.part)) * 100)) %>% 
  mutate(country = ifelse(n.perc < 1, "other", country)) %>% 
  group_by(country) %>% 
  summarize(n.perc = sum(n.perc)) %>% {
    set_names(rnd(.$n.perc, 0), .$country)
  }

age_bins <- d2$rs.core %>% 
  filter(!duplicated(id.full)) %>% 
  mutate(age.bin = cut(age.mean, c(0, 25, 30, 35, 40, 100))) %>% 
  select(age.mean, n.total, age.bin) %>% 
  group_by(age.bin) %>% 
  summarize(sm = sum(n.total)) %>% 
  filter(!is.na(age.bin)) %>% 
  mutate(sm = ((sm / sum(sm)) * 100))  %>% {
    set_names(rnd(.$sm, 0), .$age.bin)
  }

l <- c(l, 
       u3_global_unadj = mnn$U3[mnn$Indicator == "Primary Indicators"] %>% as.numeric %>% multiply_by(100),
       ovl_global_unadj = mnn$OVL[mnn$Indicator == "Primary Indicators"] %>% as.numeric %>% multiply_by(100),
       cl_global_unadj = mnn$CL[mnn$Indicator == "Primary Indicators"] %>% as.numeric %>% multiply_by(100),
       u3_global_unadj_reversed = ((1-as.numeric(mnn$U3[mnn$Indicator == "Primary Indicators"]))*100) %>% rnd(0),
       ovl_global_unadj_reversed = ((1-as.numeric(mnn$OVL[mnn$Indicator == "Primary Indicators"]))*100) %>% rnd(0),
       cl_global_unadj_reversed = ((1-as.numeric(mnn$CL[mnn$Indicator == "Primary Indicators"]))*100) %>% rnd(0))

l$perc_raw_data_primary <- str_detect(d2$rs.core$data.source, "raw") %>% table %>% prop.table %>% extract2("TRUE") %>% multiply_by(100) %>% rnd(0)

dat <- data.frame(num = numeric(), id = character(), val = character()) %>%  
  rbind(data.frame(num = 1, id ="total.n", val =l$total.n)) %>% 
  rbind(data.frame(num = 2, id = "total.m", val = l$total.m)) %>% 
  rbind(data.frame(num = 3, id = "total.k", val = l$total.k %>% extract2(1))) %>% 
  rbind(data.frame(num = 4, id = "core.m", val = l$core.m)) %>% 
  rbind(data.frame(num = 5, id = "core.k", val = l$core.k)) %>% 
  rbind(data.frame(num = 6, id = "core.n", val = l$core.n)) %>% 
  rbind(data.frame(num = 7, id = "secondary.m", val = l$secondary.m)) %>% 
  rbind(data.frame(num = 8, id = "secondary.k", val = l$secondary.k)) %>% 
  rbind(data.frame(num = 9, id = "secondary.n", val = l$secondary.n)) %>% 
  rbind(data.frame(num = 10, id = "control.m", val = l$control.m)) %>% 
  rbind(data.frame(num = 11, id = "control.k", val = l$control.k)) %>% 
  rbind(data.frame(num = 12, id = "control.n", val = l$control.n)) %>% 
  rbind(data.frame(num = 13, id = "wm.age.mean.m", val = l$wm$age.mean$m)) %>% 
  rbind(data.frame(num = 14, id = "wm.hetero.perc.m", val = l$wm$hetero.perc$m)) %>% 
  rbind(data.frame(num = 15, id = "wm.hetero.perc.cmpl", val = l$wm$hetero.perc$cmpl)) %>% 
  rbind(data.frame(num = 16, id = "wm.ethn.perc.white.m", val = l$wm$ethn.perc.white$m)) %>% 
  rbind(data.frame(num = 17, id = "wm.ethn.perc.white.cmpl", val = l$wm$ethn.perc.white$cmpl)) %>% 
  rbind(data.frame(num = 18, id = "wm.perc.religious.m", val = l$wm$perc.religious$m)) %>% 
  rbind(data.frame(num = 19, id = "wm.perc.religious.cmpl", val = l$wm$perc.religious$cmpl)) %>% 
  rbind(data.frame(num = 20, id = "wm.singles.perc.m", val = l$wm$singles.perc$m)) %>% 
  rbind(data.frame(num = 21, id = "wm.singles.perc.cmpl", val = l$wm$singles.perc$cmpl)) %>% 
  rbind(data.frame(num = 22, id = "wm.university.student.perc.m", val = l$wm$university.student.perc$m)) %>% 
  rbind(data.frame(num = 23, id = "wm.university.student.perc.cmpl", val = l$wm$university.student.perc$cmpl)) %>% 
  rbind(data.frame(num = 24, id = "min.year.core", val = l$min.year.core)) %>% 
  rbind(data.frame(num = 25, id = "max.year.core", val = l$max.year.core)) %>% 
  rbind(data.frame(num = 26, id = "mean.year.core", val = l$mean.year.core)) %>% 
  rbind(data.frame(num = 27, id = "median.year.core", val = l$median.year.core)) %>% 
  rbind(data.frame(num = 28, id = "wm.hetero.diff.m", val = l$wm$hetero.diff$m)) %>% 
  rbind(data.frame(num = 29, id = "wm.hetero.diff.cmpl", val = l$wm$hetero.diff$cmpl)) %>% 
  rbind(data.frame(num = 30, id = "wm.age.diff.m", val = l$wm$age.diff$m)) %>% 
  rbind(data.frame(num = 31, id = "wm.age.diff.cmpl", val = l$wm$age.diff$cmpl)) %>% 
  rbind(data.frame(num = 32, id = "wm.singles.diff.m", val = l$wm$singles.diff$m)) %>% 
  rbind(data.frame(num = 33, id = "wm.singles.diff.cmpl", val = l$wm$singles.diff$cmpl)) %>% 
  rbind(data.frame(num = 34, id = "wm.sex.journal.m", val = l$wm$sex.journal$m)) %>% 
  rbind(data.frame(num = 35, id = "wm.sex.journal.cmpl", val = l$wm$sex.journal$cmpl)) %>% 
  rbind(data.frame(num = 36, id = "wm.first.auth.female.m", val = l$wm$first.auth.female$m)) %>% 
  rbind(data.frame(num = 37, id = "wm.first.auth.female.cmpl", val = l$wm$first.auth.female$cmpl)) %>% 
  rbind(data.frame(num = 38, id = "wm.publication.status.m", val = l$wm$publication.status$m)) %>% 
  rbind(data.frame(num = 39, id = "wm.publication.status.cmpl", val = l$wm$publication.status$cmpl)) %>% 
  rbind(data.frame(num = 40, id = "wm.electronic.m", val = l$wm$electronic$m)) %>% 
  rbind(data.frame(num = 41, id = "wm.electronic.cmpl", val = l$wm$electronic$cmpl)) %>% 
  rbind(data.frame(num = 42, id = "wm.anonymity.participant.m", val = l$wm$anonymity.participant$m)) %>% 
  rbind(data.frame(num = 43, id = "wm.anonymity.participant.cmpl", val = l$wm$anonymity.participant$cmpl)) %>% 
  rbind(data.frame(num = 44, id = "wm.focus.gender.sexdrive.m", val = l$wm$focus.gender.sexdrive$m)) %>% 
  rbind(data.frame(num = 45, id = "wm.focus.gender.sexdrive.cmpl", val = l$wm$focus.gender.sexdrive$cmpl)) %>% 
  rbind(data.frame(num = 46, id = "wm.comp.mat.m", val = l$wm$comp.mat$m)) %>% 
  rbind(data.frame(num = 47, id = "wm.comp.credit.m", val = l$wm$comp.credit$m)) %>% 
  rbind(data.frame(num = 48, id = "wm.comp.mixed.m", val = l$wm$comp.mixed$m)) %>% 
  rbind(data.frame(num = 49, id = "wm.comp.none.m", val = l$wm$comp.none$m)) %>% 
  rbind(data.frame(num = 50, id = "wm.comp.mixed.cmpl", val = l$wm$comp.mixed$cmpl)) %>% 
  rbind(data.frame(num = 51, id = "wm.sexuality.study.m", val = l$wm$sexuality.study$m)) %>% 
  rbind(data.frame(num = 52, id = "wm.sexuality.study.cmpl", val = l$wm$sexuality.study$cmpl)) %>% 
  rbind(data.frame(num = 53, id = "wm.age.mean.cmpl", val = l$wm$age.mean$cmpl)) %>%  
  rbind(data.frame(num = 54, id = "l$min_r_convergent", val = l$min_r_convergent)) %>%  
  rbind(data.frame(num = 55, id = "l$max_r_convergent", val = l$max_r_convergent)) %>%  
  rbind(data.frame(num = 56, id = "l$loo_cf_min", val = l$loo_cf_min)) %>%  
  rbind(data.frame(num = 57, id = "l$loo_cf_max", val = l$loo_cf_max)) %>%  
  rbind(data.frame(num = 58, id = "l$loo_bf_min", val = l$loo_bf_min)) %>%  
  rbind(data.frame(num = 59, id = "l$loo_bf_max", val = l$loo_bf_max)) %>%  
  rbind(data.frame(num = 60, id = "l$loo_af_min", val = l$loo_af_min)) %>%  
  rbind(data.frame(num = 61, id = "l$loo_af_se_max", val = l$loo_af_se_max)) %>%  
  rbind(data.frame(num = 62, id = "l$trudel_age", val = l$trudel_age)) %>%  
  rbind(data.frame(num = 63, id = "l$trudel_g", val = l$trudel_g)) %>%  
  rbind(data.frame(num = 64, id = "l$core.cf.g", val = l$core.cf.g)) %>%  
  rbind(data.frame(num = 65, id = "l$core.cf.CI95", val = l$core.cf.CI95)) %>%  
  rbind(data.frame(num = 66, id = "l$core.df.g", val = l$core.df.g)) %>%  
  rbind(data.frame(num = 67, id = "l$core.df.CI95", val = l$core.df.CI95)) %>%  
  rbind(data.frame(num = 68, id = "l$core.bf.g", val = l$core.bf.g)) %>%  
  rbind(data.frame(num = 69, id = "l$core.bf.CI95", val = l$core.bf.CI95)) %>%  
  rbind(data.frame(num = 70, id = "l$core.df.F", val = l$core.df.F)) %>%  
  rbind(data.frame(num = 71, id = "l$core.F", val = l$core.F)) %>%  
  rbind(data.frame(num = 72, id = "l$core.p.F", val = l$core.p.F)) %>%  
  rbind(data.frame(num = 73, id = "l$core.tau", val = l$core.tau)) %>%  
  rbind(data.frame(num = 74, id = "l$core.isq", val = l$core.isq)) %>%  
  rbind(data.frame(num = 75, id = "l$second.di.g", val = l$second.di.g)) %>%  
  rbind(data.frame(num = 76, id = "l$second.di.CI95", val = l$second.di.CI95)) %>%  
  rbind(data.frame(num = 77, id = "l$second.sd.g", val = l$second.sd.g)) %>%  
  rbind(data.frame(num = 78, id = "l$second.sd.CI95", val = l$second.sd.CI95)) %>%  
  rbind(data.frame(num = 79, id = "l$second.df.F", val = l$second.df.F)) %>%  
  rbind(data.frame(num = 80, id = "l$second.F", val = l$second.F)) %>%  
  rbind(data.frame(num = 81, id = "l$second.p.F", val = l$second.p.F)) %>%  
  rbind(data.frame(num = 82, id = "l$second.tau", val = l$second.tau)) %>%  
  rbind(data.frame(num = 83, id = "l$second.isq", val = l$second.isq)) %>%  
  rbind(data.frame(num = 84, id = "m.core.second", val = l$core.m + l$secondary.m)) %>%  
  rbind(data.frame(num = 85, id = "l$k.less.than.0", val = l$k.less.than.0)) %>%  
  rbind(data.frame(num = 86, id = "l$perc.less.than.0", val = l$perc.less.than.0)) %>%  
  rbind(data.frame(num = 87, id = "l$control.sf.g", val = l$control.sf.g)) %>%  
  rbind(data.frame(num = 88, id = "l$control.sf.CI95", val = l$control.sf.CI95)) %>%  
  rbind(data.frame(num = 89, id = "l$control.tp.g", val = l$control.tp.g)) %>%  
  rbind(data.frame(num = 90, id = "l$control.tp.CI95", val = l$control.tp.CI95)) %>%  
  rbind(data.frame(num = 91, id = "l$control.ons.g", val = l$control.ons.g)) %>%  
  rbind(data.frame(num = 92, id = "l$control.ons.CI95", val = l$control.ons.CI95)) %>%  
  rbind(data.frame(num = 93, id = "l$control.p12.g", val = l$control.p12.g)) %>%  
  rbind(data.frame(num = 94, id = "l$control.p12.CI95", val = l$control.p12.CI95)) %>%  
  rbind(data.frame(num = 95, id = "l$control.df.F", val = l$control.df.F)) %>%  
  rbind(data.frame(num = 96, id = "l$control.F", val = l$control.F)) %>%  
  rbind(data.frame(num = 97, id = "l$control.p.F", val = l$control.p.F)) %>%  
  rbind(data.frame(num = 98, id = "l$control.tau", val = l$control.tau)) %>%  
  rbind(data.frame(num = 99, id = "l$control.isq", val = l$control.isq)) %>%  
  rbind(data.frame(num = 100, id = "l$core.global.g", val = l$core.global.g)) %>%  
  rbind(data.frame(num = 101, id = "l$core.global.CI95", val = l$core.global.CI95)) %>%  
  rbind(data.frame(num = 102, id = "l$control.global.g", val = l$control.global.g)) %>%  
  rbind(data.frame(num = 103, id = "l$control.global.CI95", val = l$control.global.CI95)) %>%  
  rbind(data.frame(num = 104, id = "l$core.global.g.adj", val = l$core.global.g.adj)) %>%  
  rbind(data.frame(num = 105, id = "u3_global_adj", val = as.numeric(l$u3_global_adj) * 100)) %>%  
  rbind(data.frame(num = 106, id = "ovl_global_adj", val = as.numeric(l$ovl_global_adj) * 100)) %>%  
  rbind(data.frame(num = 107, id = "cl_global_adj", val = as.numeric(l$cl_global_adj) * 100)) %>%  
  rbind(data.frame(num = 108, id = "l$egger_cf_p", val = l$egger_cf_p)) %>%  
  rbind(data.frame(num = 109, id = "l$threepsm_cf_p", val = l$threepsm_cf_p)) %>%  
  rbind(data.frame(num = 110, id = "l$egger_df_p", val = l$egger_df_p)) %>%  
  rbind(data.frame(num = 111, id = "l$threepsm_df_p", val = l$threepsm_df_p)) %>%  
  rbind(data.frame(num = 112, id = "l$egger_bf_p", val = l$egger_bf_p)) %>%  
  rbind(data.frame(num = 113, id = "l$threepsm_bf_p", val = l$threepsm_bf_p)) %>%  
  rbind(data.frame(num = 114, id = "behav_df_below_4", val = sum(as.numeric(mod_results$bf_df) < 4, na.rm = T))) %>%  
  rbind(data.frame(num = 115, id = "l$mr$university.student.perc$bf_modresults", val = l$mr$university.student.perc$bf_modresults)) %>%  
  rbind(data.frame(num = 116, id = "l$mr$content$df_modresults", val = l$mr$content$df_modresults)) %>%  
  rbind(data.frame(num = 117, id = "l$mr$ga.first.cat$df_modresults", val = l$mr$ga.first.cat$df_modresults)) %>%  
  rbind(data.frame(num = 118, id = "l$mr$ga.mean$df_modresults", val = l$mr$ga.mean$df_modresults)) %>%  
  rbind(data.frame(num = 119, id = "l$mr$singles.perc$df_modresults", val = l$mr$singles.perc$df_modresults)) %>%  
  rbind(data.frame(num = 120, id = "sum(as.numeric(mod_results$df_df) < 4, na.rm = T)", val = sum(as.numeric(mod_results$df_df) < 4, na.rm = T))) %>%  
  rbind(data.frame(num = 121, id = "sum(as.numeric(str_remove(mod_results_secondary$cf_p, "<")) < .05, na.rm = T)", val = sum(as.numeric(str_remove(mod_results_secondary$cf_p, "<")) < .05, na.rm = T))) %>%  
  rbind(data.frame(num = 122, id = "l$mr2$content$cf_regtable2$extra.pair.partner$g", val = l$mr2$content$cf_regtable2$extra.pair.partner$g)) %>%  
  rbind(data.frame(num = 123, id = "l$mr2$content$cf_regtable2$a.partner$g", val = l$mr2$content$cf_regtable2$a.partner$g)) %>%  
  rbind(data.frame(num = 124, id = "l$mr2$content$cf_regtable2$not.specified$g", val = l$mr2$content$cf_regtable2$not.specified$g)) %>%  
  rbind(data.frame(num = 125, id = "l$mr2$content$cf_modresults", val = l$mr2$content$cf_modresults)) %>%  
  rbind(data.frame(num = 126, id = "l$mr$aggregation.span$cf_modresults", val = l$mr$aggregation.span$cf_modresults)) %>%  
  rbind(data.frame(num = 127, id = "l$mr$sexually.active$cf_modresults", val = l$mr$sexually.active$cf_modresults)) %>%  
  rbind(data.frame(num = 128, id = "l$mr$singles.perc$cf_modresults", val = l$mr$singles.perc$cf_modresults)) %>%  
  rbind(data.frame(num = 129, id = "l$mr$group.testing$cf_regtable2$mixed$g", val = l$mr$group.testing$cf_regtable2$mixed$g)) %>%  
  rbind(data.frame(num = 130, id = "l$mr$group.testing$cf_regtable2$yes$g", val = l$mr$group.testing$cf_regtable2$yes$g)) %>%  
  rbind(data.frame(num = 131, id = "l$mr$group.testing$cf_regtable2$no$g", val = l$mr$group.testing$cf_regtable2$no$g)) %>%  
  rbind(data.frame(num = 132, id = "l$mr$group.testing$cf_modresults", val = l$mr$group.testing$cf_modresults)) %>%  
  rbind(data.frame(num = 133, id = "l$mr$sexuality.study$cf_modresults", val = l$mr$sexuality.study$cf_modresults)) %>%  
  rbind(data.frame(num = 134, id = "l$mr$sexuality.study$cf_regtable2$yes$g", val = l$mr$sexuality.study$cf_regtable2$yes$g)) %>%  
  rbind(data.frame(num = 135, id = "l$mr$sexuality.study$cf_regtable2$no$g", val = l$mr$sexuality.study$cf_regtable2$no$g)) %>%  
  rbind(data.frame(num = 136, id = "l$mr2$aggregation.span$di_modresults", val = l$mr2$aggregation.span$di_modresults)) %>%  
  rbind(data.frame(num = 137, id = "l$mr2$content$di_modresults", val = l$mr2$content$di_modresults)) %>%  
  rbind(data.frame(num = 138, id = "l$mr2$content$di_regtable2$not.specified$g", val =l$mr2$content$di_regtable2$not.specified$g )) %>%  
  rbind(data.frame(num = 139, id = "l$mr2$content$di_regtable2$masturbation$g", val = l$mr2$content$di_regtable2$masturbation$g)) %>%  
  rbind(data.frame(num = 140, id = "l$mr2$content$di_regtable2$a.partner$g", val = l$mr2$content$di_regtable2$a.partner$g)) %>%  
  rbind(data.frame(num = 141, id = "l$mr2$content$di_regtable2$own.partner$g", val = l$mr2$content$di_regtable2$own.partner$g)) %>%  
  rbind(data.frame(num = 142, id = "l$mr2$context$di_modresults", val = l$mr2$context$di_modresults)) %>%  
  rbind(data.frame(num = 143, id = "l$mr2$context$di_regtable2$romantic.situation$g", val = l$mr2$context$di_regtable2$romantic.situation$g)) %>%  
  rbind(data.frame(num = 144, id = "l$mr2$context$di_regtable2$while.having.sexual.thoughts$g", val = l$mr2$context$di_regtable2$while.having.sexual.thoughts$g)) %>%  
  rbind(data.frame(num = 145, id = "l$mr2$context$di_regtable2$not.specified$g", val = l$mr2$context$di_regtable2$not.specified$g)) %>%  
  rbind(data.frame(num = 146, id = "l$mr2$context$di_regtable2$while.spending.time.with.an.attractive.person$g", val = l$mr2$context$di_regtable2$while.spending.time.with.an.attractive.person$g)) %>%  
  rbind(data.frame(num = 147, id = "l$mr2$context$di_regtable2$first.seeing.an.attractive.person$g", val = l$mr2$context$di_regtable2$first.seeing.an.attractive.person$g)) %>%  
  rbind(data.frame(num = 148, id = "l$mr2$ga.first.cat$di_regtable2$female$g", val = l$mr2$ga.first.cat$di_regtable2$female$g)) %>%  
  rbind(data.frame(num = 149, id = "l$mr2$ga.first.cat$di_regtable2$male$g", val = l$mr2$ga.first.cat$di_regtable2$male$g)) %>%  
  rbind(data.frame(num = 150, id = "l$mr2$ga.first.cat$di_modresults", val = l$mr2$ga.first.cat$di_modresults)) %>%  
  rbind(data.frame(num = 151, id = "l$mr2$anonymity.participant$di_regtable2$yes$g", val = l$mr2$anonymity.participant$di_regtable2$yes$g)) %>%  
  rbind(data.frame(num = 152, id = "l$mr2$anonymity.participant$di_regtable2$no$g", val = l$mr2$anonymity.participant$di_regtable2$no$g)) %>%  
  rbind(data.frame(num = 153, id = "l$mr2$anonymity.participant$di_modresults", val = l$mr2$anonymity.participant$di_modresults)) %>% 
  rbind(data.frame(num = 154, id = "l$outlier_age_m_removed", val = l$outlier_age_m_removed)) %>% 
  rbind(data.frame(num = 155, id = "l$outlier_age_k_removed", val = l$outlier_age_k_removed)) %>% 
  rbind(data.frame(num = 156, id = "l$outlier_age_next_closest", val = l$outlier_age_next_closest)) %>% 
  rbind(data.frame(num = 157, id = "l$outlier_gdi_m_removed", val = l$outlier_gdi_m_removed)) %>% 
  rbind(data.frame(num = 158, id = "l$outlier_gdi_k_removed", val = l$outlier_gdi_k_removed)) %>% 
  rbind(data.frame(num = 159, id = "l$outlier_gdi_next_closest", val = l$outlier_gdi_next_closest)) %>% 
  rbind(data.frame(num = 160, id = "l$outlier_aggregation.span_m_removed", val = l$outlier_aggregation.span_m_removed)) %>% 
  rbind(data.frame(num = 161, id = "l$outlier_aggregation.span_k_removed", val = l$outlier_aggregation.span_k_removed)) %>% 
  rbind(data.frame(num = 162, id = "l$outlier_aggregation.span_next_closest", val = l$outlier_aggregation.span_next_closest)) %>% 
  rbind(data.frame(num = 163, id = "l$outlier_scale.range_m_removed", val = l$outlier_scale.range_m_removed)) %>% 
  rbind(data.frame(num = 164, id = "l$outlier_scale.range_k_removed", val = l$outlier_scale.range_k_removed)) %>% 
  rbind(data.frame(num = 165, id = "l$outlier_scale.range_next_closest", val = l$outlier_scale.range_next_closest)) %>% 
  rbind(data.frame(num = 166, id = "l$u3_global_adj_reversed", val = l$u3_global_adj_reversed)) %>% 
  rbind(data.frame(num = 167, id = "l$ovl_global_adj_reversed", val = l$ovl_global_adj_reversed)) %>% 
  rbind(data.frame(num = 168, id = "l$cl_global_adj_reversed", val = l$cl_global_adj_reversed)) %>% 
  rbind(data.frame(num = 169, id = "contraception_complete", val = d2$rs.core %>% filter(!duplicated(id.full)) %>% summarize(mean(!is.na(contraception))) %>% extract2(1) %>% multiply_by(100) %>% rnd(0))) %>% 
  rbind(data.frame(num = 170, id = "contraception_mean", val = d2$rs.core %>% filter(!duplicated(id.full)) %>% summarize(mean(contraception, na.rm = T)) %>% extract2(1) %>% rnd(0))) %>% 
  rbind(data.frame(num = 171, id = "n_per_nat[['United States']]", val = n_per_nat[["United States"]])) %>% 
  rbind(data.frame(num = 172, id = "n_per_nat[['Australia']]", val = n_per_nat[["Australia"]])) %>% 
  rbind(data.frame(num = 173, id = "n_per_nat[['Germany']]", val = n_per_nat[["Germany"]])) %>% 
  rbind(data.frame(num = 174, id = "n_per_nat[['United Kingdom']]", val = n_per_nat[["United Kingdom"]])) %>% 
  rbind(data.frame(num = 175, id = "n_per_nat[['Canada']]", val = n_per_nat[["Canada"]])) %>% 
  rbind(data.frame(num = 176, id = "n_per_nat[['Denmark']]", val = n_per_nat[["Denmark"]])) %>% 
  rbind(data.frame(num = 177, id = "n_per_nat[['Spain']]", val = n_per_nat[["Spain"]])) %>% 
  rbind(data.frame(num = 178, id = "n_per_nat[['other']]", val = n_per_nat[["other"]])) %>% 
  rbind(data.frame(num = 179, id = "n_per_nat[['Norway']]", val = n_per_nat[["Norway"]])) %>% 
  rbind(data.frame(num = 180, id = "n_per_nat[['Croatia']]", val = n_per_nat[["Croatia"]])) %>% 
  rbind(data.frame(num = 181, id = "n_per_nat[['Finland']]", val = n_per_nat[["Finland"]])) %>% 
  rbind(data.frame(num = 182, id = "n_per_nat[['Japan']]", val = n_per_nat[["Japan"]])) %>% 
  rbind(data.frame(num = 183, id = "n_per_nat[['Portugal']]", val = n_per_nat[["Portugal"]])) %>% 
  rbind(data.frame(num = 184, id = "n_per_nat[['Sweden']]", val = n_per_nat[["Sweden"]])) %>% 
  rbind(data.frame(num = 185, id = "nation_complete", val = d2$rs.core %>% filter(!duplicated(id.full)) %>% summarize(m = mean(!is.na(nation))) %>% extract2(1) %>% multiply_by(100) %>% rnd(0))) %>%  
  rbind(data.frame(num = 186, id = "k_%_agebin:(0,25]", val = age_bins[["(0,25]"]])) %>% 
  rbind(data.frame(num = 187, id = "k_%_agebin:(25,30]", val = age_bins[["(25,30]"]])) %>% 
  rbind(data.frame(num = 188, id = "k_%_agebin:(30,35]", val = age_bins[["(30,35]"]])) %>% 
  rbind(data.frame(num = 189, id = "k_%_agebin:(35,40]", val = age_bins[["(35,40]"]])) %>% 
  rbind(data.frame(num = 190, id = "k_%_agebin:(40,100]", val = age_bins[["(40,100]"]])) %>% 
  rbind(data.frame(num = 191, id = "u3_global_unadj", val = l$u3_global_unadj)) %>% 
  rbind(data.frame(num = 192, id = "ovl_global_unadj", val = l$ovl_global_unadj)) %>% 
  rbind(data.frame(num = 193, id = "cl_global_unadj", val = l$cl_global_unadj)) %>% 
  rbind(data.frame(num = 194, id = "u3_global_unadj_reversed", val = l$u3_global_unadj_reversed)) %>% 
  rbind(data.frame(num = 195, id = "ovl_global_unadj_reversed", val = l$ovl_global_unadj_reversed)) %>% 
  rbind(data.frame(num = 196, id = "cl_global_unadj_reversed", val = l$cl_global_unadj_reversed)) %>% 
  rbind(data.frame(num = 197, id = "perc_raw_data_primary", val = l$perc_raw_data_primary)) %>%  
  rbind(data.frame(num = 198, id = "irragree", val = mean(readxl::read_xlsx("data/coding/coder_reliability.xlsx")$agreed, na.rm = T) %>% multiply_by(100) %>% rnd(0))) 

if(mean(dat$num == 1:nrow(dat)) != 1)stop("ERROR: SOMETHING IS WRONG WITH THE ROW NUMBERS")

# rbind(data.frame(num = , id = "", val = )) %>% 

write.xlsx(dat, "C:/Users/Admin/Dropbox/masd_reloaded/manuscript/results.xlsx")
# writexl::write_xlsx(dat, "C:/Users/Admin/Dropbox/masd_reloaded/manuscript/results.xlsx")
