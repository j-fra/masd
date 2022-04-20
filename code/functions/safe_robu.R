
safe_robu <- function(mod, dat, mods_info_dat, cntrl_extrapair = F){
    # For testing: safe_robu("publication.status", d2$rs.df, mods_info)
    
    # For debugging:
    # mod = "publication.status"
    # mod = "context"
    # mod = "hetero.diff"
    # mod = "ga.first.cat"
    # dat = d2$rs.bf
    # dat = d2$rs.cf
    # mods_info_dat = mods_info
    
    
    print(paste("Processing", mod, "for dataset", deparse(substitute(dat))))
    
    tryCatch(
        {
            
            m_info <- mods_info_dat %>% filter(modvarname == mod)
            
            if(!is.na(m_info$outlier_rule)){
                filter_condition <- paste0("!(", str_replace_all(m_info$outlier_rule, "x ", mod), ")", 
                                           "| is.na(", mod, ")")
                dat <- dat %>% filter(eval(parse(text = filter_condition)))  # Outlier removal.
            }
            
            if(m_info$type == "cat"){
                # Set codings with m < 5 to NA
                recode_string <- dat %>% 
                    filter(!is.na(get(mod))) %>%  
                    group_by(get(mod)) %>% summarize(n = n(), .groups = 'drop') %>% 
                    rename("moderator" = "get(mod)") %>% 
                    mutate(replacement = ifelse(n < 5, NA, moderator)) %>% 
                    {set_names(.$replacement, .$moderator)}
                dat[[mod]] <- recode(dat[[mod]], !!!recode_string)
            }
            
            if(cntrl_extrapair){
                model <- robu(formula(paste0("g~", mod, "+cntrl_extrapair")), dat, id.full, var.g, rho = 0.8)
                model$reg_table <- model$reg_table %>% {slice(., -nrow(.))}
                model2 <- robu(formula(paste0("g~", mod, "+cntrl_extrapair-1")), dat, id.full, var.g, rho = 0.8)
                model2$reg_table <- model2$reg_table %>% {slice(., -nrow(.))}
                wald <- Wald_test(model, constrain_zero(2:length(model$reg_table$labels)), vcov = "CR2")
            } else if(cntrl_extrapair == F){
                model <- robu(formula(paste0("g~", mod)), dat, id.full, var.g, rho = 0.8)
                model2 <- robu(formula(paste0("g~", mod, "-1")), dat, id.full, var.g, rho = 0.8)
                wald <- Wald_test(model, constrain_zero(2:length(model$reg_table$labels)), vcov = "CR2")
            }
            
            list(i.sq = model$mod_info$I.2 %>% extract2(1) %>% rnd_2,
                 tau = model$mod_info$tau.sq %>% extract2(1) %>% sqrt %>% rnd_2,
                 HTZ = wald$Fstat %>% rnd_2,
                 df = wald$df_denom %>% rnd_2,
                 p = wald$p_val %>% rnd_p,
                 regtable = when(m_info$type, 
                                 . == "con" ~ model$reg_table %>% 
                                     mutate(labels = c("Intercept", "Slope")) %>% 
                                     full_join(model$data %>% 
                                                   filter(!is.na(get(mod))) %>% 
                                                   summarize(m = n(), k = n_distinct(id.full), .groups = 'drop') %>% 
                                                   mutate(labels = "Intercept") %>% 
                                                   mutate(labels = str_replace_all(labels, " ", ".")) %>% 
                                                   mutate(labels = str_replace_all(labels, "-", ".")), 
                                               by = "labels") %>%
                                     select(-sig),
                                 . == "cat" ~ model2$reg_table %>% 
                                     mutate(labels = str_replace(labels, mod, "")) %>% 
                                     full_join(model2$data %>% 
                                                   filter(!is.na(get(mod))) %>% 
                                                   group_by(get(mod)) %>% 
                                                   summarize(m = n(), k = n_distinct(id.full), .groups = 'drop') %>% 
                                                   rename("labels" = "get(mod)")%>% 
                                                   mutate(labels = str_replace_all(labels, " ", ".")) %>% 
                                                   mutate(labels = str_replace_all(labels, "-", ".")), 
                                               by = "labels") %>%
                                     select(-sig)) %>% 
                     transmute(labels = labels, g = b.r %>% rnd_2, SE = SE %>% rnd_2, t = t %>% rnd_2, df = dfs %>% rnd_2, 
                               p = prob %>% rnd_p, CI95 = paste0("[", rnd_2(CI.L), ", ", rnd_2(CI.U), "]"), m = m %>% as.character, k = k %>% as.character),
                 regplot = case_when(m_info$type == "cat" ~ list(plot.rve.cat(model = model, model2 = model2, wald = wald, modvar = mod, titlename = m_info$label)),
                                     m_info$type == "con" ~ list(plot.rve.cont(model = model, modvar = mod, titlename = m_info$label))),
                 err = NA)
        },
        error=function(cond)list(i.sq = NA_character_, 
                                 tau.sq = NA_character_, 
                                 HTZ = NA_character_, 
                                 df = NA_character_, 
                                 p = NA_character_, 
                                 regtable = data.frame(labels="DROP_THIS"), 
                                 regplot = NA, err = paste0("Error fitting model for ", mod, ". ", cond)))
}
