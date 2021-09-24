{
    rm(list = ls())
    
    library(tidyverse)
    library(magrittr)
    library(flextable)
    library(officer)
    
    load("results/es_prepared2.Rda")
    
    # walk(list.files("code/functions/", full.names = T), source)
}

dat <- readxl::read_excel("data/coding/coder_reliability.xlsx")
mods_info <- readxl::read_excel("data/coding/mods_info.xlsx") %>% filter(is_moderator & class != "Outcome-level Moderators")

unique(dat$name)[unique(dat$name) %in% mods_info$modvarname] %>% writeClipboard()


types <- list(
    # integer indicates number of categories, NA means unknown number of categories, 99 means correlation
    "journal.open" = NA, # publication status and sexuality journal derived from this
    "ga.mean" = 99,   #X COMPUTE!
    "ga1" = 3,  #X
    "university.student.perc" = 99,  #X
    "focus.on.gender" = 3,  #X
    "focus.gender.sexdrive" = 3,  #X
    "aim.gender.sexdrive" = 3,  #X
    "asked.verbally" = 3,
    "personal.contact" = 3,  #X
    "group.testing" = 3,  #X
    "electronic" = 3,  #X
    "anonymity.paper" = 3,  #X
    "anonymity.participant" = 3,  #X
    "compensation" = 4,  #X
    "sexuality.study" = 3,  #X
    "sexually.active" = 3,  #X
    "singles.perc" = 99,  #X
    "partnership.duration" = 99,  #X
    "perc.children" = 99,  #X
    "ethn.perc.white" = 99,  #X
    "hetero.perc" = 99,  #X
    "age.mean" =   99,#X
    "nation" =  NA,#X  GII, GDI, country-level sex ratio derived from this
    "pubyear" =  99,#X 
    "studyyear" =  99,#X 
    "receivedyear" =  99#X 
)


### CORRELATIONS (numeric moderators)

numcols <- names(unlist(types[types == 99]))

dat2 <- dat %>% 
    na_if("NA") %>% 
    pivot_longer(cols = c(coder1, coder2), names_to = "coder", values_to = "coding") %>% 
    pivot_wider(id_cols = c("id.full", "coder"), values_from = "coding") %>% 
    rowwise() %>% 
    mutate(ga.mean = mean(as.numeric(c(ga1, ga2, ga3, ga4, ga5, ga6, ga7, ga8)), na.rm = T))


compute_cor <- function(df, varname){
    # df = d3
    # varname = "pubyear"
    df %>% 
        select(all_of(varname), coder, id.full) %>% 
        pivot_wider(id_cols = "id.full", values_from = varname, names_from = "coder") %>% 
        select(-id.full) %>% 
        {list(mod = varname, 
              value = cor(.[[1]], .[[2]], use = "p"),
              statistic = "Pearson Correlation",
              one_not_found = sum(xor(is.na(.[[1]]), is.na(.[[2]]))),
              both_not_found = sum(is.na(.[[1]]) & is.na(.[[2]])),
              type = "Numeric moderator")}
}


### KAPPAS (known number of categories)
kapcols <- names(types[!is.na(types) & types != 99] )
kapposs <- map(types[!is.na(types) & types != 99], ~1/.x)

dat3 <- dat %>% 
    na_if("NA") %>% 
    mutate(agreed = ifelse(is.na(coder1) & is.na(coder2), TRUE, agreed)) %>% 
    pivot_wider(id_cols = c("id.full"), values_from = "agreed")

compute_kappa <- function(df, varname, poslist){
    # df = dat3
    # varname = "sexually.active"
    # poslist = kapposs
    p0 <- mean(df[[varname]])
    pc <- poslist[[varname]]
    list(mod = varname, 
         value = (p0 - pc) / (1 - pc),
         statistic = "Cohen's Kappa",
         n_cat = round(pc ^-1))
}




### PERCENT AGREEMENT (unknown number of categories)
perccols <- names(types[is.na(types)])

compute_perc_agreement <- function(df, varname){
    # df = dat3
    # varname = "nation"
    # varname = "journal.open"
    list(mod = varname, 
         value = mean(df[[varname]]),
         statistic = "Percent Agreement", 
         type = "Unknown no. of categories")

}

# COLLECT

coder_reliability_results <- bind_rows(map_dfr(kapcols, compute_kappa, df = dat3, poslist = kapposs),
          
          map_dfr(perccols, compute_perc_agreement, df = dat3),
          
          dat2 %>% select(all_of(numcols), coder, id.full) %>%
              mutate(across(all_of(numcols), .fns = ~as.numeric(.x))) %>% 
              {map_dfr(numcols, compute_cor, df = .)})

save(coder_reliability_results, file = "results/coder_reliability_results.Rda")

typology <- data.frame(
    col_keys = names(coder_reliability_results),
    what = c("Moderator", "Reliability", "Statistic", "No. of Categories", "Type of Moderator", "NA Match", "NA Missmatch"))

coder_reliability_table <- regulartable(coder_reliability_results %>% 
                                        mutate(
                                            mod = ifelse(mod == "ga1", "ga.first.cat", mod),
                                            mod = recode(mod, !!!c(set_names(mods_info$label, mods_info$modvarname), 
                                                                   journal.open = "Journal (open)", 
                                                                   nation = "Nation (open)", 
                                                                   pubyear = "Year the Study was Published", 
                                                                   studyyear = "Year the Study was Conducted", 
                                                                   receivedyear = "Year the Study was Submitted")))) %>% 
    set_header_df(mapping = typology, key = "col_keys" ) %>%
    width("mod", 2.2) %>% 
    width("statistic", 1.1) %>% 
    width("type", 1.4) %>% 
    align(align = "left", part = "body") %>% 
    align(align = "center", part = "header") %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs %>% 
    fix_border_issues %>% 
    hrule(rule = "exact") %>% 
    add_footer_lines("") %>% 
    compose(value = as_paragraph(
        as_i("Note. "), 
        "Interrater reliability for study and publication level moderators. ", 
        "The results in this table were derived from 21 studies that were coded by two coders. ",
        "Some moderators reported in the main manuscript were derived from other raw codings and hence do not appear in this table. ",
        "The codings 'Publication Status' and 'Sexuality Journal' were derived from 'Journal (open)'. ",
        "The codings 'Country-Level Gender Inequality', Country-Level Gender Development', and 'Country-Level Sex Ratio' were derived from 'Nation'. ",
        "The coding 'Year' was derived from 'Year the study was conducted', 'Year the study was published', and 'Year the study was submitted'. ",
        "We report different reliability indicators for different moderators. For categorical codings, we report Cohen's ",
        as_chunk("k", fp_text(font.size = 8, italic = T, font.family = "Symbol")), " along with the number of possible categories. ",
        "For categorical codings with an unknown number of categories, we report percent agreement. ",
        "For numerical codings, we report Pearson's correlation coefficient along with the number of cases were coders agreed that the information was missing (NA Match) and the number of cases where only one coder found information (NA Missmatch). For categorical codings, 'information missing' was treated as a normal category."), 
        part = "footer" ) %>% 
    add_header_lines("Interrater Reliability ") %>% 
    add_header_lines("Table S7") %>% 
    # italic(i = 2, part = "header") %>% 
    # bold(i = 1, part = "header") %>% 
    # line_spacing(i = 1:2, space = 2, part = "header") %>% 
    fontsize(size = 8, part = "all")

read_docx() %>% 
    body_add_flextable(value = coder_reliability_table,
                       split = TRUE) %>% 
    body_end_section_portrait() %>%
    print(target = "results/figures and tables/coder_reliability_table.docx")

shell.exec(paste0(getwd(), "/results/figures and tables/coder_reliability_table.docx"))

save(coder_reliability_table, file = "results/figures and tables/coder_reliability_table.Rda")

