{
    rm(list = ls())
    
    library(tidyverse)
    library(magrittr)
    library(psych)
    library(tictoc)
    library(purrr)
    library(moments)
    library(compute.es)
    
    all_files <- dir("data/prepared/", pattern = ".xlsx") %>% (function(x)set_names(x, x))
}



process_xlsx <- function(x){
    dat <- readxl::read_xlsx(paste0("data/prepared/", x), col_names = F, .name_repair = "minimal")
    tibble(data.structure = dat[1, 2] %>% extract2(1),
           id.pub = dat[3, 2] %>% extract2(1),
           id.study = dat[4, 2] %>% extract2(1) %>% as.numeric,
           data.source = dat[5, 2] %>% extract2(1),
           item_info = dat[7:13, 2:ncol(dat)] %>% t %>% as_tibble %>% set_colnames(dat[7:13, 1] %>% extract2(1)) %>% 
               add_column(.before = T, id.out = 1:(ncol(dat)-1)) %>% list,
           very_raw_data = dat[15:nrow(dat), 1:ncol(dat)] %>% as.matrix(.name_repair = "check_unique") %>% list) %>% 
        mutate(raw_data = ifelse(data.structure == "raw_data", very_raw_data, NA),
               sum_data = ifelse(data.structure == "means_sd_n", very_raw_data, NA)) %>% 
        select(-very_raw_data) %>%
        mutate(raw_data = map_if(raw_data, data.structure == "raw_data", 
                                 ~ .x %>% as_tibble %>% set_colnames(.[1, ]) %>% slice(-1) %>%  # for debugging: .x <- prepared_data$very_raw_data[[2]]
                                     mutate_at(vars(contains("item")), function(x)as.numeric(na_if(x, "NA")))),
               sum_data = map_if(sum_data, data.structure == "means_sd_n", 
                                 ~ .x %>% as_tibble %>% set_colnames(.[1, ]) %>% slice(-1) %>%  # for debugging: .x <- prepared_data$very_raw_data[[1]]
                                     mutate_at(vars(contains("item")), function(x)as.numeric(na_if(x, "NA"))) %>% arrange(label)),
        )
}

treat_outliers <- function(x, what){
    # Function to count, drop, or find outliers.
    # Outliers are defined as values that deviate more than 3 times the median absolute deviation from the median. 
    is_outlier <- abs(scale(x)) > 3.5
    # is_outlier <- abs(x - median(x)) > 3 * mad(x)
    when(what, 
         . == "count" ~ sum(is_outlier, na.rm = T),
         . == "drop" ~ list(x[!is_outlier]),
         . == "find" ~ list(x[is_outlier]))
}

treat_outliers(rnorm(1000), "count")

count_outliers <- function(values){
    # values <- c(-4, rnorm(100), 4)
    q1 <- quantile(values, c(0.25), na.rm = T)
    q3 <- quantile(values, c(0.75), na.rm = T)
    lower <- q1 - 1.5 * (q3 - q1)
    upper <- q3 + 1.5 * (q3 - q1)
    sum(values < lower | values > upper, na.rm = T)
}

drop_outliers <- function(values){
    # values <- c(-4, rnorm(100), 4)
    # values <- xxx$male[[1]][[1]]
    q1 <- quantile(values, c(0.25), na.rm = T)
    q3 <- quantile(values, c(0.75), na.rm = T)
    lower <- q1 - 1.5 * (q3 - q1)
    upper <- q3 + 1.5 * (q3 - q1)
    values[!(values < lower | values > upper)]
}

compute_corrs <- function(.x, .y){
    # .x <- prepared_data$raw_data[[2]]; .y <- prepared_data$item_info[[2]]
    .x %>% 
        select(-gendervec) %>% 
        set_names(.y$cluster) %>% 
        list(r = cor(., use = "pairwise.complete.obs") %>% data.frame, 
             n = pairwiseCount(.) %>% data.frame, 
             use = upper.tri(cor(., use = "pairwise.complete.obs")) %>% data.frame %>% set_names(paste0(.y$cluster, ".", seq(length(.y$cluster))))) %>% 
        magrittr::extract(2:4) %>%
        map2(., names(.), ~ .x %>% 
                 mutate(x1 = names(.x)) %>% 
                 pivot_longer(names_to = "x2", values_to = .y, cols = -x1, names_repair = "universal") %>% 
                 mutate(x1 = gsub("\\..*","",x1)) %>% 
                 mutate(x2 = gsub("\\..*","",x2)) %>% 
                 mutate(xa = map2(x1, x2, ~c(.x, .y))) %>% 
                 mutate(xa = modify(xa, ~ sort(.x))) %>% 
                 mutate(xa = map(xa, ~set_names(.x, c("x1", "x2")))) %>% 
                 select(-x1, -x2) %>% 
                 unnest_wider(xa, names_repair = "universal") %>% 
                 mutate(label = paste0(x1, "_", x2)) %>% 
                 select(-x1, -x2)) %>% 
        bind_cols %>%
        filter(use & round(r, 3) != 1) %>% 
        mutate(var.r = res(r = r, n = n, verbose = F, dig = 10)$var.r) %>% 
        select(label, r, var.r, n)
}
    


prepared_data <- map(all_files, process_xlsx) %>% 
    do.call(what = rbind) %>% rownames_to_column("file_name") 

es2 <- prepared_data %>% 
    mutate(cor_data = map2(raw_data, item_info, 
                           possibly(compute_corrs, NA))) %>% 
    mutate(cor_data = map_if(cor_data, data.source != "raw data from authors" | is.na(data.source), ~ list(NA))) %>% 
    mutate(raw_data = map_if(raw_data, data.structure == "raw_data",
                             ~ .x %>%  # .x <- prepared_data$raw_data[[2]]
                                 mutate(gendervec = na_if(gendervec, "NA")) %>%
                                 filter(!is.na(gendervec)) %>% 
                                 group_by(gendervec) %>% 
                                 summarize_all(list) %>% 
                                 pivot_longer(names_to = "id.out", values_to = "value", cols = -gendervec) %>% 
                                 pivot_wider(names_from = gendervec, values_from = value) %>%
                                 mutate(id.out = as.numeric(str_replace(id.out, "item", ""))))) %>% 
    
           mutate(raw_data = map_if(raw_data, data.structure == "means_sd_n", # .x <- prepared_data$item_info[[9]]
                                    ~tibble(id.out = 1:1000, male = NA, female = NA))) %>%
    mutate(all_data = map2(.x = raw_data, .y = item_info, ~ .x %>% inner_join(.y, by = "id.out"))) %>%
    mutate(all_data = map_if(all_data, data.structure == "raw_data", 
                             ~ .x %>%  # .x <- es2$all_data[[34]]
                                 mutate(outlier_cnt_f = map(female, ~ treat_outliers(.x, "count")) %>% flatten_dbl,
                                        outlier_cnt_m = map(male, ~ treat_outliers(.x, "count")) %>% flatten_dbl, 
                                        outlier_f = map(female, ~ treat_outliers(.x, "find")),
                                        outlier_m = map(male, ~ treat_outliers(.x, "find")), 
                                        female_with_outliers = female,
                                        male_with_outliers = male,
                                        female = map(female, ~ treat_outliers(.x, "drop")), 
                                        male = map(male, ~ treat_outliers(.x, "drop"))
                                        ))) %>% 
    mutate(sum_data = map_if(sum_data, data.structure == "means_sd_n", 
                             ~ .x %>%  # .x <- prepared_data$sum_data[[1]]
                                 pivot_longer(names_to = "id.out", values_to = "value", cols = -label) %>% 
                                 pivot_wider(names_from = label, values_from = value) %>% 
                                 mutate(id.out = as.numeric(str_replace(id.out, "item", ""))))) %>% 
    mutate(sum_data2 = map_if(raw_data, data.structure == "raw_data", 
                              ~ .x %>%  # .x <- prepared_data3$raw_data[[2]]
                                  mutate(male_stat = map(male, ~ list(mean.m = mean(.x, na.rm = T), 
                                                                      sd.m = sd(.x, na.rm = T),
                                                                      n.m = sum(!is.na(.x)),
                                                                      kurtosis.m = kurtosis(.x, na.rm = T),
                                                                      skew.m = skewness(.x, na.rm = T))),
                                         female_stat = map(female, ~ list(mean.f = mean(.x, na.rm = T), 
                                                                          sd.f = sd(.x, na.rm = T),
                                                                          n.f = sum(!is.na(.x)),
                                                                          kurtosis.f = kurtosis(.x, na.rm = T),
                                                                          skew.f = skewness(.x, na.rm = T)))) %>% 
                                  unnest_wider(male_stat) %>% 
                                  unnest_wider(female_stat) %>% 
                                  select(-c("male", "female")))) %>%
    mutate(sum_data2 = map_if(sum_data2, data.structure == "means_sd_n", ~ return(NA))) %>% 
    mutate(sum_data = coalesce(sum_data, sum_data2)) %>% select(-sum_data2) %>% 
    mutate(all_data = map2(all_data, sum_data, ~ .x %>% inner_join(.y, by = "id.out"))) %>% 
    select(-sum_data, -raw_data, -item_info) %>% 
    # mutate(all_data = map(all_data, ~ .x %>% mutate(male = map(male, ~ list(.x)),
    #                                                 female = map(female, ~ list(.x))))) %>% 
    unnest(all_data) %>% 
    mutate(id.full = paste0(id.pub, "_", id.study)) %>% 
    mutate(g = mes(m.1 = mean.m, m.2 = mean.f, sd.1 = sd.m, sd.2 = sd.f, n.1 = n.m, n.2 = n.f, verbose = F, dig = 10)[["g"]],
           var.g = mes(m.1 = mean.m, m.2 = mean.f, sd.1 = sd.m, sd.2 = sd.f, n.1 = n.m, n.2 = n.f, verbose = F, dig = 10)[["var.g"]],
           id.full.out = paste0(id.full, "_", id.out)) %>% 
    mutate(outlier_perc_f = outlier_cnt_f / n.f * 100,
           outlier_perc_m = outlier_cnt_m / n.m * 100)


es2 <- es2 %>% select(-male, -female)
save(es2, file = "results/es2_raw.Rda")
