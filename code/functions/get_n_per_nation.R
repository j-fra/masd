get_n_per_nation <- function(dat){
    dat %>% 
        mutate(nation = ifelse(is.na(nation), "NA", nation)) %>% 
        group_by(id.full) %>% 
        summarize(nation = first(nation),
                  n.total = mean(n.total)) %>% 
        select(nation, n.total) %>% 
        mutate(nation_split = strsplit(nation, ",")) %>% 
        mutate(nlab = lapply(nation_split, function(x)x[c(T, F)])) %>% 
        mutate(nw = lapply(nation_split, function(x)x[c(F, T)])) %>% 
        mutate(nw = lapply(nw, function(x)gsub("%", "", x) %>% trimws %>% as.numeric)) %>% 
        mutate(nw = ifelse(is.na(nw), list(100), nw)) %>% 
        select(nlab, nw, n.total) %>% 
        mutate(nlab = purrr::map(nlab, function(x)setNames(x, paste0("nlab", 1:length(x))))) %>% 
        unnest_wider(nlab) %>% 
        gather(key, country, -starts_with("nw"), -n.total) %>% 
        select(c("key", "country"), everything()) %>% 
        mutate(position = as.numeric(gsub("nlab", "", key))) %>% 
        filter(!is.na(country)) %>% 
        mutate(weight = mapply(function(x,y)x[y], nw, position)) %>% 
        mutate(n.part = round((weight / 100) * n.total)) %>% 
        mutate(country = trimws(country)) %>% 
        group_by(country) %>% 
        summarize(n.part = sum(n.part)) %>% 
        arrange(desc(n.part))
}