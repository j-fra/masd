{
    library(tidyverse)
    library(magrittr)
    library(robumeta)
    library(readxl)
    library(psych)
    
    rm(list = ls())
    
    options(stringsAsFactors=FALSE)
    
    load("results/es2_raw.Rda")
}

# Drop items removed during R1 ---------------------------------------------

es2_remove <- es2 %>% 
    filter(grepl("Compared to other people of your age and sex", item) | 
               grepl("How would you compare your level of sex drive with that of the average person of your gender and age?", item))


# Add study coding data ---------------------------------------------------

find_unconvertible <- function(vec){
    # find_unconvertible(vec <- c("1", "2", "b", NA))
    vec <- vec[which(!is.na(vec))]
    test <- suppressWarnings(as.numeric(vec))
    vec[is.na(test)]
}

cdat <- readxl::read_xlsx("data/coding/study_coding.xlsx", col_names = F, na = "NA", .name_repair = "minimal")
# cdat <- readxl::read_xlsx("data/coding/study_coding_w-RapeRate.xlsx", col_names = F, na = "NA", .name_repair = "minimal")

modinfo <- cdat %>% 
    data.frame(stringsAsFactors = F) %>% 
    select(1:5) %>% 
    set_colnames(c("descripton", "data.type", "valid", "group","mod"))

cdat <- cdat %>% 
    t %>% data.frame(stringsAsFactors = F) %>%  slice(-1:-4) %>% set_colnames(.[1,] %>% flatten_chr) %>%  slice(-1) %>% 
    na_if("NA") %>% 
    mutate(id.full = paste0(id.pub, "_", id.study)) %>% 
    # select(modinfo %>% filter(data.type == "numeric") %>% extract2("mod")) %>% map(find_unconvertible)  # This is for finding values that cannot be converted to numeric
    mutate_at(modinfo %>% filter(data.type == "numeric") %>% extract2("mod"), as.numeric) %>% 
    select(-id.pub, -id.study)

rs <-  cdat %>% 
    inner_join(es2, by = "id.full") %>% 
    mutate(item = gsub("\n", "",  item, fixed = T),
           item = gsub("\r",  "", item, fixed = T),
           item = gsub("\\(R\\)",  "", item),
           item = gsub("\\. $", ".", item))

rm(cdat, modinfo, find_unconvertible, es2)


# Add item coding data -------------------------------------------------------------------------------------------

idat <- readxl::read_xlsx("data/coding/item_coding.xlsx", col_names = T, na = "NA", skip = 14)
idat <- idat %>% 
    mutate(item = gsub("\n", "", item, fixed = T),
           item = gsub("\\. $", ".", item),
           item = gsub("\r", "", item, fixed = T),
           item = gsub("\\(R\\)",  "", item))

rs <- rs %>% inner_join(idat, by = "item")

# R1: recode item content "a partner" to "unspecified partner"
# R3: recode item content "not specified" to "no target"

rs <- rs %>% 
    mutate(content = recode(content, "a partner" = "unspecified partner")) %>% 
    mutate(content = recode(content, "not specified" = "no target")) 
rs$content
unique(rs$content)

rm(idat)



# Add and clean variables ----------------------------------------------------------------------------------------
mods_info <- readxl::read_xlsx("data/coding/mods_info.xlsx", col_names = T)

# Sexual violence
svdata <- readxl::read_xlsx("data/coding/Sexual Violence.xlsx") %>% 
    mutate(Country = trimws(Country), XNA = NA) %>% 
    {set_names(., paste0("X", names(.)))} %>% 
    rename("Country" = "XCountry", "XNA" = "XXNA") %>% 
    mutate_at(vars(-Country), as.numeric)

# The gii and gdi datasets were downloaded from http://hdr.undp.org/ and 
# The website does not provide stable download links to the csv files. 

giidata <- read_csv2("data/coding/Gender Inequality Index (GII).csv", na = "..", skip = 1) %>% 
    mutate(Country = trimws(Country), XNA = NA) %>% 
    {set_names(., paste0("X", names(.)))} %>% 
    rename("Country" = "XCountry", "XNA" = "XXNA") %>% 
    mutate_at(vars(-Country), as.numeric)

gdidata <- read_csv2("data/coding/Gender Development Index (GDI).csv", na = "..", skip = 1) %>% 
    mutate(Country = trimws(Country), XNA = NA) %>% 
    {set_names(., paste0("X", names(.)))} %>% 
    rename("Country" = "XCountry", "XNA" = "XXNA") %>% 
    mutate_at(vars(-Country), as.numeric)

# This dataset was downloaded from https://github.com/dbouquin/IS_608/blob/master/NanosatDB_munging/Countries-Continents.csv

continent_data <- read.csv("data/coding/country_continent.csv") %>% 
    rename("nation" = "Country", "continent" = "Continent") %>% 
    mutate(nation = recode(nation, "US" = "United States"))

# This dataset was downloaded from https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F10_1_SEX_RATIO_BY_BROAD_AGE_GROUP.xlsx

srdata <- read_xlsx("data/coding/UNO-Sex-ratio-age-specific.xlsx", skip = 16) %>% 
    filter(Type == "Country/Area") %>% 
    transmute(Country = `Region, subregion, country or area *`,
              Value = `25-49`, 
              Year = `Reference date (as of 1 July)`) %>% 
    filter(Year > 1980) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    pivot_wider(names_from = Year, values_from = Value) %>% 
    {set_names(., paste0("X", names(.)))} %>% 
    rename("Country" = "XCountry") %>% 
    mutate(Country = recode(Country, "United States of America" = "United States"), XNA = NA)


get_gii_gdi_sr <- function(nation_string_raw, year, dat){
    find_values <- function(.x, .y){
        # .x = "United States"; .y = 2002
        # dat <- giidata
        # dat <- gdidata
        # dat <- srdata
        
        giiyears <- as.numeric(str_remove(names(dat), "X")) %>% discard(is.na)
        
        if(.x %in% dat$Country){
            dat %>% 
                filter(Country == .x) %>% 
                select(eval(paste0("X", as.character(giiyears)))) %>% 
                flatten_dbl %>% 
                {data.frame(giival = when(., is.null(.) ~ NA, ~ .), 
                            year_diff = abs(giiyears - .y))} %>% 
                drop_na %>% 
                slice(which.min(year_diff)) %>% 
                select(giival) %>% 
                extract2(1)
        }else{
            NA_real_
        }
    }
    
    find_weighted_values <- function(nation_string, year){
        # This is for "mixed" nation codings.
        if(nchar(nation_string) > 100){
            # This filters out the Lippa study.
            NA_real_
        } else {
            splt <- str_split(nation_string, ",") %>% extract2(1)
            tibble(
                cntrs = splt[seq(1, length(splt), 2)] %>% trimws,
                prcnt = splt[seq(2, length(splt), 2)] %>% trimws %>% str_remove_all("\\\ |%") %>% as.numeric,
                giivals = map2(cntrs, rep(year, length(cntrs)), find_values) %>% flatten_dbl
            ) %>% drop_na %>% 
                {sum(.$prcnt * .$giivals) / sum(.$prcnt)}
        }
    }
    
    if(is.na(nation_string_raw)){
        NA_real_
    }else if(str_detect(nation_string_raw, "%")){
        find_weighted_values(nation_string_raw, year)
    }else{
        find_values(nation_string_raw, year)
    }
}

rs2 <- rs %>% 
    mutate(se.g = sqrt(var.g),
           n.total = n.m + n.f,
           cluster.abbr = cluster,
           cluster = domain %>% 
               recode("Behavior Frequency" = "Behavior Frequency", 
                      "Sex Drive" = "Self-Rated Sex Drive", 
                      "Cognition Frequency" = "Cognition Frequency", 
                      "Desire Frequency" = "Affect Frequency", 
                      "Desire Intensity" = "Affect Intensity", 
                      "ONS Lifetime" = "Total One Night Stand Partners", 
                      "Partners 12 Months" = "Sex Partners in Last Year", 
                      "Intercourse Frequency" = "Intercourse Frequency", 
                      "Partners Lifetime" = "Total Sex Partners"),
           inventory.abbr = inventory %>% recode("Sexuality Scale" = "SS", "Italian SOI-R" = "SOI-R"),
           inventory = inventory.abbr %>% recode("SDI" = "Sexual Desire Inventory", "CSFQ" = "Changes in Sexual Functioning Inventory", "HISD" = "Hurlbert Index of Sexual Desire", "SBI" = "Sexual Behavior Inventory", "SCS" = "Sexual Compulsivity Scale", "SKAT-A" = "Sex Knowledge and Attitudes Test Adolescents", "SOI-R" = "Sociosexual Orientation Inventory Revised", "SOI" = "Sociosexual Orientation Inventory", "SS" = "Sexuality Scale", "M-SOI" = "Multidimensional Sociosexual Orientation Inventory", "ISI" = "Imaginal Process Inventory", "none" = "Item not part of an inventory", "MSQ" = "Multidimensional Sexuality Questionnaire", "ISBI" = "Israeli Sexual Behaviour Inventory ", "DSFI" = "Derogatis Sexual Functioning Inventory", "IPI" = "Imaginal Process Inventory", "IIEF/FSFI" = "International Index of Erectile Function / Female Sexual Functioning Index", "SDQ" = "Sex Drive Questionnaire", "TSDS" = "Trait Sex Drive Scale"),
           age.mean = ifelse(is.na(age.mean), (n.m * age.mean.male + n.f * age.mean.female) / n.total, age.mean),
           age.sd = ifelse(is.na(age.sd), (n.m * age.sd.male + n.f * age.sd.female) / n.total, age.sd),
           age.diff = age.mean.male - age.mean.female,
           scale.min = min %>% na_if("NA") %>% as.numeric,
           scale.max = max %>% na_if("NA") %>% as.numeric,
           scale.range = scale.max - scale.min,
           scale.is.open = ifelse(scale == "open", "yes", "no"),
           year = coalesce(studyyear, pubyear - 2),
           ga.mean = rowMeans(data.frame(ga1,ga2,ga3,ga4,ga5,ga6,ga7,ga8), na.rm = T),
           ga.last = coalesce(ga8, ga7, ga6, ga5, ga4, ga3, ga2, ga1),
           ga.first.cat = ga1 %>% recode("1" = "male", "0" = "female"),
           singles.perc = ifelse(is.na(singles.perc), (n.m * singles.male.perc + n.f * singles.female.perc) / n.total, singles.perc),
           singles.diff = singles.male.perc - singles.female.perc,
           hetero.perc = ifelse(is.na(hetero.perc), (n.m * hetero.male.perc + n.f * hetero.female.perc) / n.total, hetero.perc),
           hetero.diff = hetero.male.perc - hetero.female.perc,
           aggregation.span = na_if(aggregation.span, "not specified") %>% as.numeric,
           scale.is.absolute.frequency = (cluster.abbr %in% c("DF", "CF", "BF") & !is.na(aggregation.span) & scale == "open") %>% 
               as.character %>% recode("TRUE" = "Yes", "FALSE" = "No"),
           scale.is.open = recode(scale.is.open, "open" = "yes", "closed" = "no"),
           publication.status = ifelse(journal.open == "unpublished", "unpublished", "published"),
           compensation = recode(compensation, "course.credit" = "coursecredit", "course credit" = "coursecredit", "courscredit" = "coursecredit", "courscredit/material" = "mixed", "courscredit/none" = "mixed", "none/courscredit" = "mixed", "no" = "none"),
           compensation = ifelse(compensation %in% c("material", "coursecredit", "none", NA), compensation, "mixed"),
           nation.raw = nation,
           nation = recode(nation, "USA" = "United States", "GB" = "United Kingdom", "UK" = "United Kingdom", "Scotland" = "United Kingdom", "Britain" = "United Kingdom", "German language speaker" = "Germany", "Trinidad And Tobago" = "Trinidad and Tobago", "Russia" = "Russian Federation"),
           gii =       map2(nation, year, get_gii_gdi_sr, dat = giidata) %>% flatten_dbl,
           gdi =       map2(nation, year, get_gii_gdi_sr, dat = gdidata) %>% flatten_dbl,
           sex.ratio = map2(nation, year, get_gii_gdi_sr, dat = srdata) %>% flatten_dbl,
           sex.journal = ifelse(str_detect(journal.open, "sex") | str_detect(journal.open, "Sex"), "Yes", "No")) %>% 
    left_join(continent_data, "nation") %>%
    mutate(continent = case_when(nation == "United States, 96.2%, Canada, 3.8%" ~ "North America",
                                 nation == "United States, 61.5%, Canada, 30.3%, NA, 8.3%" ~ "North America",
                                 TRUE ~ continent)) %>% 
    mutate(cntrl_extrapair = (content == "extra-pair partner") %>% as.numeric) %>% 
    mutate(sex.vio = map2(nation, year, get_gii_gdi_sr, dat = svdata) %>% flatten_dbl) %>% 
    select(mods_info %>% filter(!is.na(order)) %>% arrange(order) %>% select(modvarname) %>% flatten_chr,
           mods_info %>% arrange(class) %>% select(modvarname) %>% flatten_chr) 
    

    
    

# Also add new mods to mods_info.xlsx!!!

# Split and remove outliers
d2 <- split(rs2, rs2$cluster.abbr) %>% set_names(paste0("rs.", tolower(names(.)))) %>% 
    c(rs.core = rs2 %>% filter(rs2$cluster.abbr %in% c("BF", "CF", "DF")) %>% list) %>% 
    c(rs.second = rs2 %>% filter(rs2$cluster.abbr %in% c("DI", "SD")) %>% list) %>% 
    c(rs.control = rs2 %>% filter(rs2$cluster.abbr %in% c("P12", "TP", "ONS", "SF")) %>% list) %>% 
    c(rs = rs2 %>% list) %>% 
    map(~ filter(.x, .x$id.full.out != "Trudel_Dargis_Villeneuve_2014_1_1")) %>%  # Removing outliers
    map(~ filter(.x, .x$id.full.out != "Maxwell_Muise_MacDonald_2016_5_12")) %>% 
    map(~ filter(.x, .x$id.full.out != "McIntyre_Barlow_Hayward_2015_1_1")) %>% 
    c(rs.core_with_outlier = rs2 %>% filter(rs2$cluster.abbr %in% c("BF", "CF", "DF")) %>% list) %>% 
    c(rs.df_with_outlier = rs2 %>% filter(rs2$cluster.abbr == "DF") %>% list) %>% 
    c(rs_with_outlier = rs2 %>% list) %>% 
    c(rs.control_with_outlier = rs2 %>% filter(rs2$cluster.abbr %in% c("P12", "TP", "ONS", "SF")) %>% list) %>%
    c(rs.sf_with_outlier = rs2 %>% filter(rs2$cluster.abbr == "SF") %>% list) %>% 
    c(rs.second_with_outlier = rs2 %>% filter(rs2$cluster.abbr %in% c("DI", "SD")) %>% list) %>% 
    c(rs.di_with_outlier = rs2 %>% filter(rs2$cluster.abbr == "DI") %>% list) 

# sapply(d2, nrow) %>% View
    
save(d2, file = "results/es_prepared2.Rda")

write.csv(d2$rs %>% select(-cor_data), "full effect size data.csv")

# Checks
# all nation variables in giidata?
# sum of ethn = 100?

# rs2 %>% 
#     mutate(ethn.greater.100 = select(., starts_with("ethn")) %>% rowSums) %>% 
#     select(ethn.greater.100) %>% 
#     filter(!is.na(ethn.greater.100))



# Process correlational data ----------------------------------------------


es_cor <- d2$rs %>%
    filter(!duplicated(id.full)) %>%
    filter(!is.na(cor_data)) %>%
    mutate(rclass = map(cor_data, class)) %>% 
    filter(rclass =="data.frame") %>% 
    select(id.full, cor_data) %>%
    unnest(cor_data) %>% 
    mutate(z = fisherz(r),
           var.z = 1 / (n - 3),
           diff = var.r - var.z) %>% 
    inner_join(x = group_by(., label) %>% 
                   summarise(k = n_distinct(id.full), m = n()),
               y = robu(z ~ label -1, var.eff.size = var.z, data = ., studynum = id.full) %>% 
                   magrittr::extract2("reg_table") %>% 
                   mutate(label = gsub("label", "", as.character(labels))), 
               by = "label") %>%
    select(label, z = b.r, se.z = SE, t = t, df = dfs, p = prob, ll = CI.L, ul = CI.U, k, m) %>% 
    mutate(r = fisherz2r(z)) %>% 
    mutate_if(is.numeric, round, 5) %>% 
    separate(label, c("x1", "x2"), "_", remove = F)


# Save --------------------------------------------------------------------

save(es_cor, file = "results/es_cor.Rda")
