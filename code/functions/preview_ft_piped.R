preview_ft_piped <- function(ft, portrait = F){
    # This is for previewing a flextable in a word doc.
    tf <- tempfile(fileext = ".docx")
    read_docx() %>% 
        body_add_flextable(value = ft, split = TRUE) %>% 
        {if(portrait){
            body_end_section_portrait(.)
        }else{
            body_end_section_landscape(.)
        }} %>% 
        print(target = tf)
    shell.exec(tf)
    Sys.sleep(3)
    message(paste0("I should check some time if windows actually deletes the temp files: ", tf))
    return(ft)
}