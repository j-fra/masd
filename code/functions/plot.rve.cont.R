plot.rve.cont <- function(model,
                          modvar, 
                          titlename = NULL) {
    ### this function creates plots for continuous moderators
    
    # model = robu(g ~ scale.range, d2$rs.cf, id.full, var.g)
    
    # get data
    temp.df <- model$data %>% 
        filter(!is.na(get(modvar)))
    
    
    # retrieve weights
    temp.df.x <- temp.df %>% 
        mutate(g_join = g %>% round(5),
               var.g_join = var.g %>% round(5)) %>% 
        inner_join(model$data.full %>% 
                       mutate(g_join = effect.size %>% round(5),
                              var.g_join = var.eff.size %>% round(5)), 
                   by = c("g_join", "var.g_join")) %>% 
        mutate(weights = r.weights)
    
    if(nrow(temp.df.x) != nrow(temp.df) | 
       nrow(temp.df.x) != nrow(model$data.full) | 
       nrow(temp.df) != nrow(model$data.full)){
        stop("Something went wront when joining data and data.full from the robu object")
    }
    
    
    # write universal df and rename
    temp.df2 <- temp.df.x[c("g", modvar, "weights")]
    
    
    thetitle <- ifelse(is.null(titlename), paste0("Model: ", modvar), titlename)
    
    dfval <- model$reg_table$dfs[2] %>% rnd_2
    tval <- model$reg_table$t[2] %>% rnd_2
    pval <- model$reg_table$prob[2] %>% rnd_p(withEqualSign = T)
    
    overall.test.caption <- bquote("Test of Slope:" ~ italic('AHZ') * "(" * .(dfval) * ") = " * .(tval) * ", " * italic('p ')  * .(pval))
    
    # return plots
    ggplot(data = temp.df2, aes_string(x = modvar, y = "g", size = "weights")) + 
        geom_point(alpha = 0.5) +
        geom_abline(aes(intercept = model$reg_table$b.r[1], slope = model$reg_table$b.r[2]), size = 1) + 
        ggtitle(thetitle) +
        ylab(expression(paste("Hedges' ", italic('g')))) +
        xlab(modvar) +
        theme_bw(base_size = 12) +
        scale_y_continuous(breaks = seq(-2, 2, 0.2)) + 
        guides(size = FALSE) +
        scale_shape_manual(values = c(1,19)) +
        labs(caption = overall.test.caption) +
        theme(
            panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
            axis.title.y=element_text(margin=margin(0, 7,0,0))) + 
        theme_apa(facet.title.size = 10)
}