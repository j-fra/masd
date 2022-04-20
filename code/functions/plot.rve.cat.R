plot.rve.cat <- function(model,
                         model2,
                         wald,
                         modvar,
                         titlename = NULL, 
                         levelorder = NULL, 
                         ...) {
    
    # get data
    outcome <- "g"
    out.lab <- "g"
    xvar <- "se.g"
    x.lab = "Standard Error"
    y.lab = "Hedges' g"
    
    
    temp.df <- model$data %>% 
        filter(!is.na(get(modvar))) %>% 
        droplevels() %>% 
        mutate(!!modvar := !!expr(as.factor(get(modvar))))
    
    
    # data = model$data
    # temp.df <- droplevels(subset(data, !is.na(data[modvar]) & !is.na(data[modvar])))
    # temp.df[[modvar]] <- as.factor(temp.df[[modvar]])
    
    # set defaults
    if(is.null(levelorder))levelorder <- c(1:(length(unique(temp.df[[modvar]]))))
    
    # generate facet headers
    top <- paste("textstyle('", levels(temp.df[[modvar]]), "')", sep = "")
    bottom <- paste("italic(", out.lab, ")", "*", 
                    "textstyle(' = ')", "*textstyle('",
                    as.character(format(round(model2$reg_table$b.r, 2), nsmall = 2)), "')*","textstyle(', ')*",  
                    "italic('k')", "*", 
                    "textstyle(' = ')", "*textstyle('",
                    as.character(unlist(lapply(split(temp.df, temp.df[[modvar]]), function(x)return(length(unique(x$id.full)))))), "')", "*", "textstyle(', ')*",
                    "italic('m')", "*", 
                    "textstyle(' = ')", "*textstyle('",
                    as.character(as.numeric(table(temp.df[[modvar]]))), "')",
                    sep = "")
    levels(temp.df[[modvar]]) <- paste("atop(", top, ", ", bottom, ")", sep = "")
    
    # generate dummy dataframe containing the summary effects and confidence intervalls
    dummy <- data.frame(height = model2$reg_table$b.r,
                        mod = levels(temp.df[[modvar]]),
                        lower = model2$reg_table$CI.L,
                        upper = model2$reg_table$CI.U)
    # retrieve weights
    temp.df$weights <- model$data.full$r.weights
    
    # write universal df and rename
    temp.df2 <- temp.df[c(outcome, modvar, xvar, "weights")]
    temp.df2$outcome.variable <- temp.df2[[c(outcome)]]
    names(temp.df2) <- c(outcome, "mod", "x.axis.var", "weights", "outcome.variable")
    temp.df2$mod <- factor(temp.df2$mod, levels = levels(temp.df2$mod)[levelorder])
    
    # Collect information on overall test
    DFval <- wald$df_denom  %>% rnd_2
    Fval <- wald$Fstat %>% rnd_2
    Pval <- wald$p_val %>% rnd_p(withEqualSign = T)
    overall.test.caption <- bquote("Overall test:" ~ italic('AHZ') * "(" * .(DFval) * ") = " * .(Fval) * ", " * italic('p ')  * .(Pval))
    
    thetitle <- ifelse(is.null(titlename), paste0("Model: ", modvar), titlename)
    
    dummy$mod <- factor(dummy$mod, levels = levels(as.factor(dummy$mod))[levelorder])
    
    # return plots
    ggplot(aes(x = x.axis.var, y = outcome.variable, size = weights), data = temp.df2) + 
        geom_point(alpha = 0.5) + 
        facet_grid( ~mod, labeller=label_parsed) + 
        geom_hline(data = dummy, aes(yintercept = height), size = 1.5) + 
        geom_hline(data = dummy, aes(yintercept = lower), linetype = 7) +
        geom_hline(data = dummy, aes(yintercept = upper), linetype = 7) +
        geom_hline(aes(yintercept = 0), size = 1, linetype = 2, colour = "grey") + 
        # theme_bw() + 
        scale_shape_manual(values = c(1,19)) + 
        # scale_y_continuous(breaks = seq(-2, 2, 0.2)) + 
        xlab(x.lab) + 
        ylab(y.lab) +
        ggtitle(thetitle) +
        guides(size = FALSE) + 
        labs(caption = overall.test.caption) +
        theme(panel.background = element_rect(fill = "white", colour = "white", 
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
              axis.title.y=element_text(margin=margin(0, 7,0,0))) + 
        theme_apa(facet.title.size = 10)
}