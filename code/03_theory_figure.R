library(tidyverse)
library(cowplot )

set.seed(123)
# set.seed(124)

C <- 0
y <- C
p1 <- 0.9
sig_e <- 0.8

for(i in 1:40){
    y[i + 1] <- C + p1 * y[i] + rnorm(1, 0, sig_e) 
}

prob_func <- function(p){
    sample(c(T,F), 1, prob = c(p, 1-p))
    # prop_func(0.5)
}

b_sc <- 0.8
b_sa <- 0.5 
b_sb <- 0.2

dat <- tibble(y = y,
              time = 1:length(y),
              p_sc = (1 / (1 + exp(1)^(-(y))))* b_sc,
              p_sa = (1 / (1 + exp(1)^(-(y))))* b_sa,
              p_sb = (1 / (1 + exp(1)^(-(y))))* b_sb)

set.seed(5)

dat <- dat %>% mutate(sc = map(p_sc, prob_func) %>% flatten_lgl,
                      sa = map(p_sa, prob_func) %>% flatten_lgl,
                      sb = map(p_sb, prob_func) %>% flatten_lgl) 


sqsize <- 0.5
csize <- 4


(plt <- plot_grid(
    ggplot(dat, aes(x = y)) + 
        geom_histogram(aes(y =..density..), breaks = seq(-3.5, 3.5, 1), fill = "grey") + 
        stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(sig_e^2 / (1-p1^2))), size = 1.2, linetype = 2) + 
        coord_flip() + 
        scale_x_continuous(limits = c(-4, 4)) +
        scale_y_reverse(breaks = NULL, expand = c(0, 0)) + 
        ylab("Density") + 
        xlab("State Sex Drive") + 
        theme_minimal() +
        theme(panel.grid.major = element_line(colour = "lightgrey"),   # Major grid lines
              panel.grid.minor = element_line(colour = "white"),
              plot.margin=unit(c(1,0.1,0,-2),"cm")
              # axis.title.y=element_blank(),
              # axis.text.y=element_blank(),
              # axis.ticks.y=element_blank()
        ),
    ggplot(data = spline(dat$y) %>% as_tibble, aes(x = x, y = y)) + 
        geom_line(col = "black", size = 1.2) + 
        xlab("Time") + 
        ylim(c(-4, 4)) + 
        ylab("State Sex Drive") +
        theme_minimal()+
        theme(panel.grid.major = element_line(colour = "lightgrey"),   # Major grid lines
              panel.grid.minor = element_line(colour = "white"),
              plot.margin=unit(c(1,0.3,0,1.2),"cm")) + 
        scale_x_continuous(breaks = NULL, expand = c(0, 0)),
    ggplot() + 
        theme_void() + 
        geom_rect(aes(xmin = -7.2, xmax = 3, ymin = -3, ymax = 3), fill = "white", colour = "black") +
        geom_rect(aes(xmin = -sqsize-6, xmax = sqsize-6, ymin = -sqsize, ymax = sqsize), fill = "#8CBCB9") +
        geom_rect(aes(xmin = -sqsize-6, xmax = sqsize-6, ymin = -sqsize-1.5, ymax = sqsize-1.5), fill = "#BB513B") +
        geom_rect(aes(xmin = -sqsize-6, xmax = sqsize-6, ymin = -sqsize+1.5, ymax = sqsize+1.5), fill = "#DDA448") +
        geom_text(aes(-5,1.5,label="Sexual Cognition"), size = 4, hjust = 0) +
        geom_text(aes(-5,0,label="Sexual Affect"), size = 4, hjust = 0) + 
        geom_text(aes(-5,-1.5,label="Sexual Behavior"), size = 4, hjust = 0) +
        coord_fixed(),
    ggplot(dat, aes(x = time, y = y)) +
        geom_line(aes(x, y), spline(dat$p_sc) %>% as_tibble, size = 1.2, colour = "#DDA448") + 
        geom_line(aes(x, y), spline(dat$p_sb) %>% as_tibble, size = 1.2, colour = "#BB513B") + 
        geom_line(aes(x, y), spline(dat$p_sa) %>% as_tibble, size = 1.2, colour = "#8CBCB9") +
        ylab("Probability of Sexual Event") +
        ylim(c(0, 1)) + 
        xlab("Time") +
        theme_minimal()+
        theme(panel.grid.major = element_line(colour = "lightgrey"),   # Major grid lines
              panel.grid.minor = element_line(colour = "white"),
              plot.margin=unit(c(1,0.3,0,1.2),"cm")) + 
        scale_x_continuous(breaks = NULL, expand = c(0, 0)),
    ggplot() + 
        theme_void() + 
        geom_rect(aes(xmin = -7.2, xmax = 5, ymin = -4, ymax = 4), fill = "white", colour = "white") +
        geom_text(aes(3.2,0,label="{"), size = 24, hjust = 0) +
        geom_text(aes(-2,0,label="Operationalization\nfor the present\nmeta-analysis"), size = 4) +
        theme(plot.margin=unit(c(0, 0,0, 0),"cm")) +
        coord_fixed(),
    dat %>%   pivot_longer(cols = c(sc, sb, sa)) %>%
        mutate(event = ifelse(value, toupper(name), NA)) %>% 
        filter(!is.na(event)) %>% 
        ggplot(aes(time, event)) + 
        geom_tile(aes(fill = factor(event, levels = c("SC", "SA", "SB"))), colour = "white") +
        theme_minimal() + 
        scale_y_discrete(limits=c("SB", "SA", "SC")) + 
        scale_fill_manual(values=c("#DDA448", "#8CBCB9", "#BB513B")) + 
        xlab("Time") + 
        ylab("Sexual Event") +
        guides(fill=FALSE) +
        theme(panel.grid.major = element_line(colour = "white"),   # Major grid lines
              panel.grid.minor = element_line(colour = "white"),
              plot.margin=unit(c(1,0.3,0,1.2),"cm")) + 
        scale_x_continuous(breaks = NULL, expand = c(0, 0)),
    
    align = "v", nrow = 3, rel_heights = c(4, 4, 2), rel_widths = c( 1.2, 3), ncol = 2, labels = c("A1", "A2", "", "B", "", "C"), label_x = 0.03
))

plt

ggsave(plt, filename = "results/figures and tables/theory_figure.png", width = 10, height = 8, units = 'in', dpi = 300)

shell.exec(paste0(getwd(), "/results/figures and tables/theory_figure.png"))


# Ich schließe mich komplett an. Der Punkt ist so zentral, dass eine solch einfache Figure okay ist, aber sie wirkt tatsächlich wenig spektakulär.
# Wenn wir von unserem theoretischen Konzept sprechen, umfasst dies am Ende immer zwei Elemente:
#   (1) Trias
# (2) State/Trait (Whole-Trait-Ansatz).
# 
# Warum nicht eine (per se neue und fancy) Figure, die beide Punkte verbindet?
#   Basierend auf Julius' aktuellem Konzeptentwurf z. B.:
# 
# Ebene 1: 
# Häufigkeit Cog-Aff-Beh (jeweils);
# 
# Ebene 2:
# Kontinuierlicher State
# 
# --> Ebene 1 und 2 sehen aus wie aktuelle Figure 1, nur (a) mehrfach nebeneinander und (b) mit konkreten Häufigkeiten und konkreten Werten auf dem kontinuierlichen State.
# 
# Ebene 3:
# Trait als Density Distribution
# 
# Anmerkung:
# Eine solche Verzahnung beider Elemente muss natürlich immer zur finalen Textversion passen. Ich kann mir aber vorstellen, dass wir weitgehend unabhängig von der Nähe der Anlehnung an Fleeson eine Version finden, die beide Elemente umfasst und "fancier" ist als die aktuelle Figure 1.


