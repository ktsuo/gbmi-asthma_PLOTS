library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggrepel)

# smile plot function
plot_af_effect <- function(input_data, pt_size, alpha_level, text_size){
  plot <- ggplot(input_data, aes(x = risk_allele_freq, y = risk_allele_beta, color = Category)) + 
    geom_point(size=pt_size, alpha=alpha_level) +
    geom_smooth(se = FALSE, size=pt_size) +
    scale_color_manual(values = c('PotentiallyNovel' = 'orangered3', 'PreviouslyReported' = 'steelblue3'), labels=c('Potentially Novel', 'Previously Reported')) +
    labs(color='Category') + 
    xlab('Risk Allele Frequency') + ylab('Risk Allele Effect Size') +
    theme_bw() +
    theme(text=element_text(size=text_size),panel.grid = element_blank()) #+ 
    geom_label_repel(data = input_data,
                     aes(x=risk_allele_freq, y=risk_allele_beta, color=Category, label=Gene.refGene),
                     box.padding=0.5,
                     show.legend=FALSE)
  return(plot)
}

# add risk allele columns function
add_risk_allele <- function(input_data, beta_col, AF_col, ALT_col, REF_col){
  df <- input_data %>% mutate(risk_allele = ifelse(get(beta_col) > 0, get(ALT_col), get(REF_col)),
                              risk_allele_freq = ifelse(get(beta_col) > 0, get(AF_col), 1 - get(AF_col)),
                              risk_allele_beta = ifelse(get(beta_col) > 0, get(beta_col), -get(beta_col)))
  return(df)
}

# plot

top_hits = read_excel("~/Bothsex_inv_var_meta.tophits.onetophitperlocus.txt_hg38_meta_annovar_500kb_05252021_newKnowAsthmaList.xlsx", 
                      sheet = "Asthma")
top_hits <- add_risk_allele(top_hits, 'all_inv_var_meta_beta', 'all_meta_AF', 'ALT', 'REF')

pt = 3
alpha = 0.5
text = 16
plot <- plot_af_effect(top_hits, pt, alpha, text)

plot_output_name = 'smileplot.jpeg'
ggsave(plot, height = 8, width = 12, dpi = 300, filename=plot_output_name)
