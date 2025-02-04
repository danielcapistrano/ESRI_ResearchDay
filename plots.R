library(tidyverse)
library(knitr)
library(purrr)
library(gt)
library(gtsummary)
library(see)
library(lmerTest)
library(treemapify)

# Loading dataset
load("./data/df_final_2025-02-04.RData")

# Copying dataset with deepseek-r1:8b
df_ds <- df

# Removing deepseek-r1:8b from the main dataset
df <- df |>  filter(model != "deepseek-r1:8b")

# Plot 1 - Gender distribution

plot_gender <-
    df  |> 
        filter(domain %in% c("cc", "hs", "bk") & is.na(regulation)) |>
        ggplot(aes(x = gender, y = score, fill = gender)) +
            geom_violinhalf(alpha = 0.6) + 
            geom_boxplot(width = 0.1, show.legend = FALSE) +
            scale_fill_brewer(palette="Set1")+
            facet_wrap(~domain_lab) +
            guides(shape = FALSE) +
            theme(plot.background = element_rect(fill='transparent', color=NA),
                legend.title=element_blank(),
                strip.text.x = element_text(size = 13, face = "bold"))

ggsave(plot_gender, filename = './img/plot_gender.png', width = 7, height = 4, dpi = 300)



# Plot 2 - Ethnic origin distribution

plot_ethnic <-
    df  |> 
        filter(is.na(regulation)) |> 
        ggplot(aes(x = origin, y = score, fill = origin)) +
            geom_violinhalf(alpha = 0.6) +
            geom_boxplot(width = 0.1, show.legend = FALSE) +
            scale_fill_brewer(palette="Set1")+
            guides(shape = FALSE) +
            theme(plot.background = element_rect(fill='transparent', color=NA),
                legend.title=element_blank())

ggsave(plot_ethnic, filename = './img/plot_ethnic.png', width = 7, height = 4, dpi = 300)


# Plot tree - explanation


plot_tree <- 
  df |> 
    filter(is.na(regulation) & origin %in% c("Irish", "SSA")) |>
    group_by(domain_lab, origin, explanation) |> 
    summarise(mentions = n(), avg_score=mean(score)) |>
    slice_max(mentions, n = 5) |>
    ggplot(aes(area= mentions, fill = domain_lab, label = explanation)) +
    geom_treemap() +
    geom_treemap_text(grow = T, reflow = T, colour = "white") +
    scale_fill_brewer(palette = "Set1")+
    facet_wrap(~origin)+
    theme_classic() +
    theme(plot.background = element_rect(fill='transparent', color=NA),
        strip.text = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title=element_blank())

ggsave(plot_tree, filename = './img/plot_treemap.png', width = 7, height = 4, dpi = 300)

# Plot 3 - Place

plot_place <-
    df  |>
        filter(is.na(regulation)) |>
        group_by(origin, place_lab) |>
        summarise(mean = mean(score, na.rm = TRUE), se = sd(score)/sqrt(length(score))) |>
        ggplot(aes(x = origin, y = mean, ymin = mean - 2*se, ymax = mean + 2*se, color = origin)) +
            geom_pointrange(width=1, alpha = 0.6, size =1) + 
            facet_wrap(~place_lab, nrow = 1) +
            scale_color_brewer(palette="Set1")+
            theme(plot.background = element_rect(fill='transparent', color=NA),
                legend.title=element_blank(),
                strip.text.x = element_text(size = 13, face = "bold"))

ggsave(plot_place, filename = './img/plot_place.png', width = 7.5, height = 4, dpi = 300)

# Plot 3 - Model

plot_model <-
    df_ds  |>
        mutate(model_lab = if_else(model == "deepseek-r1:8b", "DeepSeek R1 8b", model_lab)) |>
        filter(!is.na(origin) & is.na(regulation)) |>
        group_by(origin, model_lab) |>
        summarise(mean = mean(score, na.rm = TRUE), se = sd(score, na.rm = T)/sqrt(length(score))) |>
        ggplot(aes(x = origin, y = mean, ymin = mean - 2*se, ymax = mean + 2*se, color = origin)) +
            geom_pointrange(linewidth=1, alpha = 0.6, size =1) + 
            facet_wrap(~factor(model_lab, c( "GPT 4o", "GPT 4o mini", "DeepSeek R1 8b")), nrow = 1) +
            scale_color_brewer(palette="Set1")+
            theme(plot.background = element_rect(fill='transparent', color=NA),
                legend.title=element_blank(),
                strip.text.x = element_text(size = 13, face = "bold"))

ggsave(plot_model, filename = './img/plot_model.png', width = 7, height = 4, dpi = 300)

# Plot 4 - Regulation

plot_regulation <-
    df  |>
        group_by(regulation_lab, origin) |>
        summarise(mean = mean(score, na.rm = TRUE), se = sd(score)/sqrt(length(score))) |>
        ggplot(aes(x = origin, y = mean, ymin = mean - 2*se, ymax = mean + 2*se, color = origin)) +
            geom_pointrange(linewidth=1, alpha = 0.6, size =1) + 
            facet_wrap(~regulation_lab, nrow = 1) +
            scale_color_brewer(palette="Set1")+
            theme(plot.background = element_rect(fill='transparent', color=NA),
                legend.title=element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text.x = element_text(size = 13, face = "bold"))

ggsave(plot_regulation, filename = './img/plot_regulation.png', width = 7, height = 4, dpi = 300)

