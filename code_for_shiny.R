library(tidyverse)
# library(plyr)
library(viridis)
# library(plotly)
library(haven)
library(skimr)

int_paper <- read_sas("../../INT_EEG_SUNY/data/merged/all_int_scores_max.sas7bdat")

scores <- read_csv("../data/for_plots.csv")
scores$sexch <- as.factor(scores$sexch)
scores$ind_id <- as.factor(scores$ind_id)
scores$race <- as.factor(scores$race)
scores$race1 <- as.factor(scores$race1)
scores$totalbin <- as.integer(scores$total > 0)

summary(scores)

## Plot for all scores

cbPalette <- c("#D55E00", "#0072B2")

# less than 

lt <- scores %>% filter(Age <= 30) %>% count() %>% as.numeric()

## continuous

scores %>% 
    filter(Age <= 30) %>% 
        ggplot(aes(x=total, y=100*(..count..)/sum(..count..))) +
            geom_histogram(bins=21, alpha = .6) +
            labs(x="Internalizing scale total score", y = "%") +
            ylim(0,50) +
            theme_bw(base_size = 12)

## binary

scores %>% 
    filter(Age <= 30) %>% 
    ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..))) +
    geom_bar(alpha = .6) +
    labs(x="Binary internalizing scale score (0 vs 1+)", y = "%") +
    ylim(0,70) +
    theme_bw(base_size = 12)

# greater than 

gt <- scores %>% filter(Age > 30) %>% count() %>% as.numeric()

## continuous

scores %>% 
    filter(Age > 30) %>% 
    ggplot(aes(x=total, y=100*(..count..)/sum(..count..))) +
    geom_histogram(bins=21, alpha = .6) +
    labs(x="Internalizing scale total score", y = "%") +
    ylim(0,50) +
    theme_bw(base_size = 12)

## binary

scores  %>% 
    filter(Age > 30) %>% 
    ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..))) +
    geom_bar(alpha = .6) +
    labs(x="Binary internalizing scale score (0 vs 1+)", y = "%") +
    ylim(0,70) +
    theme_bw(base_size = 12)






# The palette with grey:
cbPalette <- c("#D55E00", "#0072B2")

not100 <- ggplot(scores, aes(x=total, y=100*(..count..)/sum(..count..), fill = sexch)) +
    geom_histogram(bins=21, position="dodge", alpha = .6) +
    facet_grid(race ~ .) +
    labs(title="Distribution of internalizing scale total score \nby sex and race", 
         x="Internalizing scale total score", y = "% of total sample", fill = "Sex") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)

not100

# ggplotly(not100)

yes100 <- ggplot(scores, aes(x=total, fill = sexch)) +
    geom_histogram(bins=21, position="dodge", alpha = .6, 
                   aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(race ~ .) +
    labs(title="Distribution of internalizing scale total score \nby sex and race", 
         x="Internalizing scale total score", y = "% of total in each panel", fill = "Sex") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)

yes100
# ggplotly(yes100)


## Plot windsorizing at 11 (last category is 11+), panels sum to 100%

### Race in panels

ggplot(scores, aes(x=totalw, fill = sexch)) +
    geom_histogram(bins=11, position="dodge", alpha = .6, 
                   aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(race ~ .) +
    labs(title="Distribution of internalizing scale total score \nby sex and race", 
         x="Internalizing scale total score", y = "% of total in each panel", fill = "Sex") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)

### Gender in panels

ggplot(scores, aes(x=totalw, fill = race)) +
    geom_histogram(bins=11, position="dodge", alpha = .6, 
                   aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(sexch ~ .) +
    labs(title="Distribution of internalizing scale total score \nby race and sex", 
         x="Internalizing scale total score", y = "% of total in each panel", fill = "Race") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)


# Comparing this plot and the previous one, I think that we want to go with the first one because in *the bars in the panels for the second plot indicate, for example, that out of all females*, *~23% have a score of 0 and are EA* while *~10% have a score of 0 and are AA*. We know that we have more EAs than AAs due to US population composition, so that is not very informative.

# On the other hand, *in the first plot the bars indicate*, for example, *out of all AA, ~17% of females have a score of 0 while ~23% of males have a score of 0*.

# This makes me think that perhaps we don't want that each panel sums to 100% but we want that each histogram (e.g., all orange bars within a panel) sum to 100%.

# This is why I am also including the next two plots so we can look at them and decide which one is best for our paper.

## Plots with each histogram summing to 100%

ggplot(scores, aes(x=totalw, fill = sexch)) +
    geom_histogram(bins=11, alpha = .6, 
                   aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(race1*sexch ~ .) +
    guides(fill=FALSE) +
    labs(title="Distribution of internalizing scale total score \nby race and sex", 
         x="Internalizing scale total score", y = "% of total in each panel", fill = "Race") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)

# In my opinion it is easier to compare the four groups with this plot than with the plots that have histograms interleaved.

## Plots with each histogram summing to 100%, races together

ggplot(scores, aes(x=totalw, fill = sexch)) +
    geom_histogram(bins=11, alpha = .6,
                   aes(y = 100*((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))) +
    facet_grid(sexch ~ .) +
    guides(fill=FALSE) +
    labs(title="Distribution of internalizing scale total score by sex", 
         x="Internalizing scale total score", y = "% of total in each panel", fill = "Sex") +
    scale_fill_manual(values=cbPalette) +
    theme_bw(base_size = 12)
