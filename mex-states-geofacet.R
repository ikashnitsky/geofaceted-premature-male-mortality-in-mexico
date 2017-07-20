################################################################################
#                                                    
# IDEM viz 2017-06-26 (UPD 2017-07-20)
# Challenge Rostock Retreat
# Michael Boissonneault, Jorge Cimentada, Juan Galeano,
# Corina Huisman, Ilya Kashnitsky, and Nikola Sander
# Correspondence: ilya.kashnitsky@gmail.com
#                                                  
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

# load required packages
library(tidyverse) # data manipulation and viz
library(lubridate) # easy manipulations with dates
library(ggthemes) # themes for ggplot2
library(forcats) # good for dealing with factors
library(stringr) # good for dealing with text strings
library(magrittr)
library(RColorBrewer)
library(rgdal) # deal with shapefiles
library(rgeos)
library(geofacet)


# Authors
auth <- "\nRostock Retreat Visualization Challenge: Michael Boissonneault, Jorge Cimentada, Juan Galeano, Corina Huisman, Ilya Kashnitsky, and Nikola Sander, 2017"

# to read in the R data.frame
df <- local(get(load("MexicoMalesMiddleAge.Rdata")))
names(df)  %<>%  tolower()

# fix names
df %<>% mutate(name = statename %>% 
                       str_replace("á", "a") %>% 
                       str_replace("í", "i") %>%
                       str_replace("é", "e") %>%
                       str_replace("ó", "o")) 


# load proper codes
load("codes.RData")

# attach codes
df <- df %>% select(-state, -statename, -sex, -cause) %>% 
        left_join(codes, "name")


################################################################################
# Geofaceting with real data
# note that "mx_state_grid1" is now part of `geofacet` v. 0.1.5

# select from ten causes
df_ten <- df %>% 
        group_by(code, year, age) %>% 
        filter(gap == gap %>% max()) %>% 
        ungroup() %>% 
        mutate(causename = causename %>% 
                       str_replace("Amenable to medical service", 
                                   "Amenable to\nmedical service")) 


# create colors
gr <- brewer.pal(9, "Greens")[c(3,5,7,9)]
set <-  brewer.pal(9, "Set1")
colors <- c(set[7], gr[4], gr[3], gr[1], set[1], gr[2], set[9], set[2], set[4])

# geofaceting 10 causes
gg_ten <- ggplot(df_ten)+
        coord_cartesian(expand = c(0,0))+
        geom_tile(aes(year, age, fill = causename))+
        facet_geo(~ code, grid = "mx_state_grid1", label = "name") +
        scale_x_discrete(breaks = c(1990, 2000, 2010),
                           labels = c(1990, 2000, "'10"))+
        scale_fill_manual("Causes\nof death\n", 
                          values = colors)+
        labs(x = NULL, y = NULL, 
             title = "Gap between observed and best-practice life expectancy for Mexican states",
             subtitle = "Cause of death contributing the most by age (15-49) and time (1990-2015)",
             caption = auth)+
        theme_few(base_size = 12)+
        theme(legend.position = c(1,1),
              panel.border = element_rect(color = 'black',
                                          size=.5, fill = NA),
              panel.spacing = unit(1, "lines"),
              legend.key = element_rect(size = 7),
              legend.key.size = unit(2, 'lines'),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.justification = c(1,1),
              strip.text = element_text(face = 2),
              plot.title = element_text(size = 30),
              plot.subtitle = element_text(size = 24))

ggsave("gg-ten.png", gg_ten, width = 16, height = 12)




################################################################################
# geofaceted stacked bars

# the data preparation for this plot was done by Jorge, I was too lazy to change everything to match my code. Thus, I simply load the initital datatset again

df_stacked <-
        df %>%
        mutate(cause_recode = causename %>% 
                       as_factor() %>% 
                       fct_collapse("Homicide" = "Homicide",
                                    "Other" = "Other",
                                    "Road traffic + Suicide" = c("Road traffic",
                                                            "Suicide"),
                                    "Natural + HIV" = c("Diabetes", 
                                                        "IHD",
                                                        "Cirrhosis",
                                                        "Lung cancer",
                                                        "HIV"),
                                    "Amenable to\nmedical service" = 
                                       "Amenable to medical service")
                       ) %>%
        group_by(code, year, age, cause_recode) %>%
        summarize(gap = gap %>% sum()) %>% 
        ungroup() %>% 
        group_by(cause_recode) %>%
        mutate(cause_gap = gap %>% sum()) %>% 
        ungroup() %>% 
        mutate(cause = cause_recode %>% 
                       fct_reorder(cause_gap) %>% 
                       fct_rev())

# create labels with the total years lost data
label <- df_stacked %>% 
        mutate(cause_recode = cause_recode %>% paste()) %>% 
        group_by(cause_recode) %>% 
        summarise(cause_total = gap %>% sum()) %>% 
        ungroup() %>% 
        arrange(cause_total %>%  desc()) %>% 
        mutate(label = paste0(cause_recode, " [",
                             cause_total %>% round(), "]")) %>% 
        pull(label)

levels(df_stacked$cause) <- label

col5 <- colors[c(5,7,6,1,3)]
mixblue <- colorRampPalette(colors[c(8,9)])
col5[3] <- mixblue(3)[2]



gg_stacked <- ggplot(df_stacked) +
        geom_col(aes(year, gap, fill = cause)) +
        coord_cartesian(ylim = c(0, 2), expand = c(0,0))+
        scale_x_discrete(breaks = c(1990, 2000, 2010),
                         labels = c(1990, 2000, "'10"))+
        scale_fill_manual(name = "Causes [Total years lost]\n", values = col5) +
        facet_geo(~ code, grid = "mx_state_grid1", label = "name")+
        labs(x = NULL, y = NULL, 
             title = "Gap between observed and best-practice life expectancy for Mexican states",
             subtitle = "Years of life lost by cause of death across time (1990-2015)",
             caption = auth)+
        theme_few(base_size = 12)+
        theme(legend.position = c(1,1),
              panel.border = element_rect(color = 'black',
                                          size=.5, fill = NA),
              panel.spacing = unit(1, "lines"),
              legend.key = element_rect(size = 7),
              legend.key.size = unit(2, 'lines'),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.justification = c(1,1),
              strip.text = element_text(face = 2),
              plot.title = element_text(size = 30),
              plot.subtitle = element_text(size = 24))



ggsave("gg-stacked.png", gg_stacked, width = 16, height = 12)




