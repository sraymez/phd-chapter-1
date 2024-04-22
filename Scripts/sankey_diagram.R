## Sankey diagram


# Load packages ----
library(pacman)
p_load(RColorBrewer, ggalluvial, ggplot2, tidyverse)


# pre-2000, 2000-2010, 2011-2020, post-2020


# Create data frame ----
data <- data.frame(
  Id = c(1:32),
  decade = c("2000-2010", "2011-2020", "2000-2010", "2011-2020", "2011-2020", "2000-2010",
             "2011-2020", "2011-2020",
             "pre-2000", "2011-2020", "2011-2020",
             "2011-2020",
             "pre-2000", "pre-2000",
             "2011-2020", "2000-2010",
             "pre-2000", "2000-2010", "post-2020", "2000-2010", "pre-2000", "2011-2020",
             "2011-2020", "pre-2000",
             "pre-2000",
             "2011-2020", "pre-2000",
             "post-2020",
             "pre-2000", "pre-2000", "2000-2010",
             "2011-2020"),
  decade2 = c("pre-2010", "post-2010", "pre-2010", "post-2010", "post-2010", "pre-2010",
              "post-2010", "post-2010",
              "pre-2010", "post-2010", "post-2010",
              "post-2010",
              "pre-2010", "pre-2010",
              "post-2010", "pre-2010",
              "pre-2010", "pre-2010", "post-2010", "pre-2010", "pre-2010", "post-2010",
              "post-2010", "pre-2010",
              "pre-2010",
              "post-2010", "pre-2010",
              "post-2010",
              "pre-2010", "pre-2010", "pre-2010",
              "post-2010"),
  pathogen = c("NTS", "NTS", "NTS", "NTS", "NTS", "NTS",
               "Q-fever", "Q-fever",
               "Crypto", "Crypto", "Crypto",
               "Hep E",
               "Leish", "Leish",
               "Brucella", "Brucella",
               "Trypano", "Trypano", "Trypano", "Trypano", "Trypano", "Trypano",
               "RVF", "RVF",
               "Anthrax",
               "Rabies", "Rabies",
               "CCHF",
               "YF", "YF", "YF",
               "Zika"),
  type = c("Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria", 
           "Bacteria", "Bacteria", 
           "Protozoa", "Protozoa", "Protozoa",
           "Virus",
           "Protozoa", "Protozoa",
           "Bacteria", "Bacteria",
           "Protozoa", "Protozoa", "Protozoa", "Protozoa", "Protozoa", "Protozoa",
           "Virus", "Virus",
           "Bacteria",
           "Virus", "Virus",
           "Virus",
           "Virus", "Virus", "Virus",
           "Virus"),
  population = c("Human", "Human", "Human", "Human", "Human", "Human", 
                 "Human & animal", "Animal",
                 "Human", "Human", "Human",
                 "Human",
                 "Animal", "Human",
                 "Human & animal", "Animal",
                 "Human", "Animal", "Animal", "Animal", "Animal", "Animal",
                 "Animal", "Animal",
                 "Human",
                 "Human", "Human",
                 "Animal",
                 "Human", "Human", "Human",
                 "Animal"),
  priority = c("No", "No", "No", "No", "No", "No",
               "No", "No",
               "No", "No", "No",
               "No",
               "No", "No",
               "No", "No",
               "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
               "Yes", "Yes", 
               "Yes", 
               "Yes", "Yes", 
               "Yes", 
               "Yes", "Yes", "Yes", 
               "No")
)



# Create custom colour palette ----

# Use preset 'Paired' palette then append extra colour 
custom_palette <- c(brewer.pal(12, "Paired"), "#555555")


# Create and customise sankey plot ----
ggplot(
  data = data,
  aes(
    axis1 = pathogen,
    axis2 = decade2,
    #axis3 = type, 
    axis3 = population,
    axis4 = priority)) +
  geom_alluvium(
    aes(
      fill = pathogen), 
    show.legend = F,
    width = 0.25) +
  geom_stratum(
    width = 0.25,
    colour = "red4") +
  geom_text(
    aes(
      label = after_stat(stratum)),
    stat = "stratum") +
  scale_x_discrete(
    limits = c("Paper", "Decade", "Population", "Priority"), 
    expand = c(.05, .05)) +
  scale_y_discrete(
    limits = factor(1:32)) +
  scale_fill_manual(
    values = custom_palette) +
  theme_minimal() +
  ggtitle(
    "Categorisation of papers on zoonotic diseases in The Gambia"
  )










