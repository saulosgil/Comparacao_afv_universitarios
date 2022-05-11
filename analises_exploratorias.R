# Carregando pacotes --------------------------------------------------------------------------
library(tidyverse)

# Função para fazer grafico exploratório ---------------------------------------------------------

grafico <- function(x , y){
  ggplot(mapping = aes(x = x,
                       y = y,
                       color = x)) +
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5,
      ## adjust height
      width = .6,
      ## move geom to the right
      justification = -.2,
      ## remove slab interval
      .width = 0,
      point_colour = NA,
      show.legend = FALSE
    ) +
    geom_boxplot(width = .12,
                 outlier.shape = NA,
                 outlier.color = NA,
                 show.legend = FALSE) +
    ggdist::stat_dots(
      ## orientation to the left
      side = "left",
      ## move geom to the left
      justification = 1.1,
      ## adjust grouping (binning) of observations
      binwidth = .10,
      show.legend = FALSE
    ) +
    coord_cartesian(xlim = c(1.2, NA)) +   ## remove white space on the left
    theme_classic()
}

#----------------------------------------------------------------------------

grafico(x = df_ajustado$area_conhecimento,
        y = df_ajustado$sf36_mental)




df_ajustado |>
  filter(!area_conhecimento == "Tecnologia") |>
  ggplot(mapping = aes(x = area_conhecimento,
                       y = comportamento_sedentario,
                       color = area_conhecimento)) +
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5,
    ## adjust height
    width = .6,
    ## move geom to the right
    justification = -.2,
    ## remove slab interval
    .width = 0,
    point_colour = NA,
    show.legend = FALSE
  ) +
  geom_boxplot(width = .12,
               outlier.shape = NA,
               outlier.color = NA,
               show.legend = FALSE) +
  ggdist::stat_dots(
    ## orientation to the left
    side = "left",
    ## move geom to the left
    justification = 1.1,
    ## adjust grouping (binning) of observations
    binwidth = .10,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +   ## remove white space on the left
  theme_classic()

