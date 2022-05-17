# Carregando pacotes --------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Lendo a base --------------------------------------------------------------------------------

df_ajustado <- read.csv2(file = "data/df_ajustado.csv")


# Verificando outliers ------------------------------------------------------------------------

# Função para marcar oulier

grafico_outlier <- function(variavel,z_score_maior_2){
  df_ajustado |>
    ggplot(ggplot2::aes(x = area_conhecimento,
                        y = variavel,
                        color = z_score_maior_2)) +
    geom_point(size = 3) +
    theme_minimal()
}

# MVPA

g1 <- grafico_outlier(df_ajustado$mvpa,
                df_ajustado$e_outlier_mvpa) +
  labs(title = "MVPA")

# Comportamento sedentário

g2 <- grafico_outlier(df_ajustado$comportamento_sedentario,
                df_ajustado$e_outlier_comportamento_sedentario) +
  labs(title = "Comportamento Sedentário")

# SF-36 Físico

g3 <- grafico_outlier(df_ajustado$sf36_fisico,
                df_ajustado$e_outlier_sf36_fisico) +
  labs(title = "SF-36 - Dominio Físico")

# SF-36 Mental

g4 <- grafico_outlier(df_ajustado$sf36_mental,
                df_ajustado$e_outlier_sf36_mental) +
  labs(title = "SF-36 - Dominio Mental")

# Layout

g1+g2+g3+g4


# Função para fazer grafico exploratório ---------------------------------------------------------

grafico <- function(x , y){
  ggplot(mapping = aes(x = x,
                       y = y,
                       color = x)) +
    xlab(label = "") +
    ylab(label = "") +
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

# Graficos com os outliers

# MVPA

g5 <- grafico(df_ajustado$area_conhecimento,
              df_ajustado$mvpa) +
  labs(title = "MVPA com outlier")


# Comportamento sedentário

g6 <- grafico(df_ajustado$area_conhecimento,
              df_ajustado$comportamento_sedentario) +
  labs(title = "Comportamento Sedentário com outlier")


# SF-36 Físico

g7 <- grafico(df_ajustado$area_conhecimento,
              df_ajustado$sf36_fisico) +
  labs(title = "SF-36 - Dominio Físico com outlier")


# SF-36 Mental

g8 <- grafico(df_ajustado$area_conhecimento,
              df_ajustado$sf36_mental) +
  labs(title = "SF-36 - Dominio Mental com outlier")


# Layout

g5+g6+g7+g8

# Graficos sem os outliers

# Tirando os outliers

df_sem_outlier <-
  df_ajustado |>
  filter(
    e_outlier_mvpa == "Não é outlier",
    e_outlier_comportamento_sedentario == "Não é outlier",
    e_outlier_sf36_fisico == "Não é outlier",
    e_outlier_sf36_mental == "Não é outlier"
)


# MVPA

g9 <- grafico(df_sem_outlier$area_conhecimento,
              df_sem_outlier$mvpa) +
  labs(title = "MVPA sem outlier")


# Comportamento sedentário

g10 <- grafico(df_sem_outlier$area_conhecimento,
              df_sem_outlier$comportamento_sedentario) +
  labs(title = "Comportamento Sedentário sem outlier")


# SF-36 Físico

g11 <- grafico(df_sem_outlier$area_conhecimento,
              df_sem_outlier$sf36_fisico) +
  labs(title = "SF-36 - Dominio Físico sem outlier")


# SF-36 Mental

g12 <- grafico(df_sem_outlier$area_conhecimento,
              df_sem_outlier$sf36_mental) +
  labs(title = "SF-36 - Dominio Mental sem outlier")


# Layout

g9+g10+g11+g12

# Verificando o impacto do outlier em cada variável

g5+g9
g6+g10
g7+g11
g8+g12












