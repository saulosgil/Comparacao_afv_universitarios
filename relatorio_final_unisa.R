library(tidyverse)

options(scipen = 999)

df <- read.csv2("data/df_ajustado.csv")


branco <- df |> dplyr::filter(raca == "Branco")
### MVPA

MVPA_fisico <-
  df |>
  # mutate(mvpa = log(mvpa)) |>
  # filter(mvpa > 0) |>
  ggplot(mapping = aes(x = mvpa,
                       y = sf36_fisico)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  theme_classic() +
  xlab("Atividade Física Moderada-Vigorosa (min/sem)") +
  ylab("SF-36 escore - dominio físico (u.a)")

MVPA_fisico

ggsave(filename = "mvpa_fisico.jpeg",
       width = 5,
       height = 5)

## Correlação de pearson

cor.test(df$sf36_fisico, df$mvpa)


MVPA_mental <-
  df |>
  # mutate(mvpa = log(mvpa)) |>
  # filter(mvpa > 0) |>
  ggplot(mapping = aes(x = mvpa,
                       y = sf36_mental)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  theme_classic() +
  xlab("Atividade Física Moderada-Vigorosa (min/sem)") +
  ylab("SF-36 escore - dominio mental (u.a)")

MVPA_mental

ggsave(filename = "mvpa_mental.jpeg",
       width = 5,
       height = 5)

## Correlação de pearson

cor.test(df$sf36_mental, df$mvpa)

#### Comportamento sedentário

CS_fisico <-
  df |>
  # mutate(mvpa = log(mvpa)) |>
  # filter(mvpa > 0) |>
  ggplot(mapping = aes(x = comportamento_sedentario,
                       y = sf36_fisico)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  theme_classic() +
  xlab("Comportamento sedentário (min/sem)") +
  ylab("SF-36 escore - dominio físico (u.a)")

CS_fisico

ggsave(filename = "cs_fisico.jpeg",
       width = 5,
       height = 5)

## Correlação de pearson

cor.test(df$sf36_fisico, df$comportamento_sedentario)

CS_mental <-
  df |>
  # mutate(mvpa = log(mvpa)) |>
  # filter(mvpa > 0) |>
  ggplot(mapping = aes(x = comportamento_sedentario,
                       y = sf36_mental)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  theme_classic() +
  xlab("Comportamento sedentário (min/sem)") +
  ylab("SF-36 escore - dominio mental (u.a)")

CS_mental

ggsave(filename = "cs_mental.jpeg",
       width = 5,
       height = 5)

## Correlação de pearson

cor.test(df$sf36_mental, df$comportamento_sedentario)

### COMPARAÇÃO ENTRE CURSOS - MVPA
g1 <-
  df |>
  ggplot(mapping = aes(x = area_conhecimento,
                    y = mvpa,
                    color = area_conhecimento)) +
  geom_boxplot(varwidth = TRUE) +
  geom_jitter(width = 0.2,
              size = 3,
              show.legend = FALSE,
              alpha = 0.5) +
  xlab("") +
  ylab("Atividade Física Moderada-Vigorosa\n(min/sem)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 15))

ggsave(filename = "g1.jpeg",
       width = 5,
       height = 5)

### COMPARAÇÃO ENTRE CURSOS - CS

g2 <-
  df |>
  ggplot(mapping = aes(x = area_conhecimento,
                       y = comportamento_sedentario,
                       color = area_conhecimento)) +
  geom_boxplot(varwidth = TRUE) +
  geom_jitter(width = 0.2,
              size = 3,
              show.legend = FALSE,
              alpha = 0.5) +
  xlab("") +
  ylab("Comportamento Sedentario\n(min/sem)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 15))

ggsave(filename = "g2.jpeg",
       width = 5,
       height = 5)

### COMPARAÇÃO ENTRE CURSOS - SF-36 FÍSICO

g3 <-
  df |>
  ggplot(mapping = aes(x = area_conhecimento,
                       y = sf36_fisico,
                       color = area_conhecimento)) +
  geom_boxplot(varwidth = TRUE) +
  geom_jitter(width = 0.2,
              size = 3,
              show.legend = FALSE,
              alpha = 0.5) +
  xlab("") +
  ylab("Qualidade de vida - dominio físico\n(min/sem)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 15))

ggsave(filename = "g3.jpeg",
       width = 5,
       height = 5)

### COMPARAÇÃO ENTRE CURSOS - SF-36 MENTAL

g4 <-
  df |>
  ggplot(mapping = aes(x = area_conhecimento,
                       y = sf36_mental,
                       color = area_conhecimento)) +
  geom_boxplot(varwidth = TRUE) +
  geom_jitter(width = 0.2,
              size = 3,
              show.legend = FALSE,
              alpha = 0.5) +
  xlab("") +
  ylab("Qualidade de vida - dominio mental\n(min/sem)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 15))

ggsave(filename = "g4.jpeg",
       width = 5,
       height = 5)






























