# PLOT AND ANALYSES FOR MANUSCRIPT ------------------------------------------------------------
# Packages ------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(fmsb)

# Reading dataset --------------------------------------------------------------------------------

df <-
  df_ajustado <- read_csv("data/df_ajustado.csv")

# Adjusting dataset ---------------------------------------------------------------------------

df <-
  df |>
  # adjusting areas
  mutate(area = case_when(
    curso == "Análise e desenvolvimento de sistemas" ~ "Natural and Applied Sciences",
    curso == "Serviço Social" ~ "Social Sciences",
    curso == "Engenharia Elétrica" ~ "Natural and Applied Sciences",
    curso == "Arquitetura e urbanismo" ~ "Humanities",
    curso == "Gestão Financeira" ~ "Social Sciences",
    curso == "Engenharia da Computação" ~ "Natural and Applied Sciences",
    curso == "Medicina Veterinária" ~ "Natural and Applied Sciences",
    curso == "História (Licenciatura)" ~ "Humanities",
    curso == "Fisioterapia" ~ "Natural and Applied Sciences",
    curso == "Psicologia" ~ "Social Sciences",
    curso == "Direito" ~ "Natural and Applied Sciences",
    curso == "Gestão Comercial" ~ "Humanities",
    curso == "Nutrição" ~ "Natural and Applied Sciences",
    curso == "Pedagogia (2ª graduação)" ~ "Humanities",
    curso == "Gestão Hospitalar" ~ "Social Sciences",
    curso == "Educação Física (Bacharelado)" ~ "Natural and Applied Sciences",
    curso == "Administração" ~ "Social Sciences",
    curso == "Enfermagem" ~ "Natural and Applied Sciences",
    curso == "Engenharia de Produção" ~ "Natural and Applied Sciences",
    curso == "Ciências Contábeis" ~ "Social Sciences",
    curso == "Logística" ~ "Humanities",
    curso == "Ciências Biológicas - Biologia (Bacharelado)" ~ "Natural and Applied Sciences",
    curso == "Processos gerenciais" ~ "Social Sciences",
    curso == "Geografia (Licenciatura)" ~ "Social Sciences",
    curso == "Odontologia" ~ "Natural and Applied Sciences",
    curso == "Engenharia Ambiental" ~ "Natural and Applied Sciences",
    curso == "Engenharia Civil" ~ "Natural and Applied Sciences",
    curso == "Letras (Licenciatura)" ~ "Humanities",
    curso == "Serviços jurídicos, cartoriais e notariais" ~ "Humanities",
    curso == "Farmácia" ~ "Natural and Applied Sciences",
    curso == "Medicina" ~ "Natural and Applied Sciences",
    curso == "Gestão de Recursos Humanos" ~ "Social Sciences",
    curso == "Gestão da Tecnologia da Informação" ~ "Social Sciences",
    curso == "Educação Física (Licenciatura)" ~ "Natural and Applied Sciences",
    curso == "Estética e Cosmética" ~ "Natural and Applied Sciences",
    curso == "Engenharia Mecânica" ~ "Natural and Applied Sciences",
    curso == "Engenharia Química" ~ "Natural and Applied Sciences",
    curso == "Gastronomia" ~ "Humanities",
    curso == "Ciências Biológicas - Biologia (Licenciatura)" ~ "Natural and Applied Sciences",
    curso == "Biomendicina" ~ "Natural and Applied Sciences",
    curso == "Teologia (Bacharelado)" ~ "Humanities",
    curso == "Comunicação Social - Publicidade e Propaganda" ~ "Humanities",
    curso == "Segurança da Informação" ~ "Social Sciences",
    curso == "Design de Interiores" ~ "Humanities",
    curso == "Marketing Digital" ~ "Humanities",
    curso == "Gestão da Qualidade" ~ "Social Sciences",
    curso == "Gestão Pública" ~ "Social Sciences",
    curso == "Engenharia Mecatrônica" ~ "Natural and Applied Sciences",
    curso == "Processos escolares" ~ "Social Sciences",
    curso == "Pedagogia" ~ "Humanities",
    curso == "Marketing" ~ "Humanities"
  ),
  # calculate sedentary behavior by day
  sed_day = comportamento_sedentario / 7,
  # sedentary class
  sed_class = case_when(
    sed_day < 6 ~ "< 6 h/wk",
    sed_day >= 6 ~ "> 6 h/wk"
  ),
  #anxiety class
  ansiedade_class = case_when(
    ansiedade < 8 ~ "minimo",
    ansiedade >= 8 & ansiedade < 16 ~ "leve",
    ansiedade >= 16 & ansiedade < 26 ~ "moderado",
    ansiedade >= 26 ~ "Grave"
  ),
  # depression class
  depressao_class = case_when(
    depressao < 12 ~ "minimo",
    depressao >= 12 & ansiedade < 20 ~ "leve",
    depressao >= 20 & ansiedade < 36 ~ "moderado",
    depressao >= 36 ~ "Grave"
  ),
  # poor QoL physical domain
  dominio_fisico_class = case_when(
    sf36_fisico < 50 ~ "poor_QoL_fisico",
    sf36_fisico >= 50 ~ "good_QoL_fisico"
  ),
  # poor QoL mental domain
  dominio_mental_class = case_when(
    sf36_mental < 50 ~ "poor_QoL_mental",
    sf36_mental >= 50 ~ "good_QoL_mental"
  ),
  mvpa_ativo = case_when(
    mvpa_ativo == "Ativo" ~ "Active",
    mvpa_ativo == "Inativo" ~ "Inactive"
  )
  )

# PLOTS FOR PAPER -----------------------------------------------------------------------------
# Comparing active x inactive  ----------------------------------------------------------------
# radar plot - active vs inactive -------------------------------------------------------------
# Adjusting dataset to radar plot
df_ajustada <-
  df |> select( # Selecting parameters
    mvpa_ativo,
    capacidade_funcional,
    aspecto_fisico,
    dor,
    estado_geral_saude,
    vitalidade,
    aspectos_sociais,
    aspectos_emocionais,
    saude_mental
  ) |>
  rename( # Rename !!! It is important to be similar to minimal and maximal radar plor!!!
    "Function capacity" = capacidade_funcional,
    "Physical Appearance" = aspecto_fisico,
    Pain = dor,
    "General health status" = estado_geral_saude,
    Vitality = vitalidade,
    "Social Aspect" = aspectos_sociais,
    "Emotional Aspect" = aspectos_emocionais,
    "Mental health" = saude_mental
  )

# Sumarizing parameters of SF36
sf36_score <-
  df_ajustada |>
  group_by(mvpa_ativo) |>
  summarise("Function capacity" = mean(`Function capacity`),
            "Physical Appearance" = mean(`Physical Appearance`),
            Pain = mean(Pain),
            "General health status" = mean(`General health status`),
            Vitality =  mean(Vitality),
            "Social Aspect" = mean(`Social Aspect`),
            "Emotional Aspect" = mean(`Emotional Aspect`),
            "Mental health" = mean(`Mental health`)
  ) |>
  select(-mvpa_ativo) |>
  as.data.frame()

row.names(sf36_score) <- c("Active", "Inactive") # inserindo o nome das linhas

sf36_score

# Definindo a amplitude do escore: Minimo e máximo
max_min <- tibble(
  "Function capacity" = c(100, 0),
  "Physical Appearance" = c(100, 0),
  Pain = c(100, 0),
  "General health status" = c(100, 0),
  Vitality = c(100, 0),
  "Social Aspect" = c(100, 0),
  "Emotional Aspect" = c(100, 0),
  "Mental health" = c(100, 0)
) |>
  as.data.frame()

row.names(max_min) <- c("Max", "Min") # inserindo o nome das linhas

max_min

# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar <- rbind(max_min, sf36_score)

df_radar

# Plot the data for student 1
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar),
           axislabcol = "grey",
           vlcex = 1,
           pcol = c("#F8766D", "#619CFF"), # Customizando o poligono
           # pfcol = scales::alpha(c("#F8766D", "#00BA38", "#619CFF"), 0.1),
           plwd = 3,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1,
           title = "QUALITY OF LIFE - Short-Form Health Survey (SF-36)"
)

# Add an horizontal legend
legend(
  x = 1.3,
  y = 1.2,
  legend = rownames(df_radar[-c(1,2),]),
  horiz = FALSE,
  bty = "n",
  pch = 20,
  col = c("#F8766D", "#619CFF"),
  text.col = "black",
  cex = 1,
  pt.cex = 1.5
)
par(op)

# Comparing sf36 parameters - t-test

t.test(aspecto_fisico ~ mvpa_ativo, df_ajustado) # physical appeareance
t.test(capacidade_funcional ~ mvpa_ativo, df_ajustado) # function capacity
t.test(saude_mental ~ mvpa_ativo, df_ajustado) # mental health
t.test(aspectos_emocionais ~ mvpa_ativo, df_ajustado) # Emotional aspect
t.test(aspectos_sociais ~ mvpa_ativo, df_ajustado) # Social Aspect
t.test(vitalidade ~ mvpa_ativo, df_ajustado) # Vitality
t.test(estado_geral_saude ~ mvpa_ativo, df_ajustado) # General health status
t.test(dor ~ mvpa_ativo, df_ajustado) # Pain
t.test(sf36_fisico ~ mvpa_ativo, df_ajustado) # physical domain
t.test(sf36_mental ~ mvpa_ativo, df_ajustado) # mental domain
t.test(ansiedade ~ mvpa_ativo, df_ajustado) # anxiety domain
t.test(depressao ~ mvpa_ativo, df_ajustado) # depression domain

# sf36 fisico ----------------------------------------------------------------------------------------
# Basic plot
plt_fisico_active <-
  df |>
  ggplot(mapping = aes(x = mvpa_ativo,
                       y = sf36_fisico,
                       color = mvpa_ativo)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Quality of life - Physical domain (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_fisico_active

# Comparing active vs inactive - t-test
t.test(sf36_fisico ~ mvpa_ativo, df_ajustado)

# sf36 mental ----------------------------------------------------------------------------------------
# Basic plot
plt_mental_active <-
  df |>
  ggplot(mapping = aes(x = mvpa_ativo,
                       y = sf36_mental,
                       color = mvpa_ativo)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Quality of life - Mental domain (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# PLOT!!!!
plt_mental_active

# Comparing active vs inactive - t-test
t.test(sf36_mental ~ mvpa_ativo, df_ajustado)

# Depression ----------------------------------------------------------------------------------------
# Basic plot
plt_depressao_active <-
  df |>
  ggplot(mapping = aes(x = mvpa_ativo,
                       y = depressao,
                       color = mvpa_ativo)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Beck Depression Inventory (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_depressao_active

# Comparing active vs inactive - t-test
t.test(depressao ~ mvpa_ativo, df_ajustado)

# Ansiedade ----------------------------------------------------------------------------------------
# Basic plot
plt_anxiety_active <-
  df |>
  ggplot(mapping = aes(x = mvpa_ativo,
                       y = ansiedade,
                       color = mvpa_ativo)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Beck Anxiety Inventory (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_anxiety_active

# Comparing active vs inactive - t-test
t.test(ansiedade ~ mvpa_ativo, df_ajustado)

# Figure 1. Quality of life scores betwen active and inactive. -------------------------------
(plt_fisico_active / plt_depressao_active) |
  (plt_mental_active / plt_anxiety_active)

# MVPA ----------------------------------------------------------------------------------------
# Prevalence of physical inactivity among areas
df |>
  select(area,
         mvpa_ativo) |>
  group_by(area, mvpa_ativo) |>
  summarise(n = n())

# Total prevalence of physical inactivity
df |>
  group_by(mvpa_ativo) |>
  summarise(n = n())

# plot - Prevalence
prevalence_inactivity <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 63,
    "Natural and Applied Sciences",  63,
    "Social Sciences", 68
  )

prevalence_plt <-
  prevalence_inactivity |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0,70)) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of physical inactivity (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_plt

# MVPA as continuos variable
# Basic plot
plt_mvpa <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = mvpa,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
# Customizing
# Add labels and title
  labs(
    x = "",
    y = "MVPA (min/wk)"
    ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_mvpa

# Anxiety ----------------------------------------------------------------------------------------
# Prevalence of anxiety among areas
df |>
  select(area,
         ansiedade_class) |>
  group_by(area, ansiedade_class) |>
  summarise(n = n())

# Total prevalence of physical inactivity
df |>
  group_by(mvpa_ativo) |>
  summarise(n = n())

# plot - Prevalence
prevalence_ansiedade_grave <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 26,
    "Natural and Applied Sciences",  32,
    "Social Sciences", 35
  )

prevalence_ansiedade_grave_plt <-
  prevalence_ansiedade_grave |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0,70)) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of severe symptoms of anxiety (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_ansiedade_grave_plt

# Anxiety as continuos variable
# Basic plot
plt_anxiety <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = ansiedade,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = FALSE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Beck Anxiety Inventory (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_anxiety

# Depression ----------------------------------------------------------------------------------------
# Prevalence of Depression among areas
df |>
  select(area,
         depressao_class) |>
  drop_na(depressao_class) |>
  group_by(area, depressao_class) |>
  summarise(n = n())

# Total prevalence of physical inactivity
df |>
  group_by(mvpa_ativo) |>
  summarise(n = n())

# plot - Prevalence
prevalence_depression_grave <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 3,
    "Natural and Applied Sciences",  5,
    "Social Sciences", 6
  )

prevalence_depression_grave_plt <-
  prevalence_depression_grave |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0,70)) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of  severe symptoms of depression (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_depression_grave_plt

# Depression as continuos variable
# Basic plot
plt_depression <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = ansiedade,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = FALSE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Beck Depression Inventory (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_depression

# Figure 3. Physical activity levels, anxiety and depression scores in different acade --------
(prevalence_plt / prevalence_ansiedade_grave_plt / prevalence_depression_grave_plt) |
  (plt_mvpa / plt_anxiety / plt_depression)

# SF36 fisico ----------------------------------------------------------------------------------------
# Prevalence of poor QoL fisico among areas
df |>
  select(area,
         dominio_fisico_class) |>
  drop_na(dominio_fisico_class) |>
  group_by(area, dominio_fisico_class) |>
  summarise(n = n())



# plot - Prevalence
prevalence_poor_fisico_grave <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 35,
    "Natural and Applied Sciences",  24,
    "Social Sciences", 40
  )

prevalence_poor_fisico_grave_plt <-
  prevalence_poor_fisico_grave |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of poor quality of life - physical domain (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_poor_fisico_grave_plt

# SF36 fisico as continuos variable
# Basic plot
plt_fisico <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = sf36_fisico,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = TRUE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = TRUE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Quality of life - Physical domain (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_fisico

# SF36 mental ----------------------------------------------------------------------------------------
# Prevalence of poor QoL mental among areas
df |>
  select(area,
         dominio_mental_class) |>
  drop_na(dominio_mental_class) |>
  group_by(area, dominio_mental_class) |>
  summarise(n = n())



# plot - Prevalence
prevalence_poor_mental_grave <-
  tibble::tribble(
    ~area, ~prevalencia,
    "Humanities", 59,
    "Natural and Applied Sciences",  52,
    "Social Sciences", 57
  )

prevalence_poor_mental_grave_plt <-
  prevalence_poor_mental_grave |>
  ggplot(mapping = aes(x = area,
                       y = prevalencia,
                       fill = area,
                       alpha = 0.7)) +
  geom_col(colour = "black",
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0,70))  +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black")
  ) +
  # Add labels and title
  labs(
    x = "",
    y = "Prevalence of poor quality of life - mental domain (%)"
  ) +
  coord_flip()

# PLOT!!!!
prevalence_poor_mental_grave_plt

# SF36 fisico as continuos variable
# Basic plot
plt_mental <-
  df |>
  ggplot(mapping = aes(x = area,
                       y = sf36_mental,
                       color = area)) +
  stat_boxplot(varwidth = FALSE,
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(width = 0.2,
              size = 1.5,
              alpha = 0.7,
              show.legend = FALSE) +
  stat_summary(
    fun = "mean",
    shape = 3,
    size = 1,
    colour = "red") +
  # Customizing
  # Add labels and title
  labs(
    x = "",
    y = "Quality of life - Mental domain (a.u)"
  ) +
  # Customizations
  theme(
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
    # legend
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(family = "arial",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# PLOT!!!!
plt_mental

# Figure 4. Quality of life scores in different academic areas. -------------------------------
(prevalence_poor_fisico_grave_plt / prevalence_poor_mental_grave_plt) |
  (plt_fisico / plt_mental)

# Radar plot - SF36 -----------------------------------------------------------------------------
# Adjusting dataset to radar plot
df_ajustada <-
  df |> select( # Selecting parameters
    area,
    capacidade_funcional,
    aspecto_fisico,
    dor,
    estado_geral_saude,
    vitalidade,
    aspectos_sociais,
    aspectos_emocionais,
    saude_mental
  ) |>
  rename( # Rename !!! It is important to be similar to minimal and maximal radar plor!!!
    "Function capacity" = capacidade_funcional,
    "Physical Appearance" = aspecto_fisico,
    Pain = dor,
    "General health status" = estado_geral_saude,
    Vitality = vitalidade,
    "Social Aspect" = aspectos_sociais,
    "Emotional Aspect" = aspectos_emocionais,
    "Mental health" = saude_mental
  )

# Sumarizing parameters of SF36
sf36_score <-
  df_ajustada |>
  group_by(area) |>
  summarise("Function capacity" = mean(`Function capacity`),
            "Physical Appearance" = mean(`Physical Appearance`),
            Pain = mean(Pain),
            "General health status" = mean(`General health status`),
            Vitality =  mean(Vitality),
            "Social Aspect" = mean(`Social Aspect`),
            "Emotional Aspect" = mean(`Emotional Aspect`),
            "Mental health" = mean(`Mental health`)
  ) |>
  select(-area) |>
  as.data.frame()

row.names(sf36_score) <- c("Humanities", "Social Sciences", "Natural and Applied Sciences") # inserindo o nome das linhas

sf36_score

# Definindo a amplitude do escore: Minimo e máximo
max_min <- tibble(
  "Function capacity" = c(100, 0),
  "Physical Appearance" = c(100, 0),
  Pain = c(100, 0),
  "General health status" = c(100, 0),
  Vitality = c(100, 0),
  "Social Aspect" = c(100, 0),
  "Emotional Aspect" = c(100, 0),
  "Mental health" = c(100, 0)
) |>
  as.data.frame()

row.names(max_min) <- c("Max", "Min") # inserindo o nome das linhas

max_min

# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar <- rbind(max_min, sf36_score)

df_radar

# Plot the data for student 1
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar),
           axislabcol = "grey",
           vlcex = 1,
           pcol = c("#F8766D", "#00BA38", "#619CFF"), # Customizando o poligono
           # pfcol = scales::alpha(c("#F8766D", "#00BA38", "#619CFF"), 0.1),
           plwd = 3,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1,
           title = "QUALITY OF LIFE - Short-Form Health Survey (SF-36)"
)

# Add an horizontal legend
legend(
  x = 1.3,
  y = 1.2,
  legend = rownames(df_radar[-c(1,2),]),
  horiz = FALSE,
  bty = "n",
  pch = 20,
  col = c("#F8766D", "#00BA38", "#619CFF"),
  text.col = "black",
  cex = 1,
  pt.cex = 1.5
)
par(op)



