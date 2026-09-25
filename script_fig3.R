### Mortalidad por códigos garbage en Argentina (2010–2023):
### redistribución hacia causas específicas
### Figura 3
### Autora: Tamara Ricardo
# Última modificación: 25-09-2026 12:48

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  patchwork,
  treemapify,
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
datos_gc <- import("clean/arg_recod_defun_gbd23.rds") |> 
  # --- Seleccionar columnas ---
  select(contains("gbd"), n) 


# Figura 3 ---------------------------------------------------------------
## Función auxiliar -----
treeplot_data <- function(data, var) {
  data |>
    separate(
      {{ var }},
      into = c("n1", "n2"),
      sep = ":",
      fill = "left"
    ) |>
    count(n1, n2, wt = n) |>
    mutate(pct = n / sum(n)) |>
    mutate(n1 = fct_relevel(n1, "CMNN", "CE", "ENT", "GC")) |>

    # Convertir a ggplot
    ggplot(aes(
      area = pct,
      subgroup = n1,
      fill = n1
    )) +
    theme(
      legend.position = "none",
      text = element_text(family = "Times New Roman")
    )
}


## Panel 1 -----
g1 <- datos_gc |>
  treeplot_data(var = gbd_paso1) +
  labs(subtitle = "Paso 1")


## Panel 2 -----
g2 <- datos_gc |>
  treeplot_data(var = gbd_paso2a) +
  labs(subtitle = "Paso 2A")


## Panel 3 -----
g3 <- datos_gc |>
  treeplot_data(var = gbd_paso2b) +
  labs(subtitle = "Paso 2B")


## Panel 4 -----
g4 <- datos_gc |>
  treeplot_data(var = gbd_paso3) +
  labs(subtitle = "Paso 3")


## Panel 5 ----
g5 <- datos_gc |>
  treeplot_data(var = gbd_paso4) +
  labs(subtitle = "Paso 4")


## Treemap -----
fig3 <- g1 /
  (g2 + g3) /
  ((g4 +
    theme(legend.position = "bottom")) +
    g5) &

  # Treemap
  geom_treemap(alpha = .9) &
  geom_treemap_text(
    aes(
      label = if_else(
        pct < 0.015,
        "",
        paste0(
          n2,
          " (",
          percent(pct, accuracy = .1, decimal.mark = ","),
          ")"
        )
      )
    ),
    place = "topright",
    reflow = TRUE,
    color = "white",
    family = "Times New Roman",
    fontface = "bold",
    min.size = 6,
    size = 11
  ) &

  # Subgrupo
  geom_treemap_subgroup_border() &

  # Layout
  scale_fill_manual(
    values = c(
      CMNN = "#D28C50",
      CE = "#7B3539",
      ENT = "#4C4077",
      GC = "#68A3D4"
    ),
    name = NULL
  )


## Save as PNG ----
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

## Save as SVG ----
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

