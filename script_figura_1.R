### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Código para generar la Figura 1
### Autora: Tamara Ricardo
# Última modificación: 19-05-2026 13:53

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  DiagrammeR,
  DiagrammeRsvg,
  rsvg
)



# Plot -------------------------------------------------------------------
fig1 <- grViz("
digraph G {
  graph[
  fontsize=24
  style = filled
  ]
  node[
      shape = rectangle
      style = filled
      fillcolor = white
      fontsize=22
      width = 2.5
      margin = '.1, .1'
      ]
      
  subgraph cluster_1 {
    label = <<b>Paso 1</b>>;
    fillcolor = '#FFCE66E5'
    t1[style = invis, width=6]
    a1[label = 'ENT objetivo\\n - Diabetes\\n - ECV\\n - ERC\\n - Neoplasias']
    b1[label = 'CE objetivo\\n - Tránsito\\n - Suicidio\\n - Homicidio']
    c1[label = 'Otras causas\\n - CMNN\\n - Otras CE\\n - Otras ENT']
    d1[label = '- GC1\\n - GC2\\n - GC3\\n - GC4']
  }

  subgraph cluster_2 {
    label = <<b>Paso 2</b>>;
    fillcolor = '#92463AE5'

    t2[label = 'Reasignar GC3-GC4', width=5]
    a2[label = 'ENT objetivo\\n - Diabetes\\n - ECV\\n - ERC\\n - Neoplasias\\n - Redist. 50% \\n neumonías por \\n sexo y edad']
    b2[label = 'CE objetivo\\n - Tránsito\\n - Suicidio\\n - Homicidio']
    c2[label = 'Otras causas\\n - CMNN + \\n 50% neumonías\\n - Otras CE\\n - Otras ENT']
    d2[label = '- GC1\\n - GC2']
  }
  
  subgraph cluster_3 {
    label = <<b>Paso 3</b>>;
    fillcolor = '#4D5492E5'

    t3[label = 'Redistribuir GC2 inespecíficos', width=5]
    a3[label = 'ENT objetivo\\n - Diabetes\\n - ECV + GC2-ECV\\n - ERC\\n - Neoplasias']
    b3[label = 'CE objetivo\\n - Tránsito\\n - Suicidio\\n - Homicidio\\n - Redist. GC2-CE']
    c3[label = 'Otras causas\\n - CMNN\\n - Otras CE + GC2-CE\\n - Otras ENT']
    d3[label = '- GC1']
  }
  
  subgraph cluster_4 {
    label = <<b>Paso 4</b>>;
    fillcolor = '#80E6FFE5'
    t4[label = 'Redistribuir GC1 inespecíficos', width=5]
    a4[label = 'ENT objetivo\\n - Diabetes\\n - ECV\\n - ERC + GC1-ERC\\n - Neoplasias']
    b4[label = 'CE objetivo\\n - Tránsito\\n - Suicidio\\n - Homicidio\\n - Redist. GC1-CE']
    c4[label = 'Otras causas\\n - CMNN\\n - Otras CE\\n - Otras ENT']
  }
  
 
  t1 -> {a1 b1 c1} -> d1[style = invis]
  {a2 b2 c2} -> d2[style = invis]
  t2 -> {a2 b2 c2}
  d1 -> t3[style = invis]
  d2 -> t4 [style = invis]
 {a3 b3 c3} -> d3[style = invis]
   t3 -> {a3 b3 c3} 
    t4 -> {a4 b4 c4}
}
"
)



## Guardar figura -----
export_svg(fig1) |>
  charToRaw() |>
  rsvg_png(
    file = "figuras/Figura1.png",
    width = 567
  )
