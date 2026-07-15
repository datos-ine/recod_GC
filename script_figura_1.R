### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Código para generar la Figura 1
### Autora: Tamara Ricardo
# Última modificación: 14-07-2026 11:20

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  DiagrammeR,
  DiagrammeRsvg,
  rsvg
)


# Plot -------------------------------------------------------------------
fig1 <- grViz(
  "
  digraph G {
    graph[
    fontsize=14
    style = filled,
    ranksep=.1]
  
    node[
        shape = rectangle
        style = filled
        fillcolor = white
        fontsize=12
        width = 4
        margin = '.1, .1'
        ]
        
    subgraph cluster_1 {
      label = <<b>Paso 1</b>>;
      fillcolor ='#FFCE66E5'
    
      subgraph  cluster_1a{
      label = <Causas definidas>;
      fillcolor = '#ffffff50'
  
      t1[style=invis]
      a1[label = 'ENT objetivo\n - Diabetes\n - ECV\n - ERC\n - Neoplasias']
      b1[label = 'CE objetivo\n - Tránsito\n - Suicidio\n - Homicidio']
      c1[label = 'Causas no objetivo\n - CMNN\n - Otras CE\n - Otras ENT']
      }
      d1[label = '- GC1\n - GC2\n - GC3\n - GC4 \n - GC4-NNE']
    }
  
    subgraph cluster_2 {
      label = <<b>Paso 2</b>>;
      fillcolor = '#92463AE5'
  
      subgraph cluster_2a{
      label = <Recategorizar GC3-GC4>;
      fillcolor = '#ffffff50'
  
      t2[style=invis]
      a2[label = 'ENT objetivo\n - Diabetes\n - ECV\n - ERC\n - Neoplasias\n - 50% NNE → ENT (sexo y edad)']
      b2[label = 'CE objetivo\n - Tránsito\n - Suicidio\n - Homicidio']
      c2[label = 'Causas no objetivo\n - CMNN + 50% NNE\n - Otras CE\n - Otras ENT']
  }
      d2[label = '\n - GC1\n - GC2 \n ']
    }
    
    subgraph cluster_3 {
      label = <<b>Paso 3</b>>;
      fillcolor = '#4D5492E5'
  
      subgraph cluster_3a{
      label = 'Recategorizar y redistribuir GC2';
      fillcolor = '#ffffff50'
  
      t3[style=invis]
      a3[label = 'ENT objetivo\n - Diabetes\n - ECV + GC2-ECV\n - ERC\n - Neoplasias']
      b3[label = 'CE objetivo\n - Tránsito\n - Suicidio\n - Homicidio\n + Redistribución GC2']
      c3[label = 'Causas no objetivo\n - CMNN\n - Otras CE + GC2-CE\n - Otras ENT']
      }
      d3[label = '- GC1']
    }
    
    subgraph cluster_4 {
      label = <<b>Paso 4</b>>;
      fillcolor = '#80E6FFE5'
      
      subgraph cluster_4a{
      label = 'Recategorizar y redistribuir GC1';
      fillcolor = '#ffffff50'
  
      t4[style=invis]
      a4[label = 'ENT objetivo\n - Diabetes\n - ECV\n - ERC + GC1-ERC\n - Neoplasias']
      b4[label = 'CE objetivo\n - Tránsito\n - Suicidio\n - Homicidio\n + Redistribución GC1']
      c4[label = 'Causas no objetivo\n - CMNN\n - Otras CE\n - Otras ENT']
      }
      d4[style='invis']
    }
    
    t1 -> a1 -> b1 -> c1 -> d1[style = invis]
    t2 -> a2 -> b2 -> c2 -> d2[style = invis]
    t3 -> a3 ->b3 -> c3 -> d3[style = invis]
    t4 -> a4 -> b4 -> c4 -> d4[style = invis]
    d1 -> t3[style = invis]
    d2 -> t4 [style = invis]
  }
"
)


## Guardar figura -----
export_svg(fig1) |>
  charToRaw() |>
  rsvg_png(
    file = "figuras/Figura1.png",
    width = 1772,
    height = 2126
  )
