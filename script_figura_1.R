### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Código para generar la Figura 1
### Autora: Tamara Ricardo
# Última modificación: 19-05-2026 13:53

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  scico,
  DiagrammeR,
  DiagrammeRsvg,
  rsvg
)

# Paletas colorblind-friendly --------------------------------------------
pal <- scico(n = 4, palette = "managua", alpha = .8)


# Plot -------------------------------------------------------------------
fig1 <- grViz(
  "
  digraph{
    graph [
    layout = dot,
    rankdir = TR,
    bgcolor = white,
    fontname = 'Times New Roman',
    ]
  
    node [
    shape = box,
    style = filled
    color = darkgrey,
    fillcolor = grey95,
    fontname = 'Times New Roman',
    fontsize = 18,
    labeljust = l,
    margin = 0.25,
    width = 3
    ]
  
  #### Paso 1 ####
  subgraph cluster_1{
      bgcolor = '#FFCE66'
      ranksep = 0.1
      
      t1[label = <<b>Paso 1</b> >
      style = normal,
      color = none,
      fontsize = 20]
      
      a1[label = <<b>ENT objetivo</b><br/><br align='left'/>• Diabetes <br align='left'/>• ECV <br align='left'/>• ERC <br align='left'/>• Neoplasias <br align='left'/>
      >]
      
      b1[label = <<b>CE objetivo</b><br/><br align='left'/>• Accidentes de tránsito <br align='left'/>• Homicidio <br align='left'/>• Suicidio <br align='left'/>
      >]
      
      c1[label = <<b>Otras causas</b><br/><br align='left'/>• CMNN <br align='left'/>• Otras ENT <br align='left'/>• Otras CE <br align='left'/>
      >]

    d1[label = <<b>Códigos basura</b><br/><br align='left'/>• GC1<br align='left'/>• GC2<br align='left'/>• GC3<br align='left'/>• GC4<br align='left'/>
      >]
      
      ## Ordenar nodos ##
      t1 -> a1 -> b1 -> c1 -> d1 [style = invis]
      
      
}   

#### Paso 2 ####
  subgraph cluster_2{
      bgcolor = '#92463ACC'
      ranksep = 0.1
      
      t2[label = <<b>Paso 2</b> >
      style = normal,
      color = none,
      fontsize = 20]
      
      a2[label = <<b>ENT objetivo</b><br/><br align='left'/>• Diabetes <br align='left'/>• ECV <br align='left'/>• ERC <br align='left'/>• Neoplasias <br align='left'/>
      >]
      
      b2[label = <<b>CE objetivo</b><br/><br align='left'/>• Accidentes de tránsito <br align='left'/>• Homicidio <br align='left'/>• Suicidio <br align='left'/>
      >]
      
      c2[label = <<b>Otras causas</b><br/><br align='left'/>• CMNN <br align='left'/>• Otras ENT <br align='left'/>• Otras CE <br align='left'/>
      >]

    d2[label = <<b>Códigos basura</b><br/><br align='left'/>• GC1<br align='left'/>• GC2<br align='left'/>>]
    
   e[label = <• Redistribución GC3-GC4 <br align='left'/>• Redistribución neumonías<br align='left'/>   inespecíficas a CMNN (50%)<br align='left'/>   y proporcional por edad<br align='left'/>   y sexo a ENT<br align='left'/>
   >]
      ## Ordenar nodos ##
      t2 -> a2 -> b2 -> c2 -> e -> d2 [style = invis]
      
      
}   

#### Paso 3 ####
  subgraph cluster_3{
      bgcolor = '#4D5492CC'
      ranksep = 0.1
      
      t3[label = <<b>Paso 3</b> >
      style = normal,
      color = none,
      fontsize = 20]
      
      a3[label = <<b>ENT objetivo</b><br/><br align='left'/>• Diabetes <br align='left'/>• ECV +<br align='left'/>   Hipertensión (I10, I15) +<br align='left'/>   cor pulmonale (I27) + <br align='left'/>   ateroesclerosis (I70) + <br align='left'/>   embolia arterial (I74) <br align='left'/>• ERC <br align='left'/>• Neoplasias <br align='left'/>
      >]
      
      b3[label = <<b>CE objetivo</b><br/><br align='left'/>• Accidentes de tránsito <br align='left'/>• Homicidio <br align='left'/>• Suicidio <br align='left'/>
      >]
      
      c3[label = <<b>Otras causas</b><br/><br align='left'/>• CMNN <br align='left'/>• Otras ENT <br align='left'/>• Otras CE <br align='left'/>
      >]

    d3[label = <<b>Códigos basura</b><br/><br align='left'/>• GC1<br align='left'/>
      >]
      
      f[ label = <• Redistribución GC2 de CE<br align='left'/>   por edad y sexo<br align='left'/>• Redistribución GC2<br align='left'/>   inespecíficos por edad<br align='left'/>   y sexo<br align='left'/>
   >]
      
      ## Ordenar nodos ##
      t3 -> a3 -> b3 -> c3 -> f -> d3 [style = invis]
      
}  

#### Paso 4 ####
  subgraph cluster_4{
      bgcolor = '#80E6FFCC'
      ranksep = 0.1
      
      t4[label = <<b>Paso 4</b> >
      style = normal,
      color = none,
      fontsize = 20]
      
      a4[label = <<b>ENT objetivo</b><br/><br align='left'/>• Diabetes <br align='left'/>• ECV <br align='left'/>• ERC +<br align='left'/>   falla resp. crónica (J96.1)<br align='left'/>• Neoplasias <br align='left'/>
      >]
      
      b4[label = <<b>CE objetivo</b><br/><br align='left'/>• Accidentes de tránsito <br align='left'/>• Homicidio <br align='left'/>• Suicidio <br align='left'/>
      >]
      
      c4[label = <<b>Otras causas</b><br/><br align='left'/>• CMNN <br align='left'/>• Otras ENT <br align='left'/>• Otras CE <br align='left'/>
      >]
      
      
      g[ label = <• Redistribución GC1 de CE<br align='left'/>   por edad y sexo<br align='left'/>• Redistribución GC1<br align='left'/>   inespecíficos por edad<br align='left'/>   y sexo<br align='left'/>
   >]
      ## Ordenar nodos ##
      t4 -> a4 -> b4 -> c4 -> g [style = invis]
      
}   
}
"
)
