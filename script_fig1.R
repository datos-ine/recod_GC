### Mortalidad por códigos garbage en Argentina (2010–2023):
### redistribución hacia causas específicas
### Figura 1
### Autora: Tamara Ricardo
# Última modificación: 25-09-2026 12:22

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  DiagrammeR,
  DiagrammeRsvg,
  rsvg,
)

# Figura 1 ---------------------------------------------------------------
fig1 <- grViz(
  '
  digraph G {
    rankdir=LR
   graph[
    fontsize = 14
    fontname="Times-Roman"
    style = filled
    nodesep=.5
    ranksep=.15
    compound=true
   ]
  
   node[
    shape = plain
    style = filled
    fillcolor="grey95"
    fontsize = 14
    fontname="Times-Roman"
    width=3.25
   ]
  
   subgraph cluster_paso1{
    label = <<b>Paso 1<br/> Categorizar grupos de causas</b>>
    fillcolor="#FFCE66BF" 
  
    ent1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> Neoplasias (NPL) </td>
    </tr>    
    <tr>
    <td> Cardiovasculares (ECV) </td>
    </tr>
    <tr>
    <td> Respiratorias crónicas (CRD) </td>
    </tr>    
    <tr>
    <td> Diabetes y renales crónicas (DM-CKD) </td>
    </tr>
    </table>
    >]
  
    ce1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo</b></td>
    </tr>
    <tr>
    <td> Accidentes de tránsito (TRA) </td>
    </tr>
    <tr>
    <td> Suicidio (SH) </td>
    </tr>    
    <tr>
    <td> Violencia interpersonal (VI) </td>
    </tr> 
    <tr>
    <td> Accidentes por caídas (CA) </td>
    </tr> 
    </table>
    >]
  
    otras1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Causas no objetivo</b></td>
    </tr>
    <tr>
    <td> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> Otras CE (OTR-CE) </td>
    </tr>
    <tr>
    <td> Otras ENT (OTR-ENT) </td>
    </tr>
    </table>
    >]
  
    gc1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td> GC1 </td>
    </tr>
    <tr>
    <td> GC2 </td>
    </tr>
    <tr>
    <td> GC3</td>
    </tr>
    <tr>
    <td> GC4</td>
    </tr>
    <tr>
    <td port="nne"> NNE</td>
    </tr>
    </table>
    >]
   }
  
   subgraph cluster_paso2{
    label = <<b>Paso 2<br/>Recategorizar GC3-GC4</b>>
    fillcolor="#92463ABF"
    // labelloc=b
    
  
  ent2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV </td>
    </tr>
    <tr>
    <td> CRD </td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
       </table>
    >]
  
    otras2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE </td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
  gc2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td> GC1 </td>
    </tr>
    <tr>
    <td port="gc2"> GC2 </td>
    </tr>
    </table>
    >]
  
   }
  
    subgraph cluster_paso3{
    fillcolor="#4D5492BF"
  
    t3[
      label = <<b>Paso 3:<br/> Redistribuir GC2*</b>>
        style = plaintext
        ]

    ent3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV + GC2-ECV</td>
    </tr>
    <tr>
    <td> CRD </td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo + GC2-CE*</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
    </table>
    >]
  
    otras3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE + GC2-CE*</td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
  gc3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td port="gc1"> GC1 </td>
    </tr>
    </table>
    >]
    }
  
  
    subgraph cluster_paso4{    
    fillcolor= "#80E6FFBF"

    t4[
    label = <<b>Paso 4:<br/> Redistribuir GC1*</b>>
      style = plaintext
      ]
    
  ent4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV</td>
    </tr>
    <tr>
    <td> CRD + GC1-CRD</td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo + GC1-CE*</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
    </table>
    >]
  
    otras4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE + GC1-CE*</td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
   }
  
    t1[
    label=<* Redistribución proporcional por sexo y edad <br/>
      según frecuencias calculadas luego de <br/>
      recategorizar los GC3 y GC4.>
    shape=plain
    fillcolor="none"
    width=10
          ]
  
  
  gc1:nne -> ent2 [
           headlabel="50%*"
           labelangle=-50 
           labeldistance=2.5
           ]
  gc1:nne -> otras2:cmnn[label="50%*" constraint=false]
  
  gc1 -> ent3 [style="invis"] 
  
  gc3:gc -> {ent4:ent ce4:ce otras4:otras} [style="invis"] 
  
  gc3 -> t1 [style="invis" constraint=false] 

  gc2:gc2 -> t3 [constraint=false]
  gc3 -> t4 [constraint=false]
  
  }
 '
)

## Save as PNG ----
export_svg(fig1) |>
  charToRaw() |>
  rsvg_png(
    file = "figs_tablas/Figura1.png",
    width = 560
  )

## Save as SVG ----
export_svg(fig1) |>
  charToRaw() |>
  rsvg_svg(
    file = "figs_tablas/Figura1.svg",
    width = 560
  )
