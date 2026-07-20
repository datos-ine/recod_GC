### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Código para generar la Figura 1
### Autora: Tamara Ricardo
# Última modificación: 17-07-2026 12:32

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  DiagrammeR,
  DiagrammeRsvg,
  rsvg
)


# Plot -------------------------------------------------------------------
fig1 <- grViz(
  diagram = "
  digraph G{
    graph[
    fontsize=14
    style = filled,
    ranksep=.01
    ]
    
    node[
        shape = rectangle
        style = filled
        fillcolor = white
        fontsize=12
        width = 3]
        
## Paso 1 ##
    subgraph cluster1{
        label = <<b>Paso 1</b>>
        fillcolor ='#FFCE66E5'
        
      t1[label = <Categorizar grupos de causas>, style = plaintext, color = none]

        subgraph cluster_ENT1{
            label = <<b>ENT objetivo</b>>
            fillcolor = none

            dm1[label = <Diabetes mellitus (DM)>]
            ecv1[label = <Enf. cardiovasculares (ECV)>]
            erc1[label = <Enf. respiratorias crónicas (ERC)>]
            npl1[label =<Neoplasias (NPL)>]
            
        }
        
        subgraph cluster_CE1{
            label = <<b>CE objetivo</b>>
            fillcolor = none

            tra1[label = <Tránsito (TRA)>]
            su1[label = <Suicidio (SU)>]
            ho1[label = <Homicidio (HO)>]
        }
        
        subgraph cluster_otras1{
            label = <<b>Otras causas</b>>
            fillcolor = none

            cmnn1[label = <Trasmisibles, maternas y neonatales, <br/> nutricionales (CMNN)>]
            oent1[label = <Otras ENT>]            
            oce1[label = <Otras CE>]
        }
        
        subgraph cluster_gc1{
            label = <<b>Códigos garbage</b>>
            fillcolor = none

            gc1[label = <GC1>]
            gc2[label = <GC2>]
            gc3[label = <GC3>]
            gc4[label = <GC4>]
            nne[label = <Neumonías inespecíficas (NNE)>]
        }
}
            
            
## Paso 2 ##
    subgraph cluster2{
        label = <<b>Paso 2</b>>
        fillcolor ='#92463AE5'
        
      t2[label = <Recategorizar GC3-GC4 y Redistribuir NNE>, style = plaintext, color = none]

        subgraph cluster_ENT2{
            label = <<b>ENT objetivo</b>>
            fillcolor = none

            dm2[label = <DM>]
            ecv2[label = <ECV>]
            erc2[label = <ERC>]
            npl2[label =<NPL>]
            
        }
        
        subgraph cluster_CE2{
            label = <<b>CE objetivo</b>>
            fillcolor = none

            tra2[label = <TRA>]
            su2[label = <SU>]
            ho2[label = <HO>]
        }
        
        subgraph cluster_otras2{
            label = <<b>Otras causas</b>>
            fillcolor = none

            cmnn2[label = <CMNN + 50% NNE>]
            oent2[label = <Otras ENT>]            
            oce2[label = <Otras CE>]
        }
        
        subgraph cluster_gc2{
            label = <<b>Códigos garbage</b>>
            fillcolor = none

            gc1b[label = <GC1>]
            gc2b[label = <GC2>]
        }
}

## Paso 3 ##
    subgraph cluster3{
        label = <<b>Paso 3</b>>
        fillcolor ='#4D5492E5'
        
      t3[label = <Redistribuir GC2>, style = plaintext, color = none]

        subgraph cluster_ENT3{
            label = <<b>ENT objetivo</b>>
            fillcolor = none

            dm3[label = <DM>]
            ecv3[label = <ECV + GC2-ECV>]
            erc3[label = <ERC>]
            npl3[label =<NPL>]
        }
        
        subgraph cluster_CE3{
            label = <<b>CE objetivo</b>>
            fillcolor = none

            tra3[label = <TRA>]
            su3[label = <SU>]
            ho3[label = <HO>]
        }
        
        subgraph cluster_otras3{
            label = <<b>Otras causas</b>>
            fillcolor = none

            cmnn3[label = <CMNN + 50% NNE>]
            oent3[label = <Otras ENT>]            
            oce3[label = <Otras CE>]
        }
        
        subgraph cluster_gc3{
            label = <<b>Códigos garbage</b>>
            fillcolor = none

            gc1c[label = <GC1>]
        }
}


## Paso 4 ##
    subgraph cluster4{
        label = <<b>Paso 4</b>>
        fillcolor ='#80E6FFE5'
        
      t4[label = <Redistribuir GC1>, style = plaintext, color = none]
      
        subgraph cluster_ENT4{
            label = <<b>ENT objetivo</b>>
            fillcolor = none

            dm4[label = <DM>]
            ecv4[label = <ECV>]
            erc4[label = <ERC+ GC1-ERC>]
            npl4[label =<NPL>]
        }
        
        subgraph cluster_CE4{
            label = <<b>CE objetivo</b>>
            fillcolor = none

            tra4[label = <TRA>]
            su4[label = <SU>]
            ho4[label = <HO>]
        }
        
        subgraph cluster_otras4{
            label = <<b>Otras causas</b>>
            fillcolor = none

            cmnn4[label = <CMNN>]
            oent4[label = <Otras ENT>]            
            oce4[label = <Otras CE>]
        }
}


nne2[label = <50% NNE<br/> (sexo y edad)>, width = 2]

gc2c[label = <GC2-CE<br/> (sexo y edad)>, width = 2]

gc1d[label = <GC1-CE<br/> (sexo y edad)>, width = 2]

t1 -> dm1 -> ecv1 -> erc1 -> npl1 -> tra1 -> su1 -> ho1 -> cmnn1 -> oent1 -> oce1 -> gc1 -> gc2 -> gc3 -> gc4 -> nne [style = invis]

t2 -> dm2 -> ecv2 -> {erc2 nne2} -> npl2 -> tra2 -> su2 -> ho2 -> cmnn2 -> oent2 -> oce2 -> gc1b -> gc2b [style = invis]


t3 -> dm3 -> ecv3 -> erc3 -> npl3 -> tra3 -> su3 -> ho3 -> cmnn3 -> oent3 -> oce3 -> gc1c [style = invis]

t4 -> dm4 -> ecv4 -> erc4 -> npl4 -> tra4 -> su4 -> ho4 -> cmnn4 -> oent4 -> oce4[style = invis]


nne2 -> {dm2 ecv2 erc2 npl2}
nne -> t3 [style = invis]
gc2b -> t4 [style = invis]

gc2c -> {tra3 su3 ho3 oce3}
gc1d -> {tra4 su4 ho4 oce4}
}
  "
)
# fig1 <- grViz(
#   "
#   digraph G {
#     graph[
#     fontsize=14
#     style = filled,
#     ranksep=.1]

#     node[
#         shape = rectangle
#         style = filled
#         fillcolor = white
#         fontsize=12
#         width = 4
#         margin = '.1, .1'
#         ]

#     subgraph cluster_1 {
#       label = <<b>Paso 1</b>>;
#       fillcolor ='#FFCE66E5'

#        subgraph  cluster_1a{
#       label = <Categorizar grupos de causas>;
#       # fillcolor = '#ffffff50'
#       color = 'none'

#       t1[style=invis]
#       a1[label = 'ENT objetivo\n - Diabetes mellitus (DM)\n - Enf. cardiovasculares (ECV)\n - Enf. respiratorias crónicas (ERC)\n - Neoplasias (NPL)']
#       b1[label = 'CE objetivo\n - Tránsito (TRA)\n - Suicidio (SU)\n - Homicidio (HO)']
#       c1[label = 'Causas no objetivo\n - Trasmisibles, maternas y neonatales,
#        nutricionales (CMNN)\n - Otras CE\n - Otras ENT']

#       d1[label = 'Códigos garbage\n - GC1\n - GC2\n - GC3\n - GC4 \n - Neumonías inespecíficas (NNE)']
#  }
#       }

#   #   subgraph cluster_2 {
#   #     label = <<b>Paso 2</b>>;
#   #     fillcolor = '#92463AE5'

#   #     subgraph cluster_2a{
#   #     label = <<b>Paso 2a:</b> Recategorizar GC3-GC4>;
#   #     fillcolor = '#ffffff50'

#   #     t2[style=invis]
#   #     a2[label = 'ENT objetivo\n - DM\n - ECV\n - ERC\n - NPL']
#   #     b2[label = 'CE objetivo\n - TRA\n - SU\n - HO']
#   #     c2[label = 'Causas no objetivo\n - CMNN\n - Otras CE\n - Otras ENT']
#   # }

# #     subgraph cluster2b{
# #     label = <<b>Paso 2b:</b> Redistribuir NNE};

# #     e[label = '50% a CMNN \n 50% a ENT objetivo por sexo y edad']
# # }
#     #   d2[label = 'Códigos garbage\n - GC1\n - GC2 \n ']
#     # }

#     subgraph cluster_3 {
#       label = <<b>Paso 3</b>>;
#       fillcolor = '#4D5492E5'

#       subgraph cluster_3a{
#       label = 'Recategorizar y redistribuir GC2';
#       fillcolor = '#ffffff50'

#       t3[style=invis]
#       a3[label = 'ENT objetivo\n - DM\n - ECV + GC2-ECV\n - ERC\n - NPL']
#       b3[label = 'CE objetivo\n - TRA\n - SU\n - HO\n + Redistribución GC2 CE (sexo y edad)']
#       c3[label = 'Causas no objetivo\n - CMNN\n - Otras CE + GC2-CE\n - Otras ENT']
#       }
#       d3[label = 'Códigos garbage\n - GC1']
#     }

#     subgraph cluster_4 {
#       label = <<b>Paso 4</b>>;
#       fillcolor = '#80E6FFE5'

#       subgraph cluster_4a{
#       label = 'Recategorizar y redistribuir GC1';
#       fillcolor = '#ffffff50'

#       t4[style=invis]
#       a4[label = 'ENT objetivo\n - DM\n - ECV\n - ERC + GC1-ERC\n - NPL']
#       b4[label = 'CE objetivo\n - TRA\n - SU\n - HO\n + Redistribución GC1 CE (sexo y edad)']
#       c4[label = 'Causas no objetivo\n - CMNN\n - Otras CE\n - Otras ENT']
#       }
#       d4[style='invis']
#     }

#     t1 -> a1 -> b1 -> c1 -> d1[style = invis]
#     t2 -> a2 -> b2 -> c2 -> e -> d2[style = invis]
#     t3 -> a3 ->b3 -> c3 -> d3[style = invis]
#     t4 -> a4 -> b4 -> c4 -> d4[style = invis]
#     d1 -> t3[style = invis]
#     d2 -> t4 [style = invis]
#   }
# "
# )

## Guardar figura -----
export_svg(fig1) |>
  charToRaw() |>
  rsvg_png(
    file = "figuras/Figura1.png",
    width = (17 * 300) / 2.54
  )
