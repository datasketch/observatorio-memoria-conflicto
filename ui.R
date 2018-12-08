

shinyUI(
  fluidPage(
    useShinyjs(),
    # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                 tags$img(src = 'Cargando.gif', class="loadmessage")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/onmc.js")
     ),
    tabsetPanel(id = 'omuApp',
                tabPanel('Geográfico', value = 'geografica',
    
        
        div(class = 'contCaf',
         div(class = 'mapTit',
             uiOutput('titleMap'),
        highchartOutput('mapa', width = 730, height = 610)),
        #verbatimTextOutput('fechaPrint'),
        div(class = 'contCol',
            HTML('<p style="font-size:17px;margin-top:2%;margin-left:3%;">Pasa el ratón por los botones y selecciona casos o víctimas para filtrar en el mapa:'),
            uiOutput('botonesT'),
            uiOutput('descripcion'),
            uiOutput('tiempoDat'),
          div(class = 'conBoton',
            uiOutput('pais')
        ))
    )
  ),
  tabPanel('Datos', value = 'datos',
          div(class = 'contCaf',
              div(class = 'MapTit',
                  uiOutput('titleDatos'),
              dataTableOutput('tabPrev')),
              div(class = 'contCol',
                  HTML('<p style="font-size:17px;margin-top:1%;margin-left:7%;">Pasa el ratón por los botones y selecciona casos o víctimas para filtrar la base de datos:'),
              uiOutput('botonesTabla'),
              uiOutput('descTablas'),
              verbatimTextOutput('sds'),
              uiOutput('downBase')
              )
          )
          
  ),
  tabPanel('Cronología', value = 'cronologia',
           div(class = 'contCaf',
               div(class = 'allCron',
           uiOutput('textCron'),
           highchartOutput('vizCron', height = 549)
           #verbatimTextOutput('aver')
           ),
           div( class = 'cronTem',
           uiOutput('selLugar'),
           #div(class = 'contCron',
               uiOutput('selCron'),
               uiOutput('selDeptoCron'),
           #),
           uiOutput('botonesCron')
           )
           )
  ),
  tabPanel('Clasificación', value = 'clasificacion',
           #div(class = 'contclasif',
           div(class = 'lineGrup',
               uiOutput('deptoClas'),
               uiOutput('anioGrupo')
               
           ),
           uiOutput('selGrupo'),
           div(class = 'conSanky',
           sankeyNetworkOutput('vizClasif'))#,
           #uiOutput('selGrupo')
           
           #)
  )
    )
  )
)