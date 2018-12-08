
dic_gen <- read_csv('data/aux/dic_bott.csv')
cod_map <- read_csv('data/aux/depto_code.csv')

cod_map$code[is.na(cod_map$code)] <- 'NA'
cod_map$`hc-a2`[is.na(cod_map$`hc-a2`)] <- 'NA'

dic_variables <- read_csv('data/aux/dic_casos2.csv') 


shinyServer(function(input, output, session) {
  
  output$botonesT <- renderUI({
    temas <- unique(dic_gen$grupo_lab)
    
    l <- purrr::map(temas, function(z){
      HTML( paste0('<div class = "contMenu">',
                   tags$button(class = 'butTemas', type = "button", dic_gen$grupo[dic_gen$grupo_lab ==z]
                   ),
                   ' <div class = "dropdownMenuFicha">
	        <div class = "dropdownMenuContent">',
                   paste0('<a id = "',paste0(dic_gen$vars_id[dic_gen$grupo_lab ==z],'_',z), '" href = "#" class = "itemID">', dic_gen$vars[dic_gen$grupo_lab ==z] ,'</a>', collapse = ''), '</div></div>
           </div>'
      ))
    })
    l[[1]] <- gsub('butTemas', 'butTemas active', l[[1]])
    HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
  })
  
  
  dataElg <- reactive({
    
    idBase <- input$last_btn
    if (is.null(idBase)) idBase <- 'casos_acciones_belicas'
    
    df <- read_csv(paste0('data/clean/', idBase, '.csv'))
    df[is.na(df)] <- 'Sin información'
    df
  })
  
  
  
  output$tiempoDat <- renderUI({
    data <- dataElg() %>% select(ANNOH) %>% drop_na() %>% filter(ANNOH > 0) 
    sliderInput("range",
                "",
                min = min(data$ANNOH),
                max = max(data$ANNOH),
                value = c(min(data$ANNOH), max(data$ANNOH)),
                step = 1,
                animate = TRUE)
  })
  
  
  dataMapFilf <- reactive({
    
    data <- dataElg() %>% select(ANNOH) %>% drop_na() %>% filter(ANNOH > 0) 
    
    minFech <- as.numeric(input$range[1])
    maxFech <- as.numeric(input$range[2])
    
    if (minFech == min(data$ANNOH) && maxFech == max(data$ANNOH)) {
      df <- dataElg()
    } else {
      df <- dataElg() %>% filter(ANNOH %in% c(minFech:maxFech))
    }
    df
  })
  
  
  output$fechaPrint <- renderPrint({
    dataMapFilf()
  })
  
  output$titleMap <- renderUI({
    id <- input$last_btn
    if (is.null(id)) id <- 'casos_acciones_belicas'
    idS <- gsub('_.*', '', id)
    id <- gsub('casos_|victimas_', '', id)
    HTML(unique(paste0(
      '<p style="font-size:19px;font-weight: 700;margin-left: 3%;margin-top: 1%;">', 
      dic_gen$vars[dic_gen$vars_id == idS], ' de ', dic_gen$grupo[dic_gen$grupo_lab == id]), 
      '</p>'))
  })
  
  
  output$mapa <- renderHighchart({
    
    myClickFunc <- JS("function(event) {Shiny.onInputChange('ClickedCiudad', {id:event.point.name, timestamp: new Date().getTime()});}")
  
    data <- dataMapFilf() %>% group_by(DEPTO_CASO) %>% dplyr::summarise(value = n())
    data <- data %>% select(name = DEPTO_CASO, value) 
    data <- data %>% inner_join(cod_map)
    
    idBut <- input$last_btn
    if(is.null(idBut)) idBut <- 'Casos'
    idn <- gsub('Victimas', 'Víctimas', Hmisc::capitalize(gsub("\\_.*","",idBut, perl = T)))
    
    
    hcmap("countries/co/co-all",
          data = data, 
          value = "value",
          borderColor = "transparent",
          joinBy = list('hc-a2', 'code'),
          allowPointSelect = TRUE,
          cursor= 'pointer',
          events = list(click = myClickFunc),
          states = list(
            hover = list(
              color = '#662d91'
            ),
            select = list(
              color = '#662d91',
              borderColor = 'black',
              dashStyle = 'dot'
            )
          ),
          dataLabels = list(
            enabled = TRUE,
            #color = '#323232',
            style = list(
              fontSize = '9px',
              textShadow = FALSE,
              textOutline = FALSE,
              fontFamily= '\"Source Sans Pro\", sans-serif'),
            format = '{point.name}',
            showInLegend = FALSE),
          tooltip= list(
            headerFormat= 'Da click para conocer los datos de ',
            pointFormat='<b>{point.name}</b>'
          )
    )  %>% 
      hc_colorAxis(minColor = "#EDEDED", maxColor = "#1b343d") %>% 
      hc_legend(
        title = list(text = idn, 
                     style=list(
                       color = '#323232',
                       fontFamily = '\"Source Sans Pro\", sans-serif;'
                     )),
        layout = 'vertical',
        align = 'left',
        verticalAlign = 'middle',
        floating =TRUE
      ) %>% 
      hc_mapNavigation(
        enabled = TRUE,
        enableButtons = FALSE
      ) %>% 
      hc_credits(
        enabled = FALSE
      )
    
  })
  
  output$descripcion <- renderUI({
    data <- dataElg() %>% group_by(DEPTO_CASO) %>% dplyr::summarise(value = n())
    data <- data %>% select(name = DEPTO_CASO, value) 
    
    idBut <- input$last_btn
    idn <- gsub("\\_.*","",idBut, perl = T)
    
    if (is.null(idBut)){
      idBut <- 'acciones_belicas'
      idn <- 'casos'
    } else {
      idBut <- gsub('casos_|victimas_', '', idBut)
    }
    
    
    if (sum(unique(data$name) %in% c('EXTERIOR')) >= 1) {
      df <- data %>% filter(name == 'EXTERIOR')
      a <- paste0(', y del total se documentan <span style="color:#662d91;font-size: 17px;
            font-weight: 600;">', df$value, '</span> en el exterior')
    } else {
      a <- ''
    }
    
    if (nrow(filter(data, is.na(name))) >= 1) {
      b <- paste0(', donde <span style="color:#662d91;font-size: 17px;
            font-weight: 600;">', data$value[is.na(data$name)], '</span> no tienen información del lugar de ocurrencia. ')
    } else {
      b <- ' '
    }
    
    base_time <- dataElg() %>% select(ANNOH) %>% drop_na() %>% filter(ANNOH > 0) 
    
    
    HTML(paste0('<p style="font-size:15px;margin-top: 5%;margin-left: 5%;">Entre <b>',
                min(base_time$ANNOH),
                '</b> y <b>',
                max(base_time$ANNOH),
                '</b> El Observatorio de Memoria y Conflicto (OMC) ha registrado <span style="color:#662d91;font-size: 17px;
            font-weight: 600;">', format(nrow(dataElg()), nsmall=0, big.mark=","), '</span> ', idn ,' de ',
                unique(dic_gen$label[dic_gen$grupo_lab == idBut]) ,
                b, a,'</p>' )
    )
    
  })
  
  
  
  output$pais <- renderUI({
    
    actionButton('infoColombia', 'Total Colombia')
  })
  
  
  output$varGen <- renderUI({
    #names(dataElg())  
    id_names<- names(dataElg())
    if (is.null(id_names)) id_names <- NULL
    varInf <- data.frame(id = id_names)
    varInf <- varInf %>% inner_join(dic_variables) %>% filter(ind_sel == 'SI') %>% select(id, label)
    varInf <- as.list(setNames(varInf$id, varInf$label))
    selectizeInput('varPais', 'Elige una variable de interés', varInf)
  })
  
  
  output$vizCol <- renderHighchart({
    var <- input$varPais
    varLab <- dic_variables$label[dic_variables$id == var]
    df <- dataMapFilf() %>% select_(variable = var)
    typeVar <- dic_variables$ctypes[dic_variables$id == var]
    
    if (typeVar == 'Cat') {
      h <- df %>% 
        group_by(variable) %>% 
        summarise(Total = n())
    } else {
      h <- dataElg() %>% select_(Departamento = 'DEPTO_CASO', variable = var)
      h <- h %>% group_by(Departamento) %>% dplyr::summarise(Total = sum(variable))
    }
    
    hgch_bar_CatNum(h, 
                    sort = 'desc',
                    orientation = 'hor',
                    horLabel = ' ', 
                    verLabel = ' ',
                    tooltip = list(headerFormat = NULL, pointFormat = paste0('<b>{point.name}</b><br/>', varLab, ': <b>{point.y}</b>')),
                    export = TRUE,
                    thema = tma(fontFamily = '\"Source Sans Pro\", sans-serif', showLabel = TRUE)) 
    
  })
  
  observeEvent(input$infoColombia, {
    id <- input$last_btn
    if (is.null(id)) id <- 'casos_acciones_belicas'
    idS <- gsub('_.*', '', id)
    id <- gsub('casos_|victimas_', '', id)
    idTit <- HTML(unique(paste0(
      '<p style="font-weight: 700;margin-left: 3%;margin-top: 1%;">', 
      dic_gen$vars[dic_gen$vars_id == idS], ' de ', dic_gen$grupo[dic_gen$grupo_lab == id]), 
      '</p>'))
    showModal(modalDialog(
      title = toupper(idTit),
      footer = modalButton("Cerrar"), 
      easyClose = TRUE,
      uiOutput('varGen'),
      highchartOutput('vizCol')
    ))
  })
  
  baseDepto <- reactive({
    depto <- Hmisc::capitalize(tolower(input$ClickedCiudad$id))
    df <- dataMapFilf()
    df$DEPTO_CASO[df$DEPTO_CASO == 'Archipiélago de san andrés, providencia y santa catalina'] <- 'San andrés y providencia'
    df$DEPTO_CASO[df$DEPTO_CASO == 'Bogotá, d.c.'] <- 'Bogota'
    df <- df %>% filter(DEPTO_CASO %in% depto) %>% select(-DEPTO_CASO)
    
    df <- Filter(function(z) !all(is.na(z)), df)
    
    df
  })
  
  output$varDeptos <- renderUI({
    varInf <- data.frame(id = names(baseDepto()))
    varInf <- varInf %>% inner_join(dic_variables) %>% filter(ind_sel == 'SI') %>% select(id, label)
    varInf <- as.list(setNames(varInf$id, varInf$label))
    
    if (length(varInf) > 0) {
      selectizeInput('varDeptos', 'Elige una variable de interés', varInf)
    } else {
      depto <- input$ClickedCiudad$id
      id <- input$last_btn
      if (is.null(id)) id <- 'casos_acciones_belicas'
      id <- gsub('casos_|victimas_', '', id)
      idTit <- tolower(paste0(dic_gen$vars[dic_gen$grupo_lab == id], ' de ', dic_gen$label[dic_gen$grupo_lab == id]))
      paste0('No hay ', idTit)
    }
  })
 
  output$vizDeptos <- renderHighchart({
    
    var <- input$varDeptos
    varLab <- dic_variables$label[dic_variables$id == var]
    df <- baseDepto() %>% select_(variable = var)
    df$variable <- as.character(df$variable)
    df[is.na(df)] <- "Sin información"
    
    
    # if (var == 'e') {
    # MilitantePolitico
    # Etnia
    # TipoPoblacionVulnerable
    # }
    
    if (var == 'MilitantePolitico' | var == 'Etnia' | var == 'TipoPoblacionVulnerable' | var == 'Grupo' | var == 'DESCRIPCION_GRUPO') {
      df$variable[df$variable == "Sin información"] <- NA
    }
    
    dropNa <- FALSE
    
    if (var == 'MilitantePolitico' | var == 'Etnia' | var == 'TipoPoblacionVulnerable' | var == 'Grupo' | var == 'DESCRIPCION_GRUPO') {
    dropNa <- TRUE
    }
    
    typeVar <- dic_variables$ctypes[dic_variables$id == var]
    
    if (typeVar == 'Cat') {
      h <- df %>% 
        group_by(variable) %>% 
        summarise(Total = n())
      h$variable <- as.character(h$variable)
    } else {
      h <- baseDepto() %>% select_(Municipio = 'MUNINICIO_CASO', variable = var)
      h <- h %>% group_by(Municipio) %>% dplyr::summarise(Total = sum(variable))
    }
    
    
    hgch_bar_CatNum(h, 
                    sort = 'desc',
                    dropNa = dropNa,
                    orientation = 'hor',
                    horLabel = ' ', 
                    verLabel = ' ',
                    tooltip = list(headerFormat = NULL, pointFormat = paste0('<b>{point.name}</b><br/>', varLab, ': <b>{point.y}</b>')),
                    export = TRUE,
                    thema = tma(fontFamily = '\"Source Sans Pro\", sans-serif', showLabel = TRUE)) 
      
    
  })
  
  observeEvent(input$ClickedCiudad$id, {
    depto <- input$ClickedCiudad$id
    id <- input$last_btn
    if (is.null(id)) id <- 'casos_acciones_belicas'
    idS <- gsub('_.*', '', id)
    id <- gsub('casos_|victimas_', '', id)
    idTit <- toupper(paste0(dic_gen$vars[dic_gen$vars_id == idS], ' de ', dic_gen$label[dic_gen$grupo_lab == id]))
    id <- HTML(unique(paste0(idTit, ' EN ', '<span style = "color:#662d91">',toupper(depto),'</span>')))
    showModal(modalDialog(
      title = id,
      footer = modalButton("Cerrar"), 
      easyClose = TRUE,
      uiOutput('varDeptos'),
      highchartOutput('vizDeptos')
    ))
  })
  
  
  output$downBase <- renderUI({
    list(
    downloadButton('descarga', 'Descarga los datos en CSV'),
    downloadButton('descargaXlsx', 'Descarga los datos en XLSX')
    )
  })
  

  
  output$descarga <- downloadHandler(
    
    filename = function() {
      idBase <- input$last_btnTabla
      if (is.null(idBase)) idBase <- 'casos_acciones_belicas'
      paste0(idBase, '.csv')
    },
    content = function(con) {
      idBase <- input$last_btnTabla
      if (is.null(idBase)) idBase <- 'casos_acciones_belicas'
      d <- read_csv(paste0('data/clean/', idBase, '.csv'))
      write_csv(d, con, na = '')
    }
  )
  
  output$descargaXlsx <- downloadHandler(
    filename = function() {
      idBase <- input$last_btnTabla
      if (is.null(idBase)) idBase <- 'casos_acciones_belicas'
      paste0(idBase, '.xlsx')
    },
    content = function(con) {
      idBase <- input$last_btnTabla
      if (is.null(idBase)) idBase <- 'casos_acciones_belicas'
      d <- read_csv(paste0('data/clean/', idBase, '.csv'))
      rio::export(d, con)
    }
  )
  
  # Tabla de datos ----------------------------------------------------------
  
  output$titleDatos <- renderUI({
    id <- input$last_btnTabla
    if (is.null(id)) id <- 'casos_acciones_belicas'
    idS <- gsub('_.*', '', id)
    id <- gsub('casos_|victimas_', '', id)
    HTML(unique(paste0(
      '<p style="font-size:19px;font-weight: 700;margin-left: 3%;margin-top: 1%;">', 
      dic_gen$vars[dic_gen$vars_id == idS], ' de ', dic_gen$grupo[dic_gen$grupo_lab == id]), 
      '</p>'))
  })
  output$botonesTabla <- renderUI({
    temas <- unique(dic_gen$grupo_lab)
    
    l <- purrr::map(temas, function(z){
      HTML( paste0('<div class = "contMenu">',
                   tags$button(class = 'butTemas', type = "button", dic_gen$grupo[dic_gen$grupo_lab ==z]
                   ),
                   ' <div class = "dropdownMenuFicha">
	        <div class = "dropdownMenuContent">',
                   paste0('<a id = "',paste0(dic_gen$vars_id[dic_gen$grupo_lab ==z],'_',z), '" href = "#" class = "itemIDTabla">', dic_gen$vars[dic_gen$grupo_lab ==z] ,'</a>', collapse = ''), '</div></div>
           </div>'
      ))
    })
    l[[1]] <- gsub('butTemas', 'butTemas activeTab', l[[1]])
    HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
  })
  
  
  
  dataElgTabla <- reactive({
    
    idBaseT <- input$last_btnTabla
    if (is.null(idBaseT)) idBaseT <- 'casos_acciones_belicas'
    df <- read_csv(paste0('data/clean/', idBaseT, '.csv'))
    df <- Filter(function(z) !all(is.na(z)), df)
    varInf <- data.frame(id = names(df))
    dicVar <- varInf %>% left_join(dic_variables) %>%  select(id, label)
    names(df) <- dicVar$label
    as.data.frame(df)
  })
  # 
  # 
  output$tabPrev <- renderDataTable({
    data <- dataElgTabla()
    data[is.na(data)] <- 'NA'
    data <- map(data, function(z){factor(as.character(z))}) 
    data <- bind_cols(data)
    DT::datatable(data, 
                  filter = 'top',
                  class = 'cell-border stripe',
                  rownames = FALSE,
                  selection = "none",
                  options = list(
                    dom = 'tpf',
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    scrollY ="350px",
                    pageLength= 10,
                    autoWidth = TRUE,
                    scrollX ="10%"))
  })
  

  
  output$descTablas <- renderUI({
    
    idBut <- input$last_btnTabla
    idn <- Hmisc::capitalize(gsub("\\_.*","",idBut, perl = T))
    
    if (is.null(idBut)){
      idBut <- 'acciones_belicas'
      idn <- 'Casos'
    } else {
      idBut <- gsub('casos_|victimas_', '', idBut)
      idn <- idn
    }
    
    HTML(paste0('<p style="font-size:15px;margin-top: 5%;margin-left: 9%;"> <b>Base de datos de:</b> ', idn ,' de ',
                unique(dic_gen$label[dic_gen$grupo_lab == idBut]), '</p>'))
    
  })
  
  
  # Cronología --------------------------------------------------------------
  
  output$botonesCron <- renderUI({
    temas <- unique(dic_gen$grupo_lab)
    
    l <- purrr::map(temas, function(z){
      HTML( paste0('<div class = "contMenu">',
                   tags$button(class = 'butTemas', type = "button", dic_gen$grupo[dic_gen$grupo_lab ==z]
                   ),
                   ' <div class = "dropdownMenuFicha">
	        <div class = "dropdownMenuContent">',
                   paste0('<a id = "',paste0(dic_gen$vars_id[dic_gen$grupo_lab ==z],'_',z), '" href = "#" class = "itemIDCron">', dic_gen$vars[dic_gen$grupo_lab ==z] ,'</a>', collapse = ''), '</div></div>
           </div>'
      ))
    })
    l[[1]] <- gsub('butTemas', 'butTemas activeCron', l[[1]])
    HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
  })
  
  
  dataCrono <- reactive({
    
    idBaseT <- input$last_cron
    if (is.null(idBaseT)) idBaseT <- 'casos_acciones_belicas'
    df <- read_csv(paste0('data/clean/', idBaseT, '.csv'))
    df <- df %>% filter(ANNOH > 0) %>% drop_na(ANNOH)
    as.data.frame(df)
  })
  
 output$selLugar <- renderUI({
   radioButtons('lugarCron', '', c('Departamento', 'Total Colombia'), inline = TRUE)
 })  
  
 output$selCron <- renderUI({
   varInf <- data.frame(id = names(dataCrono()))
   
   idLugar <- input$lugarCron
   
   if (idLugar == 'Departamento') {
     varInf <- varInf %>% filter(id != 'DEPTO_CASO')
   }
   
   varInf <- varInf %>% inner_join(dic_variables) %>% filter(ind_sel == 'SI') %>% select(id, label)
   varInf <- as.list(setNames(varInf$id, varInf$label))
   selectizeInput('varCronologia', '', varInf)
 })

 
 output$selDeptoCron <- renderUI({
   idLugar <- input$lugarCron
   
   if (idLugar == 'Departamento') {
     selectizeInput('deptoCron', '', unique(dataCrono()$DEPTO_CASO))
   } else {
     return()
   }
 })
   
  dataCronologia <- reactive({
    idLugar <- input$lugarCron
    idVar <- input$varCronologia
    
    if (idLugar == 'Departamento') {
     df <- dataCrono() %>% select_(Anio = 'ANNOH', Departamento = 'DEPTO_CASO', variable = idVar)
     df <- df %>% filter(Departamento %in% input$deptoCron)
     df <- df %>% select(-Departamento)
    } else {
     df <- dataCrono() %>% select_(Anio = 'ANNOH', variable = idVar)
    }
    df <- df %>% drop_na()
    var <- input$varCronologia
    varLab <- dic_variables$label[dic_variables$id == var]
    
    typeVar <- dic_variables$ctypes[dic_variables$id == var]
    
    #df <- dataCronologia()
    df[is.na(df)] <- "Sin información"
    
    if (var == 'MilitantePolitico' | var == 'Etnia' | var == 'TipoPoblacionVulnerable' | var == 'Grupo' | var == 'DESCRIPCION_GRUPO') {
      df$variable[df$variable == "Sin información"] <- NA
    }
    
    if (typeVar == 'Cat') {
      h <- df %>% 
        group_by(variable, Anio) %>% 
        summarise(Total = n())
      h$variable <- as.character(h$variable)
      h$Anio <- as.numeric(h$Anio)
      } else {
      h <- df %>% group_by(Anio) %>% dplyr::summarise(Total = sum(variable))
      }
    h
  })
  
  output$ba <- renderPrint({
    dataCronologia()
  })
  
  output$vizCron <- renderHighchart({
    var <- input$varCronologia
    varLab <- dic_variables$label[dic_variables$id == var]
    
    typeVar <- dic_variables$ctypes[dic_variables$id == var]
    h <- dataCronologia()
    
    if (typeVar == 'Cat') {
      viz <- hgch_line_CatYeaNum(h, horLabel = paste0('Total ', varLab), verLabel = 'Año', thema = tma(symbLine = FALSE), nDigits = 2, dropNa = TRUE) 
    } else {
      viz <- hgch_line_YeaNum(h, horLabel = 'Año', verLabel = paste0('Total ', varLab), theme = tma(symbLine = FALSE), dropNa = TRUE)
    }
    viz
    
  })
  
  
    output$textCron <- renderUI({
    # historio de acciones belicas en el departamento del choco por region
    # historico de acciones belicas en Colombia por region
    id <- input$last_cron
    if (is.null(id)) id <- 'casos_acciones_belicas'
    idS <- gsub('_.*', '', id)
    id <- gsub('casos_|victimas_', '', id)
    idTit <- tolower(unique(paste0(dic_gen$vars[dic_gen$vars_id == idS], ' de ', dic_gen$label[dic_gen$grupo_lab == id])))
    
    varElg <- dic_variables$label[dic_variables$id == input$varCronologia]
    
    tx <- HTML(paste0('<p style="margin-left:3%; margin-top:3%;">Histórico de ', idTit, ' en Colombia por ', varElg, '</p>'))
    
    if (input$lugarCron == 'Departamento') {
      tx <- HTML(paste0('<p style="margin-left:1%; margin-top:1%;font-size: 17px; font-weight: 700;">Histórico de ', idTit, ' en ', input$deptoCron, ' por ', varElg, '</p>'))
    }
    
    tx
    
  })

  

# Clasificación -----------------------------------------------------------

 
# 
#  
#   
  output$deptoClas <- renderUI({
    df <- read_csv('data/aux/ultimabase.csv')
   
    deptos <- c('Todos', unique(df$DEPTO_CASO))
    selectizeInput('deptoElgClas', 'Lugar del hecho:', deptos)
  })
  
  
  tabClasifFilt <- reactive({
    
    df <- read_csv('data/aux/ultimabase.csv')
    depId <- input$deptoElgClas
    
    if (depId != 'Todos') {
      df <- df %>% filter(DEPTO_CASO %in% depId)
    } else {
      df <- df
    }
    
    df
    
  })
  
  
  output$anioGrupo <- renderUI({
    anios <- c('Todos', unique(tabClasifFilt()$ANNOH))
    selectizeInput('anioClas', 'Año', anios)
  })
  
  
  tabClasifFiltAnio <- reactive({
    df <- tabClasifFilt()
    anioId <- input$anioClas
    
    if (anioId != 'Todos')
      df <- df %>% filter(ANNOH %in% anioId)
    
    df
  })
  
  

  
  output$selGrupo <- renderUI({
    #d <- tabClasifFiltAnio()
    var <- unique(tabClasifFiltAnio()$PRESUNTO_REPONSABLE)
    
    if (length(var) == 1) {
      n <- 1
    } else {
      n <- 2
    }
    
    checkboxGroupInput('varClasf', 'Ver los enfrentamientos entre grupos en acciones bélicas:', var, inline = TRUE, selected = sample(var, n))
    #, multiple = TRUE,selected = sample(var, 2))
  })


  datavizClas <- reactive({
    varGr <- input$varClasf
    varAnio <- input$anioClas


    d <- tabClasifFiltAnio() %>% filter(PRESUNTO_REPONSABLE %in% varGr)

    d <- d %>% group_by(PRESUNTO_REPONSABLE, PRESUNTO_REPONSABLE2) %>%
      dplyr::summarise(total = n()) %>% drop_na()

    d$PRESUNTO_REPONSABLE2 <- map(1:nrow(d), function(y) {
      if(d$PRESUNTO_REPONSABLE2[y] == d$PRESUNTO_REPONSABLE[y]) {
        d$PRESUNTO_REPONSABLE2[y] <- paste0(d$PRESUNTO_REPONSABLE2[y], '2')
      } else{
        d$PRESUNTO_REPONSABLE2[y] <- d$PRESUNTO_REPONSABLE2[y]
      }
    }) %>% unlist()

    d
  })
  

  
  
 # output$aclaracion <- renderUI({
 #   # if (nrow(datavizClas()) < 2) {
 #   #   'No se registra enfrentamientos entre los grupos seleccionados'
 #   # } else {
 #   #   return()
 #   # }
 #   # 
 #   nrow(datavizClas())
 # })
  
  output$vizClasif <- renderSankeyNetwork({#renderHighchart({

    d <- datavizClas()
    x <- unique(d$PRESUNTO_REPONSABLE)
    y <- unique(d$PRESUNTO_REPONSABLE2)
    ide <- setdiff(y, x)

    bsEx <- data.frame(name = c(unique(d$PRESUNTO_REPONSABLE), ide), target = 0:(length(c(unique(d$PRESUNTO_REPONSABLE), ide))-1))
    bsEx$name <- as.character(bsEx$name)

    d <- d %>% plyr::rename(c('PRESUNTO_REPONSABLE2' = 'name'))

    f <- d %>% inner_join(bsEx)
    f <- f %>% select(-name)

    f <- f %>% plyr::rename(c('PRESUNTO_REPONSABLE' = 'name'))
    bsEx <- bsEx %>% plyr::rename(c('target' = 'source'))

    f <- f %>% inner_join(bsEx)
    f <- f %>% select(-name)

    sankeyNetwork(Links = f, Nodes = bsEx, Source = "source",
                  Target = "target", Value = "total", NodeID = "name",
                  fontSize = 12)

  })
  
  # Paginas -----------------------------------------------------------------
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$id)) {
      updateTabsetPanel(session, "omuApp",
                        selected = query$id
      )
    }
  })   
  
})
