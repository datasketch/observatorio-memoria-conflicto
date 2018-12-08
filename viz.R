
acc_bel <- read_csv('data/clean/casos_acciones_belicas.csv')

# caso uno  variable categorica
df1 <- acc_bel %>% select(var = Modalidad)
df1 <- df1 %>% group_by(var) %>% dplyr::summarise(total = n()) 



# caso dos variable númerica
df2 <- acc_bel %>% select(DEPTO_CASO, total = TotalOtrasFuerzasArmadasEstatales)
df2 <- df2 %>% group_by(DEPTO_CASO) %>% dplyr::summarise(total = sum(total, na.rm = TRUE))


library(hgchmagic)

hgch_bar_CatNum(df2)


# caso fecha variable categorica

df3 <- acc_bel %>% select(ANNOH, var = Iniciativa) 
df3 <- df3 %>% group_by(ANNOH, var) %>% summarise(total =n())










highchart() %>% 
  hc_chart(type = "line") %>% 
 hc_xAxis( list (
  categories= list('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
)) %>% 

hc_series(list (
  name= 'Predefined symbol',
  data= list(29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 316.4, 294.1, 195.6, 154.4),
  marker= list (
    symbol= 'triangle',
    fillColor = 'red'
  )
), list (
  name= 'Image symbol',
  data= list(216.4, 194.1, 95.6, 54.4, 29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5),
  marker= list (
    symbol= 'url(https://github.com/CamilaAchury/icon_viz/blob/master/repo/soldier.png?raw=true)',
    fillColor = 'red'
    )
)
)

#'noun_waxy monkey tree frog_1825100.png'



custom_theme  <- function(custom = NULL,...){
  if (!is.null(custom)) {
    theme <- custom
  }
  theme
}

tma <- hc_theme(
  colors = c("#868888", "#74D1F7", "#B70F7F", "#A6CEDE", "#ACD9C2", "#61C1E8", "#A6CEDE", "#61C1E8", "#FECA84", "#ACD9C2", "#EEF1F2"),
  #colors = c("#74D1F7", "#61C1E8", "#78D8FF", "#61C1E8", "#"),
  chart = list(
    # width = 220,
    # height = 160,
    backgroundColor = "transparent"
  ),
  plotOptions = list(
  series = list( 
    dataLabels = list(
      style = list(
        color = '#323232',
        fontSize = '13px',
        fontFamily = "Source Sans Pro",
        textDecoration= 'none',
        textShadow = 'none',
        textOutline = 'none'
      )
    )
    )
  )
)



acc_bel <- read_csv('data/clean/casos_acciones_belicas.csv')
acc_bel$`Grupo Armado 2`[acc_bel$`Grupo Armado 2` == 'Agente del estado'] <- 'Otro agente del estado'

acc_bel  <- acc_bel %>% filter(ANNOH == '2002')
acc_bel <- acc_bel %>% 
            group_by(`Grupo Armado 1`, `Grupo Armado 2`) %>% 
             dplyr::summarise(total = n()) %>% drop_na() 

acc_bel <-  acc_bel %>% filter(`Grupo Armado 1` %in% c('Agente extranjero', 'Crimen organizado', 'Guerrilla'))
  
  
  
  bla <- map(1:nrow(acc_bel), function(z){
     list( acc_bel$`Grupo Armado 1`[z],
                        acc_bel$`Grupo Armado 2`[z],
                        acc_bel$total[z])
  })
  
highchart() %>% 
  hc_chart(type='sankey') %>%
hc_series( list(
  keys= list('from', 'to', 'weight'),
  data= bla#,
  # nodes = list( 
  #   list(
  #   id =  'Agente del estado',
  #   color = 'red'
  # ), list (
  #   id = 'Crimen organizado',
  #   color ='#7cb5ec'
  # )
  # )
)
) %>% 
  hc_add_theme(custom_theme(custom = tma)) 



d <- read_csv('data/clean/casos_acciones_belicas.csv')
d <- d %>% group_by(`Grupo Armado 1`, `Grupo Armado 2`) %>% 
  dplyr::summarise(total = n()) %>% drop_na()

d$`Grupo Armado 2` <- map(1:nrow(d), function(y) {
  if(d$`Grupo Armado 2`[y] == d$`Grupo Armado 1`[y]) {
    d$`Grupo Armado 2`[y] <- paste0(d$`Grupo Armado 2`[y], ' 2')
  } else{
    d$`Grupo Armado 2`[y] <- d$`Grupo Armado 2`[y]
  }
}) %>% unlist()

x <- unique(d$`Grupo Armado 1`)
y <- unique(d$`Grupo Armado 2`)
ide <- setdiff(y, x)


my_color <- paste0('d3.scaleOrdinal() .domain([', gsub(", ",",",toString(paste0('\"', c(unique(d$`Grupo Armado 1`), ide), '\"'))),']) .range(["#868888", "#74D1F7", "#B70F7F", "#A6CEDE", "#ACD9C2", "#61C1E8", "#A6CEDE", "#61C1E8", "#FECA84", "#ACD9C2", "#EEF1F2"])')




bsEx <- data.frame(name = c(unique(d$`Grupo Armado 1`), ide), target = 0:(length(c(unique(d$`Grupo Armado 1`), ide))-1))
bsEx$name <- as.character(bsEx$name)

d <- d %>% plyr::rename(c('Grupo Armado 1' = 'name'))

f <- d %>% inner_join(bsEx)
f <- f %>% select(-name)

f <- f %>% plyr::rename(c('Grupo Armado 2' = 'name'))
bsEx <- bsEx %>% plyr::rename(c('target' = 'source'))

f <- f %>% inner_join(bsEx)
f <- f %>% select(-name)

sankeyNetwork(Links = f, Nodes = bsEx, Source = "source",
              Target = "target", Value = "total", NodeID = "name",
              colourScale = my_color,
              fontSize = 12)

