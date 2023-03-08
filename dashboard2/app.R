# 0.1 Load libraries ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
#library(shinycssloaders)
library(waiter)
library(tidyverse)
library(sf)
library(geojsonsf)
library(leaflet)
library(plotly)
library(geojsonio)
library(reactable)
library(kableExtra)
library(htmltools)
library(paletteer)
library(comprehenr)
library(tippy)
library(profvis)

# 0.2 Load data -------
load("data/datos.RData") # cargar datos
load("data/geo_datos.RData")
load("data/diccionario.RData") # cargar diccionario
#load("data/intro_table.RData") # cargar intro_table
load("data/data_sun.RData") # cargar data para sunburst
admin0 <- geojson_sf('data/admin_0.json')

# 0.3 Source utils----
source("utils.R")

# temporal for manual checking only
#diccionario['unidad'][diccionario['codigo']=='sb2'] <- 'porcentaje2'
# datos['sb2'] <- datos['sb2']*100 # Esto sirve para escalar cancer y diabetes y que se puedan mostrar las barras

# 0.4 Sunburst of variables ----
sun_vars <- plot_ly(
  data_sun,
  labels = ~Name, 
  parents = ~parents, 
  values= ~values, 
  type = 'sunburst',
  rotation = 155,
  width = 450,
  height = 350,
  branchvalues = "total",
  hoverinfo = "text",
  hovertext = ~paste0("<b>", ids, ':</b><br>', nombre, '<br>',
                      "<b>Indicador:</b><br>",
                      str_wrap(indicadores, width = 35)
                      )
  ) %>% 
  layout(
    font = pl_sunburst,
    margin = list(l = 1, r = 1, b = 25, t = 0),
    sunburstcolorway = c(col_d1, col_d2, col_d3, col_d4)) %>%
  config(displaylogo = FALSE)

# 0.5 Sunburst mini ----
sun_mini <- plot_ly(
  filter(
    data_sun, ids != "Variable"),
  labels = ~Name, 
  parents = ~parents, 
  values = ~values, 
  type = 'sunburst', 
  rotation = 155,
  branchvalues ="total",
  width = 150,
  height = 150) %>% 
  layout(
    font = pl_sunburst,
    autosize = FALSE, 
    hovermode = FALSE,
    margin = list(l = 1, r = 1, b = 1, t = 1),
    sunburstcolorway = c(col_d1, col_d2, col_d3, col_d4)) %>%
  config(displayModeBar = F)

# 0.6 Items input indicadores----
items_ind <- setNames(diccionario$nombre_input, diccionario$ind_html)

# 0.7 Items input paises----
items_reg <- setNames(unique(datos$region_input), unique(datos$reg_html))

# 0.8 PickerInput styling:----
## Color list
col.list <- c("Black",
              rep_len(col_d3, 10), 
              rep_len(col_d4, 9), 
              rep_len(col_d1, 13),
              rep_len(col_d2, 12))
## Change the color
picker_style <- paste0("color:",col.list,";")
## Change the font
picker_style <- paste0(picker_style,"font-family: Arial;")
## Change to bold
## picker_style <- paste0(picker_style,"font-weight: bold;")

# 0.9 Crear Leaflet----
map <- leaflet(
    width = "100%", height = "600px",
    options = leafletOptions(
      minZoom = 3, maxZoom = 8, 
      zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }") %>% 
  setView(lng = -74.99, lat = -9.19, zoom = 3) %>% 
  setMaxBounds(lng1 = -120, lng2 = -34,
               lat1 = 34 , lat2 = -58) %>%
  addMapPane("hombres", zIndex = 410) %>% # Level 1: bottom
  addMapPane("adm0", zIndex = 420) %>%  # Level 2: middle
  addMapPane("labels", zIndex = 430) %>% # Level 3: top
  addMapPane("mujeres", zIndex = 415) %>% # Level 3: top
  addMapPane("brecha", zIndex = 417) %>% # Level 3: top
  addProviderTiles(
    providers$CartoDB.PositronOnlyLabels,
    providerTileOptions(opacity = 0.8),
    options = pathOptions(pane = "labels")) %>% 
  addPolygons(
    data = admin0,
    weight = 0.5,
    color = "black",
    fill = FALSE,
    fillColor = "white",
    fillOpacity = 1,
    smoothFactor = 0,
    options = pathOptions(pane = "adm0")) 

# 0.10 Loaders options----
options(spinner.color = central_blue)

# 1 Shiny UI #########################################
ui <- fluidPage(
  tags$head( 
    tags$head(includeCSS("www/styles.css"))),

  useShinydashboard(),
  
  autoWaiter(
    html = spin_throbber(),
    color = transparent(.2)
    ),
  
  fluidRow( ## Encabezado----
    column(2, ### Mini sunburst----
      sun_mini,
      align = "center",
      class = "mini-sunburst"
      ),
    column(10, ### Título----
      fluidRow(
        column(12,
          h1("Índice del Desarrollo Social de la Mujer y el Hombre en Países
             de América Latina 2022 - IDSMH")
           )
        ),
      fluidRow( ### Texto----
        column(12, p(txt_intro, class = "subtitulo"))
        ),
      fluidRow( ### Botón descarga----
        column(12, downloadButton("downloadpublication",
                                  "DESCARGA LA PUBLICACIÓN",
                                  class = "dwnld-button",
                                  style = button_style))
        )
      )
    ),
  tabsetPanel( ## T0 Tabset----
    tabPanel( ## T1 Índice y resultados generales----
      "EL IDSMH", icon = icon("life-ring"),
      fluidRow(
        style = "padding-bottom:0px; border-bottom:2px dotted #798BFE;",
        column(6, ### El IDSMH----
          style = "background-color:white; padding: 20px 30px 10px 15px;", # ; height:100vh
          fluidRow(
            column(12, h3("DIMENSIONES, FACTORES Y VARIABLES"))),
          fluidRow(
            column(12, p(txt_indice_a, style = "padding-bottom:0px"))
            ),
          fluidRow(
            column(12, style = "padding-top:0px;",
                   sun_vars, align = "center")
            ),
          fluidRow(
            # style = "margin-top: 20px;",
            column(12, p(txt_indice_b))
            ),
          fluidRow(
            column(12, p(txt_indice_c))
            )
          ),
        column(6, ### Resumen ejecutivo----
          style = "background-color:#E8EBF0; border-left:2px dotted #798BFE; padding: 20px 15px 40px 30px", #height:100vh
          fluidRow(
            column(12,
              h3("RESUMEN EJECUTIVO"))
            ),
          fluidRow(
            column(12, p(txt_articulo))
            )
          )
        ),
      fluidRow( ### Créditos----
        column(12, style = "padding-top:40px;",
          h3("CRÉDITOS"))
        ),
      fluidRow(
        column(12, style = "padding-bottom:40px;",
          class = "frow-bottom",
          p(class = "articulo", txt_creditos)
          )
        )
      ),
    tabPanel( ## T2 Territorios e indicadores----
      "EXPLORA INDICADORES Y TERRITORIOS", icon = icon("chart-bar"),
      fluidRow(
        class = "frow-bottom",
        style = "padding-bottom:0px; background-color: #E8EBF0",
        column(6, ### Mapa----
          height = 800, 
          style = "border-right:2px dotted #798BFE; padding: 20px 30px 40px 15px; background-color:white",
          fluidRow(
            column(12, h3("MAPA REGIONAL"))
          ),
          fluidRow(
            column(12, p(txt_map))
          ),
          fluidRow(
            column(12,
              selectizeInput("indicador_mapa", 
                label = "INDICADOR:",
                choices = items_ind,
                # selected = 0,
                width = 500,
                options = list(
                  render = I("
                    {
                    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                  }"))
                ))
            ),
          fluidRow(
            column(12,
                  # withSpinner(
                  leafletOutput("mapa", height = 600)#, 
                   #  hide.ui = FALSE)
                   )
            )
          ),
        column(6, ### Dumbell----
          style = "padding: 20px 15px 20px 30px",
          fluidRow(
            column(12, h3("PERFIL TERRITORIAL POR FACTORES"))
            ),
          fluidRow(
            column(12,
                   p(txt_dumbell))
            ),
          fluidRow(
            column(12,
              selectizeInput("region_dumbell",
                label = "TERRITORIO:",
                choices = items_reg,
                selected = "PERÚ",
                options = list(
                  render = I("
                    {
                    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                    }")
                  )
                )
              )
            ),
          fluidRow(
            column(12,
                   #withSpinner(
                   plotlyOutput("dumbell", height = 600)#, hide.ui = FALSE)
                   )
                   )
          )
        ),
      fluidRow( ### Scatter ordenado----
        class = "frow-bottom",
        style = "padding: 40px 15px",
        column(12,  
          fluidRow(
            column(12, h3("RANKING DE TERRITORIOS POR INDICADOR"))
            ),
          fluidRow(
            column(12, p(txt_scatter_ord))
            ),
          fluidRow(
            column(12,
              fluidRow(
                column(6,
                  selectizeInput(
                    "indicador_scatter_ord",
                    label = "INDICADOR:",
                    choices = items_ind,
                    selected = 1,
                    options = list(
                      render = I("
                        {
                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                        }")))
                  ),
                column(6,
                  radioButtons(
                    "orden_scatter_ord", label = "ORDEN:",
                    choices = list("Hombre" = 1, "Mujer" = 2), 
                    selected = 1,  inline = T))))
            ),
          fluidRow(
            column(12, #withSpinner(
                   plotlyOutput("disp_ordenado", height = 350)#, hide.ui = FALSE)
                   ))
          )
        ),
      fluidRow( ### Scatters----
        class = "frow-bottom",
        style = "padding: 40px 15px 60px 15px",
        column(12, h3("CORRELACIÓN ENTRE INDICADORES")),
        column(12, p(txt_scatters)),
        column(12,
          fluidRow(
            column(12,
              fluidRow(
                column(6,
                  selectizeInput("indicador_2",
                    label = "INDICADOR PARA EL EJE VERTICAL:",
                    choices = items_ind,
                    selected = unique(diccionario$nombre_input)[41],
                    options = list(
                      render = I("
                        {
                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                        }")))),
                column(6,
                  selectizeInput("indicador_1",
                    label = "INDICADOR PARA EL EJE HORIZONTAL:",
                    choices = items_ind,
                    selected = unique(diccionario$nombre_input)[10],
                    options = list(
                      render = I("
                        {
                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                        }"))))
                )
              )
            )
          ),
        column(12, #withSpinner(
               plotlyOutput("disp_hm", height = 400)#, hide.ui = FALSE)
               )
#        column(12, textOutput("pie_de_scatter", height = 50))
        )
      ),
    
    tabPanel( ## T3 Crea tabla----
      "CREA TABLAS DE DATOS", icon = icon("table"),
      
      fluidRow( ### Tabla nacional----
                class = "frow-bottom",
                style = "padding-bottom:50px",
        column(12, 
          fluidRow(
            column(12, style = "padding-top:20px",
              h3("CREA UNA TABLA A NIVEL NACIONAL"))
            ),
          fluidRow(
            column(12, p(txt_tabla_pais))
            ),
          fluidRow(
            column(12,
              fluidRow(
                column(12,
                  fluidRow(
                    column(8,
                      pickerInput("indicador_tabla_pais", "SELECCIONA HASTA 5 INDICADORES:",
                        choices = unique(diccionario$nombre_input), 
                        options = list(
                          `actions-box` = FALSE,
                          size = 10, # número de opciones mostradas en menú
                          "max-options" = 5,
                          "max-options-text" = "¡Máximo 5 indicadores!"),
                        choicesOpt = list(
                          style = picker_style
                          ),
                        multiple = TRUE,
                        width = 600)
                      ),
                    column(4,
                      csvDownloadButton("tabla_pais", 
                                        filename = "tabla_pais.csv"),
                      # downloadButton(
                      #   "downloadData", 
                      #   "DESCARGA LA TABLA", 
                      #   class = "dwnld-button",
                      #   style = button_style)
                      )
                    )
                  )
                )
              )
            ),
          fluidRow(
            column(12,
                   #withSpinner(
                     reactableOutput("tabla_pais", height = 420)#, hide.ui = FALSE)
                   )
            )
          )
        ),
      fluidRow(### Tabla regional----
        class = "frow-bottom",    
        style = "padding-bottom:50px",
        column(12,
          fluidRow(
            column(12,
              h3("CREA UNA TABLA A NIVEL REGIONAL"))
            ),
          fluidRow(
            column(12, p(txt_tabla_region))
            ),
          fluidRow(
            column(12,
              fluidRow(
                column(12,
                  fluidRow(
                    column(8,
                      pickerInput(
                        "indicador_tabla_region",
                        "SELECCIONA HASTA 5 INDICADORES:", 
                        choices = unique(diccionario$nombre_input),
                        options = list(
                          `actions-box` = FALSE,
                          size = 10, # número de opciones mostradas en menú
                          "max-options" = 5,
                          "max-options-text" = "¡Máximo 5 indicadores!"),
                        choicesOpt = list(
                          style = picker_style
                          ),
                        multiple = T)
                      ),
                    column(4,
                      csvDownloadButton(
                        "tabla_region", 
                        filename = "tabla_region.csv"),
                      # downloadButton(
                      #   "downloadDataRegion", 
                      #   "DESCARGA LA TABLA", 
                      #   class = "dwnld-button",
                      #   style = button_style)
                      )
                    )
                  )
                )
              )
            ),
          fluidRow(
            column(12,
                   #withSpinner(
                   reactableOutput("tabla_region", height = 530)#, hide.ui = FALSE)
                   )
            )
          )
        )
      ),
    tabPanel( ## T4 Metodología----
      "METODOLOGÍA", icon = icon("flask"),
      fluidRow( ### Metodología ---- 
        class = "frow-bottom",    
        style = "padding-bottom:50px",
        column(12, style = "padding-top:20px",
          fluidRow(
            column(12,
                   h3("METODOLOGÍA"))
            ),
          fluidRow(
            column(12, style = "padding:15px; columns: 280px 2; column-fill:balance;",
              h4("Definición del Índice del Desarrollo Social de Mujeres y Hombres"),
                h5("¿Qué es?"),
                  p(m_q_es, class = "metodologia"),
                h5("¿Qué mide?"),
                  p(m_q_mide, class = "metodologia"),
                h5("¿Cuáles son las características?"),
                  p(m_caracteristicas, class = "metodologia"),
                h5("Modelo del IDSMH"),
                  p(m_modelo, class = "metodologia"),
                  img(src='idsmh_arbol.jpeg', align = "center", class = "met-images"),
                  p(m_educacion, class = "metodologia"),
                  img(src='educacion.jpeg', align = "center", class = "met-images"),
                  p(m_salud, class = "metodologia"),
                  img(src='salud.jpeg', align = "center", class = "met-images"),
                  p(m_autonomia, class = "metodologia"),
                  img(src='autonomia.jpeg', align = "center", class = "met-images"),
                  p(m_oportunidades, class = "metodologia"),
                  img(src='oportunidades.jpeg', align = "center", class = "met-images"),
                h5("Consideraciones técnicas"),
                  p(m_consideraciones, class = "metodologia"),
                  h6("(i) Cálculo del factor de ponderación de los países de referencia"),
                    p(m_calculo1, class = "metodologia"),
                    p("Indicador Global de Brecha de Género (IGBG)", style = "text-align:center"),
                    reactable(met_tabla1, class = "met-table",
                              sortable = FALSE, pagination = FALSE,
                              striped = TRUE, compact = TRUE,
                              fullWidth = TRUE, defaultColDef = colDef(minWidth = 50)),
                    p(m_calculo2, class = "metodologia"),
                    p("Tabla de conversiones entre los indicadores del IDSMHRP y el IGBG", 
                      style = "text-align:center"),
                    reactable(met_tabla2, class = "met-table",
                              sortable = FALSE, pagination = FALSE,
                              striped = TRUE, compact = TRUE,
                              fullWidth = TRUE, defaultColDef = colDef(minWidth = 20),
                              columns = list(
                                'DIMENSIÓN DEL INDICADOR' = colDef(maxWidth = 85),
                                'CÓDIGO DEL INDICADOR' = colDef(maxWidth = 70),
                                'INDICADOR IGBG' = colDef(maxWidth = 70)
                                )),
                      p(m_calculo3, class = "metodologia"),
                    img(src='ecuacion.png', align = "center", class = "met-images", style = "width:50%"),
                    p(m_calculo4, class = "metodologia"),
                  h6("(ii) Aplicación de los ponderadores al indicador del país objetivo"),
                    p(m_aplicacion1, class = "metodologia"),
                    p("Estandarización de los indicadores de los países de referencia", 
                      style = "text-align:center"),
                    img(src='estandarizacion.png', align = "center", class = "met-images"),
              br(),
                    p("Aplicación de los ponderadores al indicador del país objetivo.",
                      style = "text-align:center"),
                    img(src='ponderacion.png', align = "center", class = "met-images"),
                    p(m_aplicacion2, class = "metodologia")
              )
            ),
          fluidRow(
            column(12,
                   h4("Mediciones y año de las variables por país"),
                   tableOutput("libro_kable"))
          )
                # column(8, ### Resultados generales----
                #        fluidRow(
                #          column(12, h3("RESULTADOS GENERALES"))
                #        ),
                #        fluidRow(
                #          column(12, p(txt_kable))
                #        ),
                #        fluidRow(#height = 30, 
                #          column(2, ""),
                #          column(8, 
                #                 style = "heigth: 30px; background-image: linear-gradient(to right, #FEFFD9FF , #41B7C4FF); box-sizing: border-box; margin-bottom: 20px;",
                #                 fluidRow(
                #                   column(6,
                #                          "Desarrollo social bajo", align = "left",
                #                          style = "padding: 6px; font-size: 12px; margin: 0px"),
                #                   column(6,
                #                          "Desarrollo social alto", align = "right",
                #                          style = "padding: 6px; font-size: 12px; margin: 0px"))
                #          ),
                #          column(2, "")),
                #        fluidRow(
                #          column(12, txt_kable2)
                #        ),
                #        fluidRow(
                #          column(12, style = "margin-top: 20px;",
                #                 tableOutput("intro_kable"))
                #        ),
                #        fluidRow(
                #          column(12, p("intro_kable"))
                #          )
                #        )
                )
        )
      )
    )
  )

# 2 Shiny Server ###################################

server <- function(input, output, session) {

  ## O Download publication----
  output$downloadpublication <- downloadHandler(
    filename = "prueba.pdf",
    content = function(file) {
      file.copy("www/prueba.pdf", file)
    }
  )
  
  
  ## O Mapa----
  output$mapa <- renderLeaflet({ 
    
    ### Inputs mapa----
    nombre <- str_trim(input$indicador_mapa)
    index <- diccionario[diccionario$nombre==nombre,]$codigo
    unid <- diccionario[diccionario$nombre==nombre,]$unidad
    unid_short <- diccionario[diccionario$nombre==nombre,]$unidad_short
    ord_inv <- diccionario[diccionario$nombre==nombre,]$orden_inverso
    
    df_indicador <- geo_datos %>% filter(nivel == "R") %>% 
      select(pais:nivel, index)
    df_indicador_h <- df_indicador %>% filter(sexo == "h") 
    df_indicador_m <- df_indicador %>% filter(sexo == "m") 
    df_indicador_b <- df_indicador %>% filter(sexo == "brecha") 
    
    
    ### Labels tooltip hombres----
    tooltip_h <- sprintf( 
      '<b>%s</b> - %s<br/>%s<br/>%s: %g %s',
      df_indicador_h$region, df_indicador_h$pais,
      nombre,
      "Hombres", round(pull(df_indicador_h,index), 1),
      unid_short)%>%
      lapply(htmltools::HTML)
    
    ### Labels tooltip mujeres----
    tooltip_m <- sprintf(
      '<b>%s</b> - %s<br/>%s<br/>%s: %g %s',
      df_indicador_m$region, df_indicador_m$pais,
      nombre,
      "Mujeres", round(pull(df_indicador_m,index), 1),
      unid_short)%>%
      lapply(htmltools::HTML)
    
    ### Labels para tooltip brecha----
    tooltip_b <- sprintf(
      '<b>%s</b> - %s<br/>%s<br/>%s:<br/>%g %s',
      df_indicador_b$region, df_indicador_b$pais,
      nombre,
      "Brecha (hombres menos mujeres)",
      round(pull(df_indicador_b,index), 1),
      ifelse(unid_short == "%", "pp", unid_short))%>%
      lapply(htmltools::HTML)
    
    ### Paletas----
    
    if (unid == "Puntaje") {
      palette_seq <- colorBin(
        palette_puntaje_import,
        bins = c(0,35,45,55,65,75,85,100),
        domain = c(0, 100))
    } else {
      palette_seq <- colorNumeric(
        palette_seq_import,
        reverse = ifelse(ord_inv == TRUE, TRUE, FALSE),
        domain = c(
          if (unid == "Variación porcentual") {
            min(
              min(pull(df_indicador_h, index), na.rm = TRUE),
              min(pull(df_indicador_m, index), na.rm = TRUE))
            } else {
              0
              },
          if (index == "sb2" | index == "oc2") {
            max(
              max(pull(df_indicador_h, index), na.rm = TRUE),
              max(pull(df_indicador_m, index), na.rm = TRUE))
            } else {
              if (unid == "Porcentaje") {
                100
                } else {
                  max(
                    max(pull(df_indicador_h, index), na.rm = TRUE),
                    max(pull(df_indicador_m, index), na.rm = TRUE))
                }
              }
          )
        )
      }

    palette_div <- colorNumeric( # paleta para brecha -sirve para poner el blanco en cero-
      # palette_div_import,
      c("#9e68ef", "#ffffff", "#1dc37c"), 
      reverse = FALSE,
      domain = c(-max(c(abs(min(pull(df_indicador_b,index), na.rm = TRUE)), 
                        abs(max(pull(df_indicador_b,index), na.rm = TRUE)))),
                 max(c(abs(min(pull(df_indicador_b,index), na.rm = TRUE)), 
                       abs(max(pull(df_indicador_b,index), na.rm = TRUE))))))
    
    ### Option labels----
    opciones_label <- labelOptions(
      className = "tooltip-mapa")
    ### Mapa----
    map <- map %>% 
      addPolygons(data = df_indicador_h,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_seq(pull(df_indicador_h, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "hombres"),
                  label = tooltip_h,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Hombres") %>% 
      addPolygons(data = df_indicador_m,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_seq(pull(df_indicador_m, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "mujeres"),
                  label = tooltip_m,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Mujeres") %>% 
      addPolygons(data = df_indicador_b,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_div(pull(df_indicador_b, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "brecha"),
                  label = tooltip_b,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Brecha") %>% 
      addLegend(
        "bottomleft", 
        pal = palette_seq, 
        values = 
          c(pull(df_indicador_h, index), pull(df_indicador_m, index)),
        title = nombre,
        # labFormat = labelFormat(prefix = "$"),
        opacity = 1,
        group = "Mujeres",
        className = "info legend Mujeres"
      ) %>% 
      addLegend(
         "bottomleft",
         pal = palette_seq,
         values =
           c(pull(df_indicador_h, index),pull(df_indicador_m, index)),
         title = nombre,
         # labFormat = labelFormat(prefix = "$"),
         opacity = 1,
         group = "Hombres",
         className = "info legend Hombres"
         ) %>%
      addLegend(
        "bottomleft", 
        pal = palette_div, 
        values = c(pull(df_indicador_b, index), -pull(df_indicador_b, index)),
        title = paste(nombre, "<br>(diferencia a favor de<br><span style='color: #9e68ef'>mujeres</span> o de <span style='color: #1dc37c'>hombres</span>)"),
        # labFormat = labelFormat(prefix = "$"),
        opacity = 1,
        group = "Brecha",
        className = "info legend Brecha"
        ) %>% 
      addLayersControl(
        baseGroups = c("Mujeres", "Hombres", "Brecha"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      htmlwidgets::onRender("
        function(el, x) {
           var updateLegend = function () {
              var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
  
              document.querySelectorAll('.legend').forEach(a => a.hidden=true);
              document.querySelectorAll('.legend').forEach(l => {
                 if (l.classList.contains(selectedGroup)) l.hidden=false;
              });
           };
           updateLegend();
           this.on('baselayerchange', el => updateLegend());
        }"
      )
    
    map
    
  })
  

  
  ## O Scatter ordenado----
  output$disp_ordenado <- renderPlotly({ 
    #nombre <- input$indicador_mapa # Descomentar si se quiere usar el selector del mapa
    nombre <- str_trim(input$indicador_scatter_ord) # Selector del scatter plot
    index <- diccionario[diccionario$nombre==nombre,]$codigo
    print(paste(index,' ',nombre))
    option <- input$orden_scatter_ord
    print(option)
    if(option == 1){sex = "h"}
    else if(option == 2){sex = "m"}
    
    sexo_letras <- ifelse(sex == "h", "Hombres", "Mujeres")
    unidad_index <- diccionario[diccionario$nombre==nombre,]$unidad
    indicador_index <- diccionario[diccionario$nombre==nombre,]$indicadores
    
    rankeados <- datos%>%
      ungroup() %>% 
      select(pais:nivel, index) %>%  # elegir indicador (debe ser reactive)
      filter(sexo == sex) %>% 
      filter(nivel == "R") %>% 
      arrange(desc(get(index))) %>% 
      mutate(ranking = 1:n())  
    
    error_bar <- datos%>%
      ungroup() %>% 
      select(pais:nivel, index) %>%  
      filter(sexo == 'brecha') %>% 
      filter(nivel == "R") %>% 
      arrange(desc(get(index))) %>% 
      mutate(ranking = 1:n()) %>% 
      rename('brecha' = index)
    
    rankeados <- merge(rankeados, error_bar[c('iso_3166_2','brecha')], by = "iso_3166_2")
    if (option == 1){
      rankeados['brecha'] <- -rankeados['brecha']
    }
    
     
    fig <- plot_ly( ### Plot----
      data = rankeados, 
      x = ~ranking, y = as.formula(paste0('~', index)),
      color = ~pais, colors = colores,
      type = "scatter", mode ="markers",
      marker = list(size = 7),
      error_y = ~list(array = brecha, symmetric = FALSE, thickness = 1, width = 7),
      hovertemplate = paste(
        '<span style = "color:white"><b>', paste0(pull(rankeados['region']), '</b>,'),
        # '<br>', pull(rankeados['pais']), 
        'puesto %{x} de', n_subnacionales, 'en', sexo_letras,
        '<br>', nombre,
        '<br>', unidad_index,
        '<br> ●', ifelse(sexo_letras == "Hombres", "Hombres", "Mujeres"), round(pull(rankeados[index]), 1), # linea de prueba para ver el cambio del radio button
        paste0('<br> ⟘ ', ifelse(sexo_letras == "Hombres", "Mujeres", "Hombres")), round(pull(rankeados[index])+ pull(rankeados['brecha']), 1),
        '<br>  Brecha', round(pull(rankeados['brecha']), 1), # Falta alinear
        '<br><i>', str_wrap(indicador_index, width = 80), '</i></span>',
        
        # '<br><span style="color: #8AC468">', pull(tmpdata_['anio']), ':', round(pull(tmpdata_[,c(index)]), 2), '<span>',
        '<extra>', '<b>', pull(rankeados['pais']), '</b>', '</extra>'),
      hoverlabel = list(
        bordercolor = "white")
      ) %>%
      layout(
        font = pl_font,
        legend = list(
          orientation = "h", 
          x = 0.5, y = -0.42, 
          xanchor = "center", yanchor = "top",
          font = pl_legend),
        annotations = list(
          showarrow = FALSE,
          font = pl_legend,
          x = 0.5,
          y = -0.4,
          text = "Haz doble clic para aislar un país, o un solo clic para ocultarlo y mostrarlo:",
          xref = "paper",
          yref = "paper"),
        yaxis = list(
          title = list(
            text = diccionario[diccionario$nombre==nombre,]$unidad,
            font = pl_yaxis_scatter_ord),
          showline = FALSE),
        xaxis = list(
          title = list(
            standoff = 5,
            text = "Ranking", 
            font = pl_axis_title),
          range = c(-2, 195),
          tickmode = "array",
          tickvals = c( 1, seq(10, 190, by = 10)),
          # rangemode = "nonnegative",
          showline = TRUE,
          showgrid = FALSE,
          zeroline = FALSE),
        margin = list(#l = 50, r = 50,  # p = 50,
          b = 10, t = 40
        )
        ) %>% 
      config(modeBarButtonsToRemove = c('hoverClosestGl2d','select', 'autoScale', 
                                        'hoverCompareCartesian', 'pan', 'lasso2d',
                                        'hoverClosestCartesian'),
             displaylogo = FALSE)
    
    fig
  })
  
  
  
  ## O Scatters hombre, mujer y brecha----
  output$disp_hm <- renderPlotly({  
    nombre1 <- str_trim(input$indicador_1) # Indicador 1
    index1 <- diccionario[diccionario$nombre==nombre1,]$codigo
    unid_short1 <- diccionario[diccionario$nombre==nombre1,]$unidad_short
    ind_index1 <- diccionario[diccionario$nombre==nombre1,]$indicadores
    
    nombre2 <- str_trim(input$indicador_2) # Indicador 2
    index2 <- diccionario[diccionario$nombre==nombre2,]$codigo
    unid_short2 <- diccionario[diccionario$nombre==nombre2,]$unidad_short
    ind_index2 <- diccionario[diccionario$nombre==nombre2,]$indicadores
    
    annotations = list( # Títulos de subplots y nota al pie
      list( 
        x = 0.15, y = 1, text = "Hombres", 
        font = list(family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
                    color = central_blue),
        xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.5, y = 1, text = "Mujeres",  
        font = list(family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
                    color = central_blue),
        xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.85, y = 1, text = "Brecha (hombres menos mujeres)",
        font = list(family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
                    color = central_blue),
        xref = "paper", yref = "paper", xanchor = "center",  yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.01, y = -0.27, 
        align = "left",
        text = paste0(
          nombre1, ": ", ind_index1, " - ", unid_short1, "<br>",
          nombre2, ": ", ind_index2, " - ", unid_short2),
        font = list(family = "Calibre, Helvetica, Arial, Sans-Serif",
                    color = central_bold,
                    size = 8.5),
        xref = "paper", yref = "paper", 
        xanchor = "left",  yanchor = "bottom",  
        showarrow = FALSE 
      ))
    
    a_ <- datos %>%  
      # filter(sexo %in% c("h", "m")) %>% 
      mutate_at("pais", as.factor) %>% 
      filter(nivel == "R") %>%
      select(pais:nivel, index1, index2)
    
      # group_by(sexo) %>%
      
    map(1:length(unique(a_$sexo)),
        function(h) {
          df1 <- filter(a_, sexo == unique(a_$sexo)[h])
          p <- plot_ly(
            df1, 
            x = as.formula(paste0('~', index1)), 
            y = as.formula(paste0('~', index2)), 
            color = ~pais, colors = colores,
            type = "scatter", mode = "markers",
            showlegend = (unique(a_$sexo)[h] == "h"),
            legendgroup = ~pais,
            hovertemplate = paste0(
              "<span style = 'color:white'><b>", pull(df1['region']), "</b><br>",
              ifelse(pull(df1['sexo']) == "h", "Hombres", ifelse(pull(df1['sexo']) == "m", "Mujeres", "Brecha (hombres menos mujeres)")), "<br>",
              nombre1, ": ", "<b>", format(round(pull(df1[as.character(index1)]), 1), nsmall = 1), " ", unid_short1, "</b><br>",
              nombre2, ": ", "<b>", format(round(pull(df1[as.character(index2)]), 1), nsmall = 1), " ", unid_short2, "</b><br>",
              "<br>",
              # "<div style='background-color:aliceblue;padding:25px;'>",
              # "<span style='font-size:0.75em;line-height:0;display:inline-block'>",
              "<i>",
              # "Mediciones:", "<br>",
              # "-", str_wrap(ind_index1, width = 60), "<br>",
              # "-", str_wrap(ind_index2, width = 60), "</i></span>",
              # # "</div>",
              "<extra><b>", pull(df1['pais']), "</b></extra>"),
            hoverlabel = list(
              bordercolor = "white")
            ) %>%
            layout(
              font = pl_font,
              yaxis = list(
                title = list(
                  text = nombre2,
                  font = pl_yaxis_scatter_ord,
                  standoff = 10
                )
              ),
              xaxis = list(
                title = list(
                  text = nombre1,
                  font = pl_yaxis_scatter_ord
                )
              ),
              legend = list(
                orientation = "h", y = -0.4, x = 0.5,
                xanchor = "center", yanchor = "bottom",
                font = pl_legend))
          assign(paste0("plt", h), p, envir = .GlobalEnv)
        })
    subplot(subplot(plt2, plt1, 
                    shareY = T, shareX = T,
                    margin = 0.01),
            plt3,  
            widths = c(.66, .33),
            titleX = T, titleY = T,
            margin = 0.025) %>% 
      layout(annotations = annotations,
             margin = list( l = 0, r = 0, b = 80, t = 0
                            )
             ) %>% 
      config(
        modeBarButtonsToRemove = c('hoverClosestGl2d','select', 'autoScale',
                                   'hoverCompareCartesian', 'pan', 'lasso2d', 
                                   'hoverClosestCartesian'),
        displaylogo = FALSE)
    })

  
  
  ## O Dumbell----
  output$dumbell <- renderPlotly({ 
    # reg <- input$region_dumbell
    #reg <- input$subregion_dumbell
    #print(region)
    nomb_reg <- input$region_dumbell
    reg <- datos[datos$region_input==nomb_reg,]$iso_3166_2

    recode_var <- setNames(diccionario$nombre, diccionario$codigo)
    
    datos_barras <- datos %>% 
      ungroup() %>% 
      select(pais:nivel) %>% 
      bind_cols(ungroup(datos)[,nchar(colnames(datos)) == 2]) %>% 
      filter(
        sexo %in% c("h", "m"),
        iso_3166_2 == reg) %>% 
      pivot_longer(ea:oc, names_to = "var", values_to = "valor") %>% 
      pivot_wider(
        names_from = sexo,
        values_from = valor) %>% 
      mutate(
        nombre_var = recode(var, !!!recode_var)#,
        # nombre_dim = case_when( # Esto funciona localmente pero no en deployment
        #   substr(datos_barras$var, 1, 1) == "e" ~ "EDUCACIÓN",
        #   substr(datos_barras$var, 1, 1) == "s" ~ "SALUD",
        #   substr(datos_barras$var, 1, 1) == "a" ~ "AUTONOMÍA",
        #   TRUE ~ "OPORTUNIDAD"
          # )
    )
    

    fig <- plot_ly(datos_barras, color = I("gray80"))
    fig <- fig %>% 
      add_segments(
        x = ~m, xend = ~h, y = ~nombre_var, yend = ~nombre_var,
        showlegend = FALSE, color = I("gray80")
        )
    fig <- fig %>% 
      add_markers(
        x = ~m, y = ~nombre_var,
        showlegend = TRUE,
        name = "Mujeres", 
        color = I(col_mujeres), 
        marker = list(
          symbol = 'diamond',
          size = 10),
        hovertemplate = paste0(
          "<span style='color:white'>Dimensión: ", datos_barras$nombre_dim, "<br>",
          "Factor: ", datos_barras$nombre_var, "<br>",
          "Puntaje: <b>%{x:.1f}</b><br></span>",
          "<extra><b>MUJERES</b></extra>"),
        hoverlabel = list(
          bordercolor = "white")
        )
    fig <- fig %>% 
      add_markers(
        x = ~h, y = ~nombre_var, 
        showlegend = TRUE,
        name = "Hombres", 
        color = I(col_hombres), 
        marker = list(symbol = 'diamond',
        size = 10),
        hovertemplate = paste0(
          "<span style='color:white'>Dimensión: ", datos_barras$nombre_dim, "<br>",
          "Factor: ", datos_barras$nombre_var, "<br>",
          "Puntaje: <b>%{x:.1f}</b><br></span>",
          "<extra><b>HOMBRES</b></extra>"),
        hoverlabel = list(
          bordercolor = "white")
        )
    fig <- fig %>% layout(
      title = list( 
        text = ifelse(datos_barras[datos_barras$iso_3166_2==reg,]$nivel == "R",
                      paste0(datos_barras[datos_barras$iso_3166_2==reg,]$region, ", ", 
                             datos_barras[datos_barras$iso_3166_2==reg,]$pais),
                      datos_barras[datos_barras$iso_3166_2==reg,]$region),
        font = pl_title,
        x = 0.5, y = 0.99, yanchor = "top", 
        xref = "paper", yref = "container"), # paste(region, pais, sep = ", ")
      legend = list(font = pl_legend,
                    orientation = "h",
                    x = 0.5, xanchor = "center",
                    y = 1.1, yanchor = "top"),
      paper_bgcolor = cool_neutral,
      plot_bgcolor = cool_neutral,
      font = pl_font,
      xaxis = list(title = list(text = "Puntaje", # "Puntaje", 
                                font = pl_axis_title),
                   range = list(-5, 110)),
      yaxis = list(
        title = list(text = "", # "Factor",
                     standoff = 5,
                     font = pl_axis_title),
        categoryorder = "array",
        categoryarray = rev(dput(datos_barras$nombre_var)),
        tickmode = "array",
        tickvals = dput(datos_barras$nombre_var),
        ticktext = c(
          "<span style='color:#524aa6'>PRIMARIA</span>",
           "<span style='color:#524aa6'>SECUNDARIA</span>",
           "<span style='color:#524aa6'>LOGRO EDUCATIVO</span>",
           "<span style='color:#524aa6'>ACCESO</span>",
           "<span style='color:#008df2'>MORBILIDAD</span>",
           "<span style='color:#008df2'>CUIDADOS BÁSICOS</span>",
           "<span style='color:#ef3d01'>ECONÓMICA</span>",
           "<span style='color:#ef3d01'>FÍSICA</span>",
           "<span style='color:#ef3d01'>TOMA DE DECISIONES</span>",
           "<span style='color:#00bcb0'>ACCESO A EDUC. SUPERIOR</span>",
           "<span style='color:#00bcb0'>EMPLEO</span>",
           "<span style='color:#00bcb0'>GESTIÓN Y TIEMPO</span>"
          ),
        tickfont = list(size = 10)
        ),
      margin = list( l = 40, r = 40, b = 0, t = 80, pad = 4
      )
      ) %>% 
      config(modeBarButtonsToRemove = c('hoverClosestGl2d','select', 'autoScale', 
                                        'hoverCompareCartesian', 'pan', 'lasso2d',
                                        'hoverClosestCartesian', 'zoom', 'zoomIn', 
                                        'zoomOut'),
             displaylogo = FALSE)
    
    fig
    
  })

  
  ## O Reactable pais----
  output$tabla_pais <- renderReactable({ 
    nombre <- str_trim(input$indicador_tabla_pais) ### Inputs----
    # Indexes
    indexes <- c()
    id_index <- c()
    unidades <- c()
    for (n in nombre){
      index <- diccionario[diccionario$nombre==n,]$codigo #codigo del indicador
      id_index <- c(id_index,paste0(index,"_h"),paste0(index,"_m")) #indicador name for both male and female
      indexes <- c(indexes, index)
      
      unidad <- diccionario[diccionario$nombre==n,]$unidad
      unidades <- c(unidades, unidad)
    }
    print("pais")
    print(indexes)
    print(indexes[1])
    print(length(indexes))
    print(id_index)
  
    if (length(indexes)==0){
      indexes <- c('tot','e','s','a','o')
      id_index <- c('tot_h', 'tot_m', 'e_h', 'e_m', 's_h', 's_m', 'a_h', 'a_m', 'o_h', 'o_m')
      unidades <- c('Puntaje', 'Puntaje', 'Puntaje', 'Puntaje', 'Puntaje')
    }
    
    if (length(indexes)==1){
      data_pais <- datos %>%  ## Data----
      filter(nivel == "P") %>%
        select(all_of(c('pais', 'sexo',indexes)))  %>% 
        pivot_wider(names_from = sexo, 
                    values_from = all_of(indexes) ) %>% 
        select(!ends_with("brecha", ignore.case = TRUE, vars = NULL)) %>%  
        rename(!!paste0(indexes[1],'_h') := "h",
               !!paste0(indexes[1],'_m') := "m") %>%
        
        relocate(all_of(c('pais', id_index))) %>% 
        arrange(desc(.[2])) 
    } else{
    data_pais <- datos %>%  ## Data----
      filter(nivel == "P") %>%
      select(all_of(c('pais', 'sexo',indexes)))  %>% 
      # Dividimos cada variable seleccionada en dos columnas h y m (en realidad 3 con brecha):
      pivot_wider(names_from = sexo, 
                  values_from = all_of(indexes) ) %>% 
      # Eliminamos la columna que hace referencia a la brecha:
      select(!ends_with("brecha", ignore.case = TRUE, vars = NULL)) %>%  
      # Relocalizamos columnas (el orden debería ser según diccionario$orden):
      relocate(all_of(c('pais', id_index))) %>% 
      # Reordenamos filas según la primera columna con datos (la 2da):
      arrange(desc(.[2])) 
    }
    
    # tmp func
    bar_style2 <- function(width = 1, fill = col_hombres, height = "75%", 
                           color = central_bold, datatmp=data_pais, idx=indexes) {
      
      print('inside func')
      print(idx)
      
      val <- width/max(max(pull(datatmp[,c(idx)])),
                       max(pull(datatmp[,c(paste0(substr(idx,1,nchar(idx)-2),"_m"))])))
      print(val)
      position <- paste0(val * 100, "%")
      image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, 
                       position)
      list(
        backgroundImage = image,
        backgroundSize = paste("100%", height),
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        marginLeft = "0.75rem",
        marginRight = "0.25rem",
        textAlign = "Left",
        fontSize = "1.3rem",
        # backgroundColor = "#00bfc4", # toda la celda
        color = color
      )
    }
    
    coldef_fun <- function(colnm, idxt=indexes, und=unidades) {
      print('inside colDEF')
      print(colnm)
      print(idxt)
      print(und)
      if(grepl('sa2_h', colnm) | grepl('sa2_m', colnm)){
        if(grepl('sa2_h', colnm)){
          colDef(
            name = "Hombres",
            cell = function(value) { # Estilo de barra condicional: aquí el parámetro es "cell" y no "style" como en los otros
              label <- paste0(round(value * 100), "%")
              bar_chart_pos_neg(label, value)},
            align = "center"
          )
        }
        #if(grepl('sa2_m', colnm)){
        else{
          colDef(
            name = "Mujeres",
            cell = function(value) { # Estilo de barra condicional: en este caso el parámetro es "cell" y no "style" como en los otros
              label <- paste0(round(value * 100), "%")
              bar_chart_pos_neg(label, value)},
            align = "center"
          )
        }
      }else{
        if(grepl('_h', colnm)){
          unid <- und[match(substr(colnm,1,nchar(colnm)-2),idxt)]
          print('inside if')
          print(unid)
          if (unid=='Puntaje' | unid=='Porcentaje'){
            if(grepl('sb2_h', colnm)){
              colDef(
                name = "Hombres", 
                format = colFormat(digits = 1), 
                cell = function(value) {
                  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                      tippy(value, value/100))
                },
                style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                  bar_style(width = value / 100)
                })
            }else{
              colDef(
                name = "Hombres", 
                format = colFormat(digits = 1), 
                style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                  bar_style(width = value / 100)
                })
            }
          } else {
            print('inside else')
                colDef(
                  name = "Hombres", 
                  format = colFormat(digits = 1), 
                  style = function(value) { 
                    bar_style2(width = value, fill = col_hombres, idx=colnm)
                  })
          }
        }
        #if(grepl('_m', colnm)){
        else{
          unid <- und[match(substr(colnm,1,nchar(colnm)-2),idxt)]
          if (unid=='Puntaje' | unid=='Porcentaje'){
                colDef(
                  name = "Mujeres", 
                  format = colFormat(digits = 1),
                  style = function(value) {
                    bar_style(width = value / 100, fill = col_mujeres)
                  })
          } else {
                colDef(
                  name = "Mujeres", 
                  format = colFormat(digits = 1),
                  style = function(value) {
                    bar_style2(width = value, fill = col_mujeres, idx=colnm)
                  })
          }
        }
      }
    }
    
    
    ## Dynamic columns----
    ccols <- list()
    name_ccols <- list()
    for (i in 1:length(indexes)){
      if(grepl('sa2', indexes[i])){
        if(grepl('_h', id_index[2*i-1])){
          ccols <- c(  #append(
            ccols,
            list(
              colDef(
                name = "Hombres",
                cell = function(value) { # Estilo de barra condicional: en este caso el parámetro es "cell" y no "style" como en los otros
                  label <- paste0(round(value * 100), "%")
                  bar_chart_pos_neg(label, value)},
                align = "center"
                )
              )
            )
          }
        if(grepl('_m', id_index[2*i])){
          ccols <- c( #append(
            ccols,
            list(
              colDef(
                name = "Mujeres",
                cell = function(value) {
                  label <- paste0(round(value * 100), "%")
                  bar_chart_pos_neg(label, value) # Aquí por definir falta un parámetro para color de hombres y mujeres
                  },
                align = "center",
                minWidth = 90
                )
              )
            )
          }
        }
      else{ # Note: sb2 not %
        if(grepl('_h', id_index[2*i-1])){
          if (unidades[i]=='Puntaje' | unidades[i]=='Porcentaje'){
            ccols <- c( #append(
              ccols,
              list(
                colDef(
                  name = "Hombres", # Jalar "Hombres" o "Mujeres" según si el column name termina en _h o en _m
                  format = colFormat(digits = 1), # Mantenerlo en todos los casos
                  cell = function(value) {
                        div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                            tippy(value, value))
                      },
                  style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                    bar_style(width = value / 100)
                  }))
            )
          } else {
            # maxtmp <- max(max(pull(data_pais_tmp[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais_tmp[,c(paste0(indexes[i],"_m"))])))
            # print('mmaxtmp')
            # print(maxtmp)
            ccols <- c( #append(
              ccols,
              list(
                colDef(
                  name = "Hombres", # Jalar "Hombres" o "Mujeres" según si el column name termina en _h o en _m
                  
                  cell = function(value) {
                    div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                        tippy(value, value))
                  },
                  format = colFormat(digits = 1), # Mantenerlo en todos los casos
                  style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                    #bar_style(width = value/max(max(pull(data_pais_tmp[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais_tmp[,c(paste0(indexes[i],"_m"))]))))
                    bar_style2(width = value, idx=indexes[i])
                  }))
            )
          }
        }
        if(grepl('_m', id_index[2*i])){
          if (unidades[i]=='Puntaje' | unidades[i]=='Porcentaje'){
            ccols <- c( #append(
              ccols,
              list(
                colDef(
                  name = "Mujeres", 
                  format = colFormat(digits = 1),
                  style = function(value) {
                    bar_style(width = value / 100, fill = col_mujeres)
                  }))
            )
          } else {
            # maxtmp <- max(max(pull(data_pais_tmp[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais_tmp[,c(paste0(indexes[i],"_m"))])))
            # print('mmaxtmp')
            # print(maxtmp)
            ccols <- c( #append(
              ccols,
              list(
                colDef(
                  name = "Mujeres", 
                  format = colFormat(digits = 1),
                  style = function(value) {
                    #bar_style(width = value/max(max(pull(data_pais[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais[,c(paste0(indexes[i],"_m"))]))))
                    #bar_style(width = value/max(max(pull(data_pais_tmp[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais_tmp[,c(paste0(indexes[i],"_m"))]))), fill = col_mujeres)
                    bar_style2(width = value, fill = col_mujeres, idx=indexes[i])
                  }))
            )
          }
        }
      }
      
      name_ccols <- c( #append(
        name_ccols, 
        list(
          colGroup(
            header = with_tooltip(
              diccionario$nombre[diccionario$codigo==indexes[i]],
              paste(diccionario$indicadores[diccionario$codigo==indexes[i]],
                    "-",
                    diccionario$unidad[diccionario$codigo==indexes[i]])),
            columns = c(paste0(indexes[i],"_h"), paste0(indexes[i],"_m")))
        )
      )
    }
    
    names(ccols) <- id_index
    print(name_ccols)
    #print('COLS')
    #print(ccols)
    print(data_pais)
    
    
    
    ## Table----
      reactable( # Configuración general de tabla
        data_pais,
        searchable = TRUE,
        borderless = TRUE,
        striped = TRUE,
        highlight = FALSE,
        style = list(
          fontSize = "1.5rem"),
        language = reactableLang(
          searchPlaceholder = "Filtra países",
          noData = "Sin resultados para la selección"),
        defaultColDef = colDef(
          minWidth = 80,
          maxWidth = 260,
          vAlign = "center",
          headerVAlign = "bottom",
          align = "left",
          headerClass = "header-tablas"
          ),
        columns = c(
          list( # Aquí se define cada columna
            pais = colDef( # La columna de país es fija (no varía con el input)
              name = "País",
              sticky = "left",
              minWidth = 170,
              align = "left",
              cell = function(value, index) {
                div(
                  class = "celdas-paises",
                  img(class = "bandera-pais", alt = paste(value, "-bandera"), 
                      src = ifelse(
                        value == "PERÚ",
                        "banderas/PERU.png",
                        ifelse(
                          value == "MÉXICO",
                          "banderas/MEXICO.png",
                          sprintf(
                            "banderas/%s.png", value)))
                      ),
                  span(class = "nombre-pais", value)
                  )}
              )),
          
          #ccols #dynamic list of columns
          setNames(map(names(data_pais)[2:length(names(data_pais))], 
                       ~ coldef_fun(.x)), names(data_pais[2:length(names(data_pais))]))
          
          ),
        columnGroups = name_ccols # Dynamic
        )
    })
  
  
  ## O Botón para descargar csv de data_pais ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("centrum-idsr-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_pais, file, row.names = FALSE)
    }
  )
  
  ## O Reactable region----
  ### Inputs----
  output$tabla_region <- renderReactable({ 
    nombre <- str_trim(input$indicador_tabla_region)
    # Indexes
    indexes <- c()
    id_index <- c()
    unidades <- c()
    for (n in nombre){
      print('nombre')
      print(n)
      index <- diccionario[diccionario$nombre==n,]$codigo #codigo del indicador
      id_index <- c(id_index,paste0(index,"_h"),paste0(index,"_m")) #indicador name for both male and female
      indexes <- c(indexes, index)
      
      unidad <- diccionario[diccionario$nombre==n,]$unidad
      unidades <- c(unidades, unidad)
    }
    
    
    if (length(indexes)==0){
      # Tabla por defecto
      indexes <- c('tot', 'e', 's', 'a', 'o')
      id_index <- c('tot_h', 'tot_m', 'e_h', 'e_m', 's_h', 's_m', 'a_h', 'a_m', 'o_h', 'o_m')
      unidades <- c('Puntaje', 'Puntaje', 'Puntaje', 'Puntaje', 'Puntaje')
    }
    
    
    
    ### Dynamic columns----
    ccols <- list()
    name_ccols <- list()
    for (i in 1:length(indexes)){
      print("for")
      print(i)
      print(indexes)
      print(id_index)
      print(unidades)
      
      if(grepl('sa2', indexes[i])){
        if(grepl('_h', id_index[2*i-1])){
          ccols <- append(ccols,
                          list(colDef(
                            name = "Hombres",
                            cell = function(value) { # Estilo de barra condicional: en este caso el parámetro es "cell" y no "style" como en los otros
                              label <- paste0(round(value * 100), "%")
                              bar_chart_pos_neg(label, value)},
                            align = "center"
                          ))
          )
        }
        if(grepl('_m', id_index[2*i])){
          ccols <- append(ccols,
                          list(colDef(
                            name = "Mujeres",
                            cell = function(value) {
                              label <- paste0(round(value * 100), "%")
                              bar_chart_pos_neg(label, value) # Aquí por definir falta un parámetro para color de hombres y mujeres
                            },
                            align = "center",
                            minWidth = 90
                          ))
          )
        }
      }
      
      else{
        if(grepl('_h', id_index[2*i-1])){
          if (unidades[i]=='Puntaje' | unidades[i]=='Porcentaje'){
            ccols <- append(ccols,
                            list(colDef(
                              name = "Hombres", # Jalar "Hombres" o "Mujeres" según si el column name termina en _h o en _m
                              format = colFormat(digits = 1), # Mantenerlo en todos los casos
                              style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                                bar_style(width = value / 100)
                              }))
            )
          } else {
            ccols <- append(ccols,
                            list(colDef(
                              name = "Hombres", # Jalar "Hombres" o "Mujeres" según si el column name termina en _h o en _m
                              format = colFormat(digits = 1), # Mantenerlo en todos los casos
                              style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                                bar_style(width = value/max(max(pull(data_region[,c(paste0(indexes[i],"_h"))])),max(pull(data_region[,c(paste0(indexes[i],"_m"))]))))
                              }))
            )
          }
        }
        if(grepl('_m', id_index[2*i])){
          if (unidades[i]=='Puntaje' | unidades[i]=='Porcentaje'){
            ccols <- append(ccols,
                            list(colDef(
                              name = "Mujeres", 
                              format = colFormat(digits = 1),
                              style = function(value) {
                                bar_style(width = value / 100, fill = col_mujeres)
                              }))
            )
          } else {
            ccols <- append(ccols,
                            list(colDef(
                              name = "Mujeres", 
                              format = colFormat(digits = 1),
                              style = function(value) {
                                #bar_style(width = value/max(max(pull(data_pais[,c(paste0(indexes[i],"_h"))])),max(pull(data_pais[,c(paste0(indexes[i],"_m"))]))))
                                bar_style(width = value/max(max(pull(data_region[,c(paste0(indexes[i],"_h"))])),max(pull(data_region[,c(paste0(indexes[i],"_m"))]))), fill = col_mujeres)
                              }))
            )
          }
        }
      }
      
      name_ccols <- append(name_ccols, list(
        colGroup(
          header = with_tooltip(
            diccionario$nombre[diccionario$codigo==indexes[i]],
            paste(diccionario$indicadores[diccionario$codigo==indexes[i]],
                  "-",
                  diccionario$unidad[diccionario$codigo==indexes[i]])),
          columns = c(paste0(indexes[i],"_h"), paste0(indexes[i],"_m")))
      )
      )
    }
    
    names(ccols) <- id_index
    
    
    if (length(indexes)==1){
      data_region <- datos %>% 
        filter(nivel == "R") %>% 
        select(all_of(c('pais', 'region', 'sexo',indexes)))  %>% 
        pivot_wider(names_from = sexo, 
                    values_from = all_of(indexes)) %>% 
        select(!ends_with("brecha", ignore.case = TRUE, vars = NULL)) %>%   
        rename(!!paste0(indexes[1],'_h') := "h",
               !!paste0(indexes[1],'_m') := "m") %>%
        relocate(all_of(c('pais', 'region', id_index))) %>% 
        arrange(desc(.[3])) 
    } else{
    ### Data----
    data_region <- datos %>% 
      filter(nivel == "R") %>% 
      select(all_of(c('pais', 'region', 'sexo',indexes)))  %>% 
      pivot_wider(names_from = sexo, 
                  values_from = all_of(indexes)) %>% 
      select(!ends_with("brecha", ignore.case = TRUE, vars = NULL)) %>%  
      relocate(all_of(c('pais', 'region', id_index))) %>% 
      arrange(desc(.[3])) 
    
    #print(dim(data_region))
    }
    
    
    # tmp func
    bar_style2 <- function(width = 1, fill = col_hombres, height = "75%", 
                           color = central_bold, datatmp=data_region, idx=indexes) {
      
      print('inside func reg')
      print(idx)
      
      val <- width/max(max(pull(datatmp[,c(idx)])),
                       max(pull(datatmp[,c(paste0(substr(idx,1,nchar(idx)-2),"_m"))])))
      print(val)
      position <- paste0(val * 100, "%")
      image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, 
                       position)
      list(
        backgroundImage = image,
        backgroundSize = paste("100%", height),
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        marginLeft = "0.75rem",
        marginRight = "0.25rem",
        textAlign = "Left",
        fontSize = "1.3rem",
        # backgroundColor = "#00bfc4", # toda la celda
        color = color
      )
    }

      
    
    coldef_fun <- function(colnm, idxt=indexes, und=unidades) {
      print('inside colDEF reg')
      print(colnm)
      print(idxt)
      print(und)
      if(grepl('sa2_h', colnm) | grepl('sa2_m', colnm)){
        if(grepl('sa2_h', colnm)){
          colDef(
            name = "Hombres",
            cell = function(value) { # Estilo de barra condicional: aquí el parámetro es "cell" y no "style" como en los otros
              label <- paste0(round(value * 100), "%")
              bar_chart_pos_neg(label, value)},
            align = "center"
          )
        }
        #if(grepl('sa2_m', colnm)){
        else{
          colDef(
            name = "Mujeres",
            cell = function(value) { # Estilo de barra condicional: en este caso el parámetro es "cell" y no "style" como en los otros
              label <- paste0(round(value * 100), "%")
              bar_chart_pos_neg(label, value)},
            align = "center"
          )
        }
      }else{
        if(grepl('_h', colnm)){
          unid <- und[match(substr(colnm,1,nchar(colnm)-2),idxt)]
          print('inside if')
          print(unid)
          if (unid=='Puntaje' | unid=='Porcentaje'){
            if(grepl('sb2_h', colnm)){
              colDef(
                name = "Hombres", 
                format = colFormat(digits = 1), 
                cell = function(value) {
                  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                      tippy(value, value/100))
                },
                style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                  bar_style(width = value / 100)
                })
            }else{
              colDef(
                name = "Hombres", 
                format = colFormat(digits = 1), 
                style = function(value) { # Estilo de barra condicional: ver issue #8 para ver opciones 
                  bar_style(width = value / 100)
                })
            }
          } else {
            print('inside else')
            colDef(
              name = "Hombres", 
              format = colFormat(digits = 1), 
              style = function(value) { 
                bar_style2(width = value, fill = col_hombres, idx=colnm)
              })
          }
        }
        #if(grepl('_m', colnm)){
        else{
          unid <- und[match(substr(colnm,1,nchar(colnm)-2),idxt)]
          if (unid=='Puntaje' | unid=='Porcentaje'){
            colDef(
              name = "Mujeres", 
              format = colFormat(digits = 1),
              style = function(value) {
                bar_style(width = value / 100, fill = col_mujeres)
              })
          } else {
            colDef(
              name = "Mujeres", 
              format = colFormat(digits = 1),
              style = function(value) {
                bar_style2(width = value, fill = col_mujeres, idx=colnm)
              })
          }
        }
      }
    }
    
    
    
    
    ### Table----
    reactable( 
      data_region,
      searchable = TRUE,
      borderless = TRUE,
      striped = TRUE,
      style = list(
        fontSize = "1.5rem"),
      language = reactableLang(
        searchPlaceholder = "Filtra países y regiones",
        noData = "Sin resultados para la selección"),
      defaultColDef = colDef(
        minWidth = 80,
        maxWidth = 260,
        vAlign = "center",
        headerVAlign = "bottom",
        align = "left",
        headerClass = "header-tablas"
      ),
      columns = c(
        list(
        pais = colDef(
          name = "",
          sticky = "left",
          minWidth = 40, maxWidth = 40,
          align = "left",
          cell = function(value, index) {
            div(class = "celdas-banderas",
                img(class = "bandera-pais", alt = paste(value, "-bandera"), 
                    src = ifelse(
                      value == "PERÚ",
                      "banderas/PERU.png",
                      ifelse(
                        value == "MÉXICO",
                        "banderas/MEXICO.png",
                        sprintf(
                          "banderas/%s.png", value)))
                ),
            )}
          ),
        region = colDef(
          name = "Territorio",
          sticky = "left",
          minWidth = 190,
          maxWidth = 200,
          align = "left"
          )),
        
        #ccols #dynamic list of columns
        setNames(map(names(data_region)[3:length(names(data_region))], 
                     ~ coldef_fun(.x)), names(data_region[3:length(names(data_region))]))
      ),
      columnGroups = name_ccols
    )
  })
  
  ## O Botón para descargar csv de data_region ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("centrum-idsr-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_region, file, row.names = FALSE)
    }
  )
  
  
  ## libro Kable----
  output$libro_kable <- function() {
    libro %>% 
      kable() %>%
      kable_styling(
        "striped",
        full_width = TRUE,
        font_size = 9) %>%
      scroll_box(width = "100%",
                 height = "400px")
  }
  
  # ## O Intro Kable----
  # output$intro_kable <- function() {
  #   intro_table %>%
  #     mutate(
  #       across(PERÚ_m:MÉXICO_h, 
  #              ~ format(round(.x, 1), nsmall = 1))) %>%
  #     select(-variable) %>% 
  #     kable("html", escape = F,
  #           col.names = c("Dimensión / Factor", rep(c("M", "H"), 8))) %>%
  #     kable_styling(
  #       full_width = TRUE, 
  #       font_size = 10,
  #       bootstrap_options = c("striped", "condensed", "responsive")) %>%
  #     add_indent(c(2, 6, 10, 14), level_of_indent = 1) %>% 
  #     add_indent(c(3:5, 7:9, 11:13, 15:17), level_of_indent = 2) %>% 
  #     add_header_above(c(
  #       " " = 1, 
  #       "PERÚ" = 2, # "🇵🇪" = 2, 
  #       "ARGENTINA" = 2, # "🇦🇷" = 2, 
  #       "BOLIVIA" = 2, # "🇧🇴" = 2, 
  #       "BRASIL" = 2, # "🇧🇷" = 2,
  #       "CHILE" = 2, # "🇨🇱" = 2, 
  #       "COLOMBIA" = 2, # "🇨🇴" = 2, 
  #       "ECUADOR" = 2, # "🇪🇨" = 2, 
  #       "MÉXICO" = 2 # "🇲🇽" = 2
  #       )) %>%
  #     column_spec(1, width = "190px") %>%
  #     column_spec(2, color = col_texto_kable, background = spec_color2(pull(intro_table[,3]), palette = pal_kable)) %>%
  #     column_spec(4, color = col_texto_kable, background = spec_color2(pull(intro_table[,5]), palette = pal_kable)) %>%
  #     column_spec(6, color = col_texto_kable, background = spec_color2(pull(intro_table[,7]), palette = pal_kable)) %>%
  #     column_spec(8, color = col_texto_kable, background = spec_color2(pull(intro_table[,9]), palette = pal_kable)) %>%
  #     column_spec(10, color = col_texto_kable, background = spec_color2(pull(intro_table[,11]), palette = pal_kable)) %>%
  #     column_spec(12, color = col_texto_kable, background = spec_color2(pull(intro_table[,13]), palette = pal_kable)) %>%
  #     column_spec(14, color = col_texto_kable, background = spec_color2(pull(intro_table[,15]), palette = pal_kable)) %>%
  #     column_spec(16, color = col_texto_kable, background = spec_color2(pull(intro_table[,17]), palette = pal_kable)) %>%
  #     column_spec(3, color = col_texto_kable, background = spec_color2(pull(intro_table[,4]), palette = pal_kable)) %>%
  #     column_spec(5, color = col_texto_kable, background = spec_color2(pull(intro_table[,6]), palette = pal_kable)) %>%
  #     column_spec(7, color = col_texto_kable, background = spec_color2(pull(intro_table[,8]), palette = pal_kable)) %>%
  #     column_spec(9, color = col_texto_kable, background = spec_color2(pull(intro_table[,10]), palette = pal_kable)) %>%
  #     column_spec(11, color = col_texto_kable, background = spec_color2(pull(intro_table[,12]), palette = pal_kable)) %>%
  #     column_spec(13, color = col_texto_kable, background = spec_color2(pull(intro_table[,14]), palette = pal_kable)) %>%
  #     column_spec(15, color = col_texto_kable, background = spec_color2(pull(intro_table[,16]), palette = pal_kable)) %>%
  #     column_spec(17, color = col_texto_kable, background = spec_color2(pull(intro_table[,18]), palette = pal_kable)) %>%
  #     scroll_box(width = "100%")
  # }
  
  ## List of dynamic regions----
  region_choices <- reactive({
    
    tmp <- datos[(datos$pais==input$region_dumbell),]
    tmp <- na.omit(tmp)
      
    choice <- unique(tmp$region)
    
    choice
  })
  
  ## Dynamic selector
  #observe({
  #  updateSelectInput(session = session, inputId = "subregion_dumbell", choices = region_choices())
  #  
  #})
  
  #outputOptions(output, {"mapa"}, suspendWhenHidden = FALSE)
  #outputOptions(output, "dumbell", suspendWhenHidden = FALSE)
  #outputOptions(output, "tabla_pais", suspendWhenHidden = FALSE)
  
}

# Run the application #########

shinyApp(ui = ui, server = server)

