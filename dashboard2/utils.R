# COLORES----
## Colores----
central_bold <- "#363661"
central_blue <- "#3B69FA"
central_blue_70 <- "#798BFE"
cool_neutral <- "#E8EBF0"
cool_neutral_40 <- "#F6F7F9"
col_hombres <- "#1dc37c"
col_mujeres <- "#9e68ef"
col_pos <- "#4599c0"
col_neg <- "#e93587"
colores <- c("#04cffb", "#27a7a0", "#fabe00", "#971510",
             "#008df2", "#ff791a", "#56cd47", "#e40269")
col_d3 <- "#524aa6" #educ
col_d4 <- "#008DF2"
col_d1 <- "#ef3d01"
col_d2 <- "#00bcb0"

col_texto_kable <- "Black"

# tmp
load("data/datos.RData") # cargar datos
load("data/geo_datos.RData")
load("data/diccionario.RData") # cargar diccionario
# load("data/intro_table.RData") # cargar intro_table
load("data/data_sun.RData") # cargar data para sunburst
load("data/libro.RData") # cargar intro_table


#Styling de botones
button_style <- 
  "color:#3B69FA;border-color:#3B69FA;border-width:2px;border-radius:10px;"

# FONTS de plots----
pl_font <- list(
  family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
  color = central_bold)
pl_title <- list(
  # family = "Calibre, Helvetica, Arial, Sans-Serif",
  color = central_blue
  # size = 22
  )
# pl_axis_label <- list(
#   #family = "iA Writer Duospace, Courier",
#   color = central_bold)
pl_axis_title <- list(
  #family = "iA Writer Duospace, Courier",
  color = central_bold)
pl_legend <- list(
  family = "Calibre, Helvetica, Arial, Sans-Serif",
  color = central_bold)
pl_yaxis_scatter_ord <- list(
  family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
  color = central_bold,
  size = 10)
pl_sunburst <- list(
  family = "Calibre, Arial, Sans-Serif",
  size = 20)

# FUNCIONES:----

## Download table button---
csvDownloadButton <- function(id, filename = "data.csv", label = "DESCARGA LA TABLA") {
  tags$button(
    tagList(icon("download"),
            label),
    style = button_style,
    class = "btn btn-default shiny-download-link dwnld-button shiny-bound-output",
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", 
                      id, 
                      filename)
    )
  }

## Tooltips en header de reactables----
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: solid; cursor: help",
            title = tooltip, value)
}

## Render a bar chart in the background of the cell----
bar_style <- function(width = 1, fill = col_hombres, height = "75%", 
                      color = "black") {
  position <- paste0(width * 100, "%")
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
    fontSize = "1.3rem",  # added for checking values of small bar in cells
    # backgroundColor = "#00bfc4", # toda la celda
    color = color
  )
}

bar_style3 <- function(width = 1, fill = col_hombres, height = "75%", 
                      color = "white", dftmp) {
  
  val <- width/max(max(pull(data_pais[,c(paste0(indexes[1],"_h"))])),max(pull(data_pais[,c(paste0(indexes[1],"_m"))])))
  position <- paste0(width * 100, "%")
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
    # backgroundColor = "#00bfc4", # toda la celda
    color = color
  )
}


## Render a bar chart with a label on the left----
bar_chart <- function(label, width = "100%", height = "2rem", fill = "#82758F", 
                      background = "#e1e1e1") {
  bar <- div(
    style = list(
      background = fill, 
      width = width, 
      height = height))
  chart <- div(
    style = list(
      flexGrow = 1, 
      marginLeft = "0rem", 
      background = background), 
    bar)
  div(style = list(display = "flex", 
                   alignItems = "center"), 
      format(round(label, 1), nsmall = 1), 
      chart)
}

## Render a bar chart with positive and negative values----
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "1rem",
                              pos_fill = col_pos, neg_fill = col_neg) {
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value) * 100, "%") # definir
  
  if (value < 0) {
    bar <- div(
      style = list(marginLeft = "0.5rem", background = neg_fill,
                   width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center", 
                   justifyContent = "flex-end"),
      label,
      bar
    )
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- div(
      style = list(marginRight = "0.5rem", background = pos_fill,
                   width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center"),
      bar,
      label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  div(style = list(display = "flex"), neg_chart, pos_chart)
}

## Palette for intro_kable y mapa----
pal_kable <- # Paleta intro kable secuencial
  paletteer_c("ggthemes::Blue-Green Sequential", 100)

palette_seq_import <- 
  ggthemes::tableau_div_gradient_pal(palette = "Red-Green-Gold Diverging")

palette_puntaje_import <- 
  ggthemes::tableau_div_gradient_pal(palette = "Red-Green-Gold Diverging")

palette_div_import <- "PRGn"
#  ggthemes::tableau_div_gradient_pal(palette = "Classic Orange-White-Blue")

spec_color2 <- function(x, alpha = 1, begin = 0, end = 1,
                        direction = 1, option = "D",
                        na_color = "#BBBBBB", scale_from = NULL,
                        palette = viridisLite::viridis(256, alpha, begin, end, 
                                                       direction, option)) {
  n <- length(palette)
  if (is.null(scale_from)) {
    x <- round(scales::rescale(x, c(1, n)))
  } else {
    x <- round(scales::rescale(x, to = c(1, n),
                               from = scale_from))
  }
  
  color_code <- palette[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

# OTROS:----
## Custom table container for DT intro_table ----
# sketch <- htmltools::withTags(table(
#   class = 'display',
#   thead(
#     tr(
#       th(rowspan = 2, 'IDSR'),
#       th(rowspan = 2, 'Dimensión'),
#       th(rowspan = 2, 'Factores'),
#       th(colspan = 2, 'PERÚ'),
#       th(colspan = 2, 'ARGENTINA'),
#       th(colspan = 2, 'BOLIVIA'),
#       th(colspan = 2, 'BRASIL'),
#       th(colspan = 2, 'CHILE'),
#       th(colspan = 2, 'COLOMBIA'),
#       th(colspan = 2, 'ECUADOR'), 
#       th(colspan = 2, 'MÉXICO')),
#     tr(
#       lapply(rep(c('M', 'H'), 8), th)
#     )
#   )
# ))

## N de territorios subnacionales----
n_subnacionales <- unique(datos$iso_3166_2[datos$nivel== "R"])%>% length() 


# TEXTOS----
## Intro----
txt_intro <- HTML("El Índice del Desarrollo Social de la Mujer y el Hombre en Países de América Latina 2022 (IDSMH) mide la situación actual del acceso a recursos por parte de mujeres y hombres. El índice abarca 191 territorios subnacionales de 8 países de América Latina. Esta presentación permite comparar los indicadores en unidades geográficas subnacionales, y permite también identificar las brechas en el desarrollo entre hombres y mujeres.")

## El Índice a----
txt_indice_a <- HTML("<p class = 'spaced'>EL IDSMH recopila indicadores de desarrollo social desagregados por sexo y por unidades territoriales subnacionales en ocho países de Latinoamérica. Los indicadores permiten ver el nivel de desarrollo y las brechas entre hombres y mujeres para cuatro dimensiones del desarrollo social, compuestas por tres factores en cada una de ellas. Haz click en las dimensiones para explorar los factores y variables que las componen.</p>")

## El Índice b----
txt_indice_b <- HTML("El </b>IDSMH</b> resume la información del conjunto de indicadores. Como tal, es una suma ponderada de sus cuatro dimensiones. Cada dimensión incluye los siguientes factores:  
  <ul>
  <li><span style='color: #524aa6;'>Educación</span>: Primaria, Secundaria, Logro educativo</li>
  <li><span style='color: #008DF2;'>Salud</span>: Acceso, Morbilidad, Cuidados básicos</li>
  <li><span style='color: #ef3d01;'>Autonomía</span>: Económica, Física, Toma de decisiones</li>
  <li><span style='color: #00bcb0;'>Oportunidades</span>: Acceso a educación superior y técnica, Empleo, Gestión y tiempo</li>
</ul>")
txt_indice_c <- HTML("El índice, las dimensiones y los factores son medidos a partir de un puntaje que va de 0 a 100, donde 100 representa el mayor nivel de desarrollo. Por su parte, cada variable tiene su propio indicador o unidad de medida, visible al posicionar el cursor sobre el nombre de cada variable del gráfico. No obstante, cada país realiza la medición según su propio criterio. Estos criterios, así como el año correspondiente al levantamiento de la información en cada país se puede encontrar en la sección de metodología.")

## Resumen ejecutivo----
txt_articulo <- HTML("<p class = 'spaced'>El estudio revela que la búsqueda de la igualdad entre mujeres y hombres, aún debe sostenerse con firmeza en América Latina ya que existe una brecha favorable a los hombres sobre las mujeres en todos los países. Sólo una región de un total de 191 regiones o estados evaluados (Michoacan de Ocampo en México) logró una brecha favorable a las mujeres (0.8%). La brecha promedio en América Latina es de 7% en favor de los hombres y llega a ampliarse por encima del 10% en Perú.</p>
  
<p class = 'spaced'>Por otro lado, la ventaja promedio en el índice de desarrollo social entre hombres y mujeres se incrementa si consideramos las diferencias al interior de las regiones o estados que componen estos países, registrándose una ventaja promedio en América Latina de 14,3% de hombres sobre mujeres, llegando a superar el 20% en países como Ecuador y Perú.</p>

<p class = 'spaced'>Los resultados evidencian que en salud y educación existen menores brechas y ventajas de hombres sobre mujeres; incluso existe un alto porcentaje de regiones donde la brecha es favorable a las mujeres. En Salud, la brecha es favorable a las mujeres en 148 regiones de un total de 191; mientras que en Educación la brecha es favorable a las mujeres en 107 regiones. Los resultados son totalmente contrapuestos en las dimensiones autonomía y oportunidades. A nivel de autonomía la brecha es absolutamente favorable a los hombres en todas las regiones; mientras que en la dimensión oportunidades la brecha es favorable a los hombres en 161 regiones.</p>  

<p class = 'spaced'>Los IDS más altos en mujeres los registran Argentina con 59,8 puntos, seguido de Brasil con 58,8 puntos y Colombia con 55,7 puntos; mientras que los resultados más bajos los registran Bolivia y Perú, con 42,3 y 40,2 puntos, respectivamente.</p>

<p class = 'spaced'>Los IDS más altos en hombres los registran también Argentina con 64,7 puntos, seguido de Brasil con 64,5 puntos y Colombia con 61,7 puntos; mientras que los resultados más bajos lo registran Bolivia y México, con 51 y 50,9 puntos, respectivamente.</p>

<p class = 'spaced'>A modo de conclusión, se puede inferir que se requieren equilibrar las políticas para reducir las diferencias de género. En salud y educación, las políticas han permitido incluso una ventaja de las mujeres sobre los hombres. Sin embargo, se debe mantener un equilibrio, de modo tal que no se generen mayores desigualdades o inclusive un deterioro de los IDS de los hombres. Por otro lado, las políticas deben ser más agresivas para reducir las elevadas brechas favorables a los hombres en autonomía y oportunidades. Sin lugar a dudas, las diferencias son preocupantes sobre todo en autonomía económica (por las diferencias en el ingreso, trabajo no remunerado y la dependencia económica), y la toma de decisiones; así como también, en cuanto a variables como el empleo (empleo adecuado, y subempleo) y la gestión del tiempo, (tiempo de trabajo remunerado y el género del empleador).")

txt_kable <- "El IDSMH, sus dimensiones y sus factores son indicadores cuyos puntajes representan niveles de desarrollo en un rango que va desde 0 hasta 100."

txt_kable2 <- "La tabla siguiente presenta los resultados generales, agregados para cada uno de los ocho países incluidos en el estudio y divididos entre hombres y mujeres. Las variables que componen cada uno de los factores aparecen al pasar el cursor sobre cada uno de ellos."

## Mapa----
txt_map <- "El IDSMH y todos los indicadores se han calculado para los niveles administrativos subnacionales (departamentos, estados, provincias o regiones) de cada país, según su organización territorial. Elige el indicador que quieres mostrar y explora su distribución en los distintos territorios."

## Scatter ordenado----
txt_scatter_ord <- "Este gráfico muestra 191 territorios subnacionales incluidos en el estudio ordenados de mayor a menor valor del índice para el sexo seleccionado. El punto señala el valor para el sexo elegido, y la longitud de la línea indica la magnitud de la brecha respecto al sexo opuesto. El color de los puntos representa el país al que pertenece cada territorio."

## Dumbell----
txt_dumbell <- HTML("Cada territorio tiene distintos niveles de desarrollo social dependiendo de los factores analizados. <br>
                    Selecciona un territorio para ver el puntaje de hombres y de mujeres en cada factor. La distancia horizonal entre ellos muestra el tamaño de las brechas de género.")

## Scatter h, m y brecha----
txt_scatters <- "Algunos indicadores guardan cierta asociación con otros indicadores. A continuación, el gráfico muestra la naturaleza de esta asociación para los dos indicadores seleccionados. El panel de la izquierda muestra la asociación entre indicadores para el caso de los hombres, el panel central para las mujeres, y el de la derecha para la brecha. El color de cada punto representa el país al que pertenece cada territorio."

txt_tabla_pais <- "Selecciona hasta cinco indicadores para producir una tabla que muestre los valores de hombres y mujeres para cada uno de los países del estudio."

txt_tabla_region <- "Selecciona hasta cinco indicadores para producir una tabla que muestre los valores de hombres y mujeres para cada uno de los territorios incluidos del estudio."

txt_creditos <- HTML("<span style='font-weight:bold;color:#3B69FA'>EL ÍNDICE DEL DESARROLLO SOCIAL DE LA MUJER Y EL HOMBRE EN LOS PAÍSES DE AMÉRICA LATINA 2022</span><br>
Una publicación de CENTRUM PUCP - Escuela de Negocios de la Pontificia Universidad Católica del Perú")

## Metodología----


m_q_es <- HTML("El Índice del Desarrollo Social de la Mujer y el Hombre (IDSMH) es una herramienta que permite identificar, desde una visión amplia, la realidad de la brecha de género de las regiones (estados, departamentos y provincias) en los principales países de América Latina.
<br>  
Es decir, presentar un análisis de la situación actual del acceso a los recursos por parte de mujeres y hombres en las dimensiones de Educación, Salud, Autonomía y Oportunidades.") 

m_q_mide <- HTML("El IDSMH mide la situación de mujeres y hombres con respecto a su desarrollo en las cuatro dimensiones evaluadas por medio de un grupo de indicadores. Además, este índice ofrece un mapa con todos los resultados indicando las diferencias entre mujeres y hombres a partir del real acceso a los recursos disponibles en cada país y región de América Latina. 
<br>  
El IDSMH toma en cuenta las particularidades dentro de cada país a partir de la evaluación de cuatro dimensiones: Educación, Salud, Autonomía y Oportunidades. El índice representa una herramienta para medir la situación de mujeres y hombres a través de un enfoque holístico e integrador.")

m_caracteristicas <- HTML("
  <ul>
    <li>El índice solo considera indicadores de resultados, no de procesos o gestión.</li>
    <li>Incluye una visión para entender las diferencias entre mujeres y hombres en 191 regiones.</li>
    <li>Se utilizan bases de datos de fuentes públicas y oficiales, que incluyen encuestas nacionales de hogares, estadísticas de bancos centrales, institutos de estadísticas, informes, investigaciones, entre otras fuentes.</li>
    </ul>")

m_modelo <- HTML("<ul>
  <li>El IDSMH evalúa cuatro dimensiones: Educación, Salud, Autonomía y Oportunidades.</li>
  <li>Cada dimensión comprende tres componentes. Es decir, en total existen 12 componentes.</li>
  <li>Cada componente se determina a partir de un número determinado de variables. En total se consideran 28 variables para las mujeres y 28 variables para los hombres.</li>
</ul>")

m_educacion <-HTML("La Dimensión Educación cubre los diferentes aspectos del alcance del nivel educativo. Se mide el acceso a la educación que tienen las mujeres y los hombres en los niveles de primaria y secundaria; además, se considera los logros educativos alcanzados.")

m_salud <-HTML("La Dimensión Salud mide la capacidad que tienen las mujeres y los hombres de acceder a un servicio de salud y qué tan favorables son las condiciones para facilitar tal acceso. Asimismo, calcula el nivel de morbilidad y el acceso a los servicios de cuidados básicos diferenciado entre ambos sexos.")

m_autonomia <-HTML("La Dimensión Autonomía mide qué tan autónomos son las mujeres y los hombres económicamente, físicamente y al momento de tomar decisiones. Estos tres factores son importantes para poder medir la acción de la persona como individuo y como parte de la sociedad.")

m_oportunidades <-HTML("La Dimensión Oportunidades mide componentes relacionados con el acceso a la educación superior, el nivel de empleo adecuado y no adecuado, y la gestión y el tiempo destinado al trabajo.")

m_consideraciones <-HTML("Los indicadores de desarrollo por género se obtuvieron en los 26 departamentos de Perú (incluyendo Callao, Lima Metropolitana y Lima provincia), 16 regiones de Chile, 33 departamentos de Colombia, 24 provincias de Ecuador, 9 departamentos de Bolivia, 27 estados de Brasil, 24 provincias de Argentina y 32 estados de México.
<br>  
Si bien los 28 indicadores son los mismos para los 8 países, su cálculo requirió de una adaptación a cada realidad, con el objetivo de hacerlos comparables entre países. La denominación de los indicadores corresponde al caso específico peruano, pero la precisión técnica en su construcción depende de la forma de cálculo de cada país.
<br>  
Para poner un ejemplo, en el Perú los indicadores del componente Educación en los que se mide la tasa neta de matrícula y la asistencia a la educación primaria de niños, tiene como parámetro rango las edades que van entre los 6 y los 11 años. Mientras que en Chile el rango de edad de los niños que asisten a educación primaria se enmarca entre los 6 y los 13 años, y en Colombia entre los 6 y los 10 años.
<br>  
Para estos casos, se adaptaron los indicadores a la realidad de cada país, en el ejemplo para Chile y Colombia se aplicaron las tasas de matrícula y asistencia de primaria a los rangos de edad que corresponden a dichos países.   
<br>  
Dada la heterogeneidad de fuentes de información entre países, además de las diferentes formas de cálculo de los indicadores, para algunos indicadores fue imposible conseguir información y se tuvo que estimar los datos de la información incompleta.
<br>  
La forma en que se estimó el indicador incompleto del país (denominados “indicador del país objetivo”) fue tomando como referencia a los países que si contaban con información completa (denominados “indicadores de países de referencia”), requiriendo para ello un factor de ponderación que incluyera el concepto de brechas de género.
<br>  
A continuación, se detallan los pasos realizados para la estimación del indicador del país objetivo:")

m_calculo1 <- HTML("Los insumos utilizados para la construcción del factor de ponderación de los países de referencia fueron los Índices Globales de Brecha de Género (IGBG) publicados por el Foro Económico Mundial, los cuales miden la diferencia de género en las dimensiones salud, economía, educación y política para 78 países del mundo.
<br>  
Los índices oscilan entre el 0 y 1, representando el “0” la máxima desigualdad de género y “1” perfecta igualdad entre hombres y mujeres. En el cuadro siguiente se muestran los indicadores por dimensión, según país:")

m_calculo2 <- HTML("Los IGBG utilizados para la ponderación de los países de referencia dependieron del indicador incompleto estimado. Por ejemplo, si el indicador a estimar tenía que ver con Educación, el factor utilizado fue el IGBG de la dimensión educación, mientras que, si el indicador a estimar era el de mujeres y hombres no remunerados, el factor utilizado fue el IGBG de la dimensión economía. A continuación, se presenta la tabla de conversiones utilizadas entre los indicadores del IDSMHRP y el IGBG.")

m_calculo3 <- HTML("Para obtener las ponderaciones de los países de referencia fueron calculadas las desviaciones relativas del Indicador Global de Brecha de Género (IGBG) de los países de referencia respecto al indicador del país objetivo.
<br>  
La desviación del IGBG del país de referencia resulta del cociente entre la resta del IGBG del país objetivo (IPaíso) y el país de referencia (IPaísRi), y el IGBG del país de referencia (IPaísRi). Como se puede apreciar en la siguiente fórmula:")

m_calculo4 <- HTML("A los países de referencia que registraron una menor desviación se les asignó una mayor ponderación debido a su semejanza en cuanto a brechas de género; mientras que aquellos países que registraron una mayor desviación se les asignó una menor ponderación.
<br>  
A partir de los resultados anteriores se aplicaron las siguientes reglas de decisión:
<br>  
Reglas de decisión:
<ul>
  <li>	Si Desviación País Ri = [0 – 5%]      ->  60% de peso</li>
  <li>	Si Desviación País Ri = [6% – 15%] ->  30% de peso</li>
  <li>	Si Desviación País Ri = [16% a más -> 10% de peso</li>
</ul>
Si la desviación del país de referencia se ubicaba entre 0 y 5% (en términos absolutos) se les asignó una ponderación de 60%, mientras si la desviación oscilaba entre 6 y 15% se aplicó un peso de 30%. Finalmente, si la desviación superaba el 15% se le imputó una ponderación de 10%.
<br>  
Finalmente, se calibro la suma de las ponderaciones de los países de referencia para que lleguen a 100%.")

m_aplicacion1 <- HTML("Habiendo obtenido las ponderaciones de los países de referencia, se pasó a multiplicarlas por los Indicadores Regionales de Desarrollo Social de La Mujer y Hombre de dichos países para la consecución del indicador del país objetivo. Sin embargo, previo a ello se realizó una estandarización o ajuste de los datos de los países de referencia para obtener un indicador objetivo más consistente. 
<br>  
Dado que se contaba con indicadores a nivel nacional tanto de los países objetivo como de los países de referencia, se usaron ratios hombre - mujer para ajustar las cifras de los países de referencia a la realidad de cada país objetivo. A continuación, se detallan los pasos:
  <ul>
    <li>Paso 1: Se obtuvo un ratio hombre – mujer a nivel nacional de los países de referencia y objetivo.</li>
    <li>Paso 2: Se multiplicó el ratio hombre – mujer a nivel nacional de cada país de referencia por un factor diferenciado de tal forma que el nuevo ratio sea similar al del país objetivo.</li>
    <li>Paso 3: Se aplicó dicho ratio a los indicadores de hombres a nivel regional de cada país de referencia, generando así indicadores por género que se ajustaron a la realidad del país objetivo.</li>
</ul>
")

m_aplicacion2 <- HTML("Así, se obtuvieron indicadores por género en cada una de las regiones del país objetivo, manteniendo sus indicadores a nivel nacional originales.")

met_tabla1 <- tribble(
  ~Dimensión,  ~Chile,  ~Colombia, ~Brasil,  ~Argentina, ~México, ~Ecuador, ~Bolivia,
  "Economía",  "0,610", "0,702",   "0,665",  "0,639",    "0,590", "0,675",  "0,595", 
  "Salud",     "0,970", "0,975",    "0,980", "0,977",    "0,975", "0,968",  "0,962", 
  "Educación", "1,000", "1,000",    "1,000", "1,000",    "0,997", "0,997",  "0,981", 
  "Política",  "0,283", "0,216",    "0,138", "0,390",    "0,468", "0,318",  "0,352" )

met_tabla2 <- tribble(
  ~"DIMENSIÓN DEL INDICADOR",	~"CÓDIGO DEL INDICADOR", ~"INDICADORES REGIONALES DE DESARROLLO SOCIAL DE LA MUJER Y HOMBRE", 	~"INDICADOR IGBG",
  "EDUCACIÓN", "EA1", "Tasa neta de matrícula escolar de niñas y niños de 6 a 11 años de edad a educación primaria", "EDUCACIÓN",
  "EDUCACIÓN", "EA2", "Tasa neta de asistencia a educación primaria de niñas y niños de 6 a 11 años de edad", "EDUCACIÓN",
  "EDUCACIÓN", "EB1", "Tasa neta de matrícula a educación secundaria de las y los adolescentes de 12 a 16 años de edad", "EDUCACIÓN",
  "EDUCACIÓN", "EB2", "Tasa neta de asistencia a educación secundaria de las y los adolescentes de 12 a 16 años de edad", "EDUCACIÓN",
  "EDUCACIÓN", "EC1", "Promedio de años de estudio alcanzado por mujeres y hombres de 15 y más años de edad, (Años: Inicial,  primaria y secundaria)", "EDUCACIÓN",
  "EDUCACIÓN", "EC2", "Tasa de alfabetización de mujeres y hombres de 15 y más años de edad, según sexo", "EDUCACIÓN",
  "SALUD", "SA1", "Porcentaje de la población de Mujeres y hombres que cuentan con algún seguro de salud (No SIS)", "SALUD",
  "SALUD", "SA2", "Variación anual de registros de Mujeres y hombres que tienen DNI ", "SALUD",
  "SALUD", "SB1.1", "Disponibilidad de camas para hombres y mujeres ", "SALUD",
  "SALUD", "SB2.1", "Porcentaje de mujeres y hombres que padecen de cáncer o diabetes ", "SALUD",
  "SALUD", "SC1", "Casos de mujeres y hombres que reciben consulta médica por cada 10 mil habitantes", "SALUD",
  "AUTONOMÍA", "AA1", "Ingreso promedio mensual del trabajo de hombres y mujeres", "ECONOMIA",
  "AUTONOMÍA", "AA2", "Mujeres y hombres ocupados no remunerados (Trabajador independiente o trabajador familiar)", "ECONOMIA",
  "AUTONOMÍA", "AA3", "Mujeres y hombres sin ingresos propios", "ECONOMIA",
  "AUTONOMÍA", "AB1", "Delito de trata de Hombres y Mujeres", "POLÍTICA",
  "AUTONOMÍA", "AB2", "Población de Mujeres y Hombres menores de 18 años de edad que sufrió violación sexual", "POLÍTICA",
  "AUTONOMÍA", "AB3", "Población de Mujeres y Hombres de 18 y más años de edad que sufrió violación sexual", "POLÍTICA",
  "AUTONOMÍA", "AB4", "Violencia familiar y/o sexual de Mujeres y Hombres ", "POLÍTICA",
  "AUTONOMÍA", "AC1", "Alcaldes y Alcaldesas", "POLÍTICA",
  "AUTONOMÍA", "AC2", "Mujeres y hombres parlamentarios", "POLÍTICA",
  "OPORTUNIDAD", "OA1", "Matriculados en Centros de Formación Técnico Productivo ", "EDUCACIÓN",
  "OPORTUNIDAD", "OA2", "Matriculados en Centros de Formación Tecnológica ", "EDUCACIÓN",
  "OPORTUNIDAD", "OA3", "Tasa de asistencia de educación superior universitaria", "EDUCACIÓN",
  "OPORTUNIDAD", "OB1", "Empleo Adecuado", "ECONOMÍA",
  "OPORTUNIDAD", "OB2", "Tasa de informalidad de mujeres y hombres, ", "ECONOMÍA",
  "OPORTUNIDAD", "OB3", "Mujeres y hombres subempleados", "ECONOMÍA",
  "OPORTUNIDAD", "OC1.3", "Horas trabajadas de lunes a domingo en mujeres y hombres de 15 años a más", "ECONOMÍA",
  "OPORTUNIDAD", "OC2", "Proporción de empleadores de un negocio o empresa registrado en SUNAT", "ECONOMÍA"
 )

