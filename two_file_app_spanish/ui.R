ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    id = 'main-tabs',
    
    tabPanel(
      id = 'landing-tab',
      title = 'Inicio',
      div(
        column(width=2),
        column(
          h1('Bienvenid@!',
             style = "text-align:center;"),
          div(
              h4("¿Te interesa explorar oportunidades de inversión en el mercado de valores?
                   ¿Ya conoces el mercado de valores pero te gustaría ver si es predecible?
                 ¿Quieres aprender más sobre el mercado de valores o algoritmos de predicción?",
                style="text-align:justify;line-height:1.6"),
              h4(strong("¡Entonces este es el lugar adecuado para ti!"),
                style="text-align:center;line-height:1.6"),
             style="color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
          br(),
          "Es posible que hayas tenido curiosidad por invertir en el mercado de valores, pero nunca supiste realmente cómo funciona.
           Tal vez tengas una idea de cómo funciona, pero no sabes si es para ti. O tal vez ya seas experta y estás emocionada
          sobre la predicción del mercado de valores. Quizás deseas saber qué herramientas existen que pueden ayudarte. Cualquiera que sea tu caso, espero que este sitio
          sea de alguna manera útil para ti.",
          h2("¿Qué encontrarás aquí?"),
          column(
            div(
              h4("Anális"),
              p("En esta pestaña, podrá explorar hasta 50 de las empresas más grandes de México y Estados Unidos."),
              p("Puedes observar el valor de cada una de sus acciones (que se refieren a una pequeña fracción de la empresa) y cómo fue evolucionando a lo largo del tiempo."),
              p("También podrás probar dos herramientas populares que se han utilizado para hacer predicciones de los precios de estas acciones y compararlas."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            width = 4,
            style="padding:15px;border-radius:10px"
          ),
          column(
            div(
              h4("Portafolio"),
              p("¡Aquí tendrás la oportunidad de crear tu propio portafolio virtual! Uno hipotético, por supuesto, lo que significa que no necesitarás invertir dinero."),
              p("Podrás retroceder en el tiempo y ver qué hubiera sucedido si hubieras invertido en algunas de estas 50 empresas. ¿Habrías ganado dinero?
               ¿Te habrías vuelto rico? O tal vez lo perdiste todo ... ¡Averigüemos!"),
              p("¡También puedes usar las herramientas que exploraste en la pestaña de Análisis para hacer predicciones del futuro! Puedes crear cualquier portafolio hipotético
               que te interese."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            style="padding:15px;border-radius:10px",
            width = 4
          ),
          column(
            div(
              h4("Acerca de"),
              p("En esta página podrás encontrar más información sobre lo que sucede 'bajo el capó' para generar los gráficos interactivos en las pestañas de Análisis y Portafolio."),
              p("Si tienes tiempo, te sugiero que consultes esta página antes de visitar las pestañas de análisis de datos y simulación de portafolios."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            width = 4,
            style="padding:15px;border-radius:10px"
          ),
          width = 8
        )
        
        )
    ),
    
    #ANALYSIS TAB
    tabPanel(
      id = 'analysis-tab',
      title = 'Análisis',
      headerPanel('Análisis de Datos'),
      sidebarPanel(
        "Periodo de entrenamiento",
        helpText("NO ELIJAS FECHAS EN FIN DE SEMANA"),
        dateInput('start_date_ana', 'Fecha inicial', value = "2018-01-01", max = Sys.Date()),
        dateInput('end_date_ana', 'Fecha final', value = "2020-12-31", max = Sys.Date()),
        helpText("El horizonte de predicción se refiere a la cantidad de días en el futuro para los que desea realizar predicciones."),
        numericInput('h_ana', 'Horizonte de Predicción', min = 1, value = 180),
        wellPanel(id = "checkbox_panel1",
                  style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks_ana', 
                                     "Selección de compañías para visualización",
                                     symbols, selected = symbols[1:1]
                  )
        ),
        helpText("Las dos siguientes opciones tienen que ver con el componente de estacionalidad de los algoritmos de predicción. Si no sabes
                  lo que esto significa, ¡no te preocupes! Simplemente deja los valores preseleccionados. Si deseas obtener más información sobre ellos, 
                 simplemente visite la pestaña Acerca de."),
        checkboxInput("seasonal","Utilice el factor estacional anual (esto aplica tanto para Prophet como para Holt Winters)", TRUE),
        selectInput('seasonal_type', 'Tipo de modelo estacional utilizado para el algoritmo de Holt Winters', seasonality_types),
        actionButton("runAnalysisButton", "Ejecutar análisis"),
        width = 3
      ), # end sidebarPanel
      
      mainPanel(
        id = 'inner-main-analysis',
        tabsetPanel(
          id = 'analysis-inner-tabset',
          tabPanel(
            value = 'data-panel',
            title = 'Datos',
            tableOutput('table1'),
            icon = icon("table")
          ),
          tabPanel(
            value = 'visualization-panel',
            title = 'Visualización',
            br(),
            p("Cada par de gráficos para cada una de las compañías seleccionadas, muestra los preciós históricos de la acción de la compañía para el período de entrenamiento seleccionado, así como un
              modelo ajustado a los precios históricos. En el gráfico de Prophet (a la izquierda de cada par), el modelo ajustado a los precios se muestra en amarillo. Este modelo
              se extiende a la zona de predicción (a la derecha de la línea vertical punteada) donde se vuelve naranja. Esto es simplemente el resultado
              de 'continuar' la línea para el horizonte de predicción. Esta línea (la línea amarillo-naranja) es lo que llamamos modelo. En los gráficos de Holt (lado derecho
              de los pares), el modelo se muestra en dos tonos de azul (el tono más claro para el modelo ajustado a los precios y el azul más oscuro para los
              valores predichos)", 
              style = "font-size:1.1em;line-height: 1.8"),
            div(tags$ul("Cosas en las que enfocarse:",
                        tags$li("¿Cómo ha evolucionado el precio de cada una de las acciones seleccionadas? ¿Parece que sube y baja mucho? ¿Tiene algún aumento brusco
                                 o fuerte disminución?"),
                        tags$li("¿Qué tan bien predice cada uno de los modelos lo que realmente sucedió? ¿Las predicciones coinciden en que la cartera (portafolio) suba o baje?
                                 ¿Está el valor real del precio dentro de los márgenes de error de la predicción?"),
                        tags$li("¿Qué pudo haber afectado a las buenas o malas predicciones? ¿Hubo algún evento mundial importante que pudiera haber influido en el precio (por ejemplo, un
                                 pandemia global)?")
                        ),
                style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
            br(),
            uiOutput('plots'),
            br(),
            br(),
            icon = icon("chart-line")
          )
        ), # <- end tabsetPanel 'analysis-inner-tabset'
        width = 9
      ) # <- end mainPanel 'inner-main-analysis'
    ), # <-- end tabPanel 'analysis-tab'
    
    #PORTFOLIO TAB
    tabPanel(
      id = 'portfolio-tab',
      title = 'Portafolio',
      headerPanel("Mi Cartera Virtual"),
      sidebarPanel(
        helpText("Elige una composición de cartera predefinida basada en el nivel de riesgo deseado o elige manualmente las acciones que deseas incluir en tu cartera."),
        helpText("El nivel de riesgo de una acción se evalúa en función de su volatilidad anual y la categorización en 'Bajo',
                  'Medio', 'Alto' se basa en las calificaciones de riesgo de Stockopedia."),
        p(tags$a(href="https://help.stockopedia.com/product-guide/stockranks/advanced/the-riskratings/", 
                 "Calificaciones de riesgo de Stockopedia")),
        selectInput('risk_level', 'Nivel de riesgo', risk_levels),
        wellPanel(id = "checkbox_panel2",style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks_inv', "Selección de acciones para la cartera virtual",
                                     symbols)),
        actionButton('selectStocksButton', 'Seleccionar Acciones'),
        helpText("Selecciona un monto de inversión inicial"),
        numericInput('init_capital', 'Capital de Inversión Inicial ($USD)', min = 0, value = 100),
        "Período de entrenamiento",
        helpText("NO ELIJAS FECHAS DE FIN DE SEMANA"),
        dateInput('start_date_port', 'Fecha inicial', value = "2018-01-01", max = Sys.Date()),
        dateInput('end_date_port', 'Fecha final', value = "2020-12-31", max = Sys.Date()),
        helpText("Selecciona un horizonte de predicción. Este es el número de días en el futuro para los que deseas ver la evolución de la cartera seleccionada."),
        numericInput('h_Portfolio', 'Prediction Horizon', min = 1, value = 60),
        wellPanel(id = "checkbox_panel3",style = "overflow-y:scroll; max-height: 500px",
                  uiOutput("portfolio_dist")),
        helpText("Las dos siguientes entradas que tienen que ver con el componente de estacionalidad de los algoritmos de predicción. Si no sabes
                  lo que esto significa, ¡no te preocupes! Simplemente deja los valores preseleccionados. Si deseas obtener más información sobre ellos, visita la pestaña Acerca de."),
        checkboxInput("seasonal2","Utilice el factor estacional anual (esto aplica tanto para Prophet como para Holt Winters)", TRUE),
        selectInput('seasonal_type2', 'Tipo de modelo estacional utilizado para el algoritmo de Holt Winters', seasonality_types),
        actionButton('runPortfolio', 'Simular Portafolio'),
        width = 3
        ),
      mainPanel(
        id = 'inner-main-portfolio',
        br(),
        plotlyOutput("pie_chart") %>% withSpinner(color="#0dc5c1"),
        helpText("Desplázate sobre el gráfico de arriba para ver el porcentaje exacto de tu cartera asignado a cada una de las acciones individuales.", style="text-align:center"),
        br(),
        p("Los dos gráficos superiores a continuación muestran la evolución de tu cartera a lo largo del horizonte de predicción. Esto significa que cualquiera que sea la cantidad que elijas para empezar
           (Monto de inversión inicial ($ USD)), si lo invirtieras en las acciones seleccionadas y proporciones seleccionadas para cada compañía, su cartera hubiera evolucionado como lo muestran los puntos 
           negros en los dos gráficos de arriba a continuación. Cada uno de esos gráficos también te muestran lo que cada uno de los modelos te diría que sucedería.", style = "font-size:1.1em;line-height: 1.8"),
        div(tags$ul("Hay tres cosas en las que debe centrarse en lo que se muestra en la siguiente figura:",
           tags$li("La evolución real de la cartera seleccionada que se muestra en negro en cualquiera de los dos gráficos superiores. Esto muestra lo que habría",
                   strong("realmente"),"sucedido si alguien hubiera invertido la cantidad de dinero seleccionada y hubiera distribuido la cartera de la forma en que se muestra en el gráfico circular."),
           tags$li("¿Qué tan bien predijo cada uno de los modelos lo que realmente sucedió? ¿Coincidieron las predicciones en si la cartera sube o baja? ¿Está el valor real
                    de la cartera dentro de los márgenes de error de las predicciones de los modelo?"),
           tags$li("En los dos gráficos siguientes, ¿existe alguna acción específica que pareciera ser responsable del aumento o disminución de la cartera?")
           ),
          style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
        br(),
        plotlyOutput("portfolio_forecast", height = "800px") %>% withSpinner(color="#0dc5c1"),
        br(),
        helpText("Pasa el cursor sobre los gráficos de la figura anterior para obtener más información sobre lo que muestra cada línea y el valor exacto en un momento dado.", style="text-align:center"),
        p("Puedes intentar modificar su entrada para ver qué habría sucedido si hubieras asignado más (o menos) de la cantidad inicial a una acción determinada.
            Agrega o elimine acciones para ver cómo cambiaría esto. También puedes cambiar el período de entrenamiento o elegir un horizonte de predicción diferente.", style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
        width = 9
      ) # <- end mainPanel 'inner-main-portfolio
    ), # <-- end tabPanel 'protfolio-tab'
    
    #ABOUT TAB
    tabPanel(
      id = 'about-tab',
      title = 'Acerca de',
      headerPanel('Cómo usar esta aplicación'),
      tabsetPanel(
        id = 'about-inner-tabset',
        tabPanel(
          id = 'about-how-tab',
          title = 'Cómo usar',
          fluidRow(column(
                     #tags$img(src="Antioquia.png",width="200px",height="260px"),
                     width=2),
                   column(
                     
                     br(),
                     p("El objetivo de esta aplicación es que puedas explorar el mercado de valores de una forma interactiva que
                        con suerte, te permitirá comprender mejor tus opciones de inversión. Simplemente hablando del mercado de valores, y mucho menos de invertir en él,
                       puede parecer alucinante. Esta aplicación, a primera instancia, con un montón de gráficos y números, también podría dar un poco de miedo. Sin embargo,
                       Tras una pequeña inspección, a través de la cual te guiaré, podrás tomar decisiones financieras mejor informadas. ", 
                       style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                     p("El objetivo de esta aplicación", strong("NO"), "persuadirle para que invierta en bolsa. El único propósito
                        de esta aplicación es ampliar su horizonte de opciones financieras. Es posible que, al aprender más sobre el mercado de valores a través de la aplicación,
                       decide que el mercado de valores no es para ti, y eso también está completamente bien. \n
                       Mi único objetivo es que tú", em("aprendas"), "y, en función de tus aprendizajes, decidas
                        lo que funciona mejor para ti, financieramente hablando. Esta aplicación NO debe tomarse como asesoramiento financiero.",
                       style="text-align:justify;color:black;background-color:papayawhip;padding:30px;border-radius:10px"),
                     width=8),
                   column(
                     width=2)
                   #02A4D3
          ),
          fluidRow(
            column(width=2),
            column(
              h1("Pestaña de Análisis"),
              width = 8)
          ),
          fluidRow(column(
            width=2),
            column(
              p("En esta pestaña, podrás ver el precio de diferentes acciones. Al principio, en la pestaña 'Datos', verás una descripción general en
                 una tabla que muestra el precio actual de la acción y el porcentaje de cambio en el último día, el cual se refier al porcentaje de cambio en el precio
                de la acción en el último día. Por ejemplo, si las acciones de Apple estaban valoradas en $100 ayer y hoy están valoradas en $102,
                el % de cambio mostrará 2%, porque el aumento de $2 representa el 2% del precio de ayer."),
              p("En la pestaña 'Visualización', puedes ver los", em (" precios históricos ")," de las acciones que puedes elegir utilizando el panel de la barra lateral.
                 En el panel de la barra lateral también podrás seleccionar el", em (" período de entrenamiento "),". El período de entrenamiento determinará los precios históricos
                que se le dan a los modelos para que realicen predicciones. Esta aplicación utiliza el pronóstico de series de tiempo de Prophet de Facebook y Holt Winters Exponential
                Smoothing. Para comprender mejor cómo funcionan estos modelos, consulte las pestañas 'Prophet' y 'Holt'. "),
              p("Los modelos se utilizan para hacer predicciones sobre lo que sucederá con el precio de las acciones en los siguientes días, semanas o años.
                 El ", em (" horizonte de predicción ")," en el panel de la barra lateral le permite elegir cuántos días en el futuro desea que los modelos hagan las predicciones.
                Por ejemplo, si eliges 180, verá lo que cada modelo predice que sucederá con el precio de las acciones en el próximo
                180 días (alrededor de 6 meses) "),
              h2("Cómo interpretar los gráficos"),
              p("Para cada una de las acciones seleccionadas, podrás ver los precios históricos en el período de entrenamiento seleccionado y la predicción. Estos lo harán
                 ser fácilmente identificables por la línea vertical punteada en cada uno de los gráficos, que denota el final del período de entrenamiento. Es decir, la parte del
                 gráfico a la izquierda de esta línea se refiere a los precios históricos y la parte a la derecha de la línea se refiere a las predicciones."),
              width=5),
            column(
              p(strong("Precios históricos"), "se refiere al precio de las acciones en el pasado. Los precios históricos
                 nos muestran cuál ha sido el precio cada día durante un período de tiempo determinado en el ", em (" pasado "),". Observamos estos datos porque
                esperamos identificar ", em (" tendencias ")," o ", em (" patrones ")," y potencialmente usar esa información para ", em (" predecir ")," el futuro.",
                style="text-align:justify;color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
              p(strong("Período de entrenamiento"), "se refiere al período de precios históricos que se utilizarán para", em ("predecir"), "el futuro.
                 Se llama período de", em (" entrenamiento ")," porque las predicciones serán hechas por un ", em (" modelo ")," al que le tenemos que enseñar, o entrenar,
                para hacer buenas predicciones. Entrenamos al modelo mostrándole algunos datos en el pasado y dejamos que identifique patrones. Más información
                acerca de los modelos utilizados en esta aplicación la puedes encontrar en las pestañas de Holt y Prophet.",
                style="text-align:justify;color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
              width=3),
            column(
              width=2)
          ), #<- end fkuid Row
          fluidRow(
            column(width=2),
            column(
              tags$img(src="how_to1.png", width ="100%"),
              width = 8)
          ),
          fluidRow(
            column(width=2),
            column(
              br(),
              p("Cada uno de los gráficos tiene un modelo ajustado a los datos. En el gráfico de Prophet (izquierda), el modelo ajustado a los datos de entrenamiento se muestra en amarillo.
                Este modelo se extiende a la zona de predicción (a la derecha de la línea vertical discontinua) donde se vuelve naranja, que es simplemente el resultado de
                'continuar' la línea durante los próximos 180 días. Esta línea (la línea amarillo-naranja) es lo que llamamos modelo. La parte que está en naranja se refiere a la
                predicción que el modelo hace para los siguientes 180 días (primera mitad del año 2018), basado en datos desde 2010. Es una predicción porque
                para crear esta línea, solo teníamos los precios del período de entrenamiento disponibles. Es decir, no sabíamos nada sobre lo que realmente sucedió con el precio después del final
                del período de entrenamiento (marcado por la línea discontinua horizontal). La región sombreada alrededor de la sección naranja se refiere al margen de error de
                las predicciones. En violeta, tenemos el precio real de las acciones. A la izquierda de la línea punteada vertical, podemos ver cómo los datos reales
                y el modelo están muy juntos. Esto se debe a que el modelo ", em (" usa ")," los datos históricos para dibujar la línea amarilla. A la derecha, podemos
                comparar las predicciones con los precios reales para ver qué tan buenas fueron las predicciones hechas por el modelo."),
              p("Todo lo dicho sobre el gráfico de Prophet (izquierda) se aplica al gráfico de Holt (derecha). En el caso de Holt, el modelo y las predicciones se muestran en
                 sombras de azul. La principal diferencia entre los dos es que el método utilizado para construir cada uno de los modelos es diferente. Para obtener detalles sobre cómo estos
                 modelos hacen predicciones, visita las pestañas correspondientes."),
              width = 8)
          ),
          fluidRow(
            column(width=2),
            column(
              h1("Portfolio Tab"),
              p("¡Ahora puedes pasar a la pestaña Portafolio para crear tu propio portafolio virtual! Para comenzar, puedes elegir el nivel de riesgo que deseas para tu cartera.
                Esto preseleccionará algunas de las empresas asociadas a ese nivel de riesgo. También puede elegir manualmente las empresas que llamaron tu atención cuando probaste
                la pestaña de análisis. Una vez que hayas seleccionado las empresas que deseas tener en tu portafolio, seleccionarás cuánto dinero deseas
                invertir (hipotéticamente, por supuesto). Luego, seleccionarás qué fracción de tu dinero deseas invertir en cada una de las empresas seleccionadas. También
                tendrás que especificar, como lo hiciste en la pestaña de Análisis, el período de entrenamiento que deseas utilizar para las predicciones y el horizonte de predicción. Luego, usa los controles deslizantes
                para seleccionar la fracción del monto de inversión que se asignará a cada una de las empresas que seleccionaste. Por defecto, la cantidad se distribuye equitativamente
                entre todas las empresas, pero puedes cambiar esto si deseas favorecer a algunas empresas sobre otras (por cierto, no te preocupe si los valores de los controles deslizantes no
                suman 100%, la aplicación se encarga de hacerlo por ti)."),
              h2("Cómo interpretar gráficos"),
              br(),
              tags$img(src="how_to2.png", width ="100%"),
              br(),
              p("El gráfico circular te mostrará la distribución de tu cartera entre las empresas seleccionadas. En el ejemplo que se muestra arriba, elegí una Cartera de alto riesgo y
                 una inversión inicial de $ 100 USD. El período de entrenamiento y el horizonte de predicción se dejaron en sus valores predeterminados."),
              tags$img(src="how_to3.png", width ="100%"),
              br(),
              br(),
              p("Los dos gráficos superiores muestran cómo cambió el valor de la cartera a lo largo de los 60 días elegidos como horizonte de predicción, así como la predicción de la cartera
                según el modelo Prophet (izquierda) y el modelo Holt Winters (derecha). Los dos gráficos inferiores muestran cómo cambió cada una de las empresas individuales
                durante el mismo período. Esto te permitirá ver si una empresa determinada fue la principal responsable de tus ganancias o de perder dinero. Sobre la
                la pestaña del Portafolio, puedes colocar el cursor sobre ambos gráficos para ver información más detallada sobre los valores mostrados (por ejemplo, el valor exacto de su
                cartera en una fecha exacta y la predicción exacta en esa fecha, o el nombre de la empresa que corresponde a un color dado en los dos gráficos inferiores)."),
              br(),
              width = 8)
          ),
          icon = icon("info-circle")
        ),
        tabPanel(
          id = 'about-holt-tab',
          title = 'Holt Winters',
          fluidRow(
            column(width=2),
            column(
              h1('Suavizado Exponencial Holt Winters'),
              p("El pronóstico de Holt-Winters es una técnica para modelar y predecir el comportamiento de una serie de tiempo, y es uno de las técnicas más populares
                para pronosticar series de tiempo. Te explicaré de forma sencilla y general cómo funciona. Para hacerlo, iremos paso a paso a través de los componentes 
                del modelo: promedio ponderado, suavizado exponencial, suavizado exponencial de Holt y suavizado exponencial Holt-Winters. Si estás interesad@ en una explicación 
                más técnica, te sugiero que visites este",
                tags$a(href="https://otexts.com/fpp2/expsmooth.html","libro.")),
              h2("Promedio ponderado"),
              p("Un promedio ponderado es simplemente un promedio de números donde cada uno de los números tiene una peso mayor o menor en el promedio final. Por ejemplo,
                 un promedio ponderado de los números [3, 5, 7] que asigna el mayor peso al primer número, peso medio al segundo y peso bajo a
                el último sería:"),
              tags$img(src="weighted_average.png", width ="60%", align = "center"),
              p("donde los pesos son 1,5, 1 y 0,5, respectivamente. En el contexto del precio de una acción, al asignar diferentes pesos al precio de la acción en diferentes 
                momentos, cambiamos cuánto afecta el precio en diferentes días en el pasado al precio futuro ya que es posible que algunos días sean más influyentes
                que otros. El algoritmo encontrará cuál combinación de pesos es mejor probando diferentes opciones y viendo cuál resulta en mejores predicciones, usandodo los
                precios históricos."),
              h2("Suavizado exponencial"),
              p("Esta técnica se basa en el promedio ponderado discutido anteriormente asignando una caída exponencial a los precios de las acciones. Lo que esto significa es que
                  nos preocuparemos más por los precios recientes, ya que creemos que estos tienen una mayor influencia en el precio de mañana que los precios que los precios más
                  antiguos. La forma en que reducimos la influencia en los valores anteriores es usando un factor que sigue una caída exponencial. Esto significa que el factor
                 se vuelve pequeño rápidamente a medida que nos adentramos en el pasado."),
              h2("Suavizado exponencial Holt"),
              p("Esta variación del Suavizado exponencial permite identificar tendencias. Si simplemente reducimos exponencialmente la influencia de los datos pasados, podemos perder
                 información de valor. El suavizado exponencial de Holt nos ayuda a recuperar tendencias que ocurrieron en el pasado para compensar el decaimiento exponencial que se 
                asoció con datos pasados."),
              h2("Suavizado exponencial Holt Winters"),
              p("Finalmente, el suavizado exponencial de Holt Winters agrega un componente más al algoritmo: la estacionalidad. En la vida real, es muy probable que los precios estén influenciados
                 por estaciones (y aquí no hablamos necesariamente de estaciones climáticas, sino de diferentes períodos del año que están asociados con un comportamiento
                periódico, es decir, un comportamiento que se repite). Por ejemplo, las personas pueden recibir bonificaciones antes del fin de año, por lo que es una gran oportunidad para
                usar parte de ese dinero hacia inversiones, o simplemente gastarlo. De cualquier manera, la actividad económica podría incrementarse cada invierno debido a este
                fenómeno que afecta al mercado de valores (es más complicado que eso, pero te haces una idea). El componente estacional del algoritmo tiene como objetivo
                capturar este comportamiento e incorporarlo al modelo para hacer mejores predicciones."),
              style="line-height:1.5;font-size:1.2em",
              width = 8)
              ),
          icon = icon("chart-line")
        ),
        tabPanel(
          id = 'about-prophet-tab',
          title = 'Prophet',
          fluidRow(
          column(width=2),
          column(
            h1('Modelo de Series de tiempo Prophet'),
            p("Prophet utiliza un modelo de series de tiempo descomponible en tres componentes principales: crecimiento, estacionalidad y días festivos. En esta pestaña, encontrarás un
               descripción general de cómo funciona Prophet. Audiencias más técnicas encontrarán este ",tags$a(href="https://peerj.com/preprints/3190.pdf","documento"),"más útil."),
            h2("Crecimiento"),
            p("Prophet realiza una regresión utilizando la tendencia de la curva de crecimiento logístico o lineal por partes para detectar cambios en las tendencias de los datos. La imagen de abajo fue tomada
               de", tags$a(href="https://research.fb.com/blog/2017/02/prophet-forecasting-at-scale/","del blog de Prophet de Facebook"), ", y corresponde a un conjunto de datos sobre
               visitas a la página de Peyton Manning (temporada de fútbol y playoffs)."),
            tags$img(src="trend_prophet.png", width ="100%"),
            h2("Estacionalidad"),
            p("Muy similar al modelo de Holt-Winters, Prophet intenta identificar la estacionalidad que puede ser anual, semanal o diaria
               dependiendo de la opción seleccionada. La imagen de abajo también fue tomada del blog Prophet de Facebook con los mismos datos que la imagen de arriba."),
            tags$img(src="yearly_prophet.png", width ="100%"),
            h2("Holidays"),
            p("El modelo de Prophet permite al usuario proporcionar fechas específicas para las vacaciones que podrían afectar los cambios de precios en estas fechas específicas. Por simplicidad,
               este componente no se incluyó en el modelo que realiza las previsiones en este sitio web. La razón principal es que las fechas de los días festivos variarán a medida que
               el usuario elije diferentes períodos de entrenamiento."),
            br(),
            br(),
            style="line-height:1.5;font-size:1.2em",
            width = 8)
            ),
          icon = icon("chart-line")
        )
      ) # <- end tabsetPanel 'about-inner-tabset'
    ) # <-- end tabPanel 'about-tab'
      ) # <-- end tabsetPanel 'main-tabs'
) # <-- end fluid Page