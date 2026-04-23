# ============================================================
#   DASHBOARD GANADERO - RESUMEN GENERAL
#   Análisis integral de producción, reproducción, salud y manejo del hato
#   Autor: Dashboard automático basado en datos reales
# ============================================================

# ---- INSTALAR LIBRERÍAS (si es necesario) -------------------
# install.packages(c("shiny","shinydashboard","plotly","dplyr","lubridate",
#                    "ggplot2","DT","shinycssloaders","shinyWidgets","fresh"))

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(readr)

# ============================================================
#   CARGA DE DATOS — ajusta la ruta a tu carpeta
# ============================================================
ruta <- "data/"


INSEMINADAS      <- read_csv(paste0(ruta, "INSEMINADAS_LIMPIO.csv"))
MASTITIS         <- read_csv(paste0(ruta, "MASTITIS_LIMPIO.csv"))
ORDENIO          <- read_csv(paste0(ruta, "ORDENIO_LIMPIO.csv"))
PRENEZ           <- read_csv(paste0(ruta, "PRENEZ_LIMPIO.csv"))
PRENIEZ_VACAS    <- read_csv(paste0(ruta, "PRENIEZ_VACAS_LIMPIO.csv"))
PRODUCCION       <- read_csv(paste0(ruta, "PRODUCCION_LIMPIO.csv"))
PROXIMAS_PARTO   <- read_csv(paste0(ruta, "PROXIMAS_PARTO_LIMPIO.csv"))
VACAS_GESTANTES  <- read_csv(paste0(ruta, "VACAS_GESTANTES_LIMPIO.csv"))
VACAS_PARA_SECAR <- read_csv(paste0(ruta, "VACAS_PARA_SECAR_LIMPIO.csv"))
VACAS_PROBLEMA   <- read_csv(paste0(ruta, "VACAS_PROBLEMA_LIMPIO.csv"))
VACAS_SECAS_     <- read_csv(paste0(ruta, "VACAS_SECAS_LIMPIO.csv"))
VAQUILLAS_GENERAL<- read_csv(paste0(ruta, "VAQUILLAS_GENERAL_LIMPIO.csv"))
VAQUILLAS_PARTO  <- read_csv(paste0(ruta, "VAQUILLAS_PARTO_LIMPIO.csv"))
VAQUILLAS_PRENIEZ<- read_csv(paste0(ruta, "VAQUILLAS_PRENIEZ_LIMPIO.csv"))

# ============================================================
#   PRE-PROCESAMIENTO GLOBAL
# ============================================================

# ---- Parseo de fechas ----
parse_fecha <- function(x) dmy(x)

INSEMINADAS <- INSEMINADAS %>%
  mutate(Fecha_Parto = parse_fecha(Fecha_Parto),
         Fecha_Insem = parse_fecha(Fecha_Insem),
         Mes_Insem   = floor_date(Fecha_Insem, "month"))

MASTITIS <- MASTITIS %>%
  mutate(Fecha_Evento = parse_fecha(Fecha_Evento),
         Mes          = floor_date(Fecha_Evento, "month"),
         Mes_label    = format(Mes, "%B %Y"))

PROXIMAS_PARTO <- PROXIMAS_PARTO %>%
  mutate(Fecha_Parto_Est = parse_fecha(Fecha_Parto_Est))

VACAS_GESTANTES <- VACAS_GESTANTES %>%
  mutate(Fecha_Parto_Ant = parse_fecha(Fecha_Parto_Ant))

VAQUILLAS_PARTO <- VAQUILLAS_PARTO %>%
  mutate(Fecha_Parto_Est = parse_fecha(Fecha_Parto_Est),
         Fecha_Insem     = parse_fecha(Fecha_Insem))

VAQUILLAS_PRENIEZ <- VAQUILLAS_PRENIEZ %>%
  mutate(Fecha_Ult_Insem = parse_fecha(Fecha_Ult_Insem))

PRENEZ <- PRENEZ %>%
  mutate(Fecha_Ult_Insem = parse_fecha(Fecha_Ult_Insem))

VACAS_PROBLEMA <- VACAS_PROBLEMA %>%
  mutate(Fecha_Ult_Insem = parse_fecha(Fecha_Ult_Insem),
         Fecha_Ult_Parto = parse_fecha(Fecha_Ult_Parto))

# ---- Paleta corporativa ----
PAL <- list(
  azul       = "#1565C0",
  azul_claro = "#42A5F5",
  verde      = "#2E7D32",
  verde_claro= "#66BB6A",
  naranja    = "#E65100",
  naranja_cl = "#FFA726",
  rojo       = "#B71C1C",
  rojo_cl    = "#EF5350",
  morado     = "#6A1B9A",
  morado_cl  = "#CE93D8",
  gris       = "#546E7A",
  gris_cl    = "#B0BEC5",
  amarillo   = "#F9A825",
  cyan       = "#00838F",
  bg         = "#0D1B2A",
  card       = "#162032",
  texto      = "#E8F4FD"
)

# ============================================================
#   CSS PERSONALIZADO
# ============================================================
css_custom <- "
  @import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700&family=Barlow:wght@300;400;500&display=swap');

  * { box-sizing: border-box; }

  body, .content-wrapper, .main-sidebar, .wrapper {
    background-color: #0D1B2A !important;
    font-family: 'Barlow', sans-serif;
    color: #E8F4FD;
  }

  .skin-blue .main-header .logo {
    background: linear-gradient(135deg, #1565C0, #0D47A1) !important;
    font-family: 'Barlow Condensed', sans-serif !important;
    font-weight: 700 !important;
    font-size: 15px !important;
    letter-spacing: 1px;
  }
  .skin-blue .main-header .navbar { background: #0D47A1 !important; }
  .skin-blue .main-sidebar { background: #0A1520 !important; }
  .skin-blue .sidebar-menu > li > a {
    color: #90CAF9 !important;
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 13px;
    letter-spacing: 0.5px;
  }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li > a:hover {
    background: #1565C0 !important;
    color: #fff !important;
  }
  .skin-blue .sidebar-menu > li > a .fa {
    color: #42A5F5 !important;
  }

  /* ---- Tarjetas KPI ---- */
  .kpi-card {
    background: linear-gradient(145deg, #162032, #1a2a40);
    border: 1px solid rgba(66,165,245,0.2);
    border-radius: 10px;
    padding: 14px 18px;
    text-align: center;
    margin-bottom: 14px;
    transition: transform 0.2s, box-shadow 0.2s;
    box-shadow: 0 4px 15px rgba(0,0,0,0.4);
  }
  .kpi-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 25px rgba(21,101,192,0.3);
  }
  .kpi-number {
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 2.2em;
    font-weight: 700;
    line-height: 1.1;
    margin: 0;
  }
  .kpi-label {
    font-size: 0.72em;
    color: #90CAF9;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    margin-top: 4px;
    font-weight: 500;
  }
  .kpi-icon { font-size: 1.4em; margin-bottom: 4px; }

  /* ---- Caja de gráfico ---- */
  .chart-box {
    background: linear-gradient(145deg, #162032, #1a2a40);
    border: 1px solid rgba(66,165,245,0.15);
    border-radius: 10px;
    padding: 16px;
    margin-bottom: 16px;
    box-shadow: 0 4px 15px rgba(0,0,0,0.35);
  }
  .chart-title {
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 1em;
    font-weight: 700;
    color: #42A5F5;
    text-transform: uppercase;
    letter-spacing: 1px;
    margin-bottom: 4px;
  }
  .chart-vars {
    font-size: 0.72em;
    color: #78909C;
    font-style: italic;
    margin-bottom: 10px;
    padding-bottom: 8px;
    border-bottom: 1px solid rgba(66,165,245,0.1);
  }

  /* ---- Tabla ---- */
  .dataTables_wrapper { color: #E8F4FD !important; }
  table.dataTable thead th {
    background: #0D47A1 !important;
    color: #E8F4FD !important;
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 12px;
    letter-spacing: 0.5px;
  }
  table.dataTable tbody tr { background: #162032 !important; color: #E8F4FD !important; }
  table.dataTable tbody tr:hover { background: #1e3050 !important; }
  table.dataTable tbody tr.odd { background: #1a2a40 !important; }
  .dataTables_paginate .paginate_button { color: #42A5F5 !important; }

  /* ---- Filtros sidebar ---- */
  .filtro-label {
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 11px;
    color: #64B5F6;
    text-transform: uppercase;
    letter-spacing: 1px;
    margin-top: 10px;
    margin-bottom: 3px;
  }
  .selectize-control .selectize-input {
    background: #1a2a40 !important;
    border: 1px solid #1565C0 !important;
    color: #E8F4FD !important;
    font-size: 12px;
  }
  .selectize-dropdown { background: #162032 !important; color: #E8F4FD !important; }
  .btn-limpiar {
    background: #1565C0; color: #fff; border: none;
    padding: 7px 16px; border-radius: 6px;
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 12px; letter-spacing: 1px;
    margin-top: 12px; width: 100%; cursor: pointer;
    transition: background 0.2s;
  }
  .btn-limpiar:hover { background: #0D47A1; }

  /* ---- Título principal ---- */
  .dash-title {
    font-family: 'Barlow Condensed', sans-serif;
    font-size: 2em;
    font-weight: 700;
    color: #E8F4FD;
    text-align: center;
    letter-spacing: 2px;
    text-transform: uppercase;
    margin-bottom: 2px;
    padding-top: 10px;
  }
  .dash-subtitle {
    font-size: 0.82em;
    color: #78909C;
    text-align: center;
    letter-spacing: 0.5px;
    margin-bottom: 18px;
  }

  /* ---- Indicadores tabla resumen ---- */
  .ind-table { width: 100%; font-size: 0.78em; }
  .ind-table tr td:first-child { color: #90CAF9; padding: 2px 6px 2px 0; }
  .ind-table tr td:last-child { font-weight: 600; color: #E8F4FD; text-align: right; }

  /* scrollbar personalizado */
  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: #0A1520; }
  ::-webkit-scrollbar-thumb { background: #1565C0; border-radius: 3px; }
"

# ============================================================
#   HELPERS PLOTLY
# ============================================================
tema_plotly <- function(p, leyenda = TRUE) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(family = "Barlow, sans-serif", color = "#E8F4FD", size = 11),
    legend        = if (leyenda) list(bgcolor = "rgba(0,0,0,0)", font = list(size = 10)) else list(visible = FALSE),
    margin        = list(l = 40, r = 10, t = 10, b = 50),
    xaxis         = list(gridcolor = "rgba(66,165,245,0.08)", zerolinecolor = "rgba(66,165,245,0.15)",
                         tickfont = list(size = 10), title = list(font = list(size = 11))),
    yaxis         = list(gridcolor = "rgba(66,165,245,0.08)", zerolinecolor = "rgba(66,165,245,0.15)",
                         tickfont = list(size = 10), title = list(font = list(size = 11)))
  )
}

colores_pie <- c("#1565C0","#FFA726","#2E7D32","#B71C1C","#6A1B9A","#00838F","#F9A825","#E65100")

# ============================================================
#   UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "🐄 DASHBOARD GANADERO",
    titleWidth = 240
  ),
  
  dashboardSidebar(
    width = 210,
    tags$style(HTML(css_custom)),
    tags$div(class = "filtro-label", style = "margin-left:10px;", "🗂 FILTROS"),
    tags$div(style = "padding:0 10px;",
             tags$div(class = "filtro-label", "AÑO"),
             selectInput("fil_anio", NULL,
                         choices  = c("Todos", sort(unique(year(INSEMINADAS$Fecha_Insem)), decreasing = TRUE)),
                         selected = "Todos", width = "100%"),
             tags$div(class = "filtro-label", "LOCALIDAD"),
             selectInput("fil_loc", NULL,
                         choices  = c("Todas", sort(unique(INSEMINADAS$Localidad))),
                         selected = "Todas", width = "100%"),
             tags$div(class = "filtro-label", "ESTATUS REPRODUCTIVO"),
             selectInput("fil_estatus", NULL,
                         choices  = c("Todos", sort(unique(PRODUCCION$Estatus))),
                         selected = "Todos", width = "100%"),
             tags$div(class = "filtro-label", "GRUPO"),
             selectInput("fil_grupo", NULL,
                         choices  = c("Todos", sort(unique(PRODUCCION$Grupo))),
                         selected = "Todos", width = "100%"),
             actionButton("limpiar", "↺  LIMPIAR FILTROS", class = "btn-limpiar")
    ),
    tags$hr(style = "border-color:rgba(66,165,245,0.2);margin:12px 10px;"),
    tags$div(class = "filtro-label", style = "margin-left:10px;", "📊 KPIs GENERALES"),
    uiOutput("kpi_sidebar")
  ),
  
  dashboardBody(
    tags$style(HTML(css_custom)),
    
    # Título
    tags$div(class = "dash-title",  "DASHBOARD GANADERO — RESUMEN GENERAL"),
    tags$div(class = "dash-subtitle",
             "Análisis integral de producción, reproducción, salud y manejo del hato"),
    
    # ---- Fila 1: KPIs numéricos grandes ----
    fluidRow(
      column(2, uiOutput("kpi_total")),
      column(2, uiOutput("kpi_prod")),
      column(2, uiOutput("kpi_gest")),
      column(2, uiOutput("kpi_prox_parto")),
      column(2, uiOutput("kpi_mastitis")),
      column(2, uiOutput("kpi_problema"))
    ),
    
    # ---- Fila 2: Gráficos de distribución (pie charts) ----
    fluidRow(
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "1. Distribución: Preñez Actual"),
                      tags$div(class = "chart-vars",
                               "Variables: PRENEZ → Estatus (Insem / Vacía)"),
                      withSpinner(plotlyOutput("pie_prenez", height = "200px"), color = "#42A5F5")
             )
      ),
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "2. Estado Productivo"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → Estatus (Seca / En ordeño)"),
                      withSpinner(plotlyOutput("pie_estado_prod", height = "200px"), color = "#42A5F5")
             )
      ),
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "3. Distribución por Lactancia"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → Lact_N agrupado (LC1, LC2, LC3+)"),
                      withSpinner(plotlyOutput("pie_lactancia", height = "200px"), color = "#42A5F5")
             )
      ),
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "4. Vacas Secas (Detalle)"),
                      tags$div(class = "chart-vars",
                               "Variables: VACAS_SECAS_ → Estatus (Prepa / Preñ / Vacía)"),
                      withSpinner(plotlyOutput("pie_secas", height = "200px"), color = "#42A5F5")
             )
      )
    ),
    
    # ---- Fila 3: Indicadores clave + Mastitis por mes + % Salida por mes ----
    fluidRow(
      column(3,
             tags$div(class = "chart-box", style = "height:370px; overflow-y:auto;",
                      tags$div(class = "chart-title", "5. Indicadores Clave"),
                      tags$div(class = "chart-vars",
                               "Variables: PRENEZ, PRODUCCION, MASTITIS, VAQUILLAS_PRENIEZ, INSEMINADAS"),
                      uiOutput("tabla_indicadores")
             )
      ),
      column(5,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "6. % Mastitis por Mes"),
                      tags$div(class = "chart-vars",
                               "Variables: MASTITIS → Fecha_Evento (mes), ID_Vaca; PRODUCCION → total vacas"),
                      withSpinner(plotlyOutput("bar_mastitis_mes", height = "280px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "7. Recría vs Total Hato"),
                      tags$div(class = "chart-vars",
                               "Variables: VAQUILLAS_GENERAL → Categoria; PRODUCCION → total"),
                      withSpinner(plotlyOutput("pie_recria", height = "280px"), color = "#42A5F5")
             )
      )
    ),
    
    # ---- Fila 4: Abortos vacas / vaquillas + Curva lactancia ----
    fluidRow(
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "8. Nº Inseminaciones por Mes"),
                      tags$div(class = "chart-vars",
                               "Variables: INSEMINADAS → Mes_Insem, ID_Vaca"),
                      withSpinner(plotlyOutput("bar_insem_mes", height = "240px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "9. Vaquillas en Preñez por Grupo"),
                      tags$div(class = "chart-vars",
                               "Variables: VAQUILLAS_PRENIEZ → Grupo, Nro_Inseminaciones, Estatus"),
                      withSpinner(plotlyOutput("bar_vaq_prenez", height = "240px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "10. Días Abiertos por Localidad"),
                      tags$div(class = "chart-vars",
                               "Variables: INSEMINADAS → Dias_Abiertos, Localidad"),
                      withSpinner(plotlyOutput("box_dias_abiertos", height = "240px"), color = "#42A5F5")
             )
      )
    ),
    
    # ---- Fila 5: Curva producción + Tasa concepción + Próximos partos ----
    fluidRow(
      column(5,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "11. Curva de Producción de Leche (lts/día)"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → DIM, Leche_Total, Leche_AM"),
                      withSpinner(plotlyOutput("curva_leche", height = "280px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "12. Tasa de Concepción por Nº de Servicio"),
                      tags$div(class = "chart-vars",
                               "Variables: PRENEZ → Servicios, Estatus (Preñ vs total)"),
                      withSpinner(plotlyOutput("bar_tc_servicio", height = "280px"), color = "#42A5F5")
             )
      ),
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "13. Próximos Partos"),
                      tags$div(class = "chart-vars",
                               "Variables: PROXIMAS_PARTO → ID_Vaca, Nombre, Fecha_Parto_Est, Dias_Faltan"),
                      withSpinner(DTOutput("tabla_proximos_partos"), color = "#42A5F5")
             )
      )
    ),
    
    # ---- Fila 6: Producción promedio por estatus + Mastitis individual + CCS ----
    fluidRow(
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "14. Producción Promedio por Estatus"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → Estatus, Leche_Total"),
                      withSpinner(plotlyOutput("bar_prod_estatus", height = "240px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "15. Distribución CCS (Células Somáticas)"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → CCS, Estatus"),
                      withSpinner(plotlyOutput("hist_ccs", height = "240px"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "16. Días en Leche (DIM) por Estatus"),
                      tags$div(class = "chart-vars",
                               "Variables: PRODUCCION → DIM, Estatus"),
                      withSpinner(plotlyOutput("box_dim_estatus", height = "240px"), color = "#42A5F5")
             )
      )
    ),
    
    # ---- Fila 7: Vacas problema + Toros más usados + Servicios gestantes ----
    fluidRow(
      column(5,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "17. Vacas Problema — Detalle"),
                      tags$div(class = "chart-vars",
                               "Variables: VACAS_PROBLEMA → ID_Vaca, DIM, Prod_24h, Nro_Inseminaciones, Comentarios"),
                      withSpinner(DTOutput("tabla_problema"), color = "#42A5F5")
             )
      ),
      column(4,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "18. Toros más Utilizados en Inseminación"),
                      tags$div(class = "chart-vars",
                               "Variables: INSEMINADAS → Toro, Cant_Insem"),
                      withSpinner(plotlyOutput("bar_toros", height = "280px"), color = "#42A5F5")
             )
      ),
      column(3,
             tags$div(class = "chart-box",
                      tags$div(class = "chart-title", "19. Servicios en Gestantes"),
                      tags$div(class = "chart-vars",
                               "Variables: VACAS_GESTANTES → Servicios, Dias_Abiertos"),
                      withSpinner(plotlyOutput("scatter_serv_gest", height = "280px"), color = "#42A5F5")
             )
      )
    ),
    
    tags$div(
      style = "text-align:right; color:#546E7A; font-size:0.72em; padding:10px 20px 20px;",
      "Fuente: 14 bases de datos del sistema ganadero  |  ",
      paste0("Actualización: ", format(Sys.Date(), "%d/%m/%Y"))
    )
  )
)

# ============================================================
#   SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ---- Limpiar filtros ----
  observeEvent(input$limpiar, {
    updateSelectInput(session, "fil_anio",    selected = "Todos")
    updateSelectInput(session, "fil_loc",     selected = "Todas")
    updateSelectInput(session, "fil_estatus", selected = "Todos")
    updateSelectInput(session, "fil_grupo",   selected = "Todos")
  })
  
  # ---- Reactivos de datos filtrados ----
  prod_f <- reactive({
    d <- PRODUCCION
    if (input$fil_grupo   != "Todos") d <- d %>% filter(Grupo   == as.numeric(input$fil_grupo))
    if (input$fil_estatus != "Todos") d <- d %>% filter(Estatus == input$fil_estatus)
    d
  })
  
  insem_f <- reactive({
    d <- INSEMINADAS
    if (input$fil_anio != "Todos") d <- d %>% filter(year(Fecha_Insem) == as.numeric(input$fil_anio))
    if (input$fil_loc  != "Todas") d <- d %>% filter(Localidad == as.numeric(input$fil_loc))
    d
  })
  
  # ============================================================
  #   KPIs — Sidebar
  # ============================================================
  output$kpi_sidebar <- renderUI({
    total_vacas   <- nrow(PRODUCCION) + nrow(VACAS_SECAS_)
    total_vaq     <- nrow(VAQUILLAS_GENERAL)
    en_prod       <- nrow(PRODUCCION %>% filter(!grepl("Seca|Prepa", Estatus)))
    gestantes     <- nrow(VACAS_GESTANTES)
    prox_parto    <- nrow(PROXIMAS_PARTO)
    ev_mast       <- nrow(MASTITIS)
    problema      <- nrow(VACAS_PROBLEMA)
    
    make_kpi <- function(num, label, color, icon) {
      tags$div(class = "kpi-card", style = paste0("border-left: 3px solid ", color, ";"),
               tags$div(class = "kpi-icon", icon),
               tags$div(class = "kpi-number", style = paste0("color:", color), num),
               tags$div(class = "kpi-label", label)
      )
    }
    tags$div(style = "padding:0 10px;",
             make_kpi(total_vacas + total_vaq, "Total Vacas/Vaquillas", PAL$azul_claro, "🐄"),
             make_kpi(en_prod,   "En Producción",     PAL$verde_claro, "🥛"),
             make_kpi(gestantes, "Gestantes",          PAL$naranja_cl,  "🤰"),
             make_kpi(prox_parto,"Próximas a Parto",   PAL$amarillo,    "📅"),
             make_kpi(ev_mast,   "Eventos Mastitis",   PAL$morado_cl,   "⚕️"),
             make_kpi(problema,  "Vacas Problema",     PAL$rojo_cl,     "⚠️")
    )
  })
  
  # ---- KPIs fila superior ----
  kpi_box <- function(valor, label, color) {
    tags$div(class = "kpi-card",
             style = paste0("border-top: 3px solid ", color, "; padding:18px 10px;"),
             tags$div(class = "kpi-number", style = paste0("color:", color, "; font-size:2.4em;"), valor),
             tags$div(class = "kpi-label", label)
    )
  }
  
  output$kpi_total    <- renderUI(kpi_box(nrow(PRODUCCION) + nrow(VACAS_SECAS_) + nrow(VAQUILLAS_GENERAL), "TOTAL VACAS / VAQUILLAS", PAL$azul_claro))
  output$kpi_prod     <- renderUI(kpi_box(round(mean(prod_f()$Leche_Total, na.rm=TRUE), 2), "PROD. PROM. (LTS/DÍA)", PAL$verde_claro))
  output$kpi_gest     <- renderUI(kpi_box(nrow(VACAS_GESTANTES), "GESTANTES", PAL$naranja_cl))
  output$kpi_prox_parto <- renderUI(kpi_box(nrow(PROXIMAS_PARTO), "PRÓXIMAS A PARTO", PAL$amarillo))
  output$kpi_mastitis <- renderUI(kpi_box(nrow(MASTITIS), "EVENTOS MASTITIS", PAL$morado_cl))
  output$kpi_problema <- renderUI(kpi_box(nrow(VACAS_PROBLEMA), "VACAS PROBLEMA", PAL$rojo_cl))
  
  # ============================================================
  #   GRÁFICOS
  # ============================================================
  
  # ---- 1. Pie: Preñez actual ----
  output$pie_prenez <- renderPlotly({
    d <- PRENEZ %>%
      mutate(cat = ifelse(grepl("Preñ|Insem", Estatus, ignore.case=TRUE),
                          "Preñada/Inseminada", "Vacía")) %>%
      count(cat)
    plot_ly(d, labels = ~cat, values = ~n, type = "pie",
            marker = list(colors = c(PAL$azul, PAL$naranja_cl)),
            textinfo = "label+percent+value",
            hoverinfo = "label+value",
            textfont = list(size = 11)) %>%
      tema_plotly()
  })
  
  # ---- 2. Pie: Estado productivo ----
  output$pie_estado_prod <- renderPlotly({
    d <- bind_rows(
      PRODUCCION %>% mutate(tipo = ifelse(grepl("Seca|Prepa", Estatus), "Vacas Secas", "Vacas Ordeño")),
      VACAS_SECAS_ %>% mutate(tipo = "Vacas Secas")
    ) %>% count(tipo)
    plot_ly(d, labels = ~tipo, values = ~n, type = "pie",
            marker = list(colors = c(PAL$verde, PAL$azul)),
            textinfo = "label+percent+value",
            textfont = list(size = 11)) %>%
      tema_plotly()
  })
  
  # ---- 3. Pie: Distribución por lactancia ----
  output$pie_lactancia <- renderPlotly({
    d <- prod_f() %>%
      filter(!is.na(DIM)) %>%
      mutate(lac = case_when(
        DIM <= 120  ~ "LC1 (0-120 días)",
        DIM <= 240  ~ "LC2 (121-240 días)",
        TRUE        ~ "LC3+ (>240 días)"
      )) %>% count(lac)
    plot_ly(d, labels = ~lac, values = ~n, type = "pie",
            marker = list(colors = c(PAL$azul, PAL$naranja_cl, PAL$verde_claro)),
            textinfo = "label+percent+value",
            textfont = list(size = 11)) %>%
      tema_plotly()
  })
  
  # ---- 4. Pie: Vacas secas detalle ----
  output$pie_secas <- renderPlotly({
    d <- VACAS_SECAS_ %>%
      mutate(cat = case_when(
        grepl("Prepa|Preñ", Estatus) ~ "Seca Preñada",
        grepl("Insem", Estatus)      ~ "Seca Inseminada",
        TRUE                         ~ "Seca Vacía"
      )) %>% count(cat)
    plot_ly(d, labels = ~cat, values = ~n, type = "pie",
            marker = list(colors = c(PAL$azul, PAL$naranja_cl, PAL$gris_cl)),
            textinfo = "label+percent+value",
            textfont = list(size = 11)) %>%
      tema_plotly()
  })
  
  # ---- 5. Tabla indicadores clave ----
  output$tabla_indicadores <- renderUI({
    total_hato   <- nrow(PRODUCCION) + nrow(VACAS_SECAS_)
    vacas_c      <- nrow(PRENEZ %>% filter(grepl("Preñ|Insem", Estatus)))
    tc_vacas     <- round(vacas_c / total_hato * 100, 2)
    lc1          <- nrow(prod_f() %>% filter(DIM <= 120))
    lc_gt1       <- nrow(prod_f() %>% filter(DIM > 120))
    tc_1lc       <- round(lc1 / (lc1 + lc_gt1) * 100, 2)
    tc_gt1lc     <- round(lc_gt1 / (lc1 + lc_gt1) * 100, 2)
    mast_pct     <- round(nrow(MASTITIS) / total_hato * 100, 2)
    prom_serv    <- round(mean(PRENEZ$Servicios, na.rm=TRUE), 2)
    dias_ab      <- round(mean(INSEMINADAS$Dias_Abiertos, na.rm=TRUE), 1)
    edad_prom    <- round(mean(VAQUILLAS_GENERAL$Edad_Meses, na.rm=TRUE), 1)
    vaq_pct      <- round(nrow(VAQUILLAS_PRENIEZ %>% filter(grepl("Preñ", Estatus))) /
                            nrow(VAQUILLAS_PRENIEZ) * 100, 2)
    prob_pct     <- round(nrow(VACAS_PROBLEMA) / total_hato * 100, 2)
    salida_pct   <- round(nrow(VACAS_PROBLEMA %>% filter(Estatus == "Matar")) /
                            total_hato * 100, 2)
    
    indicadores <- list(
      c("%TC VACAS",            paste0(tc_vacas, "%")),
      c("%TC VACAS 1LC",        paste0(tc_1lc, "%")),
      c("%TC VACAS >1LC",       paste0(tc_gt1lc, "%")),
      c("%MASTITIS",            paste0(mast_pct, "%")),
      c("PROM Nº SERVICIO",     prom_serv),
      c("DÍAS ABIERTOS",        dias_ab),
      c("EDAD PROM VAQ (meses)",edad_prom),
      c("%PREÑEZ VAQUILLAS",    paste0(vaq_pct, "%")),
      c("%VACAS PROBLEMA",      paste0(prob_pct, "%")),
      c("%SALIDA VACA",         paste0(salida_pct, "%"))
    )
    
    filas <- lapply(indicadores, function(x) {
      tags$tr(tags$td(x[1]), tags$td(x[2]))
    })
    tags$table(class = "ind-table", do.call(tags$tbody, filas))
  })
  
  # ---- 6. Bar: Mastitis por mes ----
  output$bar_mastitis_mes <- renderPlotly({
    d <- MASTITIS %>%
      filter(!is.na(Fecha_Evento)) %>%
      mutate(Mes = floor_date(Fecha_Evento, "month")) %>%
      count(Mes) %>%
      arrange(Mes) %>%
      mutate(pct = round(n / nrow(PRODUCCION) * 100, 2),
             mes_label = format(Mes, "%b %Y"))
    plot_ly(d, x = ~mes_label, y = ~pct, type = "bar",
            marker = list(color = PAL$morado,
                          line = list(color = PAL$morado_cl, width = 1.5)),
            text = ~paste0(pct, "%"), textposition = "outside",
            textfont = list(size = 10, color = "#E8F4FD"),
            hovertemplate = "%{x}<br>%{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Mes"), yaxis = list(title = "% Mastitis")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 7. Pie: Recría vs Total ----
  output$pie_recria <- renderPlotly({
    d <- data.frame(
      cat = c("Vacas Adultas", "Recría Total"),
      n   = c(nrow(PRODUCCION) + nrow(VACAS_SECAS_), nrow(VAQUILLAS_GENERAL))
    )
    plot_ly(d, labels = ~cat, values = ~n, type = "pie",
            marker = list(colors = c(PAL$azul, PAL$naranja_cl)),
            textinfo = "label+percent+value",
            textfont = list(size = 11)) %>%
      tema_plotly()
  })
  
  # ---- 8. Bar: Inseminaciones por mes ----
  output$bar_insem_mes <- renderPlotly({
    d <- insem_f() %>%
      filter(!is.na(Fecha_Insem)) %>%
      count(Mes = floor_date(Fecha_Insem, "month")) %>%
      arrange(Mes) %>%
      mutate(mes_label = format(Mes, "%b %Y"))
    plot_ly(d, x = ~mes_label, y = ~n, type = "bar",
            marker = list(color = PAL$azul,
                          line = list(color = PAL$azul_claro, width = 1.5)),
            text = ~n, textposition = "outside",
            textfont = list(size = 10, color = "#E8F4FD"),
            hovertemplate = "%{x}<br>%{y} inseminaciones<extra></extra>") %>%
      layout(xaxis = list(title = "Mes"), yaxis = list(title = "Nº Inseminaciones")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 9. Bar: Vaquillas preñez por grupo ----
  output$bar_vaq_prenez <- renderPlotly({
    d <- VAQUILLAS_PRENIEZ %>%
      count(Grupo, Estatus) %>%
      mutate(Grupo = factor(Grupo))
    plot_ly(d, x = ~Grupo, y = ~n, color = ~Estatus,
            type = "bar",
            colors = c(PAL$azul, PAL$verde_claro, PAL$naranja_cl),
            hovertemplate = "Grupo %{x}<br>%{y} vaquillas<extra></extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Grupo"),
             yaxis = list(title = "Nº Vaquillas")) %>%
      tema_plotly()
  })
  
  # ---- 10. Box: Días abiertos por localidad ----
  output$box_dias_abiertos <- renderPlotly({
    d <- insem_f() %>% filter(!is.na(Dias_Abiertos), !is.na(Localidad))
    plot_ly(d, x = ~factor(Localidad), y = ~Dias_Abiertos,
            type = "box",
            marker = list(color = PAL$naranja_cl, opacity = 0.7),
            line   = list(color = PAL$naranja),
            fillcolor = "rgba(230,81,0,0.25)",
            hovertemplate = "Loc %{x}<br>%{y} días<extra></extra>") %>%
      layout(xaxis = list(title = "Localidad"),
             yaxis = list(title = "Días Abiertos")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 11. Curva de producción de leche ----
  output$curva_leche <- renderPlotly({
    d <- prod_f() %>%
      filter(!is.na(DIM), !is.na(Leche_Total)) %>%
      arrange(DIM)
    plot_ly() %>%
      add_trace(data = d, x = ~DIM, y = ~Leche_Total,
                type = "scatter", mode = "markers",
                marker = list(color = PAL$azul_claro, size = 4, opacity = 0.5),
                name = "Leche Total (lts)") %>%
      add_trace(data = d %>% mutate(smooth = stats::lowess(DIM, Leche_Total)$y),
                x = ~DIM, y = ~smooth,
                type = "scatter", mode = "lines",
                line = list(color = PAL$naranja_cl, width = 2.5),
                name = "Tendencia") %>%
      layout(xaxis = list(title = "DIM (Días en Leche)"),
             yaxis = list(title = "Litros / Día")) %>%
      tema_plotly()
  })
  
  # ---- 12. Tasa concepción por nº servicio ----
  output$bar_tc_servicio <- renderPlotly({
    d <- PRENEZ %>%
      group_by(Servicios) %>%
      summarise(
        total = n(),
        preniadas = sum(grepl("Preñ", Estatus), na.rm=TRUE),
        .groups = "drop"
      ) %>%
      mutate(tc = round(preniadas / total * 100, 1)) %>%
      filter(Servicios <= 8)
    
    plot_ly(d, x = ~Servicios, y = ~tc, type = "bar",
            marker = list(color = PAL$verde,
                          line = list(color = PAL$verde_claro, width = 1.5)),
            text = ~paste0(tc, "%"), textposition = "outside",
            textfont = list(size = 10, color = "#E8F4FD"),
            hovertemplate = "Servicio %{x}<br>TC: %{y:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Número de Servicio", dtick = 1),
             yaxis = list(title = "Tasa Concepción (%)")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 13. Tabla próximos partos ----
  output$tabla_proximos_partos <- renderDT({
    d <- PROXIMAS_PARTO %>%
      select(ID_Vaca, Nombre, Lact_N, Fecha_Parto_Est, Dias_Faltan, Estatus) %>%
      arrange(Dias_Faltan) %>%
      mutate(Fecha_Parto_Est = format(Fecha_Parto_Est, "%d-%m-%y"))
    datatable(d, options = list(
      pageLength = 8, dom = "tip",
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ), rownames = FALSE,
    colnames = c("ID", "Nombre", "Lact", "F.Parto Est.", "Días", "Estatus")) %>%
      formatStyle("Dias_Faltan",
                  color = styleInterval(c(-30, 0), c("#EF5350", "#FFA726", "#66BB6A")),
                  fontWeight = "bold")
  })
  
  # ---- 14. Bar: Producción promedio por estatus ----
  output$bar_prod_estatus <- renderPlotly({
    d <- prod_f() %>%
      filter(!is.na(Leche_Total)) %>%
      group_by(Estatus) %>%
      summarise(prom = round(mean(Leche_Total, na.rm=TRUE), 1), .groups="drop") %>%
      arrange(desc(prom))
    plot_ly(d, x = ~reorder(Estatus, -prom), y = ~prom, type = "bar",
            marker = list(color = PAL$verde,
                          line = list(color = PAL$verde_claro, width = 1.5)),
            text = ~prom, textposition = "outside",
            textfont = list(size = 10, color = "#E8F4FD"),
            hovertemplate = "%{x}<br>%{y:.1f} lts<extra></extra>") %>%
      layout(xaxis = list(title = "Estatus"),
             yaxis = list(title = "Leche Prom. (lts/día)")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 15. Histograma CCS ----
  output$hist_ccs <- renderPlotly({
    d <- prod_f() %>% filter(!is.na(CCS), CCS > 0)
    plot_ly(d, x = ~log10(CCS + 1), type = "histogram",
            marker = list(color = PAL$cyan,
                          line = list(color = "#00BCD4", width = 0.5)),
            nbinsx = 30,
            hovertemplate = "log10(CCS): %{x:.1f}<br>Frecuencia: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "log10(CCS)"),
             yaxis = list(title = "Frecuencia")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 16. Box: DIM por estatus ----
  output$box_dim_estatus <- renderPlotly({
    d <- prod_f() %>% filter(!is.na(DIM))
    plot_ly(d, x = ~Estatus, y = ~DIM,
            type = "box",
            marker  = list(color = PAL$azul_claro, opacity = 0.6),
            line    = list(color = PAL$azul),
            fillcolor = "rgba(21,101,192,0.25)",
            hovertemplate = "%{x}<br>DIM: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Estatus"),
             yaxis = list(title = "DIM (Días en Leche)")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 17. Tabla vacas problema ----
  output$tabla_problema <- renderDT({
    d <- VACAS_PROBLEMA %>%
      select(ID_Vaca, Estatus, Grupo, DIM, Prod_24h, Nro_Inseminaciones, Comentarios) %>%
      arrange(desc(Nro_Inseminaciones))
    datatable(d, options = list(
      pageLength = 8, dom = "tip",
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ), rownames = FALSE,
    colnames = c("ID","Estatus","Grupo","DIM","Prod 24h","Nº Insem","Comentarios")) %>%
      formatStyle("Nro_Inseminaciones",
                  backgroundColor = styleInterval(c(3, 5),
                                                  c("rgba(46,125,50,0.2)", "rgba(230,81,0,0.2)", "rgba(183,28,28,0.3)")))
  })
  
  # ---- 18. Bar: Toros más usados ----
  output$bar_toros <- renderPlotly({
    d <- insem_f() %>%
      filter(!is.na(Toro)) %>%
      group_by(Toro) %>%
      summarise(total = sum(Cant_Insem, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 12)
    plot_ly(d, x = ~total, y = ~reorder(Toro, total), type = "bar",
            orientation = "h",
            marker = list(color = PAL$naranja,
                          line = list(color = PAL$naranja_cl, width = 1.5)),
            text = ~total, textposition = "outside",
            textfont = list(size = 10, color = "#E8F4FD"),
            hovertemplate = "%{y}<br>%{x} inseminaciones<extra></extra>") %>%
      layout(xaxis = list(title = "Nº Inseminaciones"),
             yaxis = list(title = "")) %>%
      tema_plotly(leyenda = FALSE)
  })
  
  # ---- 19. Scatter: Servicios vs Días abiertos en gestantes ----
  output$scatter_serv_gest <- renderPlotly({
    d <- VACAS_GESTANTES %>%
      filter(!is.na(Servicios), !is.na(Dias_Abiertos))
    plot_ly(d, x = ~Servicios, y = ~Dias_Abiertos,
            type = "scatter", mode = "markers",
            color = ~Estatus,
            colors = c(PAL$azul, PAL$verde_claro, PAL$naranja_cl, PAL$rojo_cl),
            marker = list(size = 8, opacity = 0.75),
            hovertemplate = "Servicios: %{x}<br>Días Abiertos: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Nº Servicios", dtick = 1),
             yaxis = list(title = "Días Abiertos")) %>%
      tema_plotly()
  })
  
}

# ============================================================
#   EJECUTAR APP
# ============================================================
shinyApp(ui = ui, server = server)
