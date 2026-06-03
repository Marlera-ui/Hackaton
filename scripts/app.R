library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(geobr)
library(sidrar)
library(nasapower) # Adicionado para busca na API em tempo real

# =====================================================================
# CONFIGURAÇÕES GLOBAIS E LEITURA DE DADOS
# =====================================================================

PRECO_SACA_BRL <- 142.00
IMPORTANCIA_SEGURADA_HA <- 6000.00
PROB_OCORRENCIA_ANUAL <- 0.20
FATOR_CARREGAMENTO <- 1.40
COEF_DANO_PADRAO <- 0.74
COEF_DANO_TARDIO <- 0.51

# 1. Carregar base climática local (Safra 2023/2024)
clima_diario_mg <- readRDS("clima_nasa_diario_safra2324.rds") |>
  mutate(data_real = as.Date(YYYYMMDD))

# 2. Obter mapa de Minas Gerais (Polígonos)
mapa_mg_poligonos <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE) |>
  mutate(code_muni = as.numeric(code_muni)) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 0.005)

# 3. Baixar dados de produtividade agrícola do SIDRA (IBGE)
dados_sidra <- get_sidra(
  x = 1612,                
  variable = c(214, 112),  
  classific = "c81",       
  category = list(2713),   
  geo = "City",            
  geo.filter = list("State" = 31),
  period = c("2022", "2023", "2024") 
)

# Processar produtividade para pegar o máximo e também a área plantada
produtividade_soja <- dados_sidra |>
  filter(!is.na(Valor)) |>
  mutate(
    codigo_ibge = as.numeric(`Município (Código)`),
    var_codigo = `Variável (Código)`,
    Valor = as.numeric(Valor)
  ) |>
  group_by(codigo_ibge, municipio = `Município`, var_codigo) |>
  summarise(valor_max = max(Valor, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = var_codigo, values_from = valor_max) |>
  rename(area_plantada_ha = `214`, prod_maxima = `112`) |>
  filter(!is.na(prod_maxima) & !is.na(area_plantada_ha))

municipios_produtores <- sort(unique(produtividade_soja$municipio))


# =====================================================================
# INTERFACE DO USUÁRIO (UI)
# =====================================================================

meu_tema <- bs_theme(
  version = 5,
  preset = "cosmo",
  primary = "#228B22",   # Verde Floresta
  secondary = "#FFA500", # Laranja
  success = "#228B22",   
  info = "#FFD700"       # Amarelo
)

ui <- page_navbar(
  title = "Gestão de Risco: Ferrugem da Soja",
  theme = meu_tema,
  
  sidebar = sidebar(
    title = "Filtros Globais",
    HTML("<h5>Modo de Visualização</h5>"),
    # Switch para alternar entre dark mode e light mode nativo do bslib
    input_dark_mode(id = "dark_mode_toggle")
  ),
  
  # --- ABA INICIAL ---
  nav_panel(
    title = "Início",
    fluidRow(
      column(12, align = "center",
             br(),
             h1("Gestão de Risco Agroclimático: Ferrugem da Soja"),
             h4("Modelo de Impacto Fitossanitário e Atuarial em Minas Gerais", style="color: #666;"),
             br(),
             # Placeholder de imagem. Quando quiser, substitua a URL ou crie uma pasta www/ e aponte src="imagem.png"
             tags$img(src = "fotocapa.png", 
                      width = "100%", style = "max-width: 800px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"),
             br(), br(), br(),
             h5("Desenvolvedores:"),
             p("Camila, João, Maria e Marlon", style="font-size: 18px; font-weight: bold;"),
             br()
      )
    )
  ),
  
  # --- ABA 1: ANÁLISE ESPACIAL ---
  nav_panel(
    title = "1. Análise Espacial e Ranking",
    layout_sidebar(
      sidebar = sidebar(
        # Calendário travado na Safra 23/24 para garantir mapa instantâneo
        dateInput("data_infeccao_aba1", "Data de Início da Infecção:", 
                  value = "2024-01-15", min = "2023-11-01", max = "2024-02-29"),
        helpText("Nota: A análise espacial estadual está otimizada para dados locais (Safra 23/24) visando processamento instantâneo.")
      ),
      layout_columns(
        col_widths = c(12, 12),
        card(
          card_header("Mapa de Calor do Risco Estimado"),
          leafletOutput("mapa_risco", height = "450px")
        ),
        card(
          card_header("Ranking de Vulnerabilidade Econômica"),
          DTOutput("tabela_ranking")
        )
      )
    )
  ),
  
  # --- ABA 2: PRECIFICAÇÃO DE SEGURO ---
  nav_panel(
    title = "2. Precificação de Seguro",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("mun_aba2", "Município do Segurado:", choices = municipios_produtores),
        # Calendário livre - usa API da NASA se for fora da Safra 23/24
        dateInput("data_plantio_aba2", "Data de Plantio:", value = "2023-11-15"),
        numericInput("area_aba2", "Área Plantada (ha):", value = 500, min = 1),
        hr(),
        helpText("O surgimento da doença é estipulado atuarialmente para 45 dias após o plantio.")
      ),
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        value_box(
          title = "Classe de Risco",
          value = textOutput("vb_classe_risco"),
          theme = "primary"
        ),
        value_box(
          title = "Taxa do Seguro",
          value = textOutput("vb_taxa_seguro"),
          theme = "secondary"
        ),
        value_box(
          title = "Prêmio (R$/ha)",
          value = textOutput("vb_premio_ha"),
          theme = "info"
        ),
        value_box(
          title = "Prêmio Total Apólice",
          value = textOutput("vb_premio_total"),
          theme = "success"
        )
      )
    )
  ),
  
  # --- ABA 3: REGULAÇÃO DE SINISTRO ---
  nav_panel(
    title = "3. Regulação de Sinistro",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("mun_aba3", "Município Atingido:", choices = municipios_produtores),
        # Calendário livre - usa API da NASA se for fora da Safra 23/24
        dateInput("data_doenca_aba3", "Data de Início da Doença:", value = "2024-01-15"),
        selectInput("feno_aba3", "Estádio Fenológico:", 
                    choices = c("Antes de R1 (Precoce)", "Depois de R1 (Tardio)")),
        numericInput("area_aba3", "Área Afetada (ha):", value = 500, min = 1)
      ),
      layout_columns(
        col_widths = c(4, 4, 4, 12),
        value_box(
          title = "Coeficiente de Dano",
          value = textOutput("vb_coef_dano"),
          theme = "primary"
        ),
        value_box(
          title = "Perda Ajustada (kg/ha)",
          value = textOutput("vb_perda_kgha"),
          theme = "secondary"
        ),
        value_box(
          title = "Reembolso Total Estimado",
          value = textOutput("vb_reembolso"),
          theme = "success"
        ),
        card(
          card_header("Laudo Preliminar do Sinistro"),
          DTOutput("tabela_laudo")
        )
      )
    )
  ),
  
  # --- ABA FINAL: SOBRE ---
  nav_panel(
    title = "Sobre",
    fluidRow(
      column(8, offset = 2,
             br(),
             h3("Sobre o Aplicativo"),
             p("Este aplicativo interativo auxilia a avaliar o risco de ferrugem asiática da soja com base em dados meteorológicos de pluviosidade ao longo do ciclo da cultura nas cidades que mais plantam soja no estado de Minas Gerais."),
             hr(),
             h4("Objetivo"),
             p("Auxiliar na tomada de decisões de uma seguradora agrícola, com base nos riscos fitossanitários, acerca do seu planejamento econômico."),
             br(),
             h4("Metodologia e Referências"),
             p("O modelo de risco integra dados de produtividade municipal do IBGE com precipitação diária via satélite da NASA POWER."),
             p("A severidade fitossanitária é calculada através do modelo de predição desenvolvido por Del Ponte et al. (2006). A conversão da severidade da doença para a perda de produtividade e impacto econômico aplica o Coeficiente de Dano relativo ao estádio fenológico, demonstrado nas literaturas agronômicas mais atuais."),
             br(),
             p("Este projeto foi desenvolvido como protótipo durante a disciplina FIP 606 - ANÁLISE E VISUALIZAÇÃO DE DADOS EM FITOPATOLOGIA.")
      )
    )
  )
)

# =====================================================================
# SERVIDOR (SERVER)
# =====================================================================

server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  # Funções de Busca Climática
  # -------------------------------------------------------------------
  
  # 1. Consulta Local Rápida (Para Aba 1 e datas dentro da safra 23/24)
  get_chuva_30d_local <- function(data_foco, codigo_ibge_filtro = NULL) {
    data_limite <- data_foco + 30
    df <- clima_diario_mg |>
      filter(data_real >= data_foco & data_real <= data_limite)
    
    if (!is.null(codigo_ibge_filtro)) {
      df <- df |> filter(codigo_ibge == codigo_ibge_filtro)
    }
    
    df |>
      group_by(codigo_ibge) |>
      summarise(chuva_acumulada = sum(PRECTOTCORR, na.rm = TRUE), .groups = "drop")
  }
  
  # 2. Consulta Dinâmica (Local ou API da NASA para Abas 2 e 3)
  get_chuva_30d_dinamico <- function(data_foco, codigo_ibge_filtro) {
    data_limite <- data_foco + 30
    
    # Se estiver 100% dentro do arquivo local, usa o local para ser rápido
    if (data_foco >= as.Date("2023-11-01") && data_limite <= as.Date("2024-03-31")) {
      return(get_chuva_30d_local(data_foco, codigo_ibge_filtro))
    }
    
    # Se estiver fora, busca via API
    muni_sf <- mapa_mg_poligonos |> filter(code_muni == codigo_ibge_filtro)
    if(nrow(muni_sf) == 0) return(data.frame())
    
    # Extrai centroide de forma segura para a geometria
    # Suppress warnings caso existam de st_centroid
    coords <- suppressWarnings(st_centroid(muni_sf) |> st_coordinates())
    lon <- coords[1, 1]
    lat <- coords[1, 2]
    
    dados_nasa <- tryCatch({
      get_power(
        community = "ag",
        lonlat = c(lon, lat),
        pars = "PRECTOTCORR", 
        dates = c(data_foco, data_limite),
        temporal_api = "daily"
      )
    }, error = function(e) { return(NULL) })
    
    if(is.null(dados_nasa) || nrow(dados_nasa) == 0) return(data.frame())
    
    # Sumariza
    res <- dados_nasa |>
      summarise(chuva_acumulada = sum(PRECTOTCORR, na.rm = TRUE)) |>
      mutate(codigo_ibge = codigo_ibge_filtro)
      
    return(res)
  }
  
  # -------------------------------------------------------------------
  # ABA 1
  # -------------------------------------------------------------------
  
  dados_aba1 <- reactive({
    req(input$data_infeccao_aba1)
    
    chuva_30dias <- get_chuva_30d_local(input$data_infeccao_aba1)
    
    produtividade_soja |>
      inner_join(chuva_30dias, by = "codigo_ibge") |>
      mutate(
        sev_calculada = -3.8983 + (0.3777 * chuva_acumulada) - (0.0003 * (chuva_acumulada^2)),
        severidade_final_pct = pmax(0, pmin(100, sev_calculada)),
        perda_kg_ha = prod_maxima * (COEF_DANO_PADRAO * (severidade_final_pct / 100)),
        perda_total_ton = (area_plantada_ha * perda_kg_ha) / 1000,
        prejuizo_total_milhoes = ( (area_plantada_ha * perda_kg_ha) / 60 * PRECO_SACA_BRL ) / 1000000
      ) |>
      arrange(desc(prejuizo_total_milhoes))
  })
  
  output$mapa_risco <- renderLeaflet({
    dados <- dados_aba1()
    
    mapa_interativo <- mapa_mg_poligonos |>
      left_join(dados, by = c("code_muni" = "codigo_ibge")) |>
      st_transform(crs = 4326)
    
    paleta <- colorNumeric(
      palette = "YlOrRd",
      domain = mapa_interativo$prejuizo_total_milhoes,
      na.color = "#EAEAEA"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Perda Física: %s kg/ha<br/>Prejuízo: R$ %s Milhões",
      mapa_interativo$name_muni,
      ifelse(is.na(mapa_interativo$perda_kg_ha), "N/A", format(round(mapa_interativo$perda_kg_ha, 1), decimal.mark=",")),
      ifelse(is.na(mapa_interativo$prejuizo_total_milhoes), "N/A", format(round(mapa_interativo$prejuizo_total_milhoes, 2), decimal.mark=","))
    ) |> lapply(htmltools::HTML)
    
    leaflet(mapa_interativo) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~paleta(prejuizo_total_milhoes),
        weight = 1.2, opacity = 1, color = "#909090", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 2.5, color = "#333333", fillOpacity = 0.95, bringToFront = TRUE),
        label = labels
      ) |>
      addLegend(pal = paleta, values = ~prejuizo_total_milhoes, title = "Prejuízo (Mi R$)", position = "bottomright")
  })
  
  output$tabela_ranking <- renderDT({
    dados <- dados_aba1() |>
      select(
        `Município` = municipio,
        `Área (ha)` = area_plantada_ha,
        `Prod. Potencial (kg/ha)` = prod_maxima,
        `Severidade (%)` = severidade_final_pct,
        `Perda Estimada (kg/ha)` = perda_kg_ha,
        `Prejuízo Total (Mi R$)` = prejuizo_total_milhoes
      ) |>
      mutate(across(where(is.numeric), ~round(.x, 2)))
      
    datatable(
      dados, 
      options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100)), 
      rownames = FALSE,
      selection = "none"
    ) |>
    formatCurrency("Prejuízo Total (Mi R$)", currency = "R$ ", mark = ".", dec.mark = ",")
  })
  
  # -------------------------------------------------------------------
  # ABA 2
  # -------------------------------------------------------------------
  
  dados_aba2 <- reactive({
    req(input$mun_aba2, input$data_plantio_aba2, input$area_aba2)
    
    data_doenca <- input$data_plantio_aba2 + 45
    prod_muni <- produtividade_soja |> filter(municipio == input$mun_aba2)
    codigo <- prod_muni$codigo_ibge[1]
    
    # Barra de progresso para caso a API seja acionada
    withProgress(message = 'Consultando banco climático...', value = 0.5, {
      chuva <- get_chuva_30d_dinamico(data_doenca, codigo)
    })
    
    if(nrow(chuva) == 0) return(NULL)
    
    sev <- -3.8983 + (0.3777 * chuva$chuva_acumulada[1]) - (0.0003 * (chuva$chuva_acumulada[1]^2))
    sev <- pmax(0, pmin(100, sev))
    
    perda_kg <- prod_muni$prod_maxima[1] * (COEF_DANO_PADRAO * (sev / 100))
    reducao_pct <- (perda_kg / prod_muni$prod_maxima[1]) * 100
    
    classe <- case_when(
      reducao_pct <= 5.0 ~ "I. Baixo Risco",
      reducao_pct <= 10.0 ~ "II. Risco Moderado",
      reducao_pct <= 15.0 ~ "III. Risco Alto",
      reducao_pct <= 20.0 ~ "IV. Muito Alto",
      TRUE ~ "V. Risco Crítico"
    )
    
    taxa_puro <- reducao_pct * PROB_OCORRENCIA_ANUAL
    taxa_comercial <- taxa_puro * FATOR_CARREGAMENTO
    premio_ha <- (taxa_comercial / 100) * IMPORTANCIA_SEGURADA_HA
    premio_total <- premio_ha * input$area_aba2
    
    list(
      classe = classe,
      taxa = taxa_comercial,
      premio_ha = premio_ha,
      premio_total = premio_total
    )
  })
  
  output$vb_classe_risco <- renderText({
    res <- dados_aba2()
    if(is.null(res)) return("Sem dados da API")
    res$classe
  })
  
  output$vb_taxa_seguro <- renderText({
    res <- dados_aba2()
    if(is.null(res)) return("-")
    paste0(format(round(res$taxa, 2), decimal.mark=","), "%")
  })
  
  output$vb_premio_ha <- renderText({
    res <- dados_aba2()
    if(is.null(res)) return("-")
    paste0("R$ ", format(round(res$premio_ha, 2), big.mark=".", decimal.mark=","))
  })
  
  output$vb_premio_total <- renderText({
    res <- dados_aba2()
    if(is.null(res)) return("-")
    paste0("R$ ", format(round(res$premio_total, 2), big.mark=".", decimal.mark=","))
  })
  
  # -------------------------------------------------------------------
  # ABA 3
  # -------------------------------------------------------------------
  
  dados_aba3 <- reactive({
    req(input$mun_aba3, input$data_doenca_aba3, input$area_aba3)
    
    prod_muni <- produtividade_soja |> filter(municipio == input$mun_aba3)
    codigo <- prod_muni$codigo_ibge[1]
    
    # Barra de progresso para caso a API seja acionada
    withProgress(message = 'Consultando banco climático...', value = 0.5, {
      chuva <- get_chuva_30d_dinamico(input$data_doenca_aba3, codigo)
    })
    
    if(nrow(chuva) == 0) return(NULL)
    
    sev <- -3.8983 + (0.3777 * chuva$chuva_acumulada[1]) - (0.0003 * (chuva$chuva_acumulada[1]^2))
    sev <- pmax(0, pmin(100, sev))
    
    # Lógica Agronômica (0.74 precoce, 0.51 tardio)
    coef <- ifelse(input$feno_aba3 == "Antes de R1 (Precoce)", COEF_DANO_PADRAO, COEF_DANO_TARDIO)
    
    perda_real_kgha <- prod_muni$prod_maxima[1] * (coef * (sev / 100))
    perda_sacas <- perda_real_kgha / 60
    
    reembolso_total <- perda_sacas * PRECO_SACA_BRL * input$area_aba3
    
    list(
      coef = coef,
      perda_kgha = perda_real_kgha,
      reembolso = reembolso_total,
      prod_maxima = prod_muni$prod_maxima[1],
      severidade = sev,
      chuva = chuva$chuva_acumulada[1]
    )
  })
  
  output$vb_coef_dano <- renderText({
    res <- dados_aba3()
    if(is.null(res)) return("-")
    format(res$coef, decimal.mark=",")
  })
  
  output$vb_perda_kgha <- renderText({
    res <- dados_aba3()
    if(is.null(res)) return("-")
    format(round(res$perda_kgha, 1), decimal.mark=",")
  })
  
  output$vb_reembolso <- renderText({
    res <- dados_aba3()
    if(is.null(res)) return("-")
    paste0("R$ ", format(round(res$reembolso, 2), big.mark=".", decimal.mark=","))
  })
  
  output$tabela_laudo <- renderDT({
    res <- dados_aba3()
    if(is.null(res)) {
      return(datatable(data.frame(Mensagem = "Não foi possível obter dados climáticos da API para esta data.")))
    }
    
    laudo <- data.frame(
      `Município Segurado` = input$mun_aba3,
      `Área Afetada (ha)` = input$area_aba3,
      `Fase da Doença` = input$feno_aba3,
      `Chuva Acumulada` = paste0(round(res$chuva, 1), " mm"),
      `Severidade (%)` = paste0(round(res$severidade, 1), "%"),
      `Perda Ajustada` = paste0(format(round(res$perda_kgha, 1), decimal.mark=","), " kg/ha"),
      `Reembolso Final` = paste0("R$ ", format(round(res$reembolso, 2), big.mark=".", decimal.mark=","))
    )
    
    datatable(laudo, options = list(dom = 't', bSort = FALSE), rownames = FALSE)
  })
  
}

# Iniciar App
shinyApp(ui, server)
