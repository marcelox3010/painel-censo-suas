# ---
# GLOBALS
# ---

library(shiny)
library(bs4Dash)
library(readr)
library(dplyr)
library(ggplot2)
library(digest)


hash_sha1_dadosgerais <- digest(
  file = "dados/2024/CRAS-2024-DADOSGERAIS.csv", 
  algo = "sha1", 
  serialize = FALSE
)

hash_sha1_rh <- digest(
  file = "dados/2024/CRAS-2024-RH.csv", 
  algo = "sha1",
  serialize = FALSE
)

df_regioes <- data.frame(
  codigo_ibge_regiao =  c(1, 2, 3, 4, 5),
  nome_regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
  cor_hex = c(
    "#228B22",  # Verde (Norte)
    "#FFD700",  # Dourado (Nordeste)
    "#4169E1",  # Azul (Sudeste)
    "#B22222",  # Vermelho/Bordô (Sul)
    "#FF8C00"   # Laranja (Centro-Oeste)
  )
)

codigos_ibge <- c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53)
nomes_estados <- c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal")
siglas_estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")

df_estados <- data.frame(
  codigo_ibge_estado = codigos_ibge,
  nome = nomes_estados,
  sigla = siglas_estados
)
df_estados$codigo_ibge_regiao <- trunc(df_estados$codigo_ibge_estado/10)

df_municipios <- read_csv("dados/df-municipios.csv")
df_municipios <- inner_join(df_municipios,df_estados,c("uf" = "codigo_ibge_estado"))
df_municipios <- df_municipios %>% mutate(nome_completo = paste0(sigla, "-", nome_municipio, sep = ""))

CRAS_2024_DADOSGERAIS <- read_delim("dados/2024/CRAS-2024-DADOSGERAIS.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                    trim_ws = TRUE)

CRAS_2024_DADOSGERAIS$IBGE_REGIAO <- trunc(CRAS_2024_DADOSGERAIS$IBGE/100000)


CRAS_2024_RH <- read_delim("dados/2024/CRAS-2024-RH.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                           trim_ws = TRUE)

formatarNumeroInteiro <- function(n){
  
  format(n, big.mark = ".", decimal.mark = ",", nsmall = 0)
  
}

colocaCodigoIBGE <- function(x){
  
  sapply(x,function(x){
    
    if(grepl("-", x)) df_municipios[df_municipios$nome_completo==x,]$codigo_municipio_completo 
    else df_estados[df_estados$nome==x,]$codigo_ibge_estado
      
  })
  
}

qtdCRAS <- function(codigosGeo){
  
    if(length(codigosGeo)==0) nrow(CRAS_2024_DADOSGERAIS)
    else nrow(CRAS_2024_DADOSGERAIS %>%
                filter(IBGE7 %in% codigosGeo[codigosGeo>100] 
                       | trunc(IBGE7/100000) %in% codigosGeo[codigosGeo<100]))
  
}

qtdProfissionais <- function(codigosGeo){
  
  if(length(codigosGeo)==0) nrow(CRAS_2024_RH)
  else nrow(CRAS_2024_RH %>%
              filter(IBGE %in% trunc(codigosGeo[codigosGeo>100]/10) 
                     | trunc(IBGE/10000) %in% codigosGeo[codigosGeo<100]))
  
}

filtroGeral <- function(){
  
  if(req(input$geo_selecionado)){
    
  }
  
}



# ---
# UI
# ---

ui <- dashboardPage(
  title = "Censo SUAS",
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Censo SUAS 2024 - CRAS",
      href = "#"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "dashboard", icon = icon("home")),
      menuItem("CRAS", tabName = "graficos", icon = icon("chart-line")),
      menuItem("Sobre", tabName = "sobre", icon = icon("chart-line"))
    )
  ),
  
  body = dashboardBody(
    
    selectInput(
      inputId = "geo_selecionado",
      label = "Selecione Estados e Municípios:",
      choices = c(sort(df_estados$nome), sort(df_municipios$nome_completo)),
      multiple = TRUE
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          
          bs4ValueBoxOutput(outputId = "vb_qtd_cras"),
          bs4ValueBoxOutput(outputId = "vb_qtd_cras_rh")
          
        )
      ),
      tabItem(
        tabName = "graficos",
        fluidRow(
          bs4Card(
            title = "Exemplo de Gráfico",
            width = 12,
            plotOutput("grafico")
          )
        )
      ),
      tabItem(
        tabName = "sobre",
        fluidRow(
         p("Painel não oficial do Censo SUAS 2024"),  
         p("Arquivos originais baixados em: http://aplicacoes.mds.gov.br/sagi/snas/vigilancia/index2.php"), 
         p(paste("HASH Dados Gerais",
                 hash_sha1_dadosgerais,
                 tags$br(),
                 paste("HASH RH",hash_sha1_rh))), 
         
         p("Download do questionários CRAS 2024:"),
         a("clique aqui", 
           href = "https://paineis.gadelha.pro/censosuas/questinario/CensoSUAS_2024_CRAS.pdf", 
           target = "_blank")
         
        )
      )
    )
  )
)

# ---
# SERVER
# ---

server <- function(input, output, session) {

  output$grafico <- renderPlot({
    plot(cars, pch = 19)
  })
  
  output$vb_qtd_cras <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = formatarNumeroInteiro(qtdCRAS(colocaCodigoIBGE(unlist(input$geo_selecionado)))), 
      subtitle = "Qtd. CRAS",
      icon = icon("users")
    )
    
  })
  
  output$vb_qtd_cras_rh <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = formatarNumeroInteiro(qtdProfissionais(colocaCodigoIBGE(unlist(input$geo_selecionado)))),
      subtitle = "Qtd. Profissionais CRAS",
      icon = icon("users")
    )
    
  })
  
}

shinyApp(ui, server)
