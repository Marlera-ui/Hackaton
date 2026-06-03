Gestão de Risco: Ferrugem da Soja em Minas Gerais
Aplicativo Shiny para análise de risco agronômico e atuarial da ferrugem da soja (Phakopsora pachyrhizi) nos municípios produtores de Minas Gerais. Integra dados climáticos diários da NASA POWER com dados de produtividade do IBGE/SIDRA para estimar severidade da doença, precificar seguros agrícolas e auxiliar na regulação de sinistros.

Como executar o app
Pré-requisitos

R >= 4.3.0
RStudio (recomendado)

Instalação dos pacotes
rinstall.packages(c(
  "shiny", "bslib", "tidyverse", "sf",
  "leaflet", "DT", "geobr", "sidrar"
))
Execução

Clone o repositório:

git clone https://github.com/seu-usuario/ferrugem-soja-mg.git

Abra o RStudio e defina o diretório de trabalho como a pasta do projeto
Execute o app:

rshiny::runApp("app.R")

Os arquivos .rds em data/raw/ são necessários para rodar o app. Veja a seção Fontes de dados para saber como obtê-los.


Fontes de dados
Base de dadosFonteVariáveis utilizadasData de acessoClima diário Safra 2023/2024NASA POWERPrecipitação diária (PRECTOTCORR)dd/mm/aaaaProdutividade e área da sojaIBGE/SIDRA – Tabela 1612Área plantada (Var. 214), Produção (Var. 112)dd/mm/aaaaMalha municipal de MGIBGE via pacote geobrPolígonos municipais (ano-base 2020)dd/mm/aaaa

Atenção: preencha as datas de acesso antes de entregar o repositório.


Pacotes e bibliotecas utilizados
PacoteFinalidadeshinyFramework do aplicativo webbslibTema visual e layout responsivotidyverseManipulação e transformação de dadossfLeitura e processamento de dados espaciaisleafletMapa interativo de calorDTTabelas interativasgeobrMalha municipal do IBGEsidrarAcesso à API do IBGE/SIDRA

Parâmetros atuariais utilizados
ParâmetroValorPreço da saca de soja (R$)R$ 142,00Importância segurada por hectareR$ 6.000,00Probabilidade de ocorrência anual20%Fator de carregamento1,40Coeficiente de dano padrão (precoce, antes de R1)0,74Coeficiente de dano tardio (após R1)0,51
A severidade da doença é estimada pela equação:
Severidade (%) = -3,8983 + (0,3777 × chuva_30d) - (0,0003 × chuva_30d²)
onde chuva_30d é a precipitação acumulada nos 30 dias seguintes ao início da infecção (mm).

Estrutura do repositório
ferrugem-soja-mg/
│
├── README.md                        ← este arquivo
├── app.R                            ← aplicativo Shiny principal
│
├── data/
│   ├── raw/
│   │   ├── clima_nasa_diario_safra2324.rds   ← dados brutos NASA POWER
│   │   └── mg_municipios.rds                 ← polígonos municipais MG
│   └── processed/                            ← dados processados (gerados pelo app)
│
├── scripts/                         ← scripts auxiliares (download, limpeza)
│
└── reports/
    ├── Ferrugem_agora_vai_relatorio.qmd      ← código-fonte do relatório
    └── Ferrugem_agora_vai_relatorio.html     ← relatório renderizado

Funcionalidades do app
Aba 1 – Análise Espacial e Ranking
Mapa de calor interativo com o risco estimado de prejuízo por município de MG, com ranking de vulnerabilidade econômica baseado na data de infecção selecionada.
Aba 2 – Precificação de Seguro
Cálculo atuarial da taxa e prêmio do seguro agrícola contra ferrugem, com base no município, data de plantio e área segurada.
Aba 3 – Regulação de Sinistro
Estimativa de reembolso com base no estádio fenológico da cultura no momento da infecção (precoce/tardio), gerando laudo preliminar do sinistro.

Observações de reprodutibilidade

Os dados climáticos foram obtidos via API da NASA POWER para todos os municípios produtores de soja em MG, Safra 2023/2024 (novembro/2023 a fevereiro/2024).
O arquivo mg_municipios.rds foi gerado com o pacote geobr (ano-base 2020) e salvo localmente para evitar dependência de conexão durante a execução do app.
Os dados de produtividade são obtidos em tempo real via pacote sidrar (anos 2022, 2023 e 2024) — é necessária conexão com a internet para executar o app.
