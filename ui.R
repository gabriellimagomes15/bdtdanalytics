library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(plotly)
library(DT)
library(ggthemes)
library(dplyr)

## URL PARA CARREGAR PARTE DA BASE E RENOMEAR NOME DAS COLUNAS
url             <- "http://192.168.0.46:8081/solr/biblio/select?q=*%3A*&rows=500000&fl=instname%2CpublishDate%2Clanguage%2Cformat%2Ceu_rights&wt=csv&indent=true"
dados           = read.csv(url)
nomeColunas     <- c("instituicao","data","language","tipo","acesso")
colnames(dados) <- nomeColunas
dados$data      <- as.character(dados$data)

# LENDO DADOS ACESSO POR INSTITUIÇÕES
acessoInst <- read.table("/home/gabrielgomes/Documentos/acessoInst.csv", quote = "\"", sep = ",", header = T)

# Define UI for application that draws a histogram
## Preparing sidebar items
sidebar <- dashboardSidebar(
    width = 250,
    sidebarMenu(
      #menuItem("Início", tabName = "inicio", icon = icon("dashboard")),
      menuItem("Visão Geral", tabName = "visgeral", icon = icon("dashboard")),
      menuItem("Visualização", icon = icon("navicon"), tabName = "graphs", 
               menuSubItem("Visualização Geral BDTD", tabName = "geral", icon = icon("bar-chart")), 
               menuSubItem("Visualização Times Series BDTD", tabName = "times", icon = icon("line-chart")),
               menuSubItem("Acessos BDTD", tabName = "acessos", icon = icon("line-chart"))
      ),
      menuItem("Dados", tabName = "datafile", icon = icon("th")),
      br(),br(),hr(),
      
      # email sharing link
      menuItem(HTML("Email: repoanalises@ibict.br<br/><br/>Telefone: +55 61 3217-6449/6460"), icon = icon("envelope-o"), text = "Feedback & Sugestões"
               #href = "mailto:?ow.raza@hotmail.com?subject=Feedback on VAS app")
               )
    )
)
  
body <- dashboardBody(
    tabItems(
      tabItem(tabName = "inicio",
              h2("Introdução",  align = "center", style = "font-family: 'times'; color:red"),
              p("Instituto Brasileiro de Informação em Ciência e Tecnologia...", a("IBICT", href = "http://www.ibict.br/", target="_blank"),
                " and", style = "font-family: 'times'"),
              fluidPage(
                fluidRow(
                  column(
                    h2("Sobre este app...", align = "center", style = "font-family: 'times'; color:red"),
                    p("Esse app ajudará a explorar e visualizar ... 
                      Os dados utilizados são da base da BDTD e está disponível para ser baixado nesse site, na seção DADOS", 
                      style = "font-family: 'times'"),
                      width = 4,
                      align = "left"
                  ),
                  column(
                    h2("Como usar!", style = "font-family: 'times'; color:red"),
                    p("This app contains two major sections; database and graphs. After cleaning and re-defining type of incident, 
                      two variables were added, i.e, type of government and president at the day of incident. This new 
                      addition was done to explore and understand further relationship of sectarian violence in Pakistan.", 
                      style = "font-family: 'times'"), 
                    p("Section for", strong("data"), "presents the database, which is provided with copying, printing and 
                      downloading options. Section for", strong("visualization"), "has two sub-sections, a. explorer for 
                      people killed & b. explorer for people injured in sectarian violence. In these sub-sections, data is
                      plotted against geo-political regions of Pakistan. Within each sub-section, navigation panel contains four 
                      tabs that plot data points according to the selected tab. Graphs are plotted on an interactive platform,
                      such that when the mouse-pointer hovers over data-point it shows further information. Users can choose to
                      turn off the data-points by clicking on labels given in the legend.",
                      style = "font-family: 'times'"),
                    width = 8,
                    align = "left"
                  ),
                  br(),br()
                ) #fim fluidRow
              ),#fim fluidpage
              
              br(),br(),
              
              p(strong("IBICT CopyRight, 2016, Version 1.0", style = "font-family: 'times'", 
                       tags$img(src = "C_thin.png", width = "10px", height = "10px")))
      ), #fim tabItem INICIO
      tabItem(tabName = "visgeral",
              br(),br(),br(),br(),br(),br(),br(),
              column(
                fluidRow(
                  valueBoxOutput("vbox1", width = 8),
                  valueBoxOutput("vbox2", width = 8)
                ),br(),
                width = 12,
                float = "left"
              ),
              column(
                plotlyOutput("piebox1", width= "auto"),
                width = 12
              )
      ), #fim tabItem VISGERAL
      tabItem(tabName = "geral",
              mainPanel(
                tabsetPanel(
                  tabPanel("Comparativo Geral", 
                           sidebarPanel(
                             selectInput("eixo_x", "Campo", 
                                         choice = c("tipo", "acesso","language","data")),
                             selectInput("compara", "Campo para Comparação", 
                                         choice = c(None = 'nulo', "language","tipo", "acesso","data")),
                             width = 50
                           ),
                           plotlyOutput("plotGeral"),
                           style='width: 950px; height: 550px'
                   ),
                   tabPanel("Comparativo Instituição",
                           sidebarPanel(
                             selectInput("campo", "Campo", 
                                         choice = c("tipo", "acesso","language")),
                             selectizeInput("instituicao2", label = "Instituições",
                                            choices = unique(dados$instituicao), multiple = T,
                                            options = list(maxItems = 5, placeholder = 'Select a name'),
                                            selected = "UNB"),
                             width = 50
                           ),
                           plotlyOutput("plotComparativo"),
                           style='width: 950px; height: 550px'
                   )
                )#fim tabsetPanel
              )#fim mainPanel
      ), #fim tabItem GERAL
      tabItem(tabName = "times",
              sidebarPanel(width = 50,
                selectizeInput("instituicao", label = "Instituições",
                               choices = unique(dados$instituicao), multiple = T,
                               options = list(maxItems = 5, placeholder = 'Select a name'),
                               selected = "UNB")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Gráfico", plotlyOutput("lineInstituicao"), style='width: 950px;')
                )
              )
      ), #fim tabItem TIMES
      tabItem(tabName = "acessos",
              sidebarPanel(width = 50,
                selectizeInput("instituicao3", label = "Instituições",
                                choices = unique(dados$instituicao), multiple = T,
                                options = list(maxItems = 5, placeholder = 'Select a name'),
                                selected = "UNB")
                
              ),
              mainPanel(tabsetPanel(
                          tabPanel("Acessos", plotlyOutput("lineAcessos"), style = 'width: 1000px')
                        )
              )
      ),
      tabItem(tabName = "datafile",
              box(title = "Dados da BDTD",
                  width = 112, 
                  DT::dataTableOutput('da.tab'))  
      )#fim tabItem DATAFILE
    ),
    tags$head(tags$style(HTML('.main-sidebar {background-color: blue;} .sidebar-menu, .main-sidebar{background-color: cian;}')))
)#fim body
  
ui <- dashboardPage(
    title = "IBICT",
    skin="green",
    dashboardHeader(title="IBICT", titleWidth = 250,
      #Facebook Sharing
      tags$li(class = "dropdown", tags$a(href = "https://www.facebook.com/IBICTbr/?fref=ts", 
                                         target = "_blank", tags$img(height = "20px", src = "fb2.png"))),
      # Linkedin link
      tags$li(class = "dropdown", tags$a(href = "https://br.linkedin.com/in/gabriellimagomes", 
                                         target = "_blank", tags$img(height = "20px", src = "linkedin.png")))
    ),
    sidebar,
    body
)

#Carregando dados diretamente do solr
## URL PARA CARREGAR BASE COMPLETA E RENOMEAR NOME DAS COLUNAS
#url         <- "http://192.168.0.46:8081/solr/biblio/select?q=*%3A*&rows=500000&fl=id%2Ctitle%2Cauthor%2Cauthor2%2Cinstname%2CpublishDate%2Clanguage%2Coai_identifier%2Cdescription%2C_version_%2Curl%2Ctopic%2Cspelling%2Cformat%2Ceu_rights&wt=csv&indent=true"
#nomeColunas <- c("id","titulo","autor","autor2","instituicao","data","language","oaiIdentifier","description","version","url","topic","spelling","tipo","acesso")
