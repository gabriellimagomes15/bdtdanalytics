##GABRIEL LIMA GOMES
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(plotly)
library(DT)
library(ggthemes)
library(dplyr)
############## Alteração de algmas configurações #############################
options(digits = 22) ## Altera tamanho de atributos do tipo númerico. Evita visualizar como notação científica
options(scipen = 10) ##Altera plots, para não inserir notação cientifica nos eixos

## URL PARA CARREGAR PARTE DA BASE E RENOMEAR NOME DAS COLUNAS
url             <- "http://192.168.0.46:8081/solr/biblio/select?q=*%3A*&rows=500000&fl=instname%2CpublishDate%2Clanguage%2Cformat%2Ceu_rights&wt=csv&indent=true"
dados           = read.csv(url)
nomeColunas     <- c("instituicao","data","language","tipo","acesso")
colnames(dados) <- nomeColunas
dados$data      <- as.character(dados$data)

# LENDO DADOS ACESSO POR INSTITUIÇÕES
acessoInst <- read.table("/home/gabrielgomes/Documentos/acessoInst.csv", quote = "\"", sep = ",", header = T)

server <- function(input, output) {
  ## FUNCAO PARA LER DADOS ESTATISTICOS DA BDTD
  lerDados <- reactive({
    url <- "http://mutamba:8080/solr/stats/select?q=recordSource%3A+"
    url <- paste(url, input$instituicao3, sep = "")
    url <- paste(url, "&rows=22750&fl=recordSource%2Cdatestamp&wt=csv&indent=true", sep = "")
    
    read.csv(url)
    
  })
  
  ## stuffs for dashboard tab
  output$vbox1 <- renderValueBox({ 
    valueBox(
      paste("Quantidade de Registros:", dim(dados)[1]),
      input$count,
      color = "orange",
      icon = icon("empire"))
  })
  output$vbox2 <- renderValueBox({ 
    valueBox(
      paste("Quantidade Instituições: ",length(table(dados$instituicao))),
      input$count,
      color = "red",
      icon = icon("bullseye")
    )
  })
  output$piebox1 <- renderPlotly({
    plot_ly() %>% 
      add_pie(data = count(dados, language), labels = ~language, values = ~n,
              name = "Idioma",domain = list(x = c(0,0.4), y = c(0.2,1))) %>%
      add_pie(data = count(dados, acesso), labels = ~acesso, values = ~n,
              name = "Acesso",domain = list(x = c(0.6, 1), y = c(0.2,1))) %>%
      add_pie(data = count(dados, tipo), labels = ~tipo, values = ~n,
              name = "Tipo",domain = list(x = c(0.25, 0.75), y = c(-.5, 0.8))) %>%
      layout(title = 'Idioma, Tipo Pesquisa, Tipo Acesso,',showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ## stuffs for data tab
  # data table output
  output$da.tab <- DT::renderDataTable(datatable(dados, extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 15, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(extend = 'collection', 
                                                                                             buttons = c('csv', 'excel', 'pdf'),text = 'Download')
                                                                  )
                                                                )
                                                 )
                                      )
  output$plotGeral <- renderPlotly({
    colunas <- c("x")
    if(input$compara != 'nulo') {
      compara <- input$compara
      colunas <- c("x", "compara")
    }
    else{
      compara <- NULL
    }
    
    p2 <- data.frame(dados[,c(input$eixo_x, compara)])
    names(p2) <- colunas 
    g1 <- ggplot(data = p2)
    
    if(input$compara != 'nulo'){
     # g1 <- g1 + geom_bar(aes(x, fill = compara), position = "dodge", show.legend = T, inherit.aes = T,size = 500) +
      #  xlab(input$eixo_x) + ylab("Quantidade") + ggthemes::theme_hc()
      # ggplotly(g1) #%>% layout(dragmode = "select")
      plot_ly(data = p2, x = ~x, color = ~compara)
    }else{
      if(input$eixo_x == 'data'){
        plot_ly(data = p2, x = ~x)
      }else{
        plot_ly(data = p2, x = ~x, color = ~x)
        #g1 <- g1 + geom_bar(aes(x, fill = 2),position = "dodge") + xlab(input$eixo_x) + ylab("Quantidade") +
         # guides(fill = FALSE)        g1 <- g1 + ggthemes::theme_hc()        ggplotly(g1) #%>% layout(dragmode = "select")
      }
    }
  })
  
  output$plotComparativo <- renderPlotly({
    colunas <- c("instituicao", input$campo)
    subDados <- as.data.frame(droplevels(dados[dados$instituicao %in% input$instituicao2, colunas]))
    names(subDados) <- c("instituicao", "campo")
    plot_ly(data = subDados, x = ~instituicao, color = ~campo)
  })
  
  output$lineInstituicao<- renderPlotly({
    col <- c("instituicao","data")
    data <- droplevels(dados[dados$instituicao %in% input$instituicao,col])
    data <- as.data.frame(table(data))
    plot_ly() %>%
      add_lines(data = data, x = ~data, y = ~Freq, color = ~instituicao)
  })
  
  output$lineAcessos <- renderPlotly({
    #arq <- lerDados()     print(arq)
    
    #
    #if(!is.na(input$instituicao3)){
      
     # instCount <- data.frame(recordSource = character(),dateStamp=character(), anoMes=character())
      
      #for(instituicao in input$instituicao3){
       # url <- "http://mutamba:8080/solr/stats/select?q=recordSource%3A+"
        #url <- paste(url, instituicao, sep = "")
        #url <- paste(url, "&rows=500000&fl=recordSource%2Cdatestamp&wt=csv&indent=true", sep = "")
        #instAcesso        <- read.csv(url)
        #instAcesso$anoMes <- substr(instAcesso$datestamp,start = 1,stop = 7)
        #instDesc  <- as.data.frame(table(instAcesso[,c(1,3)]))
        #instCount <- rbind(instCount,instDesc)
      #}
    #}else{
     # return(NULL)
    #}
    
    #print(instCount)
    #plot_ly() %>%add_lines(data = instCount, x = ~anoMes, y = ~Freq, color = ~recordSource)
    plot_ly() %>%
      add_lines(data = acessoInst[acessoInst$recordSource %in% input$instituicao3,], x = ~anoMes, y = ~Freq, color = ~recordSource)
    
  })

}

####################### Reading data set ######################################
## Campos buscados no SOLR. Este campos compõe a URL para acessa diretamento o solr via R
# id,title,author,author2,instname,publishDate,language,oai_identifier,description,_version_,url,
# topic,spelling,format,eu_rights

#nomeColunas <- c("id","titulo","autor","autor2","instituicao","data","language","oaiIdentifier","description","version","url","topic","spelling","tipo","acesso")

#Carregando dados diretamente do solr
#url         <- "http://192.168.0.46:8081/solr/biblio/select?q=*%3A*&rows=100000&fl=id%2Ctitle%2Cauthor%2Cauthor2%2Cinstname%2CpublishDate%2Clanguage%2Coai_identifier%2Cdescription%2C_version_%2Curl%2Ctopic%2Cspelling%2Cformat%2Ceu_rights&wt=csv&indent=true"
#dados       = read.csv(url)

#colnames(dados) <- nomeColunas

#Carregando dados em arquivo csv local
#path <- "/home/gabrielgomes/Documentos/bdtd.csv"
#dados <- as.data.frame(read.csv(path, header = F, quote = "", sep = "|", stringsAsFactors = F))
#names(dados) <- nomeColunas
#nomeColunas <- c("id","titulo","autor","autor2","instituicao","data","language","oaiIdentifier","description",                 "version","url","topic","spelling","tipo","acesso")
#nomeColunas     <- c("instituicao","data","language","tipo","acesso")
#colnames(dados) <- nomeColunas

#dados$data      <- as.character(dados$data)



