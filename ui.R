library(shiny)
library(shinydashboard)
library(anicon)

shinyUI( 
  dashboardPage(skin="blue",
    dashboardHeader(title ="polyCID", titleWidth = 300),
    
    dashboardSidebar(
      width=300,tags$head(tags$style(HTML(".sidebar { height: 100vh; overflow-y: auto; }"))),
      
      sidebarMenu(
        id = "menu1",
        HTML(paste0(
          "<br>",
          "<a href='https://lagm.cbmeg.unicamp.br/index.html' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo.png' width = '220'></a>",
          "<br>"
        )),
        menuItem("Project", icon = icon("seedling"), tabName = "description"),
        menuItem("Tool", icon = icon("bar-chart"), tabName = "analyses"),
        menuItem("Info", icon = icon("info-circle"), tabName = "team")
        
      )
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML('.main-header .logo {font-family: "Helvetica font-weight: bold; font-size: 24px;} '))),
      tags$head(tags$style(HTML('.content-wrapper .content {height: 1210px;}'))),
      
      tabItems(
        tabItem(tabName = "description", uiOutput("image_menu1")),
        tabItem(tabName = "analyses", uiOutput("image_menu2")),
        
        #Contato
        tabItem(tabName = "team", wellPanel(
          box(title = "An automated SNP-based approach for contaminant identification in biparental polyploid progenies of tropical forage grasses", background = "black", solidHeader = TRUE, width = NULL,withTags({
            div(class="header",checked=NA,
                "Available paper at ", a(href="https://www.biorxiv.org/", "bioRxiv")," server.",br(),
                "Documentation available at ", a(href="https://github.com/", "GitHub")," server."
            )
          })),
          helpText(
            withTags({
              div(class="header",checked=NA,
                h1("Felipe Bitencourt Martins", style = "font-size:14px;line-height: 3px;"),
                h1("Aline da Costa Lima Moraes", style = "font-size:14px;line-height: 3px;"),
                h1("Alexandre Hild Aono", style = "font-size:14px;line-height: 3px;"),
                h1("Rebecca Caroline Ulbricht Ferreira", style = "font-size:14px;line-height: 3px;"),
                h1("Lucimara Chiari", style = "font-size:14px;line-height: 3px;"),
                h1("Rosangela Maria Simeão", style = "font-size:14px;line-height: 3px;"),
                h1("Sanzio Carvalho Lima Barrios", style = "font-size:14px;line-height: 3px;"),
                h1("Mateus Figueiredo Santos", style = "font-size:14px;line-height: 3px;"),
                h1("Liana Jank", style = "font-size:14px;line-height: 3px;"),
                h1("Cacilda Borges do Valle", style = "font-size:14px;line-height: 3px;"),
                h1("Bianca Baccili Zanotto Vigna", style = "font-size:14px;line-height: 3px;"),
                h1("Anete Pereira de Souza", style = "font-size:14px;line-height: 3px;"),
                br(),
                a(href="https://lagm.cbmeg.unicamp.br/index.html", "Laboratório de Análise Genética e Molecular (LAGM)"),
                a(href="https://www.unicamp.br/unicamp/", "- University of Campinas"),
                "- São Paulo/Brazil",
                br(),
                a(href="https://www.embrapa.br/gado-de-corte", "Embrapa Gado de Corte"),
                a(href="https://www.embrapa.br/", "- Brazilian Agricultural Research Corporation "),
                "- Mato Grosso/Brazil",
                br(),
                a(href="https://www.embrapa.br/pecuaria-sudeste", "Embrapa Pecuária Sudeste"),
                a(href="https://www.embrapa.br/", "- Brazilian Agricultural Research Corporation "),
                "- São Paulo/Brazil",
                br(),
                HTML(paste0(
                  "<br>",
                  "<a href='https://lagm.cbmeg.unicamp.br/index.html' target='_blank'><img style = 'display: inline; margin-left: auto; margin-right: 10px;' src='logo-LAGM.png' width = '110'></a>",
                  "<a href='https://www.unicamp.br/unicamp/' target='_blank'><img style = 'display: inline; margin-left: auto; margin-right: 10px;' src='unicamp-logo.png' width = '110'></a>",
                  "<a href='https://www.embrapa.br/' target='_blank'><img style = 'display: inline; margin-left: auto; margin-right: auto;' src='embrapa-logo.png' width = '110'></a>",
                  "<br>"
                ))
              )
            })
          )
        ))
        
      )#endItems
      
    )#end body
    
  )#end Page
)#end UI
