library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

tags$head(
  tags$style(
    HTML("#dashboard{margin-bottom:50px;}")
  )
)

data_ <- read_xlsx(path="AGRIBALYSE3.0.1_vf.xlsm",sheet=2,skip=1)
data_1 <- read_xlsx(path="AGRIBALYSE3.0.1_vf.xlsm",sheet=3,skip=3,na="no data")
data_2 <- read_xlsx(path="AGRIBALYSE3.0.1_vf.xlsm",sheet=4,skip=3,na="no data") %>% fill(1:6, .direction="down")

header <- dashboardHeader(title="Impact des aliments sur l'environnement",
                          tags$li(class = "dropdown", 
                                  actionLink('aliment_sidebar',label="Aliment"),
                          ),
                          tags$li(class = "dropdown", 
                                  actionLink('menu_sidebar',label="Menu")
                          ),
                          titleWidth = 450)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar",
    menuItem("Aliment", tabName = "aliment"),
    menuItem("Menu", tabName = "menu")
  ),
  disable = T
)

body_aliment <- fluidRow(
  box(
    h2("Choisir son aliment"),
    selectInput(inputId = 'groupe_aliment',label = "Groupe d'aliment",
                choices = unique(data_$`Groupe d'aliment`)),
    uiOutput("sous_groupe_aliment_out"),
    uiOutput("nom_produit_out"),
    numericInput(inputId = "quantity",label="Quantité (g)",value=100,min=0),
    actionButton("button_add", "Ajouter au menu"),
    br(),
    h3("Detail produit"),
    infoBoxOutput(outputId = "pack", width = 12),
    infoBoxOutput(outputId = "deliv", width = 12),
    infoBoxOutput(outputId = "prep", width = 12),
    br(),
    div("Source ADEME"),
    a("Données Agribalyse® v3.0 - 2020",href="https://agribalyse.ademe.fr/"),
    div("Dashboard publié sous License GPL-3.0"),
    a("Code Source",href="https://github.com/zloak/impact-environnement-aliment"),
    width=2,status="primary"
  ),
  
  box(
    h2("Impact sur l'environnement"),
    infoBoxOutput(outputId = "score", width = 12),
    infoBoxOutput(outputId = "co2eq", width = 12),
    infoBoxOutput(outputId = "note", width = 12),
    h3("Les autres effets sur l'environnement"),
    tableOutput('effects'),
    width=5,status="danger"
  ),
  
  uiOutput("impact_plots")
)

body_menu <- fluidRow(
  box(
    infoBoxOutput(outputId = "menu_co2_total",width=12),
    width=3,status="danger"),
  box(
    h2("Votre menu"),
    uiOutput('your_menu'),
    tableOutput("menu_table"),
    width=6,status="primary"),
  box(
    h2("Modifier le menu"),
    uiOutput("select_menu_item"),
    actionButton("button_delete", "Supprimer du menu"),
    width=3,status="warning")
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "aliment",
            body_aliment
    ),
    tabItem(tabName = "menu",
            body_menu)
  )
)

server <- function(input, output,session) {
  ## SIDEBAR ACTION
  
  observeEvent(input$aliment_sidebar, {
    updateTabItems(session, "sidebar", "aliment")
  })
  
  observeEvent(input$menu_sidebar, {
    updateTabItems(session, "sidebar", "menu")
  })
  
  ## REACTIVE
  data_sub <- reactive({
    data_sub <- data_ %>% filter(`Groupe d'aliment` == input$groupe_aliment)
    return(data_sub)
  })
  
  
  data_sub_sub <- reactive({
    data_sub_sub <- data_ %>% filter(`Groupe d'aliment` == input$groupe_aliment,
                                     `Sous-groupe d'aliment` == input$sous_groupe_aliment)
    return(data_sub_sub)
  })
  
  data_prod <- reactive({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_prod <- data_ %>% filter(`Groupe d'aliment` == input$groupe_aliment,
                                  `Sous-groupe d'aliment` == input$sous_groupe_aliment,
                                  `Nom du Produit en Français` == input$nom_produit)
    return(data_prod)
  })
  
  data_step <- reactive({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_prod <- data_prod()
    if (nrow(data_prod)!=0){
      data_step <- data_1 %>% filter(`Code AGB`==data_prod$`Code\nAGB`)
    }
    else{
      data_step <- tibble()
    }
    return(data_step)
  })
  
  data_ingr <- reactive({
    data_prod <- data_ %>% filter(`Groupe d'aliment` == input$groupe_aliment,
                                  `Sous-groupe d'aliment` == input$sous_groupe_aliment,
                                  `Nom du Produit en Français` == input$nom_produit)
    if (nrow(data_prod)!=0){
      data_ingr <- data_2 %>% filter(`Ciqual \nAGB`==data_prod$`Code\nAGB`)
    }
    else{
      data_ingr <- tibble()
    }
    return(data_ingr)
  })
  
  ## MENU ACTION
  menu <- reactiveVal({
    menu <- tibble(id=c(),
                   aliment=c(),
                   quantity=c(),
                   co2=c())
  })
  
  observeEvent(input$button_add, {
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    if (input$quantity>0){
      menu0 <- menu()
      if (nrow(menu0)==0 & !is.null(menu0)){
        id0 <- 1
      }
      else{id0 <- max(menu0$id)+1}
      t <- tibble(id=id0,
                  aliment=input$nom_produit,
                  quantity=input$quantity,
                  co2=data_0$`Changement climatique (kg CO2 eq/kg de produit)`)
      menu(rbind(menu(),t))
    }
    
  })
  
  observeEvent(input$button_delete,{
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    menu0 <- menu()
    if (nrow(menu0)!=0){
      menu1 <- menu0 %>% filter(id!=input$menu_item)
      menu(menu1)
    }
  })
  
  ## INPUT
  output$sous_groupe_aliment_out <- renderUI({
    data_0 <- data_sub()
    selectInput(inputId = "sous_groupe_aliment",label="Sous-Groupe d'aliment",
                choices = unique(data_0$`Sous-groupe d'aliment`))
  })
  
  
  output$nom_produit_out <- renderUI({
    req(input$sous_groupe_aliment)
    data_0 <- data_sub_sub()
    selectInput(inputId = "nom_produit",label="Nom du produit",
                choices = unique(data_0$`Nom du Produit en Français`))
  })
  
  output$select_menu_item <- renderUI({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    menu0 <- menu()
    if (nrow(menu0)!=0){
      selectInput(inputId="menu_item",label="ID aliment du menu",choices=menu0$id)
    }
    else{
      selectInput(inputId="menu_item",label="Aliment du menu",choices=NULL)
    }
  })
  
  
  ## OUTPUT DETAILS PRODUITS
  
  output$pack <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("Emballage",data_0$`Matériau d'emballage`,icon=icon("box-open"),color="olive",width=12)
  })
  
  output$deliv <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("Transport",data_0$`Livraison`,icon=icon("truck"),color="olive",width=12)
  })
  
  output$prep <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("Préparation",data_0$`Préparation`,icon=icon("gear"),color="olive",width=12)
  })
  
  
  ## IMPACT
  
  output$score <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("Score unique PEF",subtitle="mPt/kg de produit",
            data_0$`Score unique EF (mPt/kg de produit)`,icon=icon("bar-chart"),color="blue",width=12)
  })
  
  output$co2eq <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("Equivalent CO2",subtitle="kg CO2 eq/kg de produit",
            data_0$`Changement climatique (kg CO2 eq/kg de produit)`,icon=icon("burn"),color="red",width=12)
  })
  
  output$note <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    infoBox("DQR Note de qualité de la donnée ",subtitle="1 excellente - 5 très faible. La Commission Européenne recommande de la prudence dans l'utilisation des données avec des DQR supérieurs à 3.",
            data_0$`DQR - Note de qualité de la donnée (1 excellente ; 5 très faible)`,icon=icon("table"),color="yellow",width=12)
  })
  
  output$effects <- renderTable({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_prod()
    if (nrow(data_0)!=0){
      data_1 <- data_0[,c(5,15:27)] %>% pivot_longer(!`Nom du Produit en Français`) %>%
        select(!`Nom du Produit en Français`)%>% rowwise() %>% 
        mutate(unit_index = str_locate(name," \\(")[[1]],
               Indicateur = str_sub(name,1,unit_index-1),
               Unité = str_replace(str_sub(name,unit_index+2,-2),"E-0","10^-"),
               Valeur = value)
      data_2 <- data_1 %>% select(Indicateur,Valeur,Unité)
    }
    else{
      data_2 <- tibble()
    }
  })
  
  
  ## PLOTS
  
  output$repart_step <- renderPlot({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_step()
    if (nrow(data_0)!=0){
      data_1 <- data_0[1,7:13] %>% pivot_longer(!DQR) %>% select(!DQR)
      total <- data_0[1,14]$`Total...14`
      data_1 <- data_1 %>% rowwise() %>% mutate(value = value/total*100,
                                                value2 = 100,
                                                name_index = str_locate(name,"\\.")[[1]],
                                                name = str_sub(name,1,name_index-1))
      order_value <- sort(data_1$value,decreasing=F,index.return=T)
      data_1$name <- factor(data_1$name,levels=data_1$name[order_value$ix])
      p <- ggplot()+
        geom_col(data=data_1,aes(y=name,x=value),fill="#dd4b39",orientation="y",width=0.5,alpha=0.5)+
        geom_col(data=data_1,aes(y=name,x=value2),color="#dd4b39",alpha=0,orientation="y",width=0.5,linetype = "dashed")+
        geom_text(data=data_1,aes(x=value2-2,y=name,label=name),hjust=1,size=5)+
        labs(y="",x="Contribution au PEF (%)")+
        coord_cartesian(xlim = c(0,100))+
        theme_light()+theme(legend.position = "None",
                            text=element_text(size=18),
                            axis.text.y = element_blank())
    }
    p
  })
  
  output$repart_recette <- renderPlot({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_ingr()
    if(nrow(data_0)!=0){
      data_1 <- data_0[,c(7,9)]
      n <- length(data_1$Ingredients)
      total <-data_1$`Score unique EF (mPt/kg de produit)`[n]
      data_2 <- data_1[1:(n-1),] %>% mutate(value=`Score unique EF (mPt/kg de produit)`/total*100,
                                            value2=100)
      order_value <- sort(data_2$value,decreasing=F,index.return=T)
      data_2$Ingredients <- factor(data_2$Ingredients,levels=data_2$Ingredients[order_value$ix])
      p <- ggplot()+
        geom_col(data=data_2,aes(y=Ingredients,x=value),fill="#3d9970",alpha=0.5,orientation="y",width=0.5)+
        geom_col(data=data_2,aes(y=Ingredients,x=value2),color="#3d9970",alpha=0,orientation="y",width=0.5,linetype = "dashed")+
        geom_text(data=data_2,aes(x=value2-2,y=Ingredients,label=Ingredients),hjust=1,size=5)+
        labs(y="",x="Contribution au PEF (%)")+
        coord_cartesian(xlim = c(0,100))+
        theme_light()+theme(legend.position = "None",
                            text=element_text(size=18),
                            axis.text.y=element_blank())
      return(p)
    }
  })
  
  observe({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    data_0 <- data_ingr()
    if(nrow(data_0)!=0){
      output$impact_plots <- renderUI({
        tagList(
          box(
            h3("Impact par étapes du cycle de vie"),
            plotOutput("repart_step"),
            h3("Impact par ingrédients (uniquement si recette)"),
            plotOutput("repart_recette"),
            width = 5,status="warning"
          )
        )
      })
    }
    else{
      output$impact_plots <- renderUI({
        tagList(
          box(
            h3("Impact par étapes du cycle de vie"),
            plotOutput("repart_step"),
            width = 5,status="warning"
          )
        )
      })
    }
  })
  
  
  ### RENDER MENU
  output$your_menu <- renderUI({
    menu0 <- menu()
    if (nrow(menu0)==0){
      div("C'est un peu vide ici! C'est bon pour la planète mais vous allez avoir faim.")
    }
  })
  
  output$menu_table <- renderTable({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    menu0 <- menu()
    if (nrow(menu0)!=0){
      menu0 <- menu0 %>% mutate(co2_mass = co2*quantity/1e3)
      menu0$id <- as.character(menu0$id)
      colnames(menu0) <- c("id","Aliment","Quantité (g)","Equivalent CO2 (kg CO2 eq/kg de produit)","Total Equivalent CO2 (kg de produit)")
    }
    else{
      colnames(menu0) <- c("id","Aliment","Quantité (g)","Equivalent CO2 (kg CO2 eq/kg de produit)")
    }
    return(menu0)
  })
  
  output$menu_co2_total <- renderInfoBox({
    req(input$sous_groupe_aliment)
    req(input$nom_produit)
    menu0 <- menu() 
    if (nrow(menu0)!=0){
      menu0 <- menu0 %>% mutate(co2_mass = co2*quantity/1e3)
      menu0$id <- as.character(menu0$id)
      total_co2 <- max(cumsum(menu0$co2_mass))
      infoBox("Total Equivalent CO2 du MENU",subtitle="kg CO2 eq",
              round(total_co2,2),icon=icon("burn"),color="red",width=12)
    }
    else{
      infoBox("Total Equivalent CO2 du MENU",subtitle="kg CO2 eq",
              0,icon=icon("burn"),color="red",width=12)
    }
    
    
  })
}


ui <- dashboardPage(
  skin="black",
  header,
  sidebar,
  body
)

shinyApp(ui, server)
