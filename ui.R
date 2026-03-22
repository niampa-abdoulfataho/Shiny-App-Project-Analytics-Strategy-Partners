library(shiny)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)


#Le header 
pageHeader <- dashboardHeader(
    title = "Basic dashboard",
    dropdownMenuOutput("messageMenu"),
    dropdownMenuOutput("notificationMenu"),
    dropdownMenu(type = "tasks", 
                 badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
)


# La barre latérale 
pageSiderbar <- dashboardSidebar(
    sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        # Custom CSS to hide the default logout panel
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        # The dynamically-generated user panel
        uiOutput("userpanel"),
        menuItem(tabName = "formulaire", text = "Home", icon = icon("home")),
        menuItem(tabName = "another", text = "Another Tab", icon = icon("heart")),
        menuItem("Source code", icon = icon("file-code-o"), 
                 href = "https://github.com/rstudio/shinydashboard/",
                 badgeLabel = "link", badgeColor = "blue"),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
    )
)


# Le corps de la page
pageBody <- dashboardBody(
        useShinyjs(),
        tags$head(tags$style(HTML("
    body { font-family: Arial, sans-serif; font-size: 13px; }
    .titre  { background:#d9a520; color:white; padding:10px; border-radius:4px; }
    .btn-add  { background:#4CAF50; color:white; margin:4px; }
    .btn-del  { background:#f44336; color:white; }
    .btn-save { background:#2196F3; color:white; margin:4px; }
    .btn-load { background:#9C27B0; color:white; margin:4px; }
    .row-block { border:1px solid #ddd; border-radius:4px; padding:10px;
                 margin-bottom:8px; background:#fafafa; }
    .taux-field input { background:#fff3cd !important; }
    h4 { color:#333; border-bottom:2px solid #d9a520; padding-bottom:5px; }
  "))),
        
        div(class="titre", h4("📋 FICHE DE COLLECTE DES DONNÉES DU PROGRAMME D’INVESTISSEMENT PUBLIC (PIP) : BILAN AU 31 DÉCEMBRE 2025")),
        br(),
        
        # ── Onglet Saisie ─────────────────────────────────────────────────────────
        tabItems(tabItem(tabName = "formulaire",
         tabsetPanel(id = "tabs",
             
             tabPanel("✏️ Saisie", br(),
                      fluidRow(
                          column(4,
                                 textInput("session_label", "🏷️ Libellé du formulaire",
                                           placeholder = "Ex: Rapport Q1 2025")
                          ),
                          column(8,br(),
                                 actionButton("add_row",    "➕ Ajouter indicateur", class = "btn-add"),
                                 actionButton("save_all",   "💾 Sauvegarder en BD",  class = "btn-save"),
                                 actionButton("reset_form", "🔄 Réinitialiser",
                                              class = "btn btn-warning", style = "margin:4px;")
                          )
                          
                      ),
                      br(),
                      uiOutput("form_table_finance"),
                      br()
                      #h4("📊 Aperçu avant sauvegarde"),
                      #DTOutput("preview_table")
             ),
             
             # ── Onglet Historique / CRUD ──────────────────────────────────────────────
             tabPanel("🗄️ Historique & Modification", br(),
                      fluidRow(
                          column(5,
                                 selectInput("sel_session", "Formulaire sauvegardé :", choices = NULL),
                                 actionButton("load_session", "📂 Charger",        class = "btn-load"),
                                 actionButton("del_session",  "🗑️ Supprimer ce formulaire",
                                              class = "btn btn-danger", style = "margin:4px;")
                          )
                      ),
                      br(),
                      #DTOutput("history_table"),
                      br(),
                      hidden(div(id = "edit_panel",
                                 wellPanel(
                                     h4("✏️ Modifier la ligne sélectionnée"),
                                     fluidRow(
                                         column(3, textInput("e_indicateur", "Indicateur de produit")),
                                         column(2, textInput("e_unite",      "Unité physique")),
                                         column(2, numericInput("e_prevision",   "Prévision",   value = NA, min = 0)),
                                         column(2, numericInput("e_realisation", "Réalisation", value = NA, min = 0)),
                                         column(3, textInput("e_localite",   "Localité"))
                                     ),
                                     fluidRow(
                                         column(5, textAreaInput("e_preuves",     "Preuves / Observations", rows = 2)),
                                         column(2, numericInput("e_taux_annee",  "Taux exec annuel (%)",
                                                                value = NA, min = 0, max = 100)),
                                         column(2, numericInput("e_taux_global", "Taux exec global (%)",
                                                                value = NA, min = 0, max = 100))
                                     ),
                                     actionButton("update_row", "💾 Enregistrer la modification", class = "btn-save")
                                 )
                      ))
             )
            )
        )
)
)
    



#===============================================================================
# Le tableau de bord 
dashboardPage(pageHeader, pageSiderbar, pageBody)

#===============================================================================

