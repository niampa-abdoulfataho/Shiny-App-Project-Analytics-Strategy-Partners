library(shiny)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)


#Le header 
pageHeader <- dashboardHeader(
    title = tags$span(
      tags$img(
        src  = "https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
        height = "26px",
        style  = "margin-right:8px; vertical-align:middle;"
      ),
      tags$span("GestioProjets",
                style = "font-family:'Playfair Display',serif;
                   font-weight:700; font-size:17px; letter-spacing:1px;")
    ),
    titleWidth = 260,

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
  width = 260,
  
  tags$head(
    tags$link(rel  = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=DM+Sans:wght@300;400;500&display=swap"),
    
    tags$style(HTML("

        /* ════ BASE ════ */
        body, .content-wrapper, .main-footer { background:#0f1117 !important; }
        .skin-black .main-header .logo,
        .skin-black .main-header .navbar {
          background:#0f1117 !important;
          border-bottom:1px solid #1e2232;
        }
        .skin-black .main-sidebar { background:#13151f !important; }
        * { font-family:'DM Sans', sans-serif; }

        /* ════ SIDEBAR ════ */
        .sidebar-menu > li > a {
          color:#6b7280 !important; font-size:12.5px;
          border-left:3px solid transparent; transition:all .2s;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li > a:hover {
          color:#f0c040 !important; background:#1a1d2a !important;
          border-left:3px solid #f0c040 !important;
        }
        .sidebar-menu > li > a > .fa { color:#f0c040; margin-right:10px; }

        /* Barre de progression sidebar */
        .progress-sidebar {
          margin:16px; padding:14px;
          background:#1a1d2a; border-radius:8px;
          border:1px solid #1e2232;
        }
        .prog-label {
          color:#4a5568; font-size:10px;
          text-transform:uppercase; letter-spacing:1.5px; margin-bottom:8px;
        }
        .prog-bar-bg  { background:#0f1117; border-radius:4px; height:6px; }
        .prog-bar-fill {
          background:linear-gradient(90deg,#f0c040,#e07b20);
          height:6px; border-radius:4px; transition:width .4s;
        }
        .prog-count { color:#f0c040; font-size:12px; font-weight:600; margin-top:6px; }

        /* ════ CONTENU ════ */
        .content { padding:20px 22px; }

        /* ════ BOX OVERRIDE ════ */
        /* Carte principale */
        .box {
          background:#13151f !important;
          border:1px solid #1e2232 !important;
          border-radius:12px !important;
          box-shadow:none !important;
          margin-bottom:14px !important;
          overflow:hidden;
        }
        /* Ligne dorée en haut de chaque box */
        .box::before {
          content:'';
          display:block; height:3px;
          background:linear-gradient(90deg,#f0c040,#e07b20,#f0c040);
        }

        /* Header de la box */
        .box-header {
          background:#1a1d2a !important;
          border-bottom:1px solid #1e2232 !important;
          padding:14px 18px !important;
        }
        .box-title {
          font-family:'Playfair Display', serif !important;
          font-size:14px !important; font-weight:700 !important;
          color:#d4d8e8 !important; letter-spacing:.3px;
          display:flex; align-items:center; gap:10px;
        }
        /* Badge numéro dans le titre */
        .box-badge {
          background:linear-gradient(135deg,#f0c040,#e07b20);
          color:#0f1117; border-radius:50%;
          width:22px; height:22px;
          display:inline-flex; align-items:center; justify-content:center;
          font-size:11px; font-weight:700; flex-shrink:0;
        }
        /* Icône titre */
        .box-title .fa { color:#f0c04088; font-size:13px; }

        /* Bouton collapse natif shinydashboard */
        .box-header .btn {
          color:#3a3f52 !important; background:transparent !important;
          border:none !important; font-size:14px !important;
          transition:color .2s !important; padding:0 4px !important;
        }
        .box-header .btn:hover { color:#f0c040 !important; }

        /* Corps de la box */
        .box-body {
          background:#13151f !important;
          padding:20px 22px !important;
        }

        /* Statut badge dans le titre */
        .statut-badge {
          font-size:10px; padding:2px 9px; border-radius:20px;
          font-weight:600; letter-spacing:.5px; margin-left:8px;
        }
        .statut-vide    { background:#1e2232; color:#3a3f52; }
        .statut-partiel { background:#f0c04022; color:#f0c040;
                          border:1px solid #f0c04033; }
        .statut-complet { background:#4ade8022; color:#4ade80;
                          border:1px solid #4ade8033; }

        /* ════ LABELS & INPUTS ════ */
        .control-label, label {
          color:#6b7280 !important; font-size:11px !important;
          font-weight:500 !important; text-transform:uppercase;
          letter-spacing:.7px; margin-bottom:4px !important;
        }
        .required-star { color:#e05c5c; }
        .form-control {
          background:#0a0c14 !important; color:#d4d8e8 !important;
          border:1px solid #1e2232 !important; border-radius:7px !important;
          font-size:13px !important; padding:8px 12px !important;
          transition:border-color .2s, box-shadow .2s;
        }
        .form-control:focus {
          border-color:#f0c040 !important;
          box-shadow:0 0 0 3px rgba(240,192,64,.10) !important;
          outline:none !important;
        }
        .form-control::placeholder { color:#2e3347 !important; }
        textarea.form-control { resize:vertical; min-height:72px; }

        .hint-text { color:#2e3347; font-size:11px; margin-top:2px; }

        /* Séparateur interne */
        .inner-section {
          margin-top:18px; padding-top:16px;
          border-top:1px solid #1e2232;
        }
        .inner-title {
          color:#f0c04066; font-size:10px; font-weight:600;
          text-transform:uppercase; letter-spacing:1.5px; margin-bottom:14px;
        }

        /* ════ BLOCS FINANCEMENT ════ */
        .fin-block {
          background:#0a0c14; border:1px solid #1e2232;
          border-radius:8px; padding:16px 18px;
          margin-bottom:10px;
        }
        .fin-block-header {
          display:flex; justify-content:space-between;
          align-items:center; margin-bottom:14px;
        }
        .fin-block-num {
          color:#f0c040; font-size:11px; font-weight:700;
          text-transform:uppercase; letter-spacing:1px;
        }
        .btn-del-fin {
          background:#e05c5c18; color:#e05c5c;
          border:1px solid #e05c5c33; border-radius:5px;
          padding:3px 10px; font-size:11px; cursor:pointer;
          transition:all .15s;
        }
        .btn-del-fin:hover { background:#e05c5c; color:white; }

        /* Paires AE / CP */
        .fin-pair {
          display:flex; align-items:center; gap:6px; margin-bottom:2px;
        }
        .fin-badge {
          font-size:9px; font-weight:700; padding:2px 6px;
          border-radius:3px; flex-shrink:0; letter-spacing:.5px;
        }
        .fin-badge.ae {
          background:#3b4fd922; color:#7b8ff5;
          border:1px solid #3b4fd944;
        }
        .fin-badge.cp {
          background:#f0a50018; color:#f0c040;
          border:1px solid #f0a50033;
        }
        .fin-pair .form-group { margin-bottom:0 !important; flex:1; }
        .fin-pair .form-group input {
          padding:6px 10px !important; font-size:12px !important;
        }

        /* Bouton ajouter ligne */
        .btn-add-line {
          background:transparent; color:#f0c04077;
          border:1px dashed #f0c04033; border-radius:7px;
          padding:9px; width:100%; font-size:12px; cursor:pointer;
          transition:all .2s; margin-top:6px;
          display:flex; align-items:center; justify-content:center; gap:6px;
        }
        .btn-add-line:hover {
          border-color:#f0c040; color:#f0c040; background:#f0c04008;
        }

        /* ════ BARRE DE SOUMISSION ════ */
        .submit-bar {
          background:#13151f; border:1px solid #1e2232;
          border-radius:12px; padding:18px 24px;
          display:flex; align-items:center; justify-content:space-between;
          margin-top:8px; margin-bottom:30px;
        }
        .submit-info { color:#4a5568; font-size:12px; line-height:1.7; }
        .submit-info strong { color:#d4d8e8; }
        .btn-submit {
          background:linear-gradient(135deg,#f0c040,#e07b20);
          color:#0f1117; border:none; border-radius:8px;
          font-weight:700; font-size:13px; letter-spacing:.5px;
          padding:12px 32px; cursor:pointer;
          transition:transform .15s, box-shadow .15s;
          text-transform:uppercase;
        }
        .btn-submit:hover {
          transform:translateY(-1px);
          box-shadow:0 8px 24px rgba(240,192,64,.3);
        }
        .btn-reset {
          background:transparent; color:#6b7280;
          border:1px solid #1e2232; border-radius:7px;
          padding:10px 20px; font-size:12px; cursor:pointer;
          transition:all .15s; margin-right:8px;
        }
        .btn-reset:hover { border-color:#e05c5c; color:#e05c5c; }

        /* ════ PAGE HEADER ════ */
        .page-header { margin-bottom:22px; }
        .page-title {
          font-family:'Playfair Display', serif;
          font-size:22px; font-weight:700; color:#e8eaf0;
        }
        .page-subtitle { color:#3a3f52; font-size:12px; margin-top:3px; }
      "))
  ),
  
  tags$div(style = "padding:20px 16px 10px;",
           tags$div(class = "prog-label", "Progression globale")
  ),
  tags$div(class = "progress-sidebar",
           tags$div(class = "prog-bar-bg",
                    tags$div(class = "prog-bar-fill", style = "width:33%;")
           ),
           tags$div(class = "prog-count", "2 / 6 sections")
  ),
  
  sidebarMenu(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    menuItem("Saisie rapport",   tabName = "saisie",    icon = icon("edit")),
    menuItem("Liste projets",    tabName = "liste",     icon = icon("table")),
    menuItem("Tableau de bord",  tabName = "dashboard", icon = icon("chart-bar"))
  ),
  
  tags$div(
    style = "position:absolute; bottom:16px; left:0; right:0;
               padding:12px 16px 0; border-top:1px solid #1a1d2a;",
    tags$div(style = "color:#2e3347; font-size:10px; text-align:center;",
             "GestioProjets v1.0 · Burkina Faso")
  )
)


# Le corps de la page
pageBody <- dashboardBody(
  useShinyjs(),
  
  tabItems(
    tabItem(tabName = "saisie",
            
            tags$div(class = "page-header",
                     tags$div(class = "page-title", "Nouveau rapport annuel"),
                     tags$div(class = "page-subtitle",
                              "Développez chaque section via le bouton  −  , remplissez les données, puis soumettez.")
            ),
            
            # ══ BOX 1 : Identification du projet ════════════════════════════════
            box(
              title = tags$span(
                tags$span(class = "box-badge", "1"),
                icon("folder-open"),
                "Identification du projet",
                uiOutput("statut_projets", inline = TRUE)
              ),
              width       = 12,
              collapsible = TRUE,
              collapsed   = FALSE,
              solidHeader = FALSE,
              
              fluidRow(
                column(8,
                       tags$label(HTML("Intitulé du projet <span class='required-star'>★</span>")),
                       textInput("intitule_projet", NULL,
                                 placeholder = "Ex: Projet d'appui au développement rural (PADR)")
                ),
                column(2,
                       tags$label(HTML("Année cible <span class='required-star'>★</span>")),
                       numericInput("annee_cible", NULL,
                                    value = as.integer(format(Sys.Date(), "%Y")),
                                    min = 2000, max = 2100)
                ),
                column(2,
                       tags$label("Date de remplissage"),
                       dateInput("date_remplissage", NULL,
                                 value    = Sys.Date(),
                                 format   = "dd/mm/yyyy",
                                 language = "fr")
                )
              ),
              
              fluidRow(
                column(5,
                       tags$label(HTML("Ministère de tutelle <span class='required-star'>★</span>")),
                       textInput("ministere_tutelle", NULL,
                                 placeholder = "Ex: Ministère de l'Agriculture")
                ),
                column(3,
                       tags$label("Siège du projet"),
                       textInput("siege_projet", NULL, placeholder = "Ex: Ouagadougou")
                ),
                column(2,
                       tags$label("Année de démarrage"),
                       numericInput("annee_demarrage", NULL, value = NA, min = 2000, max = 2100)
                ),
                column(2,
                       tags$label("Année de fin"),
                       numericInput("annee_fin", NULL, value = NA, min = 2000, max = 2100)
                )
              ),
              
              tags$div(class = "inner-section",
                       tags$div(class = "inner-title", icon("align-left"), "  Description"),
                       fluidRow(
                         column(6,
                                tags$label("Objectif global"),
                                textAreaInput("objectif_global", NULL, rows = 3,
                                              placeholder = "Décrire l'objectif principal du projet...")
                         ),
                         column(6,
                                tags$label("Résultats attendus"),
                                textAreaInput("resultats_attendus", NULL, rows = 3,
                                              placeholder = "Lister les résultats escomptés...")
                         )
                       ),
                       fluidRow(
                         column(6,
                                tags$label("Secteurs d'activités"),
                                textInput("secteurs_activites", NULL,
                                          placeholder = "Ex: Agriculture, Élevage, Hydraulique")
                         ),
                         column(6,
                                tags$label("Zone d'intervention"),
                                textInput("zone_intervention", NULL,
                                          placeholder = "Ex: Régions du Centre, Sahel, Est")
                         )
                       )
              ),
              
              tags$div(class = "inner-section",
                       tags$div(class = "inner-title", icon("user-tie"), "  Responsable & Références"),
                       fluidRow(
                         column(3,
                                tags$label("Nom & Prénoms du responsable"),
                                textInput("responsable_nom", NULL,
                                          placeholder = "Ex: SAWADOGO Hamidou")
                         ),
                         column(3,
                                tags$label("Téléphone"),
                                textInput("responsable_tel", NULL, placeholder = "+226 70 00 00 00")
                         ),
                         column(3,
                                tags$label("Adresse e-mail"),
                                textInput("responsable_email", NULL, placeholder = "nom@projet.bf"),
                                tags$div(class = "hint-text", "Format : nom@domaine.xx")
                         ),
                         column(3,
                                tags$label("Réf. arrêté de création"),
                                textInput("ref_arrete_creation", NULL, placeholder = "N°2023-045/MAAH")
                         )
                       )
              )
            ),
            
            #=======================================================================
            #======================== Données financières ==========================
            #=======================================================================
            
            box(
              title = tags$span(
                tags$span(class = "box-badge", "2"),
                icon("coins"),
                "Exécution financière",
                uiOutput("statut_exec_fin", inline = TRUE)
              ),
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              uiOutput("finance_form"),
              
              tags$button(
                icon("plus"), "  Ajouter une source de financement",
                class   = "btn-add-line",
                onclick = "Shiny.setInputValue('add_row', Math.random())"
              )
            ),
            
            #=======================================================================
            # Situation d’exécution physique par indicateur (en unité physique) ====
            #=======================================================================
            
            box(
              title = tags$span(
                tags$span(class = "box-badge", "3"),
                icon("coins"),
                "Exécution physique",
                uiOutput("statut_exec_fin", inline = TRUE)
              ),
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              uiOutput("indicateur_form"),
              
              tags$button(
                icon("plus"), "  Ajouter un indicateur",
                class   = "btn-add-line",
                onclick = "Shiny.setInputValue('add_row', Math.random())"
              )
            ),
            
            #=======================================================================
            #============ Difficultés rencontreés ==========================
            #=======================================================================
            
            box(
              title = tags$span(
                tags$span(class = "box-badge", "4"),
                icon("coins"),
                "Difficultés rencontrées",
                uiOutput("statut_exec_fin", inline = TRUE)
              ),
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              uiOutput("difficulte_form"),
              
              tags$button(
                icon("plus"), "  Ajouter une difficulté",
                class   = "btn-add-line",
                onclick = "Shiny.setInputValue('add_row', Math.random())"
              )
            ),
            
            #=======================================================================
            #======================== Recommandations ==========================
            #=======================================================================
            
            box(
              title = tags$span(
                tags$span(class = "box-badge", "5"),
                icon("coins"),
                "Recommandations",
                uiOutput("statut_exec_fin", inline = TRUE)
              ),
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              uiOutput("recommandation_form"),
              
              tags$button(
                icon("plus"), "  Ajouter une recommandation",
                class   = "btn-add-line",
                onclick = "Shiny.setInputValue('add_row', Math.random())"
              )
            ),
            
            # ══ Barre de soumission ══════════════════════════════════════════════
            tags$div(class = "submit-bar",
                     tags$div(class = "submit-info",
                              tags$strong("Prêt à soumettre ?"), tags$br(),
                              "Vérifiez toutes les sections. La soumission est une ",
                              tags$strong("transaction unique (tout ou rien).")
                     ),
                     tags$div(
                       tags$button(icon("redo"), " Réinitialiser",
                                   class   = "btn-reset",
                                   onclick = "Shiny.setInputValue('reset_all', Math.random())"
                       ),
                       tags$button(icon("paper-plane"), "  Soumettre en base",
                                   class   = "btn-submit",
                                   onclick = "Shiny.setInputValue('submit_all', Math.random())"
                       )
                     )
            )
    ),
    
    # ══ Onglets placeholder ══════════════════════════════════════════════════
    tabItem(tabName = "liste",
            tags$div(class = "page-header",
                     tags$div(class = "page-title", "Liste des projets"),
                     tags$div(class = "page-subtitle", "Consultez et gérez les projets enregistrés")
            ),
            box(width = 12,
                tags$p(style = "color:#3a3f52; text-align:center; padding:40px 0;",
                       icon("table"), "  DTOutput ici")
            )
    ),
    tabItem(tabName = "dashboard",
            tags$div(class = "page-header",
                     tags$div(class = "page-title", "Tableau de bord"),
                     tags$div(class = "page-subtitle", "Vue synthétique des indicateurs clés")
            ),
            box(width = 12,
                tags$p(style = "color:#3a3f52; text-align:center; padding:40px 0;",
                       icon("chart-bar"), "  Graphiques ici")
            )
    )
  )
)



#===============================================================================
# Le tableau de bord 
dashboardPage(pageHeader, pageSiderbar, pageBody)

#===============================================================================

