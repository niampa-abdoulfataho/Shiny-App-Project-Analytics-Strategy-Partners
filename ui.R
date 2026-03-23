library(shiny)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)
library(shinyjs)
library(lubridate)

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
    # ── Toggle thème dans la navbar ──
    tags$li(
      class = "dropdown",
      style = "padding:8px 16px; display:flex; align-items:center;",
      tags$span(style = "font-size:15px; margin-right:8px;", "🌙"),
      tags$div(
        class   = "toggle-track",
        onclick = "toggleTheme()",
        title   = "Basculer le thème",
        tags$div(class = "toggle-thumb")
      ),
      tags$span(style = "font-size:15px; margin-left:8px;", "☀️")
    ),

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
        /* ══════════════════════════════════════════════
           VARIABLES PAR THÈME
        ══════════════════════════════════════════════ */

        /* 🌑 SOMBRE (défaut) */
        body, body.theme-dark {
          --bg-main:       #0f1117;
          --bg-card:       #13151f;
          --bg-header:     #1a1d2a;
          --bg-input:      #0a0c14;
          --bg-sidebar:    #13151f;
          --border:        #1e2232;
          --text-main:     #d4d8e8;
          --text-muted:    #6b7280;
          --text-faint:    #2e3347;
          --accent:        #f0c040;
          --accent2:       #e07b20;
          --accent-glow:   rgba(240,192,64,.12);
          --accent-soft:   rgba(240,192,64,.08);
          --danger:        #e05c5c;
          --success:       #4ade80;
          --badge-text:    #0f1117;
        }

        /* ☀️ CLAIR */
        body.theme-light {
          --bg-main:       #f4f5f9;
          --bg-card:       #ffffff;
          --bg-header:     #eef0f6;
          --bg-input:      #f9fafb;
          --bg-sidebar:    #1a1d2a;
          --border:        #e2e5ef;
          --text-main:     #111827;
          --text-muted:    #4b5563;
          --text-faint:    #9ca3af;
          --accent:        #d97706;
          --accent2:       #b45309;
          --accent-glow:   rgba(217,119,6,.10);
          --accent-soft:   rgba(217,119,6,.06);
          --danger:        #dc2626;
          --success:       #16a34a;
          --badge-text:    #ffffff;
        }

        /* Texte noir en mode clair sur le body uniquement */
        body.theme-light .content-wrapper,
        body.theme-light .content-wrapper p,
        body.theme-light .content-wrapper span,
        body.theme-light .content-wrapper div {
          color: #111827;
        }
        /* ════ BASE ════ */
        body, .content-wrapper, .main-footer {
          background: var(--bg-main) !important;
          transition: background .3s, color .3s;
        }
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

        /* ── Contenu ── */
        .content { padding: 20px 22px; }

        /* ── BOX override ── */
        .box {
          background: var(--bg-card) !important;
          border: 1px solid var(--border) !important;
          border-radius: 12px !important;
          box-shadow: none !important;
          margin-bottom: 14px !important;
          overflow: hidden;
          transition: background .3s, border-color .3s;
        }
        .box::before {
          content: '';
          display: block; height: 3px;
          background: linear-gradient(90deg, var(--accent), var(--accent2), var(--accent));
        }
        .box-header {
          background: var(--bg-header) !important;
          border-bottom: 1px solid var(--border) !important;
          padding: 14px 18px !important;
          transition: background .3s;
        }
        .box-title {
          font-family: 'Playfair Display', serif !important;
          font-size: 14px !important; font-weight: 700 !important;
          color: var(--text-main) !important; letter-spacing: .3px;
          display: flex; align-items: center; gap: 10px;
        }
        .box-badge {
          background: linear-gradient(135deg, var(--accent), var(--accent2));
          color: var(--badge-text);
          border-radius: 50%; width: 22px; height: 22px;
          display: inline-flex; align-items: center; justify-content: center;
          font-size: 11px; font-weight: 700; flex-shrink: 0;
        }
        .box-title .fa { color: var(--accent); opacity: .6; font-size: 13px; }
        .box-header .btn {
          color: var(--text-faint) !important;
          background: transparent !important;
          border: none !important; font-size: 14px !important;
          transition: color .2s !important; padding: 0 4px !important;
        }
        .box-header .btn:hover { color: var(--accent) !important; }
        .box-body {
          background: var(--bg-card) !important;
          padding: 20px 22px !important;
          transition: background .3s;
        }

        /* Badges statut */
        .statut-badge {
          font-size: 10px; padding: 2px 9px; border-radius: 20px;
          font-weight: 600; letter-spacing: .5px; margin-left: 8px;
        }
        .statut-vide    { background: var(--border); color: var(--text-faint); }
        .statut-partiel { background: var(--accent-soft); color: var(--accent);
                          border: 1px solid var(--accent-glow); }
        .statut-complet { background: rgba(74,222,128,.12); color: var(--success);
                          border: 1px solid rgba(74,222,128,.25); }

        /* Labels & inputs */
        .control-label, label {
          color: var(--text-muted) !important; font-size: 11px !important;
          font-weight: 500 !important; text-transform: uppercase;
          letter-spacing: .7px; margin-bottom: 4px !important;
        }
        .required-star { color: var(--danger); }
        .form-control {
          background: var(--bg-input) !important;
          color: var(--text-main) !important;
          border: 1px solid var(--border) !important;
          border-radius: 7px !important;
          font-size: 13px !important; padding: 8px 12px !important;
          transition: border-color .2s, box-shadow .2s, background .3s;
        }
        .form-control:focus {
          border-color: var(--accent) !important;
          box-shadow: 0 0 0 3px var(--accent-glow) !important;
          outline: none !important;
        }
        .form-control::placeholder { color: var(--text-faint) !important; }
        textarea.form-control { resize: vertical; min-height: 72px; }
        .hint-text { color: var(--text-faint); font-size: 11px; margin-top: 2px; }

        /* Séparateurs internes */
        .inner-section {
          margin-top: 18px; padding-top: 16px;
          border-top: 1px solid var(--border);
        }
        .inner-title {
          color: var(--accent); opacity: .5;
          font-size: 10px; font-weight: 600;
          text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 14px;
        }

        /* Blocs financement */
        .fin-block {
          background: var(--bg-input); border: 1px solid var(--border);
          border-radius: 8px; padding: 16px 18px; margin-bottom: 10px;
          transition: background .3s;
        }
        .fin-block-header {
          display: flex; justify-content: space-between;
          align-items: center; margin-bottom: 14px;
        }
        .fin-block-num {
          color: var(--accent); font-size: 11px; font-weight: 700;
          text-transform: uppercase; letter-spacing: 1px;
        }
        .btn-del-fin {
          background: transparent; color: var(--danger);
          border: 1px solid var(--danger); border-radius: 5px;
          padding: 3px 10px; font-size: 11px; cursor: pointer;
          opacity: .5; transition: all .15s;
        }
        .btn-del-fin:hover { opacity: 1; background: var(--danger); color: white; }

        .fin-pair { display: flex; align-items: center; gap: 6px; margin-bottom: 2px; }
        .fin-badge {
          font-size: 9px; font-weight: 700; padding: 2px 6px;
          border-radius: 3px; flex-shrink: 0; letter-spacing: .5px;
        }
        .fin-badge.ae { background: var(--accent-soft); color: var(--accent2);
                        border: 1px solid var(--accent-glow); }
        .fin-badge.cp { background: var(--accent-soft); color: var(--accent);
                        border: 1px solid var(--accent-glow); }
        .fin-pair .form-group { margin-bottom: 0 !important; flex: 1; }
        .fin-pair .form-group input { padding: 6px 10px !important; font-size: 12px !important; }

        .btn-add-line {
          background: transparent; color: var(--accent);
          border: 1px dashed var(--accent); border-radius: 7px;
          padding: 9px; width: 100%; font-size: 12px; cursor: pointer;
          opacity: .4; transition: all .2s; margin-top: 6px;
          display: flex; align-items: center; justify-content: center; gap: 6px;
        }
        .btn-add-line:hover { opacity: 1; background: var(--accent-soft); }

        /* ══ TOGGLE THÈME ══ */
        .theme-switcher {
          display: inline-flex; align-items: center; gap: 10px;
          background: var(--bg-card);
          border: 1px solid var(--border);
          border-radius: 30px; padding: 6px 14px;
          margin-bottom: 16px;
          transition: background .3s;
          width: fit-content;
        }
        .theme-icon {
          font-size: 15px; line-height:1;
          transition: opacity .3s, transform .3s;
        }
        .theme-icon.moon { color: #a0aec0; }
        .theme-icon.sun  { color: #f0c040; }

        /* Le toggle switch */
        .toggle-track {
          width: 44px; height: 24px; border-radius: 12px;
          background: var(--border);
          position: relative; cursor: pointer;
          transition: background .3s;
        }
        body.theme-light .toggle-track { background: #d97706; }
        .toggle-thumb {
          width: 18px; height: 18px; border-radius: 50%;
          background: #fff;
          position: absolute; top: 3px; left: 3px;
          transition: transform .3s cubic-bezier(.4,0,.2,1);
          box-shadow: 0 1px 4px rgba(0,0,0,.3);
        }
        body.theme-light .toggle-thumb { transform: translateX(20px); }

        /* Barre de soumission */
        .submit-bar {
          background: var(--bg-card); border: 1px solid var(--border);
          border-radius: 12px; padding: 18px 24px;
          display: flex; align-items: center; justify-content: space-between;
          margin-top: 8px; margin-bottom: 30px;
          transition: background .3s;
        }
        .submit-info { color: var(--text-muted); font-size: 12px; line-height: 1.7; }
        .submit-info strong { color: var(--text-main); }
        .btn-submit {
          background: linear-gradient(135deg, var(--accent), var(--accent2));
          color: var(--badge-text); border: none; border-radius: 8px;
          font-weight: 700; font-size: 13px; letter-spacing: .5px;
          padding: 12px 32px; cursor: pointer;
          transition: transform .15s, box-shadow .15s;
          text-transform: uppercase;
        }
        .btn-submit:hover {
          transform: translateY(-1px);
          box-shadow: 0 8px 24px var(--accent-glow);
        }
        .btn-reset {
          background: transparent; color: var(--text-muted);
          border: 1px solid var(--border); border-radius: 7px;
          padding: 10px 20px; font-size: 12px; cursor: pointer;
          transition: all .15s; margin-right: 8px;
        }
        .btn-reset:hover { border-color: var(--danger); color: var(--danger); }

        /* Page header */
        .page-header { margin-bottom: 22px; }
        .page-title {
          font-family: 'Playfair Display', serif;
          font-size: 22px; font-weight: 700; color: var(--text-main);
          transition: color .3s;
        }
        .page-subtitle { color: var(--text-faint); font-size: 12px; margin-top: 3px; }
      ")),
    
    # ── JS : toggle dark / light ──
    tags$script(HTML("
        function toggleTheme() {
          var isLight = document.body.classList.contains('theme-light');
          var next    = isLight ? 'dark' : 'light';
          document.body.classList.remove('theme-dark', 'theme-light');
          document.body.classList.add('theme-' + next);
          localStorage.setItem('gp_theme', next);
          Shiny.setInputValue('active_theme', next);
        }

        document.addEventListener('DOMContentLoaded', function() {
          var saved = localStorage.getItem('gp_theme') || 'dark';
          document.body.classList.remove('theme-dark', 'theme-light');
          document.body.classList.add('theme-' + saved);
          Shiny.setInputValue('active_theme', saved);
        });
      "))
  ),
  
  #tags$div(style = "padding:20px 16px 10px;",
  #         tags$div(class = "prog-label", "Progression globale")
  #),
  #tags$div(class = "progress-sidebar",
  #         tags$div(class = "prog-bar-bg",
   #                 tags$div(class = "prog-bar-fill", style = "width:33%;")
   #        ),
   #        tags$div(class = "prog-count", "2 / 6 sections")
  #),
  
  sidebarMenu(
    #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                  label = "Search..."),
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
                column(4,
                       tags$label(HTML("Intitulé du projet <span class='required-star'>★</span>")),
                       textInput("intitule_projet", NULL,
                                 placeholder = "Ex: Projet d'appui au développement rural (PADR)")
                ),
                column(4,
                       tags$label(HTML("Ministère de tutelle <span class='required-star'>★</span>")),
                       textInput("ministere_tutelle", NULL,
                                 placeholder = "Ex: Ministère de l'Agriculture")
                ),
                column(4,
                       tags$label("Siège du projet"),
                       textInput("siege_projet", NULL, placeholder = "Ex: Ouagadougou")
                )),
              
              fluidRow(
                column(4,
                       tags$label(HTML("Année cible <span class='required-star'>★</span>")),
                       numericInput("annee_cible", NULL,
                                    value = as.integer(format(Sys.Date(), "%Y"))-1,
                                    min = 2000, max = 2100)
                ),
                column(4,
                       tags$label("Année de démarrage"),
                       numericInput("annee_demarrage", NULL, value = NA, min = 2000, max = 2100)
                ),
                column(4,
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
                                tags$label("Nom & Prénoms"),
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
                                tags$label("Réf de création"),
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
                icon("tasks"),
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
                icon("ban"),
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
                icon("lightbulb"),
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
                              "Vérifiez toutes les sections." # La soumission est une ",
                              #tags$strong("transaction unique (tout ou rien).")
                     ),
                     tags$div(
                       tags$button(icon("redo"), " Réinitialiser",
                                   class   = "btn-reset",
                                   onclick = "Shiny.setInputValue('reset_all', Math.random())"
                       ),
                       tags$button(icon("paper-plane"), "  Soumettre",
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

