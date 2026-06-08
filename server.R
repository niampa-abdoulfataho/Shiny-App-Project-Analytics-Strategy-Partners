# ============================================================
# server.R — Authentification + routing par rôle
# ============================================================

source("modules/global.R")
source("modules/auth.R")

server <- function(input, output, session) {

  # ── Vérification credentials via PostgreSQL ─────────────────
  res_auth <- secure_server(
    check_credentials = check_credentials_db
  )

  # ── Infos utilisateur connecté ───────────────────────────────
  normalize_user_info <- function(info) {
    if (is.null(info)) return(NULL)

    if (is.data.frame(info)) {
      if (nrow(info) == 0) return(NULL)
      info <- as.list(info[1, , drop = FALSE])
      return(lapply(info, function(x) x[[1]]))
    }

    info
  }

  user_info <- reactive({
    user_email <- res_auth$user

    if (!is.null(user_email) && nzchar(as.character(user_email))) {
      user_db <- tryCatch(
        db_get_user_by_email(as.character(user_email)),
        error = function(e) data.frame()
      )

      if (nrow(user_db) > 0) {
        return(list(
          user = user_db$email[[1]],
          nom_structure = user_db$nom_structure[[1]],
          role = user_db$role[[1]],
          utilisateur_id = user_db$id[[1]]
        ))
      }
    }

    info <- normalize_user_info(res_auth$user_info)
    req(info)
    info
  })

  user_role <- reactive({
    info <- user_info()
    role <- tolower(trimws(as.character(info$role %||% "ong")))
    if (!role %in% c("admin", "ong")) "ong" else role
  })

  user_id <- reactive({
    info <- user_info()
    as.integer(info$utilisateur_id)
  })

  # ── Routing : afficher la bonne UI selon le rôle ────────────
  output$app_title <- renderUI({
    role <- tryCatch(user_role(), error = function(e) NULL)

    if (identical(role, "admin")) {
      tags$span(
        tags$img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
                 height = "26px", style = "margin-right:8px;vertical-align:middle;"),
        tags$span("GestioProjets - Admin",
          style = "font-family:'Playfair Display',serif;font-weight:700;font-size:15px;")
      )
    } else {
      tags$span(
        tags$img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
                 height = "26px", style = "margin-right:8px;vertical-align:middle;"),
        tags$span("GestioProjets",
          style = "font-family:'Playfair Display',serif;font-weight:700;font-size:17px;")
      )
    }
  })

  output$sidebar_menu <- renderUI({
    role <- tryCatch(user_role(), error = function(e) NULL)
    if (is.null(role)) {
      return(tags$div(
        style = "padding:16px; color:#6b7280; font-size:12px;",
        "Chargement du profil..."
      ))
    }

    if (role == "admin") {
      sidebarMenu(
        id = "main_menu",
        menuItem("Sessions", tabName = "sessions", icon = icon("calendar")),
        menuItem("Suivi enquete", tabName = "suivi_enquete", icon = icon("chart-line")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Soumissions", tabName = "soumissions", icon = icon("folder")),
        menuItem("Demandes", tabName = "demandes", icon = icon("envelope")),
        menuItem("Utilisateurs", tabName = "utilisateurs", icon = icon("users")),
        menuItem("Tableau de bord", tabName = "dashboard", icon = icon("chart-bar")),
        menuItem("Notifications", tabName = "notifications", icon = icon("bell"))
      )
    } else {
      sidebarMenu(
        id = "main_menu",
        menuItem("Saisie", tabName = "saisie", icon = icon("edit")),
        menuItem("Historique", tabName = "historique", icon = icon("history")),
        menuItem("Notifications", tabName = "notifications", icon = icon("bell"))
      )
    }
  })

  output$role_body <- renderUI({
    role <- tryCatch(user_role(), error = function(e) NULL)
    if (is.null(role)) {
      return(tags$div(
        class = "page-header",
        tags$div(class = "page-title", "Chargement du profil"),
        tags$div(class = "page-subtitle", "Connexion reussie, recuperation du role en cours.")
      ))
    }

    if (role == "admin") {
      tabItems(
        tabItem(tabName = "sessions", uiOutput("page_sessions")),
        tabItem(tabName = "suivi_enquete", uiOutput("page_suivi_enquete")),
        tabItem(tabName = "data", uiOutput("page_data")),
        tabItem(tabName = "soumissions", uiOutput("page_soumissions")),
        tabItem(tabName = "demandes", uiOutput("page_demandes")),
        tabItem(tabName = "utilisateurs", uiOutput("page_utilisateurs")),
        tabItem(tabName = "dashboard", uiOutput("page_dashboard")),
        tabItem(tabName = "notifications", uiOutput("page_notifications"))
      )
    } else {
      tabItems(
        tabItem(tabName = "saisie", uiOutput("page_saisie")),
        tabItem(tabName = "historique", uiOutput("page_historique")),
        tabItem(tabName = "notifications", uiOutput("page_notifications"))
      )
    }
  })

  observeEvent(user_role(), {
    updateTabItems(
      session,
      inputId = "main_menu",
      selected = if (user_role() == "admin") "sessions" else "saisie"
    )
  }, ignoreInit = FALSE)
  
  # ── Info utilisateur dans le header ─────────────────────────
  output$header_user_info <- renderUI({
    info <- tryCatch(user_info(), error = function(e) NULL)
    if (is.null(info)) {
      return(tags$span(
        style = "color:#6b7280; font-size:11px;",
        "Profil en cours..."
      ))
    }
    tags$span(
      style = "color:#6b7280; font-size:11px;",
      "🏢 ", info$nom_structure %||% info$user
    )
  })

  
  # ── Déconnexion ──────────────────────────────────────────────
  observeEvent(input$btn_logout, {
    session$reload()
  })

  # ════════════════════════════════════════════════════════════
  # NOTIFICATIONS
  # ════════════════════════════════════════════════════════════

  # Timer : rafraîchir les notifs toutes les 30 secondes
  notif_timer <- reactiveTimer(30000)

  notif_count <- reactive({
    notif_timer()
    req(user_id())
    tryCatch(db_count_notif_non_lues(user_id()), error = function(e) 0)
  })

  output$notif_count_badge <- renderUI({
    n <- notif_count()
    if (n > 0)
      tags$span(class = "notif-badge", n)
    else
      NULL
  })

  output$page_notifications <- renderUI({
    req(user_id())
    notifs <- tryCatch(
      db_get_notifications_user(user_id()),
      error = function(e) data.frame()
    )
    db_marquer_notif_lues(user_id())

    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title", "🔔 Notifications"),
        tags$div(class="page-subtitle",
          paste0(nrow(notifs), " notification(s)"))
      ),
      box(width=12,
        title = tags$span(tags$span(class="box-badge","N"),
                          icon("bell"), "Mes notifications"),
        collapsible = FALSE,
        if (nrow(notifs) == 0) {
          tags$p(style="color:var(--text-faint); text-align:center; padding:30px;",
            "Aucune notification.")
        } else {
          tags$div(
            lapply(seq_len(nrow(notifs)), function(i) {
              n <- notifs[i,]
              tags$div(
                style = paste0(
                  "padding:12px 16px; border-bottom:1px solid var(--border);",
                  if(!n$lu) "border-left:3px solid var(--accent);" else ""
                ),
                tags$div(style="font-size:12px; color:var(--text-main);", n$message),
                tags$div(style="font-size:10px; color:var(--text-faint); margin-top:4px;",
                  format(n$created_at, "%d/%m/%Y %H:%M"))
              )
            })
          )
        }
      )
    )
  })

  # ════════════════════════════════════════════════════════════
  # PAGE SAISIE (ONG)
  # ════════════════════════════════════════════════════════════
  output$page_saisie <- renderUI({
    req(user_role() == "ong")
    sessions_ouvertes <- tryCatch(
      db_get_sessions_ouvertes(),
      error = function(e) data.frame()
    )

    if (nrow(sessions_ouvertes) == 0) {
      return(tags$div(
        tags$div(class="page-header",
          tags$div(class="page-title","📋 Saisie du rapport"),
          tags$div(class="page-subtitle","Aucune session ouverte pour le moment.")
        ),
        box(width=12,
          tags$p(style="color:var(--text-faint); text-align:center; padding:40px;",
            icon("lock"), "  Aucune enquête n'est actuellement ouverte.",
            tags$br(),
            tags$span(style="font-size:11px;",
              "Vous serez notifié par mail dès qu'une session sera disponible."))
        )
      ))
    }

    choices_sessions <- setNames(sessions_ouvertes$id, sessions_ouvertes$label)

    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","📋 Saisie du rapport"),
        tags$div(class="page-subtitle","Remplissez toutes les sections puis soumettez.")
      ),

      # Sélection session
      box(width=12, collapsible=FALSE,
        title=tags$span(icon("calendar"), " Session en cours"),
        fluidRow(
          column(6,
            tags$label(HTML("Session <span class='required-star'>★</span>")),
            selectInput("sel_session_saisie", NULL,
              choices = choices_sessions)
          ),
          column(6,
            tags$br(),
            uiOutput("session_info_badge")
          )
        )
      ),

      # Formulaire sections (injecté depuis server_form.R)
      uiOutput("form_sections")
    )
  })

  output$session_info_badge <- renderUI({
    req(input$sel_session_saisie)
    sessions <- db_get_sessions_ouvertes()
    sel <- sessions[sessions$id == as.integer(input$sel_session_saisie),]
    if (nrow(sel) == 0) return(NULL)
    tags$div(
      tags$span(class="statut-badge statut-complet",
        paste0("📅 Fermeture : ", format(sel$date_fin, "%d/%m/%Y")))
    )
  })

  output$form_sections <- renderUI({
    tags$div(
      class = "submit-bar",
      tags$div(
        class = "submit-info",
        tags$strong("Formulaire de saisie"),
        tags$br(),
        "Les sections detaillees seront branchees ici."
      )
    )
  })

  output$page_historique <- renderUI({
    req(user_role() == "ong")
    soumissions <- tryCatch(
      db_get_soumissions_user(user_id()),
      error = function(e) data.frame()
    )

    tags$div(
      tags$div(
        class = "page-header",
        tags$div(class = "page-title", "Historique"),
        tags$div(class = "page-subtitle", "Vos soumissions precedentes.")
      ),
      box(
        width = 12,
        title = tags$span(tags$span(class = "box-badge", "H"), icon("history"), "Mes soumissions"),
        if (nrow(soumissions) == 0) {
          tags$p(
            style = "color:var(--text-faint); text-align:center; padding:30px;",
            "Aucune soumission pour le moment."
          )
        } else {
          DTOutput("dt_historique")
        }
      )
    )
  })

  output$dt_historique <- renderDT({
    soumissions <- tryCatch(
      db_get_soumissions_user(user_id()),
      error = function(e) data.frame()
    )
    req(nrow(soumissions) > 0)
    datatable(
      soumissions[, c("id", "session_label", "version", "statut", "created_at", "updated_at")],
      selection = "single",
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, dom = "ft"),
      colnames = c("ID", "Session", "Version", "Statut", "Creee le", "Mise a jour")
    )
  })

  # ════════════════════════════════════════════════════════════
  # PAGES ADMIN
  # ════════════════════════════════════════════════════════════

  # ── Sessions ─────────────────────────────────────────────────
  output$page_sessions <- renderUI({
    req(user_role() == "admin")
    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","🗓️ Gestion des sessions"),
        tags$div(class="page-subtitle","Créer, ouvrir et fermer les enquêtes trimestrielles.")
      ),
      # Créer une nouvelle session
      tags$div(
        class = "submit-bar",
        tags$div(class = "submit-info",
          tags$strong("Nouvelle session"),
          tags$br(),
          "Creez une enquete trimestrielle depuis une fenetre dediee."
        ),
        actionButton("btn_open_session_modal", "Nouvelle session", icon = icon("plus"))
      ),

      # Liste des sessions
      box(width=12, collapsible=TRUE, collapsed=FALSE,
        title=tags$span(tags$span(class="box-badge","L"), icon("list"), "Toutes les sessions"),
        DTOutput("dt_sessions"),
        br(),
        fluidRow(
          column(6,
            actionButton("btn_ouvrir_session","🔓 Ouvrir la session sélectionnée",
              style="background:#4ade8022; color:#4ade80; border:1px solid #4ade8044;
                     border-radius:7px; padding:8px 16px; font-size:12px;")
          ),
          column(6,
            actionButton("btn_fermer_session","🔒 Fermer la session sélectionnée",
              style="background:#e05c5c22; color:#e05c5c; border:1px solid #e05c5c44;
                     border-radius:7px; padding:8px 16px; font-size:12px;")
          )
        )
      )
    )
  })

  output$dt_sessions <- renderDT({
    sessions <- tryCatch(db_get_all_sessions(), error=function(e) data.frame())
    req(nrow(sessions) > 0)
    datatable(
      sessions[,c("id","label","trimestre","date_ouverture","date_fin","statut","createur")],
      selection="single", rownames=FALSE,
      options=list(scrollX=TRUE, pageLength=10, dom="ft"),
      colnames=c("ID","Label","Trimestre","Ouverture","Clôture","Statut","Créé par")
    ) |>
      formatStyle("statut",
        backgroundColor = styleEqual(c("ouverte","fermee"),
          c("rgba(74,222,128,.15)","rgba(224,92,92,.15)")),
        color = styleEqual(c("ouverte","fermee"), c("#4ade80","#e05c5c"))
      )
  })

  observeEvent(input$btn_open_session_modal, {
    showModal(modalDialog(
      title = "Nouvelle session",
      fluidRow(
        column(6,
          tags$label(HTML("Label <span class='required-star'>*</span>")),
          textInput("sess_modal_label", NULL, placeholder = "Ex: Enquete T1 2026")
        ),
        column(6,
          tags$label("Trimestre"),
          selectInput("sess_modal_trimestre", NULL, choices = c("T1", "T2", "T3", "T4"))
        )
      ),
      fluidRow(
        column(6,
          tags$label("Date ouverture"),
          dateInput("sess_modal_ouverture", NULL, value = Sys.Date(), language = "fr")
        ),
        column(6,
          tags$label("Date de cloture"),
          dateInput("sess_modal_fin", NULL, value = Sys.Date() + 30, language = "fr")
        )
      ),
      tags$label("Description"),
      textAreaInput("sess_modal_description", NULL, rows = 3,
        placeholder = "Contexte et instructions pour les ONG..."),
      checkboxInput("sess_modal_notifier", "Notifier les ONG par mail", value = TRUE),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("btn_create_session_modal", "Creer la session", icon = icon("save"))
      )
    ))
  })

  observeEvent(input$btn_create_session_modal, {
    req(input$sess_modal_label, input$sess_modal_ouverture, input$sess_modal_fin)
    tryCatch({
      sess_id <- db_create_session(
        label          = input$sess_modal_label,
        trimestre      = input$sess_modal_trimestre,
        date_ouverture = input$sess_modal_ouverture,
        date_fin       = input$sess_modal_fin,
        description    = input$sess_modal_description %||% "",
        created_by     = user_id()
      )
      if (isTRUE(input$sess_modal_notifier)) {
        ongs <- db_get_all_ong()
        lapply(seq_len(nrow(ongs)), function(i) {
          mail_session_ouverte(
            to            = ongs$email[i],
            nom_structure = ongs$nom_structure[i],
            session_label = input$sess_modal_label,
            date_fin      = input$sess_modal_fin
          )
          db_create_notification(
            utilisateur_id = ongs$id[i],
            type           = "session_ouverte",
            message        = paste0("Nouvelle enquete ouverte : ", input$sess_modal_label),
            lien           = "#saisie"
          )
        })
      }
      removeModal()
      showNotification(paste0("Session '", input$sess_modal_label, "' creee."), type="message")
    }, error=function(e) {
      showNotification(paste0("Erreur : ", e$message), type="error")
    })
  })

  observeEvent(input$btn_create_session, {
    req(input$sess_label, input$sess_ouverture, input$sess_fin)
    tryCatch({
      sess_id <- db_create_session(
        label          = input$sess_label,
        trimestre      = input$sess_trimestre,
        date_ouverture = input$sess_ouverture,
        date_fin       = input$sess_fin,
        description    = input$sess_description %||% "",
        created_by     = user_id()
      )
      # Notifier les ONG par mail si coché
      if (isTRUE(input$sess_notifier)) {
        ongs <- db_get_all_ong()
        lapply(seq_len(nrow(ongs)), function(i) {
          mail_session_ouverte(
            to            = ongs$email[i],
            nom_structure = ongs$nom_structure[i],
            session_label = input$sess_label,
            date_fin      = input$sess_fin
          )
          db_create_notification(
            utilisateur_id = ongs$id[i],
            type           = "session_ouverte",
            message        = paste0("Nouvelle enquête ouverte : ", input$sess_label),
            lien           = "#saisie"
          )
        })
      }
      showNotification(paste0("✅ Session '", input$sess_label, "' créée."),
                       type="message")
    }, error=function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type="error")
    })
  })

  observeEvent(input$btn_ouvrir_session, {
    idx <- input$dt_sessions_rows_selected
    req(idx)
    sessions <- db_get_all_sessions()
    db_update_session_statut(sessions$id[idx], "ouverte")
    showNotification("✅ Session ouverte.", type="message")
  })

  observeEvent(input$btn_fermer_session, {
    idx <- input$dt_sessions_rows_selected
    req(idx)
    sessions <- db_get_all_sessions()
    db_update_session_statut(sessions$id[idx], "fermee")
    showNotification("✅ Session fermée.", type="message")
  })

  # ── Demandes ─────────────────────────────────────────────────
  # Suivi de l'enquete en cours
  suivi_refresh <- reactiveTimer(30000)
  suivi_state <- reactiveValues(last_absents = NULL, last_brouillons = NULL)
  correction_soumission_id <- reactiveVal(NULL)

  pct <- function(n, total) {
    if (is.null(total) || total == 0) 0 else round(100 * n / total)
  }

  kpi_card <- function(label, value, percent, color_class) {
    tags$div(
      class = "kpi-card",
      tags$div(class = "kpi-label", label),
      tags$div(class = "kpi-value", value),
      tags$div(class = "kpi-sub", paste0(percent, "% du total attendu")),
      tags$div(class = "mini-progress",
        tags$div(
          class = paste("mini-progress-fill", color_class),
          style = paste0("width:", min(max(percent, 0), 100), "%;")
        )
      )
    )
  }

  status_badge_html <- function(statut) {
    if (identical(statut, "Soumis")) {
      "<span class='status-pill status-submitted'>Soumis</span>"
    } else if (identical(statut, "En cours")) {
      "<span class='status-pill status-draft'>En cours</span>"
    } else {
      "<span class='status-pill status-absent'>Absent</span>"
    }
  }

  pending_badge_html <- function(n) {
    if (!is.na(n) && n > 0) {
      paste0("<span class='status-pill status-draft'>Oui (", n, ")</span>")
    } else {
      "<span class='status-pill status-neutral'>Non</span>"
    }
  }

  fmt_empty <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "-" else as.character(x)
  }

  detail_row <- function(label, value) {
    tags$tr(
      tags$th(style = "width:28%; color:var(--c-muted); font-weight:600;", label),
      tags$td(fmt_empty(value))
    )
  }

  compact_table <- function(data, empty_label) {
    if (is.null(data) || nrow(data) == 0) {
      return(tags$p(class = "hint-text", empty_label))
    }
    keep <- setdiff(names(data), c("id", "projet_id", "soumission_id", "created_at", "updated_at"))
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-condensed",
        style = "font-size:11px; color:var(--c-text);",
        tags$thead(tags$tr(lapply(keep, tags$th))),
        tags$tbody(lapply(seq_len(nrow(data)), function(i) {
          tags$tr(lapply(keep, function(col) tags$td(fmt_empty(data[[col]][i]))))
        }))
      )
    )
  }

  submission_review_ui <- function(soum, details) {
    p <- details$projet
    tags$div(
      tags$div(class = "submit-info",
        tags$strong(soum$nom_structure), tags$br(),
        paste0("Session : ", soum$session_label, " | Statut : ", soum$statut, " | Version : v", soum$version)
      ),
      tags$hr(),
      tags$h5("Identification"),
      if (nrow(p) == 0) {
        tags$p(class = "hint-text", "Aucune donnee d'identification.")
      } else {
        tags$table(
          class = "table table-condensed",
          style = "font-size:11px; color:var(--c-text);",
          tags$tbody(
            detail_row("Intitule du projet", p$intitule_projet),
            detail_row("Annee cible", p$annee_cible),
            detail_row("Date de remplissage", p$date_remplissage),
            detail_row("Ministere de tutelle", p$ministere_tutelle),
            detail_row("Siege", p$siege_projet),
            detail_row("Objectif global", p$objectif_global),
            detail_row("Resultats attendus", p$resultats_attendus),
            detail_row("Secteurs", p$secteurs_activites),
            detail_row("Zone d'intervention", p$zone_intervention),
            detail_row("Responsable", p$responsable_nom),
            detail_row("Telephone", p$responsable_tel),
            detail_row("Email", p$responsable_email)
          )
        )
      },
      tags$h5("Execution financiere"),
      compact_table(details$finance, "Aucune ligne financiere."),
      tags$h5("Execution physique"),
      compact_table(details$physique, "Aucun indicateur physique."),
      tags$h5("Difficultes"),
      compact_table(details$difficultes, "Aucune difficulte renseignee."),
      tags$h5("Recommandations"),
      compact_table(details$recommandations, "Aucune recommandation renseignee.")
    )
  }

  suivi_sessions <- reactive({
    req(user_role() == "admin")
    suivi_refresh()
    tryCatch(db_get_all_sessions(), error = function(e) data.frame())
  })

  selected_suivi_session <- reactive({
    sessions <- suivi_sessions()
    req(nrow(sessions) > 0)
    session_id <- input$suivi_session %||% sessions$id[1]
    sessions[sessions$id == as.integer(session_id), , drop = FALSE]
  })

  suivi_data <- reactive({
    sess <- selected_suivi_session()
    req(nrow(sess) == 1)
    data <- tryCatch(db_get_suivi_session(sess$id), error = function(e) data.frame())
    if (nrow(data) == 0) return(data)
    data$statut_suivi <- ifelse(
      is.na(data$soumission_id), "Absent",
      ifelse(data$statut == "soumis", "Soumis", "En cours")
    )
    data
  })

  suivi_counts <- reactive({
    data <- suivi_data()
    total <- nrow(data)
    submitted <- sum(data$statut_suivi == "Soumis", na.rm = TRUE)
    draft <- sum(data$statut_suivi == "En cours", na.rm = TRUE)
    absent <- sum(data$statut_suivi == "Absent", na.rm = TRUE)
    pending <- sum(data$demandes_en_attente, na.rm = TRUE)
    list(total = total, submitted = submitted, draft = draft, absent = absent, pending = pending)
  })

  output$page_suivi_enquete <- renderUI({
    req(user_role() == "admin")
    sessions <- suivi_sessions()

    if (nrow(sessions) == 0) {
      return(tags$div(
        tags$div(class = "page-header",
          tags$div(class = "page-title", "Suivi de l'enquete en cours"),
          tags$div(class = "page-subtitle", "Aucune session disponible.")
        ),
        box(width = 12,
          tags$p(style = "color:var(--c-muted); text-align:center; padding:40px;",
            icon("calendar"), " Creez une session avant d'activer le suivi.")
        )
      ))
    }

    open_sessions <- sessions[sessions$statut == "ouverte", , drop = FALSE]
    default_session <- if (nrow(open_sessions) > 0) open_sessions$id[1] else sessions$id[1]
    choices <- setNames(sessions$id, sessions$label)

    tags$div(
      tags$div(class = "page-header",
        tags$div(class = "page-title", "Suivi de l'enquete en cours"),
        tags$div(class = "page-subtitle", "Participation, soumissions, alertes et relances.")
      ),
      box(width = 12, collapsible = FALSE,
        title = tags$span(tags$span(class = "box-badge", "S"), icon("calendar-check"), "Session surveillee"),
        tags$div(class = "tracking-meta",
          tags$div(class = "tracking-meta-selector",
            tags$div(class = "tracking-meta-label", "Session"),
            selectInput("suivi_session", NULL, choices = choices, selected = default_session)
          ),
          uiOutput("suivi_meta_trimestre"),
          uiOutput("suivi_meta_ouverture"),
          uiOutput("suivi_meta_jours"),
          uiOutput("suivi_meta_statut")
        )
      ),
      fluidRow(
        column(12, uiOutput("suivi_kpis"))
      ),
      fluidRow(
        column(12,
          box(width = 12,
            title = tags$span(tags$span(class = "box-badge", "T"), icon("table"), "Suivi par ONG"),
            radioButtons("suivi_filtre_statut", NULL,
              choices = c("Tous", "Soumis", "En cours", "Absent"),
              selected = "Tous", inline = TRUE),
            DTOutput("dt_suivi_ong"),
            tags$div(class = "tracking-actions",
              actionButton("btn_relance_absents", "Relancer les absents", icon = icon("paper-plane")),
              actionButton("btn_relance_brouillons", "Relancer les brouillons", icon = icon("pen")),
              uiOutput("suivi_last_relance")
            )
          )
        )),
        fluidRow(
        column(8,
          box(width = 12,
            title = tags$span(tags$span(class = "box-badge", "P"), icon("chart-pie"), "Participation"),
            plotOutput("plot_participation", height = "180px")
          )),
        column(4,
          box(width = 12,
            title = tags$span(tags$span(class = "box-badge", "A"), icon("exclamation-triangle"), "Alertes"),
            uiOutput("suivi_alertes")
          )
        )
      ),
      box(width = 12,
        title = tags$span(tags$span(class = "box-badge", "F"), icon("clock"), "Activite recente"),
        uiOutput("suivi_activite")
      )
    )
  })

  output$suivi_meta_trimestre <- renderUI({
    sess <- selected_suivi_session()
    tags$div(class = "tracking-meta-item",
      tags$div(class = "tracking-meta-label", "Trimestre"),
      tags$div(class = "tracking-meta-value", sess$trimestre %||% "-"))
  })

  output$suivi_meta_ouverture <- renderUI({
    sess <- selected_suivi_session()
    tags$div(class = "tracking-meta-item",
      tags$div(class = "tracking-meta-label", "Ouverture"),
      tags$div(class = "tracking-meta-value", format(sess$date_ouverture, "%d/%m/%Y")))
  })

  output$suivi_meta_jours <- renderUI({
    sess <- selected_suivi_session()
    jours <- as.integer(as.Date(sess$date_fin) - Sys.Date())
    class_jours <- if (jours < 7 && sess$statut == "ouverte") "tracking-meta-value is-danger" else "tracking-meta-value"
    tags$div(class = "tracking-meta-item",
      tags$div(class = "tracking-meta-label", "Jours restants"),
      tags$div(class = class_jours, ifelse(jours >= 0, paste(jours, "jour(s)"), "Cloturee")))
  })

  output$suivi_meta_statut <- renderUI({
    sess <- selected_suivi_session()
    status_class <- if (sess$statut == "ouverte") "tracking-meta-value is-success" else "tracking-meta-value is-danger"
    tags$div(class = "tracking-meta-item",
      tags$div(class = "tracking-meta-label", "Statut"),
      tags$div(class = status_class, tools::toTitleCase(sess$statut)))
  })

  output$suivi_kpis <- renderUI({
    counts <- suivi_counts()
    tags$div(class = "kpi-grid",
      kpi_card("ONG enregistrees", counts$total, 100, "fill-blue"),
      kpi_card("Soumissions completes", counts$submitted, pct(counts$submitted, counts$total), "fill-green"),
      kpi_card("En cours", counts$draft, pct(counts$draft, counts$total), "fill-orange"),
      kpi_card("Pas encore commence", counts$absent, pct(counts$absent, counts$total), "fill-red"))
  })

  output$plot_participation <- renderPlot({
    counts <- suivi_counts()
    values <- c(counts$submitted, counts$draft, counts$absent)
    names(values) <- c("Soumis", "En cours", "Absent")
    colors <- c("#35b86b", "#e8a020", "#df5b5b")
    par(mar = c(2, 4, 1, 1), bg = NA, fg = "#cdd2e0")
    if (sum(values) == 0) {
      plot.new()
      text(.5, .5, "Aucune ONG active", col = "#6e778f", cex = .9)
      return()
    }
    bp <- barplot(values, horiz = TRUE, col = colors, border = NA, las = 1, xlim = c(0, max(values) * 1.2))
    text(values + max(values) * .04, bp, paste0(pct(values, sum(values)), "%"), col = "#cdd2e0", cex = .85)
  })

  output$dt_suivi_ong <- renderDT({
    data <- suivi_data()
    req(nrow(data) > 0)
    filtre <- input$suivi_filtre_statut %||% "Tous"
    if (filtre != "Tous") data <- data[data$statut_suivi == filtre, , drop = FALSE]

    table <- data.frame(
      Structure = data$nom_structure,
      Statut = vapply(data$statut_suivi, status_badge_html, character(1)),
      Version = ifelse(is.na(data$version), "-", paste0("v", data$version)),
      `Derniere activite` = ifelse(is.na(data$updated_at), "-", format(as.POSIXct(data$updated_at), "%d/%m/%Y %H:%M")),
      `Sections remplies` = paste0(data$sections_remplies, "/5"),
      `Demande en attente` = vapply(data$demandes_en_attente, pending_badge_html, character(1)),
      Actions = paste0(
        "<button class='table-action' onclick=\"Shiny.setInputValue('suivi_view_submission', ",
        ifelse(is.na(data$soumission_id), "null", data$soumission_id),
        ", {priority:'event'})\">Voir</button>",
        "<button class='table-action' onclick=\"Shiny.setInputValue('suivi_remind_one', ",
        data$utilisateur_id,
        ", {priority:'event'})\">Relancer</button>"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    datatable(table, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(scrollX = TRUE, pageLength = 12, dom = "ftip"))
  })

  output$suivi_alertes <- renderUI({
    counts <- suivi_counts()
    sess <- selected_suivi_session()
    jours <- as.integer(as.Date(sess$date_fin) - Sys.Date())
    participation <- pct(counts$submitted, counts$total)
    tags$div(class = "alert-list",
      if (counts$absent > 0 && jours < 7 && sess$statut == "ouverte")
        tags$div(class = "tracking-alert danger",
          paste0(counts$absent, " ONG n'ont pas encore commence - cloture dans ", jours, " jour(s).")),
      if (counts$pending > 0)
        tags$div(class = "tracking-alert warning",
          paste0(counts$pending, " demande(s) de modification en attente de traitement.")),
      if (participation >= 85)
        tags$div(class = "tracking-alert success",
          paste0("Taux de participation : ", participation, "% - objectif atteint."))
      else
        tags$div(class = "tracking-alert warning",
          paste0("Taux de participation : ", participation, "% - objectif a consolider.")))
  })

  output$suivi_activite <- renderUI({
    sess <- selected_suivi_session()
    activite <- tryCatch(db_get_activite_session(sess$id), error = function(e) data.frame())
    if (nrow(activite) == 0) {
      return(tags$p(style = "color:var(--c-muted); text-align:center; padding:20px;", "Aucune activite recente."))
    }
    tags$div(class = "activity-feed",
      lapply(seq_len(nrow(activite)), function(i) {
        a <- activite[i, ]
        dot <- "●"
        color <- switch(a$type, soumis = "#35b86b", brouillon = "#e8a020", demande = "#df5b5b", session = "#9aa3b8", "#9aa3b8")
        tags$div(class = "activity-item",
          tags$div(class = "activity-time", format(as.POSIXct(a$event_time), "%H:%M")),
          tags$div(
            tags$div(class = "activity-title",
              tags$span(style = paste0("color:", color, "; margin-right:6px;"), dot),
              a$structure),
            tags$div(class = "activity-text", paste("->", a$message))))
      })
    )
  })

  output$suivi_last_relance <- renderUI({
    logs <- c(
      if (!is.null(suivi_state$last_absents)) paste0("Absents : ", suivi_state$last_absents),
      if (!is.null(suivi_state$last_brouillons)) paste0("Brouillons : ", suivi_state$last_brouillons)
    )
    if (length(logs) == 0) return(tags$span(class = "hint-text", "Aucune relance envoyee depuis l'ouverture de la page."))
    tags$span(class = "hint-text", paste(logs, collapse = " | "))
  })

  send_tracking_reminders <- function(target) {
    data <- suivi_data()
    sess <- selected_suivi_session()
    if (target == "absents") {
      rows <- data[data$statut_suivi == "Absent", , drop = FALSE]
      cible <- "absent"
    } else if (target == "brouillons") {
      rows <- data[data$statut_suivi == "En cours", , drop = FALSE]
      cible <- "brouillon"
    } else {
      rows <- data[data$utilisateur_id == as.integer(target), , drop = FALSE]
      cible <- if (nrow(rows) && rows$statut_suivi[1] == "En cours") "brouillon" else "absent"
    }

    if (nrow(rows) == 0) {
      showNotification("Aucune ONG a relancer pour cette cible.", type = "warning")
      return(invisible(FALSE))
    }

    sent <- 0
    for (i in seq_len(nrow(rows))) {
      ok <- mail_relance_enquete(rows$email[i], rows$nom_structure[i], sess$label, sess$date_fin, cible)
      db_create_notification(
        utilisateur_id = rows$utilisateur_id[i],
        type = "relance_enquete",
        message = paste0("Rappel pour la session : ", sess$label),
        lien = "#saisie")
      if (isTRUE(ok)) sent <- sent + 1
    }

    stamp <- paste0(format(Sys.time(), "%d/%m/%Y %H:%M"), " (", nrow(rows), " ONG)")
    if (identical(target, "absents")) suivi_state$last_absents <- stamp
    if (identical(target, "brouillons")) suivi_state$last_brouillons <- stamp
    showNotification(paste0("Relance traitee pour ", nrow(rows), " ONG. Emails envoyes : ", sent, "."), type = "message")
    invisible(TRUE)
  }

  observeEvent(input$btn_relance_absents, {
    counts <- suivi_counts()
    showModal(modalDialog(
      title = "Confirmer la relance",
      paste0("Envoyer une relance aux ", counts$absent, " ONG sans soumission ?"),
      footer = tagList(modalButton("Annuler"), actionButton("confirm_relance_absents", "Confirmer"))))
  })

  observeEvent(input$confirm_relance_absents, {
    removeModal()
    send_tracking_reminders("absents")
  })

  observeEvent(input$btn_relance_brouillons, {
    counts <- suivi_counts()
    showModal(modalDialog(
      title = "Confirmer la relance",
      paste0("Envoyer une relance aux ", counts$draft, " ONG avec brouillon non soumis ?"),
      footer = tagList(modalButton("Annuler"), actionButton("confirm_relance_brouillons", "Confirmer"))))
  })

  observeEvent(input$confirm_relance_brouillons, {
    removeModal()
    send_tracking_reminders("brouillons")
  })

  observeEvent(input$suivi_remind_one, {
    req(input$suivi_remind_one)
    data <- suivi_data()
    ong <- data[data$utilisateur_id == as.integer(input$suivi_remind_one), , drop = FALSE]
    req(nrow(ong) == 1)
    showModal(modalDialog(
      title = "Relancer cette ONG",
      paste0("Envoyer une relance a ", ong$nom_structure, " ?"),
      footer = tagList(modalButton("Annuler"), actionButton("confirm_relance_one", "Confirmer"))))
  })

  observeEvent(input$confirm_relance_one, {
    removeModal()
    req(input$suivi_remind_one)
    send_tracking_reminders(input$suivi_remind_one)
  })

  observeEvent(input$suivi_view_submission, {
    req(input$suivi_view_submission)
    soum <- tryCatch(db_get_soumission_by_id(as.integer(input$suivi_view_submission)), error = function(e) data.frame())
    req(nrow(soum) == 1)
    details <- tryCatch(db_get_soumission_complete(soum$id), error = function(e) NULL)
    req(!is.null(details))
    correction_soumission_id(soum$id)

    showModal(modalDialog(
      title = paste0("Soumission - ", soum$nom_structure),
      submission_review_ui(soum, details),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("btn_open_correction_form", "Signaler une correction", icon = icon("triangle-exclamation")),
        modalButton("Fermer")
      )))
  })

  observeEvent(input$btn_open_correction_form, {
    req(correction_soumission_id())
    soum <- tryCatch(db_get_soumission_by_id(correction_soumission_id()), error = function(e) data.frame())
    req(nrow(soum) == 1)

    showModal(modalDialog(
      title = paste0("Correction a demander - ", soum$nom_structure),
      tags$p(class = "hint-text", paste0("Session : ", soum$session_label, " | Version : v", soum$version)),
      textInput("correction_objet", "Objet", placeholder = "Ex: Montants financiers a verifier"),
      textAreaInput(
        "correction_corps",
        "Corps du message",
        rows = 5,
        placeholder = "Decrivez clairement les champs a corriger et les pieces attendues."
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_send_correction", "Envoyer a l'ONG", icon = icon("paper-plane"))
      )
    ))
  })

  observeEvent(input$confirm_send_correction, {
    req(correction_soumission_id(), input$correction_objet, input$correction_corps)
    soum <- tryCatch(db_get_soumission_by_id(correction_soumission_id()), error = function(e) data.frame())
    req(nrow(soum) == 1)

    tryCatch({
      db_create_demande(
        soumission_id = soum$id,
        utilisateur_id = soum$utilisateur_id,
        nom_structure = soum$nom_structure,
        email = soum$email,
        date_soumission_originale = as.Date(soum$updated_at),
        objet = input$correction_objet,
        corps = input$correction_corps
      )
      db_update_soumission_statut(soum$id, "modifiable")
      db_create_notification(
        utilisateur_id = soum$utilisateur_id,
        type = "correction_soumission",
        message = paste0("Corrections demandees pour la session : ", soum$session_label),
        lien = "#saisie"
      )
      mail_correction_soumission(
        to = soum$email,
        nom_structure = soum$nom_structure,
        session_label = soum$session_label,
        objet = input$correction_objet,
        corps = input$correction_corps
      )
      removeModal()
      showNotification("Demande de correction envoyee a l'ONG.", type = "message")
    }, error = function(e) {
      showNotification(paste0("Erreur lors de l'envoi : ", e$message), type = "error")
    })
  })

  output$page_demandes <- renderUI({
    req(user_role() == "admin")
    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","✉️ Demandes de modification"),
        tags$div(class="page-subtitle","Gérez les demandes soumises par les ONG.")
      ),
      box(width=12,
        title=tags$span(tags$span(class="box-badge","D"), icon("envelope"),
          "Demandes en attente"),
        DTOutput("dt_demandes"),
        br(),
        fluidRow(
          column(6,
            actionButton("btn_accepter_demande","✅ Accepter",
              style="background:#4ade8022; color:#4ade80; border:1px solid #4ade8044;
                     border-radius:7px; padding:8px 16px; font-size:12px;")
          ),
          column(6,
            actionButton("btn_refuser_demande","❌ Refuser",
              style="background:#e05c5c22; color:#e05c5c; border:1px solid #e05c5c44;
                     border-radius:7px; padding:8px 16px; font-size:12px;")
          )
        )
      )
    )
  })

  output$dt_demandes <- renderDT({
    demandes <- tryCatch(db_get_demandes_en_attente(), error=function(e) data.frame())
    req(nrow(demandes) > 0)
    datatable(
      demandes[,c("id","ong_nom","session_label","objet","date_demande","statut")],
      selection="single", rownames=FALSE,
      options=list(scrollX=TRUE, pageLength=10, dom="ft"),
      colnames=c("ID","ONG","Session","Objet","Date demande","Statut")
    )
  })

  observeEvent(input$btn_accepter_demande, {
    idx <- input$dt_demandes_rows_selected
    req(idx)
    demandes <- db_get_demandes_en_attente()
    dem <- demandes[idx,]
    tryCatch({
      db_traiter_demande(dem$id, "accepte", user_id())
      db_update_soumission_statut(dem$soumission_id, "modifiable")
      db_create_notification(
        utilisateur_id = dem$utilisateur_id,
        type    = "demande_acceptee",
        message = paste0("Votre demande de modification a été acceptée : ", dem$objet),
        lien    = "#historique"
      )
      mail_demande_traitee(dem$email, dem$nom_structure, "accepte", dem$objet)
      showNotification("✅ Demande acceptée.", type="message")
    }, error=function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type="error")
    })
  })

  observeEvent(input$btn_refuser_demande, {
    idx <- input$dt_demandes_rows_selected
    req(idx)
    demandes <- db_get_demandes_en_attente()
    dem <- demandes[idx,]
    tryCatch({
      db_traiter_demande(dem$id, "refuse", user_id())
      db_create_notification(
        utilisateur_id = dem$utilisateur_id,
        type    = "demande_refusee",
        message = paste0("Votre demande de modification a été refusée : ", dem$objet),
        lien    = "#historique"
      )
      mail_demande_traitee(dem$email, dem$nom_structure, "refuse", dem$objet)
      showNotification("✅ Demande refusée.", type="message")
    }, error=function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type="error")
    })
  })

  # Data explorer admin
  data_filters <- reactiveValues(session_id = NULL, structures = NULL, statut = "Tous", date_range = NULL)
  data_table_labels <- c(
    projets = "Projets",
    finance = "Execution financiere",
    physique = "Execution physique",
    difficultes = "Difficultes",
    recommandations = "Recommandations"
  )

  data_active_key <- reactive(input$data_table_tabs %||% "projets")

  data_sessions <- reactive({
    req(user_role() == "admin")
    tryCatch(db_get_all_sessions(), error = function(e) data.frame())
  })

  output$page_data <- renderUI({
    req(user_role() == "admin")
    sessions <- data_sessions()
    if (nrow(sessions) == 0) {
      return(tags$div(
        tags$div(class = "page-header",
          tags$div(class = "page-title", "Data"),
          tags$div(class = "page-subtitle", "Aucune session disponible.")
        )
      ))
    }

    choices <- setNames(sessions$id, sessions$label)
    default_session <- sessions$id[1]
    tags$div(
      tags$div(class = "page-header",
        tags$div(class = "page-title", "Data"),
        tags$div(class = "page-subtitle", "Explorer, filtrer et exporter les donnees collectees.")
      ),
      box(width = 12, collapsible = FALSE,
        title = tags$span(tags$span(class = "box-badge", "F"), icon("filter"), "Filtres"),
        fluidRow(
          column(3, selectInput("data_session", "Session / Enquete", choices = choices, selected = default_session)),
          column(3, selectizeInput("data_structures", "Structure / ONG", choices = NULL, multiple = TRUE)),
          column(2, selectInput("data_statut", "Statut", choices = c("Tous", "Soumis", "En cours"), selected = "Tous")),
          column(3, dateRangeInput("data_period", "Periode", start = Sys.Date() - 365, end = Sys.Date(), language = "fr")),
          column(1,
            tags$label("&nbsp;"),
            tags$div(style = "display:flex; gap:6px;",
              actionButton("btn_apply_data_filters", NULL, icon = icon("check"), title = "Appliquer"),
              actionButton("btn_reset_data_filters", NULL, icon = icon("rotate-left"), title = "Reinitialiser")
            )
          )
        )
      ),
      tags$div(class = "submit-bar",
        tags$div(class = "submit-info",
          tags$strong("Telechargement"),
          tags$br(),
          "Les exports respectent les filtres appliques."
        ),
        tags$div(class = "tracking-actions",
          downloadButton("download_data_xlsx_all", "Excel - toutes les tables"),
          downloadButton("download_data_xlsx_active", "Excel - table active"),
          downloadButton("download_data_csv_active", "CSV - table active")
        )
      ),
      uiOutput("data_summary"),
      tabsetPanel(
        id = "data_table_tabs",
        tabPanel("Projets", value = "projets", DTOutput("dt_data_projets")),
        tabPanel("Execution financiere", value = "finance", DTOutput("dt_data_finance")),
        tabPanel("Execution physique", value = "physique", DTOutput("dt_data_physique")),
        tabPanel("Difficultes", value = "difficultes", DTOutput("dt_data_difficultes")),
        tabPanel("Recommandations", value = "recommandations", DTOutput("dt_data_recommandations"))
      )
    )
  })

  observeEvent(input$data_session, {
    req(user_role() == "admin", input$data_session)
    data <- tryCatch(db_get_data_explorer(as.integer(input$data_session)), error = function(e) list())
    structures <- sort(unique(unlist(lapply(data, function(x) {
      if (is.data.frame(x) && "nom_structure" %in% names(x)) x$nom_structure else character(0)
    }))))
    updateSelectizeInput(session, "data_structures", choices = structures, selected = structures, server = TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$btn_apply_data_filters, {
    data_filters$session_id <- as.integer(input$data_session)
    data_filters$structures <- input$data_structures
    data_filters$statut <- input$data_statut %||% "Tous"
    data_filters$date_range <- input$data_period
    showNotification("Filtres appliques.", type = "message", duration = 2)
  })

  observeEvent(input$btn_reset_data_filters, {
    sessions <- data_sessions()
    req(nrow(sessions) > 0)
    updateSelectInput(session, "data_session", selected = sessions$id[1])
    updateSelectInput(session, "data_statut", selected = "Tous")
    updateDateRangeInput(session, "data_period", start = Sys.Date() - 365, end = Sys.Date())
    data_filters$session_id <- sessions$id[1]
    data_filters$structures <- NULL
    data_filters$statut <- "Tous"
    data_filters$date_range <- c(Sys.Date() - 365, Sys.Date())
  })

  observeEvent(data_sessions(), {
    sessions <- data_sessions()
    if (nrow(sessions) > 0 && is.null(data_filters$session_id)) {
      data_filters$session_id <- sessions$id[1]
      data_filters$date_range <- c(Sys.Date() - 365, Sys.Date())
    }
  }, ignoreInit = FALSE)

  filter_data_table <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(df)
    if (!is.null(data_filters$structures) && length(data_filters$structures) > 0) {
      df <- df[df$nom_structure %in% data_filters$structures, , drop = FALSE]
    }
    if (!is.null(data_filters$statut) && data_filters$statut != "Tous") {
      if (data_filters$statut == "Soumis") {
        df <- df[df$statut_soumission == "soumis", , drop = FALSE]
      } else {
        df <- df[df$statut_soumission != "soumis", , drop = FALSE]
      }
    }
    dr <- data_filters$date_range
    if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr)) && "soumission_updated_at" %in% names(df)) {
      d <- as.Date(df$soumission_updated_at)
      df <- df[d >= as.Date(dr[1]) & d <= as.Date(dr[2]), , drop = FALSE]
    }
    df
  }

  data_tables_filtered <- reactive({
    req(user_role() == "admin")
    sid <- data_filters$session_id
    req(!is.null(sid))
    raw <- tryCatch(db_get_data_explorer(as.integer(sid)), error = function(e) {
      showNotification(paste0("Erreur chargement data : ", e$message), type = "error")
      list(projets = data.frame(), finance = data.frame(), physique = data.frame(),
           difficultes = data.frame(), recommandations = data.frame())
    })
    lapply(raw, filter_data_table)
  })

  render_data_dt <- function(key) {
    renderDT({
      data <- data_tables_filtered()[[key]]
      datatable(data, rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 15, dom = "ftip"))
    })
  }

  output$dt_data_projets <- render_data_dt("projets")
  output$dt_data_finance <- render_data_dt("finance")
  output$dt_data_physique <- render_data_dt("physique")
  output$dt_data_difficultes <- render_data_dt("difficultes")
  output$dt_data_recommandations <- render_data_dt("recommandations")

  output$data_summary <- renderUI({
    data <- data_tables_filtered()[[data_active_key()]]
    key <- data_active_key()
    if (!is.data.frame(data)) data <- data.frame()
    metric <- function(label, value) {
      tags$div(class = "kpi-card", style = "min-height:70px;",
        tags$div(class = "kpi-label", label),
        tags$div(class = "kpi-value", value)
      )
    }
    non_empty_count <- function(x) sum(!is.na(x) & nzchar(as.character(x)))
    cards <- switch(key,
      projets = list(
        metric("Nb projets", nrow(data)),
        metric("Annees cibles", if ("annee_cible" %in% names(data)) length(unique(na.omit(data$annee_cible))) else 0),
        metric("Ministeres distincts", if ("ministere_tutelle" %in% names(data)) length(unique(na.omit(data$ministere_tutelle))) else 0)
      ),
      finance = list(
        metric("Total cout", if ("cout_total" %in% names(data)) round(sum(data$cout_total, na.rm = TRUE), 0) else 0),
        metric("Total depense CP", if ("depense_cp" %in% names(data)) round(sum(data$depense_cp, na.rm = TRUE), 0) else 0),
        metric("Lignes financieres", nrow(data))
      ),
      physique = list(
        metric("Nb indicateurs", nrow(data)),
        metric("Taux annuel moyen", if ("taux_annuel" %in% names(data)) round(mean(data$taux_annuel, na.rm = TRUE), 1) else 0),
        metric("Taux global moyen", if ("taux_global" %in% names(data)) round(mean(data$taux_global, na.rm = TRUE), 1) else 0)
      ),
      difficultes = list(
        metric("Nb difficultes", nrow(data)),
        metric("Avec mesure", if ("mesure" %in% names(data)) non_empty_count(data$mesure) else 0),
        metric("Structures", if ("nom_structure" %in% names(data)) length(unique(na.omit(data$nom_structure))) else 0)
      ),
      recommandations = list(
        metric("Nb recommandations", nrow(data)),
        metric("Avec responsable", if ("responsable" %in% names(data)) non_empty_count(data$responsable) else 0),
        metric("Structures", if ("nom_structure" %in% names(data)) length(unique(na.omit(data$nom_structure))) else 0)
      )
    )
    tags$div(class = "kpi-grid", style = "grid-template-columns:repeat(auto-fit, minmax(150px, 1fr));", cards)
  })

  xml_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  write_simple_xlsx <- function(sheets, file) {
    tmp <- tempfile("xlsx_")
    dir.create(tmp)
    dir.create(file.path(tmp, "_rels"))
    dir.create(file.path(tmp, "xl"))
    dir.create(file.path(tmp, "xl", "_rels"))
    dir.create(file.path(tmp, "xl", "worksheets"))
    sheet_names <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", names(sheets)), 1, 31)
    names(sheets) <- sheet_names
    sheet_overrides <- paste0('<Override PartName="/xl/worksheets/sheet', seq_along(sheet_names), '.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>', collapse = "")
    writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>', sheet_overrides, '</Types>'), file.path(tmp, "[Content_Types].xml"), useBytes = TRUE)
    writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>', file.path(tmp, "_rels", ".rels"), useBytes = TRUE)
    workbook_sheets <- paste0('<sheet name="', xml_escape(sheet_names), '" sheetId="', seq_along(sheet_names), '" r:id="rId', seq_along(sheet_names), '"/>', collapse = "")
    writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>', workbook_sheets, '</sheets></workbook>'), file.path(tmp, "xl", "workbook.xml"), useBytes = TRUE)
    rels <- paste0('<Relationship Id="rId', seq_along(sheet_names), '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet', seq_along(sheet_names), '.xml"/>', collapse = "")
    writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">', rels, '</Relationships>'), file.path(tmp, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)
    for (i in seq_along(sheets)) {
      df <- sheets[[i]]
      if (!is.data.frame(df)) df <- data.frame()
      df[] <- lapply(df, as.character)
      rows <- list(names(df))
      if (nrow(df) > 0) rows <- c(rows, lapply(seq_len(nrow(df)), function(r) unname(as.character(df[r, ]))))
      row_xml <- paste0(vapply(seq_along(rows), function(r) {
        cells <- paste0(vapply(rows[[r]], function(v) paste0('<c t="inlineStr"><is><t>', xml_escape(v), '</t></is></c>'), character(1)), collapse = "")
        paste0("<row>", cells, "</row>")
      }, character(1)), collapse = "")
      writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>', row_xml, '</sheetData></worksheet>'), file.path(tmp, "xl", "worksheets", paste0("sheet", i, ".xml")), useBytes = TRUE)
    }
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)
    utils::zip(zipfile = file, files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-r9Xq")
  }

  output$download_data_csv_active <- downloadHandler(
    filename = function() paste0("data_", data_active_key(), "_", Sys.Date(), ".csv"),
    content = function(file) write.csv(data_tables_filtered()[[data_active_key()]], file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$download_data_xlsx_active <- downloadHandler(
    filename = function() paste0("data_", data_active_key(), "_", Sys.Date(), ".xlsx"),
    content = function(file) {
      key <- data_active_key()
      write_simple_xlsx(setNames(list(data_tables_filtered()[[key]]), data_table_labels[[key]]), file)
    }
  )
  output$download_data_xlsx_all <- downloadHandler(
    filename = function() paste0("data_toutes_tables_", Sys.Date(), ".xlsx"),
    content = function(file) {
      data <- data_tables_filtered()
      names(data) <- data_table_labels[names(data)]
      write_simple_xlsx(data, file)
    }
  )

  # ── Utilisateurs ─────────────────────────────────────────────
  output$page_utilisateurs <- renderUI({
    req(user_role() == "admin")
    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","👥 Gestion des utilisateurs"),
        tags$div(class="page-subtitle","Créer et gérer les comptes ONG.")
      ),
      tags$div(
        class = "submit-bar",
        tags$div(class = "submit-info",
          tags$strong("Nouveau compte ONG"),
          tags$br(),
          "Ajoutez une structure depuis une fenetre dediee."
        ),
        actionButton("btn_open_user_modal", "Nouveau compte", icon = icon("user-plus"))
      ),
      
      box(width=12,
        title=tags$span(tags$span(class="box-badge","L"), icon("list"), "Comptes ONG"),
        DTOutput("dt_utilisateurs")
      )
    )
  })

  output$dt_utilisateurs <- renderDT({
    users <- tryCatch(db_get_all_ong(), error=function(e) data.frame())
    req(nrow(users) > 0)
    datatable(
      users[,c("id","nom_structure","email","actif","created_at")],
      selection="single", rownames=FALSE,
      options=list(scrollX=TRUE, pageLength=15, dom="ft"),
      colnames=c("ID","Structure","Email","Actif","Créé le")
    )
  })

  observeEvent(input$btn_open_user_modal, {
    showModal(modalDialog(
      title = "Nouveau compte ONG",
      tags$label(HTML("Nom de la structure <span class='required-star'>*</span>")),
      textInput("new_modal_nom_structure", NULL, placeholder = "Ex: ONG Espoir Vert"),
      tags$label(HTML("Email <span class='required-star'>*</span>")),
      textInput("new_modal_email", NULL, placeholder = "contact@ong.bf"),
      tags$label(HTML("Mot de passe temporaire <span class='required-star'>*</span>")),
      passwordInput("new_modal_password", NULL, placeholder = "Minimum 8 caracteres"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("btn_create_user_modal", "Creer le compte", icon = icon("save"))
      )
    ))
  })

  observeEvent(input$btn_create_user_modal, {
    req(input$new_modal_nom_structure, input$new_modal_email, input$new_modal_password)
    tryCatch({
      db_create_user(input$new_modal_nom_structure, input$new_modal_email, input$new_modal_password)
      removeModal()
      showNotification(
        paste0("Compte cree pour ", input$new_modal_nom_structure), type="message")
    }, error=function(e) {
      showNotification(paste0("Erreur : ", e$message), type="error")
    })
  })

  observeEvent(input$btn_create_user, {
    req(input$new_nom_structure, input$new_email, input$new_password)
    tryCatch({
      db_create_user(input$new_nom_structure, input$new_email, input$new_password)
      showNotification(
        paste0("✅ Compte créé pour ", input$new_nom_structure), type="message")
    }, error=function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type="error")
    })
  })

  # ── Dashboard admin ───────────────────────────────────────────
  output$page_dashboard <- renderUI({
    req(user_role() == "admin")
    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","📊 Tableau de bord"),
        tags$div(class="page-subtitle","Vue synthétique de l'activité.")
      ),
      box(width=12,
        tags$p(style="color:var(--text-faint); text-align:center; padding:40px;",
          icon("chart-bar"), "  Graphiques et statistiques — Phase 5"))
    )
  })

  # ── Page soumissions admin ────────────────────────────────────
  output$page_soumissions <- renderUI({
    req(user_role() == "admin")
    sessions <- tryCatch(db_get_all_sessions(), error=function(e) data.frame())
    choices  <- if(nrow(sessions)>0)
      setNames(sessions$id, sessions$label) else c("Aucune"=0)
    tags$div(
      tags$div(class="page-header",
        tags$div(class="page-title","📂 Soumissions"),
        tags$div(class="page-subtitle","Toutes les soumissions par session.")
      ),
      box(width=12, collapsible=FALSE,
        fluidRow(
          column(4,
            selectInput("admin_sel_session","Filtrer par session :", choices=choices))
        ),
        DTOutput("dt_all_soumissions")
      )
    )
  })

  output$dt_all_soumissions <- renderDT({
    req(input$admin_sel_session)
    soum <- tryCatch(
      db_get_soumissions_session(as.integer(input$admin_sel_session)),
      error=function(e) data.frame())
    req(nrow(soum)>0)
    datatable(
      soum[,c("id","nom_structure","version","statut","created_at","updated_at")],
      selection="single", rownames=FALSE,
      options=list(scrollX=TRUE, pageLength=15, dom="ft"),
      colnames=c("ID","ONG","Version","Statut","Créée le","Mise à jour")
    )
  })

  # ════════════════════════════════════════════════════════════
  # SOURCE des modules complémentaires
  # ════════════════════════════════════════════════════════════
  # Les pages Saisie (formulaires) et Historique seront
  # développées dans les phases suivantes :
  source("modules/saisie_server.R",    local=TRUE)
  source("modules/historique_server.R", local=TRUE)
  
  
}
