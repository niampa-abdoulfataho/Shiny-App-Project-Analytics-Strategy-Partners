#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(RPostgres)
library(DBI)
library(shinyjs)

# ── Connexion PostgreSQL ──────────────────────────────────────────────────────
# Modifie ces paramètres selon ton environnement
con_params <- list(
        host     = "localhost",
        port     = 5433,
        dbname   = "suiviprojet",
        user     = "postgres",
        password = "0023421Postgres"
)

get_con <- function() {
        do.call(dbConnect, c(list(drv = RPostgres::Postgres()), con_params))
}

# ── Initialisation de la table ────────────────────────────────────────────────
init_db <- function() {
        con <- get_con()
        on.exit(dbDisconnect(con))
        dbExecute(con, "
    CREATE TABLE IF NOT EXISTS indicateurs (
      id                  SERIAL PRIMARY KEY,
      session_label       TEXT,
      indicateur          TEXT,
      unite               TEXT,
      prevision           NUMERIC,
      realisation         NUMERIC,
      localite            TEXT,
      preuves             TEXT,
      taux_annee          NUMERIC,
      taux_global         NUMERIC,
      created_at          TIMESTAMP DEFAULT NOW(),
      updated_at          TIMESTAMP DEFAULT NOW()
    )
  ")
}

init_db()

# ── Helpers DB ────────────────────────────────────────────────────────────────
db_get_sessions <- function() {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbGetQuery(con,
                   "SELECT DISTINCT session_label, MIN(created_at) AS date
     FROM indicateurs GROUP BY session_label ORDER BY date DESC")
}

db_get_by_session <- function(label) {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbGetQuery(con,
                   "SELECT * FROM indicateurs WHERE session_label = $1 ORDER BY id",
                   params = list(label))
}

db_insert_batch <- function(df) {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbAppendTable(con, "indicateurs", df)
}

db_delete_session <- function(label) {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbExecute(con,
                  "DELETE FROM indicateurs WHERE session_label = $1",
                  params = list(label))
}

db_update_row <- function(id, fields) {
        con <- get_con(); on.exit(dbDisconnect(con))
        dbExecute(con,
                  "UPDATE indicateurs
     SET indicateur=$1, unite=$2, prevision=$3, realisation=$4,
         localite=$5, preuves=$6, taux_annee=$7, taux_global=$8,
         updated_at=NOW()
     WHERE id=$9",
                  params = c(as.list(fields), list(id)))
}


"==========================================================================="
"======         Données pour le message ===================================="
"==========================================================================="
messageData <- reactive(
   data.frame(
   from    = c("Alice", "Bob"),
   message = c("Rapport prêt", "Réunion à 14h ?"),
   time    = c("Il y a 5 min", "Il y a 1h"),
   stringsAsFactors = FALSE
))

"=============================================================================="

# Define server logic required to draw a histogram
function(input, output, session) {
        set.seed(122)
        histdata <- rnorm(500)
        output$plot1 <- renderPlot({
            data <- histdata[seq_len(input$slider)]
            hist(data)
        })
        
        # Message pour afficher dans le header
        messageData <- reactive(
           data.frame(
              from    = c("Saïdou YAMEOGO", "A. Fatah NIAMPA"),
              message = c("Rapport prêt", "Réunion à 14h ?"),
              time    = c("Il y a 5 min", "Il y a 1h"),
              stringsAsFactors = FALSE
           ))
        # Le contenu de menu message
        output$messageMenu <- renderMenu({
           # Code to generate each of the messageItems here, in a list. This assumes
           # that messageData is a data frame with two columns, 'from' and 'message'.
           msgs <- lapply(seq_len(nrow(messageData())), function(i) {
              messageItem(
                 from    = messageData()$from[i],
                 message = messageData()$message[i],
                 time = messageData()$message[i]
              )
           })
           
           # This is equivalent to calling:
           dropdownMenu(type = "messages", .list = msgs)
        })
        
        # Message pour les notifications dans le header
        notificationData <- reactive(
           data.frame(
              text    = c(
                 "5 new users today", 
                 "12 items delivered", 
                 "Server load at 86%"
                 ),
              icon = c("users", "truck", "exclamation-triangle"),
              status = c("primary", "success","warning"),
              stringsAsFactors = FALSE
           ))
        
        # Le contenu de menu notification 
        output$notificationMenu <- renderMenu({
           # Code to generate each of the messageItems here, in a list. This assumes
           # that messageData is a data frame with two columns, 'from' and 'message'.
           msgs <- lapply(seq_len(nrow(notificationData())), function(i) {
              notificationItem(
                 text    = notificationData()$text[i],
                 icon = icon(notificationData()$icon[i]),
                 status = notificationData()$status[i]
              )
           })
           
           # This is equivalent to calling:
           dropdownMenu(type = "notifications", .list = msgs)
        })
        
        #connexion deconnexion
        output$userpanel <- renderUI({
           # session$user is non-NULL only in authenticated sessions
           if (!is.null(session$user)) {
              sidebarUserPanel(
                 span("Logged in as ", session$user),
                 subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
           }
        })
        
        #=======================================================================
        # ── Server formulaire──────────────────────────────────────────────────
        #=======================================================================
        `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
        
        active_rows  <- reactiveVal(c(1))
        selected_id  <- reactiveVal(NULL)
        history_data <- reactiveVal(NULL)
        
        # ── Lignes dynamiques ──────────────────────────────────────────────────────
        observeEvent(input$add_row, {
                new_id <- max(active_rows()) + 1
                active_rows(c(active_rows(), new_id))
        })
        
        observeEvent(input$reset_form, { active_rows(c(1)) })
        
        observe({
                lapply(active_rows(), function(i) {
                        local({
                                rid <- i
                                observeEvent(input[[paste0("del_", rid)]], {
                                        rows <- active_rows()
                                        if (length(rows) > 1) active_rows(rows[rows != rid])
                                        else showNotification("Au moins une ligne requise.", type = "warning")
                                }, 
                                ignoreInit = TRUE)
                        })
                })
        })
        
        # ── Rendu du formulaire ───────────────────────────────────────────────────
        output$form_rows <- renderUI({
                rows <- active_rows()
                lapply(seq_along(rows), function(idx) {
                        i <- rows[idx]
                        div(class = "row-block",
                            fluidRow(
                                    column(12,
                                           strong(paste("Indicateur", idx)),
                                           actionButton(paste0("del_", i), "✖", class = "btn-del btn-sm",
                                                        style = "float:right; padding:2px 8px;")
                                    )
                            ),
                            fluidRow(
                                    column(3, textInput(paste0("ind_", i), "Indicateur de produit",
                                                        placeholder = "Ex: Formations réalisées")),
                                    column(2, textInput(paste0("uni_", i), "Unité physique",
                                                        placeholder = "Nombre, Km, %")),
                                    column(2, numericInput(paste0("pre_", i), "Prévision 2025",    value = NA, min = 0)),
                                    column(2, numericInput(paste0("rea_", i), "Réalisation 31/12", value = NA, min = 0)),
                                    column(3, textInput(paste0("loc_", i), "Localité",
                                                        placeholder = "Ex: Ouagadougou"))
                            ),
                            fluidRow(
                                    column(5, textAreaInput(paste0("prv_", i), "Preuves / Observations",
                                                            rows = 2, placeholder = "PV, photos, rapports...")),
                                    column(2,
                                           div(class = "taux-field",
                                               numericInput(paste0("tan_", i), "Taux exec annuel (%)",
                                                            value = NA, min = 0, max = 100))
                                    ),
                                    column(2,
                                           numericInput(paste0("tgl_", i), "Taux exec global (%)",
                                                        value = NA, min = 0, max = 100)
                                    )
                            )
                        )
                })
        })
        
        # ── Aperçu ────────────────────────────────────────────────────────────────
        get_form_data <- reactive({
                rows <- active_rows()
                do.call(rbind, lapply(seq_along(rows), function(idx) {
                        i <- rows[idx]
                        data.frame(
                                session_label = input$session_label %||% "",
                                indicateur    = input[[paste0("ind_", i)]] %||% "",
                                unite         = input[[paste0("uni_", i)]] %||% "",
                                prevision     = input[[paste0("pre_", i)]],
                                realisation   = input[[paste0("rea_", i)]],
                                localite      = input[[paste0("loc_", i)]] %||% "",
                                preuves       = input[[paste0("prv_", i)]] %||% "",
                                taux_annee    = input[[paste0("tan_", i)]],
                                taux_global   = input[[paste0("tgl_", i)]],
                                stringsAsFactors = FALSE
                        )
                }))
        })
        
        output$preview_table <- renderDT({
                datatable(get_form_data(),
                          options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
        })
        
        # ── Sauvegarde ────────────────────────────────────────────────────────────
        observeEvent(input$save_all, {
                label <- trimws(input$session_label %||% "")
                if (nchar(label) == 0) {
                        showNotification("⚠️ Renseigne un libellé de formulaire.", type = "error")
                        return()
                }
                sessions <- db_get_sessions()
                if (label %in% sessions$session_label) {
                        showModal(modalDialog(
                                title = "⚠️ Libellé existant",
                                paste0("'", label, "' existe déjà. Veux-tu l'écraser ?"),
                                footer = tagList(
                                        modalButton("Annuler"),
                                        actionButton("confirm_overwrite", "Oui, écraser", class = "btn btn-danger")
                                )
                        ))
                } else {
                        do_save(label)
                }
        })
        
        observeEvent(input$confirm_overwrite, {
                removeModal()
                label <- trimws(input$session_label)
                db_delete_session(label)
                do_save(label)
        })
        
        do_save <- function(label) {
                df <- get_form_data()
                df$session_label <- label
                tryCatch({
                        db_insert_batch(df)
                        showNotification(
                                paste0("✅ '", label, "' sauvegardé (", nrow(df), " ligne(s))."),
                                type = "message")
                        refresh_sessions()
                }, error = function(e) {
                        showNotification(paste0("❌ Erreur BD : ", e$message), type = "error")
                })
        }
        
        # ── Historique ────────────────────────────────────────────────────────────
        refresh_sessions <- function() {
                sessions <- db_get_sessions()
                if (nrow(sessions) == 0) {
                        updateSelectInput(session, "sel_session", choices = c("(aucun formulaire)" = ""))
                        return()
                }
                choices <- setNames(
                        sessions$session_label,
                        paste0(sessions$session_label, " — ", format(sessions$date, "%d/%m/%Y %H:%M"))
                )
                updateSelectInput(session, "sel_session", choices = choices)
        }
        
        refresh_sessions()
        
        observeEvent(input$load_session, {
                req(input$sel_session)
                df <- db_get_by_session(input$sel_session)
                history_data(df)
                shinyjs::hide("edit_panel")
                selected_id(NULL)
        })
        
        output$history_table <- renderDT({
                req(history_data())
                df <- history_data()[, c("id","indicateur","unite","prevision","realisation",
                                         "localite","taux_annee","taux_global")]
                datatable(df,
                          selection = "single",
                          options   = list(scrollX = TRUE, pageLength = 10),
                          rownames  = FALSE,
                          colnames  = c("ID","Indicateur","Unité","Prévision","Réalisation",
                                        "Localité","Taux annuel (%)","Taux global (%)")
                ) |>
                        formatStyle("taux_annee",
                                    backgroundColor = styleInterval(c(50, 80, 100),
                                                                    c("#ffcccc","#fff3cd","#d4edda","#c3e6cb")))
        })
        
        # ── Sélection d'une ligne pour édition ───────────────────────────────────
        observeEvent(input$history_table_rows_selected, {
                idx <- input$history_table_rows_selected
                req(idx, history_data())
                row <- history_data()[idx, ]
                selected_id(row$id)
                
                updateTextInput(session,     "e_indicateur",  value = row$indicateur  %||% "")
                updateTextInput(session,     "e_unite",        value = row$unite       %||% "")
                updateNumericInput(session,  "e_prevision",    value = row$prevision)
                updateNumericInput(session,  "e_realisation",  value = row$realisation)
                updateTextInput(session,     "e_localite",     value = row$localite    %||% "")
                updateTextAreaInput(session, "e_preuves",      value = row$preuves     %||% "")
                updateNumericInput(session,  "e_taux_annee",   value = row$taux_annee)
                updateNumericInput(session,  "e_taux_global",  value = row$taux_global)
                
                shinyjs::show("edit_panel")
        })
        
        # ── Mise à jour ──────────────────────────────────────────────────────────
        observeEvent(input$update_row, {
                req(selected_id())
                fields <- list(
                        input$e_indicateur, input$e_unite,
                        input$e_prevision,  input$e_realisation,
                        input$e_localite,   input$e_preuves,
                        input$e_taux_annee, input$e_taux_global
                )
                tryCatch({
                        db_update_row(selected_id(), fields)
                        showNotification("✅ Ligne mise à jour.", type = "message")
                        history_data(db_get_by_session(input$sel_session))
                        shinyjs::hide("edit_panel")
                        selected_id(NULL)
                }, error = function(e) {
                        showNotification(paste0("❌ Erreur : ", e$message), type = "error")
                })
        })
        
        # ── Suppression d'un formulaire ──────────────────────────────────────────
        observeEvent(input$del_session, {
                req(input$sel_session)
                showModal(modalDialog(
                        title = "🗑️ Confirmation",
                        paste0("Supprimer définitivement '", input$sel_session, "' ?"),
                        footer = tagList(
                                modalButton("Annuler"),
                                actionButton("confirm_delete", "Supprimer", class = "btn btn-danger")
                        )
                ))
        })
        
        observeEvent(input$confirm_delete, {
                removeModal()
                tryCatch({
                        db_delete_session(input$sel_session)
                        showNotification("✅ Formulaire supprimé.", type = "message")
                        history_data(NULL)
                        shinyjs::hide("edit_panel")
                        refresh_sessions()
                }, error = function(e) {
                        showNotification(paste0("❌ Erreur : ", e$message), type = "error")
                })
        })
        
        #=======================================================================
        #=== server données financières ========================================
        #=======================================================================
        
        output$form_table_finance <- renderUI({
                rows <- active_rows()
                lapply(seq_along(rows), function(idx) {
                        i <- rows[idx]
                        div(class = "row-block",
                            fluidRow(
                                    column(12,
                                           strong(paste("Indicateur", idx)),
                                           actionButton(paste0("del_", i), "✖", class = "btn-del btn-sm",
                                                        style = "float:right; padding:2px 8px;")
                                    )
                            ),
                           fluidRow(
                                column(8,
                                       tags$label(HTML("Année d'activité <span class='required-star'>★</span>")),
                                       numericInput("anne_activite", NULL,
                                        value = as.integer(format(Sys.Date(), "%Y")),
                                        min = 2000, max = 2100)
                                ),
                                column(2,
                                       tags$label(HTML("Source de financement <span class='required-star'>★</span>")),
                                       textInput("sourcefinance", NULL,
                                                    placeholder = "Ex: Etat, Bailleur1")
                                ),
                                #revoir
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
                                       tags$label(HTML("Mode de financement <span class='required-star'>★</span>")),
                                       textInput("mode_finance", NULL,
                                                 placeholder = "Ex: Don, Prêt")
                                ),
                                column(3,
                                       tags$label("Coût total(en milliers fcfa)"),
                                       numericInput("cout_total", NULL, 
                                                    value = NA,
                                                    min = 2000, max = 2100)
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
                                 tags$div(class = "inner-title", icon("align-center"), "  Description"),
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
                                         column(3,br(),
                                                tags$label("Nom & Prénoms du responsable"),
                                                textInput("responsable_nom", NULL,
                                                          placeholder = "Ex: SAWADOGO Hamidou")
                                         ),
                                         column(3,br(),br(),
                                                tags$label("Téléphone"),
                                                textInput("responsable_tel", NULL, placeholder = "+226 70 00 00 00")
                                         ),
                                         column(3,br(),br(),
                                                tags$label("Adresse e-mail"),
                                                textInput("responsable_email", NULL, placeholder = "nom@projet.bf"),
                                                tags$div(class = "hint-text", "Format : nom@domaine.xx")
                                         ),
                                         column(3,br(),br(),
                                                tags$label("Réf. arrêté de création"),
                                                textInput("ref_arrete_creation", NULL, placeholder = "N°2023-045/MAAH")
                                               )
                                 )
                        )
                        )
                }
                )
        }
        )

}
        

