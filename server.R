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
library(lubridate)

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
          
          output$finance_form <- renderUI({
            rows <- active_rows()
            lapply(seq_along(rows), function(idx) {
              i <- rows[idx]
              div(class = "row-block",
                  fluidRow(
                    column(12,
                           strong(paste("", idx)),
                           actionButton(paste0("del_", i), "✖", class = "btn-del btn-sm",
                                        style = "float:right; padding:2px 8px;")
                    )
                  ),
                  fluidRow(
                    column(4,
                           tags$label(HTML("Source de financement <span class='required-star'>★</span>")),
                           textInput(paste0("sourcefinance_",i), NULL,
                                     placeholder = "Ex: Etat, Bailleur1")
                    ),
                    column(5,
                           tags$label(HTML("Mode de financement <span class='required-star'>★</span>")),
                           textInput(paste0("mode_finance_",i), NULL,
                                     placeholder = "Ex: Don, Prêt")
                    ),
                    column(3,
                           tags$label("Coût total"),
                           numericInput(paste0("cout_total_", i), NULL, 
                                        value = NA,
                                        min = 0)
                    )),
                  
                  tags$div(class = "inner-section",
                           tags$div(class = "inner-title", icon("align-center"), paste0("  Cumul des
décaissements (depuis le démarrage du projet au 31/12/", lubridate::year(Sys.Date())-1,")")
                           ),
                           fluidRow(
                             column(6,
                                    tags$label("AE"),
                                    numericInput(paste0("c_demarage_ae_", i), NULL, value = NA, min = 0)
                             ),
                             column(6,
                                    tags$label("CP"),
                                    numericInput(paste0("c_demarage_cp_", i), NULL, value = NA, min = 0)
                             ),
                           )
                  ),
                  
                  tags$div(class = "inner-section",
                           tags$div(class = "inner-title", icon("align-center"), paste0("  Programmation
Loi de finances ", lubridate::year(Sys.Date())-1)
                           ),
                           fluidRow(
                             column(6,
                                    tags$label("AE"),
                                    numericInput(paste0("loi_finance_ae_", i), NULL, value = NA, min = 0)
                             ),
                             column(6,
                                    tags$label("CP"),
                                    numericInput(paste0("loi_finance_cp_", i), NULL, value = NA, min = 0)
                             ),
                           )
                  ),
                  tags$div(class = "inner-section",
                           tags$div(class = "inner-title", icon("align-center"), paste0("  Programmation révisée au 31/12/", lubridate::year(Sys.Date())-1)
                           ),
                           fluidRow(
                             column(6,
                                    tags$label("AE(α)"),
                                    numericInput(paste0("programme_revise_ae_", i), NULL, value = NA, min = 0)
                             ),
                             column(6,
                                    tags$label("CP(β)"),
                                    numericInput(paste0("programme_revise_cp_", i), NULL, value = NA, min = 0)
                             ),
                           )
                  ),
                  tags$div(class = "inner-section",
                           tags$div(class = "inner-title", icon("align-center"), paste0("  Dépenses
du 01/01/",lubridate::year(Sys.Date())-1," au 31/12/2025", lubridate::year(Sys.Date())-1)
                           ),
                           fluidRow(
                             column(6,
                                    tags$label("AE (α’)"),
                                    numericInput(paste0("depense_ae_", i), NULL, value = NA, min = 0)
                             ),
                             column(6,
                                    tags$label("CP (β’)"),
                                    numericInput(paste0("depense_cp_", i), NULL, value = NA, min = 0)
                             ),
                           )
                  ),
                  tags$div(class = "inner-section",
                           tags$div(class = "inner-title", icon("align-center"), paste0(
                             "  Dépenses du démarrage du projet au 31/12/", 
                             lubridate::year(Sys.Date())-1)
                           ),
                           fluidRow(
                             column(6,
                                    tags$label("AE (α’’)"),
                                    numericInput(paste0("depense_demarage_ae_",i), NULL, value = NA, min = 0)
                             ),
                             column(6,
                                    tags$label("CP (β’’)"),
                                    numericInput(paste0("depense_demarage_cp_",i), NULL, value = NA, min = 0)
                             ),
                           )
                  )
              )
            }
            )
          })
          
          # ── Rendu du formulaire ───────────────────────────────────────────────────
          output$indicateur_form <- renderUI({
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
                    column(6, textInput(paste0("ind_", i), "Indicateur de produit",
                                        placeholder = "Ex: Formations réalisées")),
                    column(6, textInput(paste0("uni_", i), "Unité physique",
                                        placeholder = "Nombre, Km, %"))),
                  fluidRow(
                    column(6, numericInput(paste0("pre_", i), 
                                           paste0("Prévision physique en ", 
                                                  lubridate::year(Sys.Date())),    
                                           value = NA, min = 0)),
                    column(6, numericInput(paste0("rea_", i), 
                                           paste0("Réalisation 31/12/", 
                                                  lubridate::year(Sys.Date())), 
                                           value = NA, min = 0))),
                  fluidRow(
                    column(6, textInput(paste0("loc_", i), "Localité de réalisation",
                                        placeholder = "Ex: Ouagadougou")),
                    column(6, textAreaInput(paste0("prv_", i), "Preuves de la matérialité des réalisations et Observations",
                                            rows = 2, placeholder = "PV, photos, rapports..."))),
                  fluidRow(
                    column(6,
                           div(class = "taux-field",
                               numericInput(paste0("tan_", i), 
                                            paste0("Taux d’execution physique de 01/01/",
                                                   lubridate::year(Sys.Date())-1, " au 31/12/",
                                                   lubridate::year(Sys.Date())-1),
                                            value = NA, min = 0, max = 100))
                    ),
                    column(6,
                           numericInput(paste0("tgl_", i), 
                                        paste0("Taux d’execution physique global au 31/12/",
                                               lubridate::year(Sys.Date())-1),
                                        value = NA, min = 0, max = 100)
                    )
                  )
              )
            })
          })
          
          # ── Rendu du formulaire difficultés───────────────────────────────────────────────────
          output$difficulte_form <- renderUI({
            rows <- active_rows()
            lapply(seq_along(rows), function(idx) {
              i <- rows[idx]
              div(class = "row-block",
                  fluidRow(
                    column(12,
                           strong(paste("Difficulté ", idx)),
                           actionButton(paste0("del_", i), "✖", class = "btn-del btn-sm",
                                        style = "float:right; padding:2px 8px;")
                    )
                  ),
                  fluidRow(
                    column(6, textAreaInput(paste0("difficulte_", i), "Difficultés",rows = 2,
                                            placeholder = " ")),
                    column(6, textAreaInput(paste0("messure_", i), "Mesure prise",rows = 2,
                                            placeholder = "")))
              )
            })
          })
          
          # ── Rendu du formulaire recommandations───────────────────────────────────────────────────
          output$recommandation_form <- renderUI({
            rows <- active_rows()
            lapply(seq_along(rows), function(idx) {
              i <- rows[idx]
              div(class = "row-block",
                  fluidRow(
                    column(12,
                           strong(paste("Recommandation ", idx)),
                           actionButton(paste0("del_", i), "✖", class = "btn-del btn-sm",
                                        style = "float:right; padding:2px 8px;")
                    )
                  ),
                  fluidRow(
                    column(6, textAreaInput(paste0("recommandation_", i), "Recommandation",rows = 2,
                                            placeholder = " ")),
                    column(6, textAreaInput(paste0("messure_", i), "Echéance de la mise en œuvre",rows = 2,
                                            placeholder = ""))),
                  fluidRow(
                    column(6, textAreaInput(paste0("responsable_", i), "Responsable de la mise en œuvre",row = 2,
                                            placeholder = " ")),
                    column(6, textAreaInput(paste0("perspective_", i), "Perspectives",row = 2,
                                            placeholder = " ")))
              )
            })
          })
          
        
}
        

