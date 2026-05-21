# ============================================================
# modules/saisie_server.R
# Logique serveur du formulaire de saisie
# Sourcé depuis server.R dans l'environnement local
# ============================================================

source("modules/db_saisie.R", local = TRUE)

# ════════════════════════════════════════════════════════════
# ÉTAT RÉACTIF DE LA SAISIE
# ════════════════════════════════════════════════════════════

# ID de la soumission en cours
soumission_id_courant <- reactiveVal(NULL)

# Compteurs de lignes dynamiques par section
rows_finance        <- reactiveVal(c(1))
rows_indicateur     <- reactiveVal(c(1))
rows_difficulte     <- reactiveVal(c(1))
rows_recommandation <- reactiveVal(c(1))

# ════════════════════════════════════════════════════════════
# INITIALISATION : créer/récupérer la soumission au choix de session
# ════════════════════════════════════════════════════════════
observeEvent(input$sel_session_saisie, {
   req(input$sel_session_saisie, user_id())
   sess_id <- as.integer(input$sel_session_saisie)
   
   soum_id <- tryCatch(
      db_get_or_create_soumission(sess_id, user_id()),
      error = function(e) {
         showNotification(paste0("❌ Erreur : ", e$message), type = "error")
         NULL
      }
   )
   soumission_id_courant(soum_id)
   
   # Pré-remplir si des données existent déjà
   if (!is.null(soum_id)) {
      data_existante <- tryCatch(
         db_get_soumission_complete(soum_id),
         error = function(e) NULL
      )
      if (!is.null(data_existante)) preremplir_formulaire(data_existante)
   }
}, ignoreInit = TRUE)

# ── Pré-remplissage des champs depuis la BD ───────────────────
preremplir_formulaire <- function(data) {
   
   # Projet
   p <- data$projet
   if (nrow(p) > 0) {
      updateTextInput(session,    "intitule_projet",    value = p$intitule_projet    %||% "")
      updateNumericInput(session, "annee_cible",        value = p$annee_cible)
      updateDateInput(session,    "date_remplissage",   value = p$date_remplissage)
      updateTextInput(session,    "ministere_tutelle",  value = p$ministere_tutelle  %||% "")
      updateTextInput(session,    "siege_projet",       value = p$siege_projet       %||% "")
      updateNumericInput(session, "annee_demarrage",    value = p$annee_demarrage)
      updateNumericInput(session, "annee_fin",          value = p$annee_fin)
      updateTextAreaInput(session,"objectif_global",    value = p$objectif_global    %||% "")
      updateTextAreaInput(session,"resultats_attendus", value = p$resultats_attendus %||% "")
      updateTextInput(session,    "secteurs_activites", value = p$secteurs_activites %||% "")
      updateTextInput(session,    "zone_intervention",  value = p$zone_intervention  %||% "")
      updateTextInput(session,    "responsable_nom",    value = p$responsable_nom    %||% "")
      updateTextInput(session,    "responsable_tel",    value = p$responsable_tel    %||% "")
      updateTextInput(session,    "responsable_email",  value = p$responsable_email  %||% "")
      updateTextInput(session,    "ref_arrete_creation",value = p$ref_arrete_creation%||% "")
   }
   
   # Finance : réinitialiser le compteur selon le nombre de lignes en BD
   if (nrow(data$finance) > 0)
      rows_finance(seq_len(nrow(data$finance)))
   
   # Physique
   if (nrow(data$physique) > 0)
      rows_indicateur(seq_len(nrow(data$physique)))
   
   # Difficultés
   if (nrow(data$difficultes) > 0)
      rows_difficulte(seq_len(nrow(data$difficultes)))
   
   # Recommandations
   if (nrow(data$recommandations) > 0)
      rows_recommandation(seq_len(nrow(data$recommandations)))
}

# ════════════════════════════════════════════════════════════
# SECTIONS DU FORMULAIRE (UI injectée dans page_saisie)
# ════════════════════════════════════════════════════════════
output$form_sections <- renderUI({
   req(soumission_id_courant())
   tagList(
      
      # ── Box 1 : Identification ─────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=FALSE,
          title=tags$span(
             tags$span(class="box-badge","1"), icon("folder-open"),
             "Identification du projet",
             uiOutput("statut_projet", inline=TRUE)
          ),
          fluidRow(
             column(8,
                    tags$label(HTML("Intitulé du projet <span class='required-star'>★</span>")),
                    textInput("intitule_projet", NULL,
                              placeholder="Ex: Projet d'appui au développement rural")
             ),
             column(2,
                    tags$label(HTML("Année cible <span class='required-star'>★</span>")),
                    numericInput("annee_cible", NULL,
                                 value=as.integer(format(Sys.Date(),"%Y")), min=2000, max=2100)
             ),
             column(2,
                    tags$label("Date de remplissage"),
                    dateInput("date_remplissage", NULL,
                              value=Sys.Date(), format="dd/mm/yyyy", language="fr")
             )
          ),
          fluidRow(
             column(5,
                    tags$label(HTML("Ministère de tutelle <span class='required-star'>★</span>")),
                    textInput("ministere_tutelle", NULL,
                              placeholder="Ex: Ministère de l'Agriculture")
             ),
             column(3,
                    tags$label("Siège du projet"),
                    textInput("siege_projet", NULL, placeholder="Ex: Ouagadougou")
             ),
             column(2,
                    tags$label("Année de démarrage"),
                    numericInput("annee_demarrage", NULL, value=NA, min=2000, max=2100)
             ),
             column(2,
                    tags$label("Année de fin"),
                    numericInput("annee_fin", NULL, value=NA, min=2000, max=2100)
             )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-left"), "  Description"),
                   fluidRow(
                      column(6,
                             tags$label("Objectif global"),
                             textAreaInput("objectif_global", NULL, rows=3,
                                           placeholder="Décrire l'objectif principal du projet...")
                      ),
                      column(6,
                             tags$label("Résultats attendus"),
                             textAreaInput("resultats_attendus", NULL, rows=3,
                                           placeholder="Lister les résultats escomptés...")
                      )
                   ),
                   fluidRow(
                      column(6,
                             tags$label("Secteurs d'activités"),
                             textInput("secteurs_activites", NULL,
                                       placeholder="Ex: Agriculture, Élevage, Hydraulique")
                      ),
                      column(6,
                             tags$label("Zone d'intervention"),
                             textInput("zone_intervention", NULL,
                                       placeholder="Ex: Régions du Centre, Sahel, Est")
                      )
                   )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("user-tie"), "  Responsable & Références"),
                   fluidRow(
                      column(3,
                             tags$label("Nom & Prénoms"),
                             textInput("responsable_nom", NULL, placeholder="Ex: SAWADOGO Hamidou")
                      ),
                      column(3,
                             tags$label("Téléphone"),
                             textInput("responsable_tel", NULL, placeholder="+226 70 00 00 00")
                      ),
                      column(3,
                             tags$label("Adresse e-mail"),
                             textInput("responsable_email", NULL, placeholder="nom@projet.bf"),
                             tags$div(class="hint-text","Format : nom@domaine.xx")
                      ),
                      column(3,
                             tags$label("Réf. arrêté de création"),
                             textInput("ref_arrete_creation", NULL, placeholder="N°2023-045/MAAH")
                      )
                   )
          )
      ),
      
      # ── Box 2 : Exécution financière ───────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","2"), icon("file-invoice-dollar"),
             "Exécution financière",
             uiOutput("statut_finance", inline=TRUE)
          ),
          uiOutput("finance_form"),
          tags$button(icon("plus"), "  Ajouter une source de financement",
                      class="btn-add-line",
                      onclick="Shiny.setInputValue('add_finance', Math.random())")
      ),
      
      # ── Box 3 : Exécution physique ─────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","3"), icon("tasks"),
             "Exécution physique",
             uiOutput("statut_physique", inline=TRUE)
          ),
          uiOutput("indicateur_form"),
          tags$button(icon("plus"), "  Ajouter un indicateur",
                      class="btn-add-line",
                      onclick="Shiny.setInputValue('add_indicateur', Math.random())")
      ),
      
      # ── Box 4 : Difficultés ────────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","4"), icon("exclamation-triangle"),
             "Difficultés rencontrées",
             uiOutput("statut_difficulte", inline=TRUE)
          ),
          uiOutput("difficulte_form"),
          tags$button(icon("plus"), "  Ajouter une difficulté",
                      class="btn-add-line",
                      onclick="Shiny.setInputValue('add_difficulte', Math.random())")
      ),
      
      # ── Box 5 : Recommandations ────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","5"), icon("lightbulb"),
             "Recommandations",
             uiOutput("statut_recommandation", inline=TRUE)
          ),
          uiOutput("recommandation_form"),
          tags$button(icon("plus"), "  Ajouter une recommandation",
                      class="btn-add-line",
                      onclick="Shiny.setInputValue('add_recommandation', Math.random())")
      ),
      
      # ── Barre de soumission ────────────────────────────────
      tags$div(class="submit-bar",
               tags$div(class="submit-info",
                        tags$strong("Prêt à soumettre ?"), tags$br(),
                        "Vérifiez toutes les sections. ",
                        tags$strong("Transaction unique — tout ou rien.")
               ),
               tags$div(
                  tags$button(icon("redo"), " Réinitialiser",
                              class="btn-reset",
                              onclick="Shiny.setInputValue('reset_saisie', Math.random())"
                  ),
                  tags$button(icon("paper-plane"), "  Soumettre en base",
                              class="btn-submit",
                              onclick="Shiny.setInputValue('submit_all', Math.random())"
                  )
               )
      )
   )
})

# ════════════════════════════════════════════════════════════
# LIGNES DYNAMIQUES — Ajouter
# ════════════════════════════════════════════════════════════
observeEvent(input$add_finance, {
   rows_finance(c(rows_finance(), max(rows_finance()) + 1))
})
observeEvent(input$add_indicateur, {
   rows_indicateur(c(rows_indicateur(), max(rows_indicateur()) + 1))
})
observeEvent(input$add_difficulte, {
   rows_difficulte(c(rows_difficulte(), max(rows_difficulte()) + 1))
})
observeEvent(input$add_recommandation, {
   rows_recommandation(c(rows_recommandation(), max(rows_recommandation()) + 1))
})

# ── Réinitialiser ─────────────────────────────────────────────
observeEvent(input$reset_saisie, {
   rows_finance(c(1)); rows_indicateur(c(1))
   rows_difficulte(c(1)); rows_recommandation(c(1))
})

# ════════════════════════════════════════════════════════════
# LIGNES DYNAMIQUES — Supprimer (fonction générique)
# ════════════════════════════════════════════════════════════
make_delete_observers <- function(rows_rv, prefix) {
   observe({
      lapply(rows_rv(), function(i) {
         local({
            rid <- i
            observeEvent(input[[paste0(prefix, rid)]], {
               rows <- rows_rv()
               if (length(rows) > 1) rows_rv(rows[rows != rid])
               else showNotification("Au moins une ligne requise.", type="warning")
            }, ignoreInit=TRUE)
         })
      })
   })
}

make_delete_observers(rows_finance,        "del_fin_")
make_delete_observers(rows_indicateur,     "del_ind_")
make_delete_observers(rows_difficulte,     "del_dif_")
make_delete_observers(rows_recommandation, "del_rec_")

# ════════════════════════════════════════════════════════════
# RENDU DES FORMULAIRES DYNAMIQUES
# ════════════════════════════════════════════════════════════

# ── Finance ───────────────────────────────────────────────────
output$finance_form <- renderUI({
   rows <- rows_finance()
   lapply(seq_along(rows), function(idx) {
      i <- rows[idx]
      div(class="row-block",
          fluidRow(
             column(12,
                    strong(paste("Source de financement", idx)),
                    actionButton(paste0("del_fin_",i),"✖",
                                 class="btn-del btn-sm", style="float:right; padding:2px 8px;")
             )
          ),
          fluidRow(
             column(4,
                    tags$label(HTML("Source <span class='required-star'>★</span>")),
                    textInput(paste0("sourcefinance_",i), NULL, placeholder="Ex: État, Bailleur")
             ),
             column(5,
                    tags$label(HTML("Mode <span class='required-star'>★</span>")),
                    textInput(paste0("mode_finance_",i), NULL, placeholder="Ex: Don, Prêt")
             ),
             column(3,
                    tags$label("Coût total (milliers FCFA)"),
                    numericInput(paste0("cout_total_",i), NULL, value=NA, min=0)
             )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-center"),
                            paste0("  Cumul décaissements depuis démarrage au 31/12/",
                                   year(Sys.Date())-1)),
                   fluidRow(
                      column(6, tags$label("AE"),
                             numericInput(paste0("c_demarage_ae_",i), NULL, value=NA, min=0)),
                      column(6, tags$label("CP"),
                             numericInput(paste0("c_demarage_cp_",i), NULL, value=NA, min=0))
                   )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-center"),
                            paste0("  Programmation Loi de finances ", year(Sys.Date())-1)),
                   fluidRow(
                      column(6, tags$label("AE"),
                             numericInput(paste0("loi_finance_ae_",i), NULL, value=NA, min=0)),
                      column(6, tags$label("CP"),
                             numericInput(paste0("loi_finance_cp_",i), NULL, value=NA, min=0))
                   )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-center"),
                            paste0("  Programmation révisée au 31/12/", year(Sys.Date())-1)),
                   fluidRow(
                      column(6, tags$label("AE (α)"),
                             numericInput(paste0("programme_revise_ae_",i), NULL, value=NA, min=0)),
                      column(6, tags$label("CP (β)"),
                             numericInput(paste0("programme_revise_cp_",i), NULL, value=NA, min=0))
                   )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-center"),
                            paste0("  Dépenses du 01/01/", year(Sys.Date())-1,
                                   " au 31/12/", year(Sys.Date())-1)),
                   fluidRow(
                      column(6, tags$label("AE (α')"),
                             numericInput(paste0("depense_ae_",i), NULL, value=NA, min=0)),
                      column(6, tags$label("CP (β')"),
                             numericInput(paste0("depense_cp_",i), NULL, value=NA, min=0))
                   )
          ),
          tags$div(class="inner-section",
                   tags$div(class="inner-title", icon("align-center"),
                            paste0("  Dépenses du démarrage au 31/12/", year(Sys.Date())-1)),
                   fluidRow(
                      column(6, tags$label("AE (α'')"),
                             numericInput(paste0("depense_demarage_ae_",i), NULL, value=NA, min=0)),
                      column(6, tags$label("CP (β'')"),
                             numericInput(paste0("depense_demarage_cp_",i), NULL, value=NA, min=0))
                   )
          )
      )
   })
})

# ── Exécution physique ────────────────────────────────────────
output$indicateur_form <- renderUI({
   rows <- rows_indicateur()
   lapply(seq_along(rows), function(idx) {
      i <- rows[idx]
      div(class="row-block",
          fluidRow(
             column(12,
                    strong(paste("Indicateur", idx)),
                    actionButton(paste0("del_ind_",i),"✖",
                                 class="btn-del btn-sm", style="float:right; padding:2px 8px;")
             )
          ),
          fluidRow(
             column(6, textInput(paste0("ind_",i),"Indicateur de produit",
                                 placeholder="Ex: Formations réalisées")),
             column(6, textInput(paste0("uni_",i),"Unité physique",
                                 placeholder="Nombre, Km, %"))
          ),
          fluidRow(
             column(6, numericInput(paste0("pre_",i),
                                    paste0("Prévision physique en ", year(Sys.Date())),
                                    value=NA, min=0)),
             column(6, numericInput(paste0("rea_",i),
                                    paste0("Réalisation 31/12/", year(Sys.Date())),
                                    value=NA, min=0))
          ),
          fluidRow(
             column(6, textInput(paste0("loc_",i),"Localité de réalisation",
                                 placeholder="Ex: Ouagadougou")),
             column(6, textAreaInput(paste0("prv_",i),
                                     "Preuves de matérialité et Observations",
                                     rows=2, placeholder="PV, photos, rapports..."))
          ),
          fluidRow(
             column(6, numericInput(paste0("tan_",i),
                                    paste0("Taux exec physique ",year(Sys.Date())-1," (%)"),
                                    value=NA, min=0, max=100)),
             column(6, numericInput(paste0("tgl_",i),
                                    paste0("Taux exec physique global au 31/12/",year(Sys.Date())-1," (%)"),
                                    value=NA, min=0, max=100))
          )
      )
   })
})

# ── Difficultés ───────────────────────────────────────────────
output$difficulte_form <- renderUI({
   rows <- rows_difficulte()
   lapply(seq_along(rows), function(idx) {
      i <- rows[idx]
      div(class="row-block",
          fluidRow(
             column(12,
                    strong(paste("Difficulté", idx)),
                    actionButton(paste0("del_dif_",i),"✖",
                                 class="btn-del btn-sm", style="float:right; padding:2px 8px;")
             )
          ),
          fluidRow(
             column(6, textAreaInput(paste0("difficulte_",i),"Difficulté",
                                     rows=2, placeholder="Décrire la difficulté...")),
             column(6, textAreaInput(paste0("mesure_",i),"Mesure prise",
                                     rows=2, placeholder="Mesure corrective appliquée..."))
          )
      )
   })
})

# ── Recommandations ───────────────────────────────────────────
output$recommandation_form <- renderUI({
   rows <- rows_recommandation()
   lapply(seq_along(rows), function(idx) {
      i <- rows[idx]
      div(class="row-block",
          fluidRow(
             column(12,
                    strong(paste("Recommandation", idx)),
                    actionButton(paste0("del_rec_",i),"✖",
                                 class="btn-del btn-sm", style="float:right; padding:2px 8px;")
             )
          ),
          fluidRow(
             column(6, textAreaInput(paste0("recommandation_",i),"Recommandation",
                                     rows=2, placeholder="Formuler la recommandation...")),
             column(6, textAreaInput(paste0("echeance_",i),"Échéance de mise en œuvre",
                                     rows=2, placeholder="Ex: 31/03/2026"))
          ),
          fluidRow(
             column(6, textAreaInput(paste0("responsable_",i),
                                     "Responsable de la mise en œuvre",
                                     rows=2, placeholder="Ex: Direction technique...")),
             column(6, textAreaInput(paste0("perspective_",i),"Perspectives",
                                     rows=2, placeholder="Ex: En cours, Planifié..."))
          )
      )
   })
})

# ════════════════════════════════════════════════════════════
# COLLECTE DES DONNÉES DU FORMULAIRE
# ════════════════════════════════════════════════════════════
get_form_data <- reactive({
   
   # ── Projet ─────────────────────────────────────────────────
   projet <- list(
      annee_cible         = input$annee_cible,
      date_remplissage    = input$date_remplissage,
      intitule_projet     = input$intitule_projet     %||% NA,
      ministere_tutelle   = input$ministere_tutelle   %||% NA,
      siege_projet        = input$siege_projet        %||% NA,
      objectif_global     = input$objectif_global     %||% NA,
      resultats_attendus  = input$resultats_attendus  %||% NA,
      secteurs_activites  = input$secteurs_activites  %||% NA,
      zone_intervention   = input$zone_intervention   %||% NA,
      annee_demarrage     = input$annee_demarrage,
      annee_fin           = input$annee_fin,
      ref_arrete_creation = input$ref_arrete_creation %||% NA,
      responsable_nom     = input$responsable_nom     %||% NA,
      responsable_tel     = input$responsable_tel     %||% NA,
      responsable_email   = input$responsable_email   %||% NA
   )
   
   # ── Finance ─────────────────────────────────────────────────
   rows_fin <- rows_finance()
   finance <- do.call(rbind, lapply(seq_along(rows_fin), function(idx) {
      i <- rows_fin[idx]
      data.frame(
         source_financement   = input[[paste0("sourcefinance_",i)]]        %||% NA,
         mode_financement     = input[[paste0("mode_finance_",i)]]         %||% NA,
         cout_total           = input[[paste0("cout_total_",i)]],
         cumul_demarrage_ae   = input[[paste0("c_demarage_ae_",i)]],
         cumul_demarrage_cp   = input[[paste0("c_demarage_cp_",i)]],
         loi_finance_ae       = input[[paste0("loi_finance_ae_",i)]],
         loi_finance_cp       = input[[paste0("loi_finance_cp_",i)]],
         programme_revise_ae  = input[[paste0("programme_revise_ae_",i)]],
         programme_revise_cp  = input[[paste0("programme_revise_cp_",i)]],
         depense_ae           = input[[paste0("depense_ae_",i)]],
         depense_cp           = input[[paste0("depense_cp_",i)]],
         depense_demarrage_ae = input[[paste0("depense_demarage_ae_",i)]],
         depense_demarrage_cp = input[[paste0("depense_demarage_cp_",i)]],
         stringsAsFactors = FALSE
      )
   }))
   
   # ── Exécution physique ───────────────────────────────────────
   rows_ind <- rows_indicateur()
   physique <- do.call(rbind, lapply(seq_along(rows_ind), function(idx) {
      i <- rows_ind[idx]
      data.frame(
         indicateur  = input[[paste0("ind_",i)]] %||% NA,
         unite       = input[[paste0("uni_",i)]] %||% NA,
         prevision   = input[[paste0("pre_",i)]],
         realisation = input[[paste0("rea_",i)]],
         localite    = input[[paste0("loc_",i)]] %||% NA,
         preuves     = input[[paste0("prv_",i)]] %||% NA,
         taux_annuel = input[[paste0("tan_",i)]],
         taux_global = input[[paste0("tgl_",i)]],
         stringsAsFactors = FALSE
      )
   }))
   
   # ── Difficultés ──────────────────────────────────────────────
   rows_dif <- rows_difficulte()
   difficultes <- do.call(rbind, lapply(seq_along(rows_dif), function(idx) {
      i <- rows_dif[idx]
      data.frame(
         difficulte = input[[paste0("difficulte_",i)]] %||% NA,
         mesure     = input[[paste0("mesure_",i)]]     %||% NA,
         stringsAsFactors = FALSE
      )
   }))
   
   # ── Recommandations ──────────────────────────────────────────
   rows_rec <- rows_recommandation()
   recommandations <- do.call(rbind, lapply(seq_along(rows_rec), function(idx) {
      i <- rows_rec[idx]
      data.frame(
         recommandation = input[[paste0("recommandation_",i)]] %||% NA,
         echeance       = input[[paste0("echeance_",i)]]       %||% NA,
         responsable    = input[[paste0("responsable_",i)]]    %||% NA,
         perspective    = input[[paste0("perspective_",i)]]    %||% NA,
         stringsAsFactors = FALSE
      )
   }))
   
   list(
      projet          = projet,
      finance         = finance,
      physique        = physique,
      difficultes     = difficultes,
      recommandations = recommandations
   )
})

# ════════════════════════════════════════════════════════════
# VALIDATION
# ════════════════════════════════════════════════════════════
valider_formulaire <- function(data) {
   erreurs <- c()
   
   p <- data$projet
   if (is.null(p$intitule_projet)   || is.na(p$intitule_projet)   || p$intitule_projet   == "")
      erreurs <- c(erreurs, "Intitulé du projet requis.")
   if (is.null(p$ministere_tutelle) || is.na(p$ministere_tutelle) || p$ministere_tutelle == "")
      erreurs <- c(erreurs, "Ministère de tutelle requis.")
   if (is.null(p$annee_cible)       || is.na(p$annee_cible))
      erreurs <- c(erreurs, "Année cible requise.")
   
   # Finance : au moins une source
   if (nrow(data$finance) == 0 ||
       all(is.na(data$finance$source_financement) | data$finance$source_financement == ""))
      erreurs <- c(erreurs, "Au moins une source de financement requise.")
   
   erreurs
}

# ════════════════════════════════════════════════════════════
# SOUMISSION — Transaction unique
# ════════════════════════════════════════════════════════════
observeEvent(input$submit_all, {
   req(soumission_id_courant())
   
   data    <- get_form_data()
   erreurs <- valider_formulaire(data)
   
   if (length(erreurs) > 0) {
      showModal(modalDialog(
         title  = "⚠️ Champs manquants",
         tags$ul(lapply(erreurs, tags$li)),
         easyClose = TRUE,
         footer = modalButton("Corriger")
      ))
      return()
   }
   
   # Confirmation avant soumission
   showModal(modalDialog(
      title  = "💾 Confirmer la soumission",
      "Êtes-vous sûr de vouloir soumettre ce formulaire ?
     Cette action enverra toutes les données en base.",
      footer = tagList(
         modalButton("Annuler"),
         actionButton("confirm_submit","✅ Confirmer",
                      style="background:linear-gradient(135deg,#f0c040,#e07b20);
               color:#0f1117; border:none; border-radius:6px;
               padding:8px 20px; font-weight:700;")
      )
   ))
})

observeEvent(input$confirm_submit, {
   removeModal()
   req(soumission_id_courant())
   
   data    <- get_form_data()
   soum_id <- soumission_id_courant()
   
   # Vérifier si re-soumission (version existante déjà soumise)
   soum_info <- db_get_soumission_by_id(soum_id)
   if (nrow(soum_info) > 0 && soum_info$statut == "soumis") {
      # Créer une nouvelle version
      nouveau_id <- tryCatch(
         db_nouvelle_version(soum_id, user_id()),
         error = function(e) NULL
      )
      if (!is.null(nouveau_id)) soum_id <- nouveau_id
   }
   
   resultat <- db_soumettre(soum_id, data)
   
   if (resultat$success) {
      soumission_id_courant(soum_id)
      showNotification(
         paste0("✅ ", resultat$message, " (Soumission #", soum_id, ")"),
         type = "message", duration = 6
      )
      # Notification in-app
      db_create_notification(
         utilisateur_id = user_id(),
         type    = "soumission_ok",
         message = paste0("Formulaire soumis avec succès (#", soum_id, ")"),
         lien    = "#historique"
      )
   } else {
      showNotification(resultat$message, type="error", duration=10)
   }
})

# ════════════════════════════════════════════════════════════
# BADGES DE STATUT (dans les titres des box)
# ════════════════════════════════════════════════════════════
statut_badge <- function(rempli, total) {
   if (rempli == 0)
      tags$span(class="statut-badge statut-vide", "VIDE")
   else if (rempli < total)
      tags$span(class="statut-badge statut-partiel", paste0(rempli,"/",total))
   else
      tags$span(class="statut-badge statut-complet", "✓ COMPLET")
}

output$statut_projet <- renderUI({
   remplis <- sum(c(
      nchar(input$intitule_projet   %||% "") > 0,
      nchar(input$ministere_tutelle %||% "") > 0,
      !is.na(input$annee_cible)
   ))
   statut_badge(remplis, 3)
})

output$statut_finance <- renderUI({
   n <- length(rows_finance())
   remplis <- sum(sapply(rows_finance(), function(i) {
      nchar(input[[paste0("sourcefinance_",i)]] %||% "") > 0
   }))
   statut_badge(remplis, n)
})

output$statut_physique <- renderUI({
   n <- length(rows_indicateur())
   remplis <- sum(sapply(rows_indicateur(), function(i) {
      nchar(input[[paste0("ind_",i)]] %||% "") > 0
   }))
   statut_badge(remplis, n)
})

output$statut_difficulte <- renderUI({
   n <- length(rows_difficulte())
   remplis <- sum(sapply(rows_difficulte(), function(i) {
      nchar(input[[paste0("difficulte_",i)]] %||% "") > 0
   }))
   statut_badge(remplis, n)
})

output$statut_recommandation <- renderUI({
   n <- length(rows_recommandation())
   remplis <- sum(sapply(rows_recommandation(), function(i) {
      nchar(input[[paste0("recommandation_",i)]] %||% "") > 0
   }))
   statut_badge(remplis, n)
})