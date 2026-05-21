# ============================================================
# modules/historique_server.R
# Onglet Historique : lecture, demande modif, édition, re-soumission
# ============================================================

source("modules/db_historique.R", local = TRUE)

# ════════════════════════════════════════════════════════════
# ÉTAT RÉACTIF
# ════════════════════════════════════════════════════════════
soumission_selectionnee <- reactiveVal(NULL)   # data complète chargée
mode_edition            <- reactiveVal(FALSE)  # TRUE si admin a accepté
snapshot_avant          <- reactiveVal(NULL)   # JSON avant modification

# ════════════════════════════════════════════════════════════
# PAGE HISTORIQUE — UI principale
# ════════════════════════════════════════════════════════════
output$page_historique <- renderUI({
   req(user_role() == "ong", user_id())
   
   sessions <- tryCatch(
      db_get_sessions_ong(user_id()),
      error = function(e) data.frame()
   )
   
   if (nrow(sessions) == 0) {
      return(tags$div(
         tags$div(class = "page-header",
                  tags$div(class = "page-title", "🗄️ Historique des soumissions"),
                  tags$div(class = "page-subtitle", "Aucune soumission trouvée.")
         ),
         box(width = 12,
             tags$p(style = "color:var(--text-faint); text-align:center; padding:40px;",
                    icon("inbox"), "  Vous n'avez encore soumis aucun formulaire.")
         )
      ))
   }
   
   choices_sessions <- setNames(sessions$id,
                                paste0(sessions$label, " — ", sessions$trimestre,
                                       " [", sessions$statut, "]"))
   
   tags$div(
      tags$div(class = "page-header",
               tags$div(class = "page-title", "🗄️ Historique des soumissions"),
               tags$div(class = "page-subtitle",
                        "Consultez vos soumissions et demandez une correction si nécessaire.")
      ),
      
      # ── Sélecteurs ──────────────────────────────────────────
      box(width = 12, collapsible = FALSE,
          title = tags$span(icon("filter"), " Sélection"),
          fluidRow(
             column(5,
                    tags$label("Session"),
                    selectInput("hist_sel_session", NULL, choices = choices_sessions)
             ),
             column(5,
                    tags$label("Soumission"),
                    uiOutput("hist_sel_soumission_ui")
             ),
             column(2,
                    tags$br(),
                    actionButton("hist_charger", "📂 Charger",
                                 style = "background:linear-gradient(135deg,#f0c040,#e07b20);
                     color:#0f1117; border:none; border-radius:7px;
                     padding:9px 18px; font-weight:700; width:100%;")
             )
          )
      ),
      
      # ── Zone d'affichage ─────────────────────────────────────
      uiOutput("hist_contenu")
   )
})

# ── Dropdown soumissions filtré par session ───────────────────
output$hist_sel_soumission_ui <- renderUI({
   req(input$hist_sel_session, user_id())
   soums <- tryCatch(
      db_get_soumissions_ong_session(
         user_id(), as.integer(input$hist_sel_session)),
      error = function(e) data.frame()
   )
   if (nrow(soums) == 0) {
      return(selectInput("hist_sel_soumission", NULL,
                         choices = c("Aucune soumission" = "")))
   }
   choices <- setNames(soums$id,
                       paste0("v", soums$version, " — ",
                              format(soums$created_at, "%d/%m/%Y %H:%M"),
                              " [", soums$statut, "]"))
   selectInput("hist_sel_soumission", NULL, choices = choices)
})

# ════════════════════════════════════════════════════════════
# CHARGEMENT D'UNE SOUMISSION
# ════════════════════════════════════════════════════════════
observeEvent(input$hist_charger, {
   req(input$hist_sel_soumission)
   soum_id <- as.integer(input$hist_sel_soumission)
   
   data <- tryCatch(
      db_get_soumission_affichage(soum_id),
      error = function(e) {
         showNotification(paste0("❌ Erreur : ", e$message), type = "error")
         NULL
      }
   )
   req(data)
   soumission_selectionnee(data)
   
   # Vérifier si le mode édition est autorisé
   statut <- data$soumission$statut
   mode_edition(statut == "modifiable")
   
   # Sauvegarder le snapshot avant modification
   snapshot_avant(snapshot_soumission(data))
})

# ════════════════════════════════════════════════════════════
# AFFICHAGE DU CONTENU
# ════════════════════════════════════════════════════════════
output$hist_contenu <- renderUI({
   data <- soumission_selectionnee()
   req(data)
   
   soum    <- data$soumission
   edition <- mode_edition()
   
   tagList(
      # ── Bandeau mode édition ──────────────────────────────
      if (edition) {
         tags$div(class = "edit-mode-banner",
                  icon("unlock"), " ",
                  tags$strong("Mode édition activé"),
                  " — Double-cliquez sur un champ pour le modifier.",
                  tags$span(style = "margin-left:auto; font-size:10px; opacity:.7;",
                            paste0("Soumission #", soum$id, " v", soum$version))
         )
      },
      
      # ── En-tête soumission ────────────────────────────────
      box(width = 12, collapsible = FALSE,
          title = tags$span(icon("info-circle"), " Informations de la soumission"),
          fluidRow(
             column(3, tags$div(class="hint-text","Session"),
                    tags$div(class="readonly-field", soum$session_label)),
             column(2, tags$div(class="hint-text","Version"),
                    tags$div(class="readonly-field", paste0("v", soum$version))),
             column(2, tags$div(class="hint-text","Statut"),
                    tags$div(class="readonly-field",
                             tags$span(
                                class = paste0("statut-badge ",
                                               switch(soum$statut,
                                                      soumis     = "statut-complet",
                                                      modifiable = "statut-partiel",
                                                      brouillon  = "statut-vide")),
                                soum$statut))),
             column(3, tags$div(class="hint-text","Soumis le"),
                    tags$div(class="readonly-field",
                             format(soum$created_at, "%d/%m/%Y %H:%M"))),
             column(2, tags$div(class="hint-text","Structure"),
                    tags$div(class="readonly-field", soum$nom_structure))
          ),
          tags$div(style = "display:flex; gap:10px; margin-top:14px;",
                   # Bouton demande de modification
                   if (!edition) {
                      actionButton("btn_demande_modif",
                                   tags$span(icon("edit"), " Demander une modification"),
                                   style = "background:var(--accent-soft); color:var(--accent);
                     border:1px solid var(--accent); border-radius:7px;
                     padding:8px 18px; font-size:12px; font-weight:600;")
                   },
                   # Bouton re-soumettre (mode édition uniquement)
                   if (edition) {
                      actionButton("btn_resoumettre",
                                   tags$span(icon("paper-plane"), " Re-soumettre"),
                                   style = "background:linear-gradient(135deg,#f0c040,#e07b20);
                     color:#0f1117; border:none; border-radius:7px;
                     padding:8px 20px; font-weight:700;")
                   }
          )
      ),
      
      # ── Section 1 : Projet ────────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=FALSE,
          title=tags$span(
             tags$span(class="box-badge","1"), icon("folder-open"),
             "Identification du projet"),
          uiOutput("hist_projet")
      ),
      
      # ── Section 2 : Finance ───────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","2"), icon("file-invoice-dollar"),
             "Exécution financière"),
          uiOutput("hist_finance")
      ),
      
      # ── Section 3 : Physique ──────────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","3"), icon("tasks"),
             "Exécution physique"),
          uiOutput("hist_physique")
      ),
      
      # ── Section 4 : Difficultés ───────────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","4"), icon("exclamation-triangle"),
             "Difficultés rencontrées"),
          uiOutput("hist_difficultes")
      ),
      
      # ── Section 5 : Recommandations ───────────────────────
      box(width=12, collapsible=TRUE, collapsed=TRUE,
          title=tags$span(
             tags$span(class="box-badge","5"), icon("lightbulb"),
             "Recommandations"),
          uiOutput("hist_recommandations")
      )
   )
})

# ════════════════════════════════════════════════════════════
# HELPERS : champ lecture seule ou éditable
# ════════════════════════════════════════════════════════════

#' Affiche un champ en lecture seule ou éditable selon le mode
champ <- function(label, valeur, input_id, type = "text", edition = FALSE) {
   valeur_affichee <- valeur %||% "—"
   tags$div(style = "margin-bottom:12px;",
            tags$div(class = "hint-text", label),
            if (!edition) {
               tags$div(class = "readonly-field", valeur_affichee)
            } else {
               if (type == "textarea") {
                  tags$textarea(
                     id            = input_id,
                     class         = "form-control",
                     rows          = "2",
                     ondblclick    = "this.removeAttribute('readonly')",
                     readonly      = "readonly",
                     style         = "cursor:pointer;",
                     title         = "Double-cliquez pour modifier",
                     valeur_affichee
                  )
               } else if (type == "numeric") {
                  tags$input(
                     id         = input_id,
                     type       = "number",
                     class      = "form-control",
                     value      = valeur_affichee,
                     ondblclick = "this.removeAttribute('readonly')",
                     readonly   = "readonly",
                     style      = "cursor:pointer;",
                     title      = "Double-cliquez pour modifier"
                  )
               } else {
                  tags$input(
                     id         = input_id,
                     type       = "text",
                     class      = "form-control",
                     value      = valeur_affichee,
                     ondblclick = "this.removeAttribute('readonly')",
                     readonly   = "readonly",
                     style      = "cursor:pointer;",
                     title      = "Double-cliquez pour modifier"
                  )
               }
            }
   )
}

# ════════════════════════════════════════════════════════════
# RENDU DES SECTIONS
# ════════════════════════════════════════════════════════════
output$hist_projet <- renderUI({
   data    <- soumission_selectionnee(); req(data)
   p       <- data$projet;              req(nrow(p) > 0)
   edition <- mode_edition()
   tagList(
      fluidRow(
         column(8,  champ("Intitulé du projet",   p$intitule_projet,   "e_intitule_projet",   "text",    edition)),
         column(2,  champ("Année cible",          p$annee_cible,       "e_annee_cible",        "numeric", edition)),
         column(2,  champ("Date de remplissage",  p$date_remplissage,  "e_date_remplissage",  "text",    edition))
      ),
      fluidRow(
         column(5,  champ("Ministère de tutelle", p$ministere_tutelle, "e_ministere_tutelle", "text", edition)),
         column(3,  champ("Siège du projet",      p$siege_projet,      "e_siege_projet",      "text", edition)),
         column(2,  champ("Année démarrage",      p$annee_demarrage,   "e_annee_demarrage",   "numeric", edition)),
         column(2,  champ("Année de fin",         p$annee_fin,         "e_annee_fin",         "numeric", edition))
      ),
      tags$div(class = "inner-section",
               tags$div(class = "inner-title", "Description"),
               fluidRow(
                  column(6, champ("Objectif global",    p$objectif_global,    "e_objectif_global",    "textarea", edition)),
                  column(6, champ("Résultats attendus", p$resultats_attendus, "e_resultats_attendus", "textarea", edition))
               ),
               fluidRow(
                  column(6, champ("Secteurs d'activités", p$secteurs_activites, "e_secteurs_activites", "text", edition)),
                  column(6, champ("Zone d'intervention",  p$zone_intervention,  "e_zone_intervention",  "text", edition))
               )
      ),
      tags$div(class = "inner-section",
               tags$div(class = "inner-title", "Responsable & Références"),
               fluidRow(
                  column(3, champ("Nom & Prénoms",     p$responsable_nom,     "e_responsable_nom",     "text", edition)),
                  column(3, champ("Téléphone",         p$responsable_tel,     "e_responsable_tel",     "text", edition)),
                  column(3, champ("Email",             p$responsable_email,   "e_responsable_email",   "text", edition)),
                  column(3, champ("Réf. arrêté",       p$ref_arrete_creation, "e_ref_arrete_creation", "text", edition))
               )
      )
   )
})

output$hist_finance <- renderUI({
   data    <- soumission_selectionnee(); req(data)
   fin     <- data$finance
   edition <- mode_edition()
   if (nrow(fin) == 0)
      return(tags$p(style="color:var(--text-faint);","Aucune donnée financière."))
   lapply(seq_len(nrow(fin)), function(i) {
      f <- fin[i,]
      div(class = "row-block",
          tags$div(class = "fin-block-num",
                   paste0("Source ", i, " : ", f$source_financement %||% "—")),
          fluidRow(
             column(4, champ("Source de financement",   f$source_financement,  paste0("e_src_",i),  "text",    edition)),
             column(4, champ("Mode de financement",     f$mode_financement,    paste0("e_mode_",i), "text",    edition)),
             column(4, champ("Coût total (milliers F)", f$cout_total,          paste0("e_cout_",i), "numeric", edition))
          ),
          tags$div(class = "inner-section",
                   tags$div(class = "inner-title", "Cumul décaissements depuis démarrage"),
                   fluidRow(
                      column(6, champ("AE", f$cumul_demarrage_ae, paste0("e_cdae_",i), "numeric", edition)),
                      column(6, champ("CP", f$cumul_demarrage_cp, paste0("e_cdcp_",i), "numeric", edition))
                   )
          ),
          tags$div(class = "inner-section",
                   tags$div(class = "inner-title", "Programmation Loi de finances"),
                   fluidRow(
                      column(6, champ("AE", f$loi_finance_ae, paste0("e_lfae_",i), "numeric", edition)),
                      column(6, champ("CP", f$loi_finance_cp, paste0("e_lfcp_",i), "numeric", edition))
                   )
          ),
          tags$div(class = "inner-section",
                   tags$div(class = "inner-title", "Programmation révisée"),
                   fluidRow(
                      column(6, champ("AE (α)", f$programme_revise_ae, paste0("e_prae_",i), "numeric", edition)),
                      column(6, champ("CP (β)", f$programme_revise_cp, paste0("e_prcp_",i), "numeric", edition))
                   )
          ),
          tags$div(class = "inner-section",
                   tags$div(class = "inner-title", "Dépenses de l'année"),
                   fluidRow(
                      column(6, champ("AE (α')", f$depense_ae, paste0("e_dae_",i), "numeric", edition)),
                      column(6, champ("CP (β')", f$depense_cp, paste0("e_dcp_",i), "numeric", edition))
                   )
          ),
          tags$div(class = "inner-section",
                   tags$div(class = "inner-title", "Dépenses depuis démarrage"),
                   fluidRow(
                      column(6, champ("AE (α'')", f$depense_demarrage_ae, paste0("e_ddae_",i), "numeric", edition)),
                      column(6, champ("CP (β'')", f$depense_demarrage_cp, paste0("e_ddcp_",i), "numeric", edition))
                   )
          )
      )
   })
})

output$hist_physique <- renderUI({
   data    <- soumission_selectionnee(); req(data)
   phy     <- data$physique
   edition <- mode_edition()
   if (nrow(phy) == 0)
      return(tags$p(style="color:var(--text-faint);","Aucun indicateur."))
   lapply(seq_len(nrow(phy)), function(i) {
      ph <- phy[i,]
      div(class = "row-block",
          tags$strong(paste("Indicateur", i)),
          fluidRow(
             column(6, champ("Indicateur de produit", ph$indicateur,  paste0("e_ind_",i),  "text", edition)),
             column(6, champ("Unité physique",         ph$unite,       paste0("e_uni_",i),  "text", edition))
          ),
          fluidRow(
             column(6, champ("Prévision",   ph$prevision,   paste0("e_pre_",i), "numeric", edition)),
             column(6, champ("Réalisation", ph$realisation, paste0("e_rea_",i), "numeric", edition))
          ),
          fluidRow(
             column(6, champ("Localité",              ph$localite, paste0("e_loc_",i), "text",    edition)),
             column(6, champ("Preuves / Observations",ph$preuves,  paste0("e_prv_",i), "textarea", edition))
          ),
          fluidRow(
             column(6, champ("Taux exec annuel (%)",  ph$taux_annuel, paste0("e_tan_",i), "numeric", edition)),
             column(6, champ("Taux exec global (%)",  ph$taux_global, paste0("e_tgl_",i), "numeric", edition))
          )
      )
   })
})

output$hist_difficultes <- renderUI({
   data    <- soumission_selectionnee(); req(data)
   dif     <- data$difficultes
   edition <- mode_edition()
   if (nrow(dif) == 0)
      return(tags$p(style="color:var(--text-faint);","Aucune difficulté saisie."))
   lapply(seq_len(nrow(dif)), function(i) {
      d <- dif[i,]
      div(class = "row-block",
          tags$strong(paste("Difficulté", i)),
          fluidRow(
             column(6, champ("Difficulté", d$difficulte, paste0("e_dif_",i), "textarea", edition)),
             column(6, champ("Mesure prise", d$mesure,   paste0("e_mes_",i), "textarea", edition))
          )
      )
   })
})

output$hist_recommandations <- renderUI({
   data    <- soumission_selectionnee(); req(data)
   rec     <- data$recommandations
   edition <- mode_edition()
   if (nrow(rec) == 0)
      return(tags$p(style="color:var(--text-faint);","Aucune recommandation saisie."))
   lapply(seq_len(nrow(rec)), function(i) {
      r <- rec[i,]
      div(class = "row-block",
          tags$strong(paste("Recommandation", i)),
          fluidRow(
             column(6, champ("Recommandation",    r$recommandation, paste0("e_rec_",i), "textarea", edition)),
             column(6, champ("Échéance",          r$echeance,       paste0("e_ech_",i), "text",     edition))
          ),
          fluidRow(
             column(6, champ("Responsable",       r$responsable,    paste0("e_res_",i), "text",     edition)),
             column(6, champ("Perspectives",      r$perspective,    paste0("e_per_",i), "textarea", edition))
          )
      )
   })
})

# ════════════════════════════════════════════════════════════
# MODAL DEMANDE DE MODIFICATION
# ════════════════════════════════════════════════════════════
observeEvent(input$btn_demande_modif, {
   data <- soumission_selectionnee(); req(data)
   soum <- data$soumission
   
   # Vérifier si une demande est déjà en attente
   if (db_has_demande_en_attente(soum$id, user_id())) {
      showNotification(
         "⚠️ Une demande est déjà en cours de traitement.", type = "warning")
      return()
   }
   
   showModal(modalDialog(
      title = tags$span(icon("edit"), " Demande de modification"),
      size  = "m",
      tags$div(
         tags$div(class = "inner-title", "Identification"),
         fluidRow(
            column(6,
                   tags$label("Nom de la structure"),
                   tags$div(class = "readonly-field", soum$nom_structure)
            ),
            column(6,
                   tags$label("Email"),
                   tags$div(class = "readonly-field", soum$email)
            )
         ),
         fluidRow(
            column(6,
                   tags$label("Date de soumission originale"),
                   tags$div(class = "readonly-field",
                            format(soum$created_at, "%d/%m/%Y"))
            ),
            column(6,
                   tags$label("Date de la demande"),
                   tags$div(class = "readonly-field",
                            format(Sys.Date(), "%d/%m/%Y"))
            )
         ),
         tags$hr(style = "border-color:var(--border); margin:16px 0;"),
         tags$div(class = "inner-title", "Contenu de la demande"),
         tags$label("Objet de la demande"),
         textInput("dem_objet", NULL,
                   placeholder = "Ex: Correction d'une erreur sur les données financières"),
         tags$label("Corps de la demande"),
         textAreaInput("dem_corps", NULL, rows = 4,
                       placeholder = paste0(
                          "Bonjour,\n\nJe souhaite modifier ma soumission du ",
                          format(soum$created_at, "%d/%m/%Y"),
                          " concernant...\n\nMotif : ..."
                       ))
      ),
      footer = tagList(
         modalButton("Annuler"),
         actionButton("btn_envoyer_demande",
                      tags$span(icon("paper-plane"), " Envoyer la demande"),
                      style = "background:linear-gradient(135deg,#f0c040,#e07b20);
                 color:#0f1117; border:none; border-radius:6px;
                 padding:8px 20px; font-weight:700;")
      )
   ))
})

# ── Envoi de la demande ───────────────────────────────────────
observeEvent(input$btn_envoyer_demande, {
   req(input$dem_objet, input$dem_corps)
   data <- soumission_selectionnee(); req(data)
   soum <- data$soumission
   
   if (nchar(trimws(input$dem_objet)) == 0 ||
       nchar(trimws(input$dem_corps)) == 0) {
      showNotification("⚠️ Objet et corps obligatoires.", type = "warning")
      return()
   }
   
   tryCatch({
      # Sauvegarder la demande en BD
      db_create_demande(
         soumission_id             = soum$id,
         utilisateur_id            = user_id(),
         nom_structure             = soum$nom_structure,
         email                     = soum$email,
         date_soumission_originale = as.Date(soum$created_at),
         objet                     = input$dem_objet,
         corps                     = input$dem_corps
      )
      
      # Notifier les admins par mail
      admins <- tryCatch(
         dbGetQuery(get_con(),
                    "SELECT email FROM utilisateurs WHERE role='admin' AND actif=TRUE"),
         error = function(e) data.frame(email = character(0))
      )
      lapply(admins$email, function(mail_admin) {
         mail_demande_recue(mail_admin, soum$nom_structure, input$dem_objet)
      })
      
      # Notification in-app pour l'ONG
      db_create_notification(
         utilisateur_id = user_id(),
         type    = "demande_envoyee",
         message = paste0("Votre demande de modification a été envoyée : ",
                          input$dem_objet),
         lien    = "#historique"
      )
      
      removeModal()
      showNotification(
         "✅ Demande envoyée. Vous serez notifié dès traitement.",
         type = "message", duration = 6)
      
   }, error = function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type = "error")
   })
})

# ════════════════════════════════════════════════════════════
# RE-SOUMISSION (mode édition)
# ════════════════════════════════════════════════════════════
observeEvent(input$btn_resoumettre, {
   showModal(modalDialog(
      title = "💾 Confirmer la re-soumission",
      "Une nouvelle version sera créée avec vos modifications.
     L'ancienne version reste conservée dans l'historique.",
      footer = tagList(
         modalButton("Annuler"),
         actionButton("confirm_resoumettre",
                      "✅ Confirmer",
                      style = "background:linear-gradient(135deg,#f0c040,#e07b20);
                 color:#0f1117; border:none; border-radius:6px;
                 padding:8px 20px; font-weight:700;")
      )
   ))
})

observeEvent(input$confirm_resoumettre, {
   removeModal()
   data <- soumission_selectionnee(); req(data)
   soum <- data$soumission
   
   tryCatch({
      # ── 1. Collecter les données modifiées depuis les champs JS ──
      n_fin <- nrow(data$finance)
      n_phy <- nrow(data$physique)
      n_dif <- nrow(data$difficultes)
      n_rec <- nrow(data$recommandations)
      
      nouveau_projet <- list(
         annee_cible         = as.numeric(input$e_annee_cible        %||% NA),
         date_remplissage    = input$e_date_remplissage               %||% NA,
         intitule_projet     = input$e_intitule_projet                %||% NA,
         ministere_tutelle   = input$e_ministere_tutelle              %||% NA,
         siege_projet        = input$e_siege_projet                   %||% NA,
         objectif_global     = input$e_objectif_global                %||% NA,
         resultats_attendus  = input$e_resultats_attendus             %||% NA,
         secteurs_activites  = input$e_secteurs_activites             %||% NA,
         zone_intervention   = input$e_zone_intervention              %||% NA,
         annee_demarrage     = as.numeric(input$e_annee_demarrage     %||% NA),
         annee_fin           = as.numeric(input$e_annee_fin           %||% NA),
         ref_arrete_creation = input$e_ref_arrete_creation            %||% NA,
         responsable_nom     = input$e_responsable_nom                %||% NA,
         responsable_tel     = input$e_responsable_tel                %||% NA,
         responsable_email   = input$e_responsable_email              %||% NA
      )
      
      nouveau_finance <- if (n_fin > 0) {
         do.call(rbind, lapply(seq_len(n_fin), function(i) {
            data.frame(
               source_financement   = input[[paste0("e_src_",i)]]  %||% NA,
               mode_financement     = input[[paste0("e_mode_",i)]] %||% NA,
               cout_total           = as.numeric(input[[paste0("e_cout_",i)]]),
               cumul_demarrage_ae   = as.numeric(input[[paste0("e_cdae_",i)]]),
               cumul_demarrage_cp   = as.numeric(input[[paste0("e_cdcp_",i)]]),
               loi_finance_ae       = as.numeric(input[[paste0("e_lfae_",i)]]),
               loi_finance_cp       = as.numeric(input[[paste0("e_lfcp_",i)]]),
               programme_revise_ae  = as.numeric(input[[paste0("e_prae_",i)]]),
               programme_revise_cp  = as.numeric(input[[paste0("e_prcp_",i)]]),
               depense_ae           = as.numeric(input[[paste0("e_dae_",i)]]),
               depense_cp           = as.numeric(input[[paste0("e_dcp_",i)]]),
               depense_demarrage_ae = as.numeric(input[[paste0("e_ddae_",i)]]),
               depense_demarrage_cp = as.numeric(input[[paste0("e_ddcp_",i)]]),
               stringsAsFactors = FALSE
            )
         }))
      } else data.frame()
      
      nouveau_physique <- if (n_phy > 0) {
         do.call(rbind, lapply(seq_len(n_phy), function(i) {
            data.frame(
               indicateur  = input[[paste0("e_ind_",i)]] %||% NA,
               unite       = input[[paste0("e_uni_",i)]] %||% NA,
               prevision   = as.numeric(input[[paste0("e_pre_",i)]]),
               realisation = as.numeric(input[[paste0("e_rea_",i)]]),
               localite    = input[[paste0("e_loc_",i)]] %||% NA,
               preuves     = input[[paste0("e_prv_",i)]] %||% NA,
               taux_annuel = as.numeric(input[[paste0("e_tan_",i)]]),
               taux_global = as.numeric(input[[paste0("e_tgl_",i)]]),
               stringsAsFactors = FALSE
            )
         }))
      } else data.frame()
      
      nouveau_difficultes <- if (n_dif > 0) {
         do.call(rbind, lapply(seq_len(n_dif), function(i) {
            data.frame(
               difficulte = input[[paste0("e_dif_",i)]] %||% NA,
               mesure     = input[[paste0("e_mes_",i)]] %||% NA,
               stringsAsFactors = FALSE
            )
         }))
      } else data.frame()
      
      nouveau_recommandations <- if (n_rec > 0) {
         do.call(rbind, lapply(seq_len(n_rec), function(i) {
            data.frame(
               recommandation = input[[paste0("e_rec_",i)]] %||% NA,
               echeance       = input[[paste0("e_ech_",i)]] %||% NA,
               responsable    = input[[paste0("e_res_",i)]] %||% NA,
               perspective    = input[[paste0("e_per_",i)]] %||% NA,
               stringsAsFactors = FALSE
            )
         }))
      } else data.frame()
      
      # ── 2. Calculer le diff ──────────────────────────────────
      snapshot_apres <- toJSON(list(
         projet          = nouveau_projet,
         finance         = lapply(seq_len(nrow(nouveau_finance)),
                                  function(i) as.list(nouveau_finance[i,])),
         physique        = lapply(seq_len(nrow(nouveau_physique)),
                                  function(i) as.list(nouveau_physique[i,])),
         difficultes     = lapply(seq_len(nrow(nouveau_difficultes)),
                                  function(i) as.list(nouveau_difficultes[i,])),
         recommandations = lapply(seq_len(nrow(nouveau_recommandations)),
                                  function(i) as.list(nouveau_recommandations[i,]))
      ), auto_unbox = TRUE, na = "null")
      
      diff <- compute_diff(
         fromJSON(snapshot_avant()),
         fromJSON(snapshot_apres)
      )
      
      # ── 3. Créer une nouvelle version en BD ──────────────────
      nouveau_id <- db_nouvelle_version(soum$id, user_id())
      
      # ── 4. Transaction de re-soumission ──────────────────────
      resultat <- db_soumettre(nouveau_id, list(
         projet          = nouveau_projet,
         finance         = nouveau_finance,
         physique        = nouveau_physique,
         difficultes     = nouveau_difficultes,
         recommandations = nouveau_recommandations
      ))
      
      if (!resultat$success) stop(resultat$message)
      
      # ── 5. Sauvegarder le diff ───────────────────────────────
      if (length(diff) > 0) {
         db_save_diff(nouveau_id, soum$version + 1, diff, user_id())
      }
      
      # ── 6. Notifier + mettre à jour l'affichage ──────────────
      db_create_notification(
         utilisateur_id = user_id(),
         type    = "resoumission_ok",
         message = paste0("Re-soumission effectuée avec succès (v",
                          soum$version + 1, ") — #", nouveau_id),
         lien    = "#historique"
      )
      
      # Recharger la nouvelle version dans l'affichage
      nouvelle_data <- db_get_soumission_affichage(nouveau_id)
      soumission_selectionnee(nouvelle_data)
      mode_edition(FALSE)
      snapshot_avant(NULL)
      
      showNotification(
         paste0("✅ Re-soumission v", soum$version + 1,
                " enregistrée. ", length(diff), " champ(s) modifié(s)."),
         type = "message", duration = 8)
      
   }, error = function(e) {
      showNotification(paste0("❌ Erreur : ", e$message), type = "error")
   })
})