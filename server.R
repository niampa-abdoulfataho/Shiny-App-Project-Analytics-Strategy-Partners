# ═══════════════════════════════════════════════════════════════════════════
# SERVER.R - Phase 3.2 : Authentification + Sélection Structure
# ═══════════════════════════════════════════════════════════════════════════

library(shiny)
library(httr)
library(jsonlite)

source("config.R")
source("R/db_utils.R")
source("R/auth_utils.R")
source("R/structure_utils.R")

# ═══════════════════════════════════════════════════════════════════════════
# SESSION UTILISATEUR - Reactive Values
# ═══════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
   
   # Stocker les données de l'utilisateur connecté
   user_session <- reactiveValues(
      logged_in = FALSE,
      user_id = NULL,
      email = NULL,
      is_admin = FALSE,
      selected_project = NULL,
      selected_trimestre = NULL
   )
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 1. AUTHENTIFICATION
   # ═══════════════════════════════════════════════════════════════════════════
   
   observeEvent(input$btn_login, {
      
      email <- input$email_input
      password <- input$password_input
      
      # Vérifier que les champs ne sont pas vides
      if (email == "" || password == "") {
         output$auth_message <- renderText({
            "<div style='color: red;'>⚠️ Veuillez remplir tous les champs</div>"
         })
         return()
      }
      
      # Appeler la fonction d'authentification
      result <- authenticate_user(email, password)
      
      if (result$success) {
         
         # Authentification réussie
         user_session$logged_in <- TRUE
         user_session$user_id <- result$user_id
         user_session$email <- result$email
         user_session$is_admin <- is_admin(result$user_id)
         
         output$auth_message <- renderText({
            sprintf(
               "<div style='color: green;'>✓ Bienvenue %s!</div>",
               result$email
            )
         })
         
         # Cacher le formulaire de login et afficher l'app
         shinyjs::hide("login_panel")
         shinyjs::show("app_panel")
         
         log_message(sprintf("Utilisateur %s connecté", result$email), "INFO")
         
      } else {
         
         # Authentification échouée
         user_session$logged_in <- FALSE
         
         output$auth_message <- renderText({
            sprintf(
               "<div style='color: red;'>❌ %s</div>",
               result$message
            )
         })
         
         log_message(sprintf("Échec authentification : %s", result$message), "WARN")
      }
   })
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 2. METTRE À JOUR LES STRUCTURES DISPONIBLES
   # ═══════════════════════════════════════════════════════════════════════════
   
   observe({
      
      # Déclencher quand l'utilisateur se connecte
      if (user_session$logged_in) {
         
         # Récupérer les structures pour cet utilisateur
         choices <- prepare_structure_choices(user_session$user_id)
         
         # Mettre à jour le dropdown
         updateSelectInput(
            session,
            "structure_select",
            choices = choices
         )
         
         output$user_info <- renderText({
            role <- if (user_session$is_admin) "Administrateur" else "Utilisateur"
            sprintf(
               "<p><strong>Connecté en tant que :</strong> %s (%s)</p>",
               user_session$email,
               role
            )
         })
      }
   })
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 3. CAPTURER LA STRUCTURE SÉLECTIONNÉE
   # ═══════════════════════════════════════════════════════════════════════════
   
   observeEvent(input$structure_select, {
      
      selected_project <- input$structure_select
      
      if (selected_project != "") {
         
         # Valider l'accès
         has_access <- validate_structure_access(
            user_session$user_id,
            selected_project
         )
         
         if (has_access) {
            
            user_session$selected_project <- selected_project
            
            output$selection_message <- renderText({
               sprintf(
                  "<p style='color: green;'>✓ Structure sélectionnée : %s</p>",
                  selected_project
               )
            })
            
            log_message(
               sprintf("Utilisateur %s a sélectionné structure %s", 
                       user_session$user_id, selected_project),
               "INFO"
            )
            
         } else {
            
            output$selection_message <- renderText({
               "<p style='color: red;'>❌ Accès refusé à cette structure</p>"
            })
            
            log_message(
               sprintf("Accès refusé : utilisateur %s ne peut pas accéder à %s",
                       user_session$user_id, selected_project),
               "WARN"
            )
         }
      }
   })
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 4. CAPTURER LE TRIMESTRE SÉLECTIONNÉ
   # ═══════════════════════════════════════════════════════════════════════════
   
   observeEvent(input$trimestre_select, {
      
      selected_trimestre <- input$trimestre_select
      
      user_session$selected_trimestre <- selected_trimestre
      
      output$trimestre_message <- renderText({
         sprintf(
            "<p style='color: blue;'>Trimestre sélectionné : %s</p>",
            selected_trimestre
         )
      })
      
      log_message(
         sprintf("Trimestre sélectionné : %s", selected_trimestre),
         "DEBUG",
         FALSE
      )
   })
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 5. BOUTON LOGOUT
   # ═══════════════════════════════════════════════════════════════════════════
   
   observeEvent(input$btn_logout, {
      
      # Réinitialiser la session
      user_session$logged_in <- FALSE
      user_session$user_id <- NULL
      user_session$email <- NULL
      user_session$is_admin <- FALSE
      user_session$selected_project <- NULL
      user_session$selected_trimestre <- NULL
      
      # Vider les inputs
      updateTextInput(session, "email_input", value = "")
      updatePasswordInput(session, "password_input", value = "")
      
      # Afficher le formulaire de login
      shinyjs::show("login_panel")
      shinyjs::hide("app_panel")
      
      output$auth_message <- renderText({
         "<div style='color: green;'>✓ Déconnexion réussie</div>"
      })
      
      log_message("Utilisateur déconnecté", "INFO")
   })
   
   # ═══════════════════════════════════════════════════════════════════════════
   # 6. AFFICHER LE STATUT ACTUEL (DEBUG)
   # ═══════════════════════════════════════════════════════════════════════════
   
   output$debug_status <- renderText({
      
      if (!user_session$logged_in) {
         return("<p><strong>Status :</strong> Non connecté</p>")
      }
      
      sprintf(
         "<hr><p><strong>Status :</strong></p>
       <ul>
         <li>User ID: %s</li>
         <li>Email: %s</li>
         <li>Admin: %s</li>
         <li>Projet sélectionné: %s</li>
         <li>Trimestre sélectionné: %s</li>
       </ul>",
         user_session$user_id,
         user_session$email,
         user_session$is_admin,
         ifelse(is.null(user_session$selected_project), "Aucun", user_session$selected_project),
         ifelse(is.null(user_session$selected_trimestre), "Aucun", user_session$selected_trimestre)
      )
   })
}
