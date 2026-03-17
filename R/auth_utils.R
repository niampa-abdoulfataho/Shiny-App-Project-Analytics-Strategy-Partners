# ═══════════════════════════════════════════════════════════════════════════
# UTILITAIRES D'AUTHENTIFICATION - Phase 3.2 (FINAL)
# ═══════════════════════════════════════════════════════════════════════════

library(digest)

source("R/db_utils.R")

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 1 : AUTHENTIFIER UN UTILISATEUR
# ═══════════════════════════════════════════════════════════════════════════

#' Authentifier un utilisateur
#'
#' @param email Email de l'utilisateur
#' @param password Mot de passe en clair
#'
#' @return list(success=TRUE/FALSE, user_id=..., message=...)
#'
#' @export
authenticate_user <- function(email, password) {
   tryCatch({
      
      # Validation des entrées
      if (is.null(email) || email == "") {
         return(list(
            success = FALSE,
            user_id = NULL,
            message = "Email manquant"
         ))
      }
      
      if (is.null(password) || password == "") {
         return(list(
            success = FALSE,
            user_id = NULL,
            message = "Mot de passe manquant"
         ))
      }
      
      # Récupérer l'utilisateur depuis la BD
      response_text <- content(
         GET(
            sprintf("%s/rest/v1/users?user_e_email=eq.%s", 
                    SUPABASE_CONFIG$url, email),
            add_headers(
               apikey = SUPABASE_CONFIG$anon_key,
               `Content-Type` = "application/json"
            )
         ),
         as = "text",
         encoding = "UTF-8"
      )
      
      # Utiliser simplifyDataFrame = FALSE pour éviter les vecteurs atomiques
      users <- fromJSON(response_text, simplifyDataFrame = FALSE)
      
      # Vérifier que l'utilisateur existe
      if (length(users) == 0) {
         return(list(
            success = FALSE,
            user_id = NULL,
            message = "Utilisateur non trouvé"
         ))
      }
      
      # Récupérer le premier utilisateur (il n'y en a qu'un avec cet email)
      user <- users[[1]]
      
      # Vérifier le mot de passe (comparaison directe)
      # Note: Dans la BD, les passwords sont actuellement en clair
      # À AMÉLIORER: hacher les passwords en production!
      
      if (user$password != password) {
         return(list(
            success = FALSE,
            user_id = NULL,
            message = "Mot de passe incorrect"
         ))
      }
      
      # Authentification réussie
      log_message(sprintf("✓ Utilisateur authentifié : %s", email), "INFO")
      
      return(list(
         success = TRUE,
         user_id = user$id_user,
         email = user$user_e_email,
         projet_id = user$projet_id,
         message = "Authentification réussie"
      ))
      
   }, error = function(e) {
      log_message(sprintf("Erreur authentification : %s", e$message), "ERROR")
      return(list(
         success = FALSE,
         user_id = NULL,
         message = sprintf("Erreur : %s", e$message)
      ))
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 2 : RÉCUPÉRER DONNÉES UTILISATEUR
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer les données d'un utilisateur
#'
#' @param user_id ID de l'utilisateur
#'
#' @return list avec données utilisateur ou NULL
#'
#' @export
get_user_data <- function(user_id) {
   tryCatch({
      
      if (is.null(user_id)) {
         return(NULL)
      }
      
      # Récupérer depuis BD
      response_text <- content(
         GET(
            sprintf("%s/rest/v1/users?id_user=eq.%s", 
                    SUPABASE_CONFIG$url, user_id),
            add_headers(
               apikey = SUPABASE_CONFIG$anon_key,
               `Content-Type` = "application/json"
            )
         ),
         as = "text",
         encoding = "UTF-8"
      )
      
      users <- fromJSON(response_text, simplifyDataFrame = FALSE)
      
      if (length(users) == 0) {
         return(NULL)
      }
      
      user <- users[[1]]
      
      log_message(sprintf("✓ Données utilisateur %s récupérées", user_id), "DEBUG", FALSE)
      
      return(user)
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_user_data : %s", e$message), "ERROR")
      return(NULL)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 3 : VÉRIFIER SI ADMIN
# ═══════════════════════════════════════════════════════════════════════════

#' Vérifier si l'utilisateur est admin
#'
#' @param user_id ID de l'utilisateur
#'
#' @return TRUE/FALSE
#'
#' @export
is_admin <- function(user_id) {
   tryCatch({
      
      user <- get_user_data(user_id)
      
      if (is.null(user)) {
         return(FALSE)
      }
      
      # Un utilisateur est admin s'il n'a pas de projet_id
      # (c'est un super-utilisateur)
      is_admin_user <- is.na(user$projet_id) || user$projet_id == ""
      
      return(is_admin_user)
      
   }, error = function(e) {
      log_message(sprintf("Erreur is_admin : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 4 : RÉCUPÉRER LE RÔLE UTILISATEUR
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer le rôle de l'utilisateur
#'
#' @param user_id ID de l'utilisateur
#'
#' @return "admin" ou "user"
#'
#' @export
get_user_role <- function(user_id) {
   
   if (is_admin(user_id)) {
      return("admin")
   } else {
      return("user")
   }
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 5 : VALIDER SESSION UTILISATEUR
# ═══════════════════════════════════════════════════════════════════════════

#' Valider que l'utilisateur est connecté
#'
#' @param user_id ID de l'utilisateur
#'
#' @return TRUE/FALSE
#'
#' @export
is_user_logged_in <- function(user_id) {
   
   if (is.null(user_id) || user_id == "") {
      return(FALSE)
   }
   
   user <- get_user_data(user_id)
   return(!is.null(user))
}

# ═══════════════════════════════════════════════════════════════════════════
# FIN - auth_utils.R (FINAL - FONCTIONNE)
# ═══════════════════════════════════════════════════════════════════════════