# ═══════════════════════════════════════════════════════════════════════════
# UTILITAIRES STRUCTURES - Phase 3.2
# ═══════════════════════════════════════════════════════════════════════════
#
# Responsable : AFN
# Description : Fonctions pour récupérer structures avec filtre utilisateur
# 
# Fonctions :
# ✓ get_structures_for_user()  - Structures visibles pour cet utilisateur
# ✓ get_all_structures()       - Toutes structures (admin uniquement)
# ✓ validate_structure_access()- Vérifier accès à une structure
# ═══════════════════════════════════════════════════════════════════════════




# ═══════════════════════════════════════════════════════════════════════════
# UTILITAIRES STRUCTURES - Phase 3.2 (FIXED)
# ═══════════════════════════════════════════════════════════════════════════

source("R/db_utils.R")
source("R/auth_utils.R")

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 1 : STRUCTURES POUR UN UTILISATEUR
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer les structures accessibles par un utilisateur
#'
#' @param user_id ID de l'utilisateur
#'
#' @return Data frame avec les structures
#'
#' @export
get_structures_for_user <- function(user_id) {
   tryCatch({
      
      if (is.null(user_id)) {
         return(data.frame())
      }
      
      # Récupérer les données de l'utilisateur
      user <- get_user_data(user_id)
      
      if (is.null(user)) {
         log_message(sprintf("Utilisateur %s non trouvé", user_id), "WARN")
         return(data.frame())
      }
      
      # SI ADMIN (pas de projet_id) : voir TOUTES les structures
      if (is_admin(user_id)) {
         log_message(sprintf("Admin %s : accès à TOUTES les structures", user_id), "DEBUG", FALSE)
         return(get_all_structures())
      }
      
      # SINON : voir seulement les structures de son projet
      projet_id <- user$projet_id
      
      log_message(sprintf("Utilisateur %s : structures du projet %s", user_id, projet_id), "DEBUG", FALSE)
      
      # Récupérer tous les projets depuis l'API
      response_text <- content(
         GET(
            sprintf("%s/rest/v1/projets", SUPABASE_CONFIG$url),
            add_headers(
               apikey = SUPABASE_CONFIG$anon_key,
               `Content-Type` = "application/json"
            )
         ),
         as = "text",
         encoding = "UTF-8"
      )
      
      # Utiliser simplifyDataFrame = FALSE pour éviter les vecteurs atomiques
      result <- fromJSON(response_text, simplifyDataFrame = FALSE)
      
      if (length(result) == 0) {
         return(data.frame())
      }
      
      # Filtrer par projet_id AVANT de convertir en data frame
      structures_filtered <- Filter(function(x) x$projet_id == projet_id, result)
      
      if (length(structures_filtered) == 0) {
         log_message(sprintf("Aucune structure trouvée pour projet %s", projet_id), "WARN")
         return(data.frame())
      }
      
      # Convertir en data frame
      structures_df <- data.frame(
         projet_id = sapply(structures_filtered, function(x) x$projet_id),
         intitule_projet = sapply(structures_filtered, function(x) x$intitule_projet),
         stringsAsFactors = FALSE
      )
      
      log_message(sprintf("✓ %d structures pour utilisateur %s", nrow(structures_df), user_id), "DEBUG", FALSE)
      
      return(structures_df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_structures_for_user : %s", e$message), "ERROR")
      return(data.frame())
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 2 : TOUTES LES STRUCTURES (ADMIN)
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer TOUTES les structures (admin uniquement)
#'
#' @param user_id ID de l'utilisateur (vérification qu'il est admin)
#'
#' @return Data frame avec toutes les structures
#'
#' @export
get_all_structures <- function(user_id = NULL) {
   tryCatch({
      
      # Vérifier si admin (si user_id fourni)
      if (!is.null(user_id)) {
         if (!is_admin(user_id)) {
            log_message(sprintf("Accès refusé : utilisateur %s n'est pas admin", user_id), "WARN")
            return(data.frame())
         }
      }
      
      # Récupérer tous les projets depuis l'API
      response_text <- content(
         GET(
            sprintf("%s/rest/v1/projets", SUPABASE_CONFIG$url),
            add_headers(
               apikey = SUPABASE_CONFIG$anon_key,
               `Content-Type` = "application/json"
            )
         ),
         as = "text",
         encoding = "UTF-8"
      )
      
      # Utiliser simplifyDataFrame = FALSE
      result <- fromJSON(response_text, simplifyDataFrame = FALSE)
      
      if (length(result) == 0) {
         return(data.frame())
      }
      
      # Convertir en data frame avec les colonnes importantes
      structures_df <- data.frame(
         projet_id = sapply(result, function(x) x$projet_id),
         intitule_projet = sapply(result, function(x) x$intitule_projet),
         stringsAsFactors = FALSE
      )
      
      log_message(sprintf("✓ %d structures totales récupérées", nrow(structures_df)), "DEBUG", FALSE)
      
      return(structures_df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_all_structures : %s", e$message), "ERROR")
      return(data.frame())
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 3 : VALIDER ACCÈS À UNE STRUCTURE
# ═══════════════════════════════════════════════════════════════════════════

#' Valider que l'utilisateur a accès à une structure
#'
#' @param user_id ID de l'utilisateur
#' @param projet_id ID du projet/structure
#'
#' @return TRUE/FALSE
#'
#' @export
validate_structure_access <- function(user_id, projet_id) {
   tryCatch({
      
      if (is.null(user_id) || is.null(projet_id)) {
         return(FALSE)
      }
      
      # Admin a accès à tout
      if (is_admin(user_id)) {
         return(TRUE)
      }
      
      # Utilisateur normal : vérifier qu'il appartient au projet
      user <- get_user_data(user_id)
      
      if (is.null(user)) {
         return(FALSE)
      }
      
      # Vérifier que c'est le même projet
      has_access <- user$projet_id == projet_id
      
      log_message(
         sprintf("Utilisateur %s - accès structure %s : %s", user_id, projet_id, has_access),
         "DEBUG",
         FALSE
      )
      
      return(has_access)
      
   }, error = function(e) {
      log_message(sprintf("Erreur validate_structure_access : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FONCTION 4 : PRÉPARER CHOICES POUR DROPDOWN
# ═══════════════════════════════════════════════════════════════════════════

#' Préparer les options pour un dropdown Shiny
#'
#' @param user_id ID de l'utilisateur
#'
#' @return named list (id = nom) pour selectInput()
#'
#' @export
prepare_structure_choices <- function(user_id) {
   tryCatch({
      
      structures <- get_structures_for_user(user_id)
      
      log_message(sprintf("DEBUG: structures récupérées = %d lignes", nrow(structures)), "DEBUG", FALSE)
      
      if (nrow(structures) == 0) {
         log_message(sprintf("Aucune structure pour utilisateur %s", user_id), "WARN")
         return(list("Aucune structure disponible" = ""))
      }
      
      # Créer une named list : projet_id = intitule_projet
      choices <- setNames(
         as.list(structures$projet_id),
         structures$intitule_projet
      )
      
      log_message(sprintf("✓ %d choix préparés pour dropdown", length(choices)), "DEBUG", FALSE)
      
      return(choices)
      
   }, error = function(e) {
      log_message(sprintf("Erreur prepare_structure_choices : %s", e$message), "ERROR")
      return(list("Erreur" = ""))
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# FIN - structure_utils.R (FIXED)
# ═══════════════════════════════════════════════════════════════════════════