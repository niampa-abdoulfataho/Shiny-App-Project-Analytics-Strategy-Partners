# ═══════════════════════════════════════════════════════════════════════════
# UTILITAIRES DE BASE DE DONNÉES v2 - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════
#
# Responsable : AFN
# Phase : 2.3
# Description : CRUD complet pour toutes les tables du questionnaire
#
# Opérations :
# ✓ GET     - Récupérer tous / par ID
# ✓ INSERT  - Insérer
# ✓ UPDATE  - Mettre à jour
# ✓ DELETE  - Supprimer
# ═══════════════════════════════════════════════════════════════════════════

library(httr)
library(jsonlite)

if (!file.exists("config.R")) {
   stop("config.R manquant")
}
source("config.R")

if (!dir.exists("logs")) {
   dir.create("logs")
}
LOG_FILE <- "logs/database.log"

# ═══════════════════════════════════════════════════════════════════════════
# LOGGING
# ═══════════════════════════════════════════════════════════════════════════

log_message <- function(message, level = "INFO", to_file = TRUE) {
   timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
   log_text <- sprintf("[%s] [%s] %s", timestamp, level, message)
   cat(log_text, "\n")
   
   if (to_file) {
      tryCatch({
         write(log_text, file = LOG_FILE, append = TRUE)
      }, error = function(e) { })
   }
   invisible(NULL)
}

# ═══════════════════════════════════════════════════════════════════════════
# TEST CONNEXION
# ═══════════════════════════════════════════════════════════════════════════

#' Tester la connexion
#'
#' @return TRUE/FALSE
#'
#' @export
test_db_connection <- function() {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets", SUPABASE_CONFIG$url)
      
      response <- GET(
         url,
         add_headers(
            apikey = SUPABASE_CONFIG$anon_key,
            `Content-Type` = "application/json"
         )
      )
      
      if (status_code(response) == 200) {
         log_message("✓ Connexion API OK", "INFO")
         return(TRUE)
      } else {
         log_message(sprintf("Connexion échouée (status: %d)", status_code(response)), "ERROR")
         return(FALSE)
      }
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# PROJETS - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer tous les projets
#' @export
get_projets <- function() {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets?order=projet_id.asc", SUPABASE_CONFIG$url)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      log_message(sprintf("✓ %d projets récupérés", nrow(df)), "DEBUG", FALSE)
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_projets : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer un projet par ID
#' @export
get_projet_by_id <- function(projet_id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets?projet_id=eq.%s", SUPABASE_CONFIG$url, projet_id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      
      log_message(sprintf("✓ Projet %s récupéré", projet_id), "DEBUG", FALSE)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_projet_by_id : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer un projet
#' @export
insert_projet <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"), 
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) {
         log_message(sprintf("Erreur INSERT projet (status: %d)", status_code(response)), "ERROR")
         return(NULL)
      }
      
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Projet inséré"), "INFO")
      return(result[[1]]$projet_id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur insert_projet : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour un projet
#' @export
update_projet <- function(projet_id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets?projet_id=eq.%s", SUPABASE_CONFIG$url, projet_id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) {
         log_message(sprintf("Erreur UPDATE projet (status: %d)", status_code(response)), "ERROR")
         return(FALSE)
      }
      
      log_message(sprintf("✓ Projet %s mis à jour", projet_id), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur update_projet : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer un projet
#' @export
delete_projet <- function(projet_id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/projets?projet_id=eq.%s", SUPABASE_CONFIG$url, projet_id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) {
         log_message(sprintf("Erreur DELETE projet (status: %d)", status_code(response)), "ERROR")
         return(FALSE)
      }
      
      log_message(sprintf("✓ Projet %s supprimé", projet_id), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur delete_projet : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# RECOMMANDATIONS - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer toutes les recommandations
#' @export
get_recommandations <- function(projet_id = NULL) {
   tryCatch({
      url <- sprintf("%s/rest/v1/recommandations", SUPABASE_CONFIG$url)
      if (!is.null(projet_id)) url <- sprintf("%s?projet_id=eq.%s", url, projet_id)
      
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      log_message(sprintf("✓ %d recommandations récupérées", nrow(df)), "DEBUG", FALSE)
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur get_recommandations : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer une recommandation par ID
#' @export
get_recommandation_by_id <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/recommandations?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      
      log_message(sprintf("✓ Recommandation %d récupérée", id), "DEBUG", FALSE)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer une recommandation
#' @export
insert_recommandation <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/recommandations", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) {
         log_message(sprintf("Erreur INSERT recommandation (status: %d)", status_code(response)), "ERROR")
         return(NULL)
      }
      
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Recommandation insérée (ID %d)", result[[1]]$id), "INFO")
      return(result[[1]]$id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur insert_recommandation : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour une recommandation
#' @export
update_recommandation <- function(id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/recommandations?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) {
         log_message(sprintf("Erreur UPDATE (status: %d)", status_code(response)), "ERROR")
         return(FALSE)
      }
      
      log_message(sprintf("✓ Recommandation %d mise à jour", id), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer une recommandation
#' @export
delete_recommandation <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/recommandations?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) {
         log_message(sprintf("Erreur DELETE (status: %d)", status_code(response)), "ERROR")
         return(FALSE)
      }
      
      log_message(sprintf("✓ Recommandation %d supprimée", id), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# INDICATEURS PHYSIQUES - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer tous les indicateurs physiques
#' @export
get_indicateurs_physiques <- function(projet_id = NULL) {
   tryCatch({
      url <- sprintf("%s/rest/v1/indicateurs_physiques", SUPABASE_CONFIG$url)
      if (!is.null(projet_id)) url <- sprintf("%s?projet_id=eq.%s", url, projet_id)
      
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      log_message(sprintf("✓ %d indicateurs récupérés", nrow(df)), "DEBUG", FALSE)
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer un indicateur par ID
#' @export
get_indicateur_by_id <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/indicateurs_physiques?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer un indicateur
#' @export
insert_indicateur_physique <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/indicateurs_physiques", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) return(NULL)
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Indicateur inséré"), "INFO")
      return(result[[1]]$id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour un indicateur
#' @export
update_indicateur_physique <- function(id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/indicateurs_physiques?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) return(FALSE)
      log_message(sprintf("✓ Indicateur mis à jour"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer un indicateur
#' @export
delete_indicateur_physique <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/indicateurs_physiques?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) return(FALSE)
      log_message(sprintf("✓ Indicateur supprimé"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# EXÉCUTION FINANCIÈRE - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer l'exécution financière
#' @export
get_execution_financiere <- function(projet_id = NULL) {
   tryCatch({
      url <- sprintf("%s/rest/v1/execution_financiere", SUPABASE_CONFIG$url)
      if (!is.null(projet_id)) url <- sprintf("%s?projet_id=eq.%s", url, projet_id)
      
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer une exécution financière par ID
#' @export
get_execution_financiere_by_id <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/execution_financiere?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer une exécution financière
#' @export
insert_execution_financiere <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/execution_financiere", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) return(NULL)
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Exécution financière insérée"), "INFO")
      return(result[[1]]$id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour une exécution financière
#' @export
update_execution_financiere <- function(id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/execution_financiere?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) return(FALSE)
      log_message(sprintf("✓ Exécution financière mise à jour"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer une exécution financière
#' @export
delete_execution_financiere <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/execution_financiere?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) return(FALSE)
      log_message(sprintf("✓ Exécution financière supprimée"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# DIFFICULTÉS/MESURES - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer les difficultés/mesures
#' @export
get_difficultes_mesures <- function(projet_id = NULL) {
   tryCatch({
      url <- sprintf("%s/rest/v1/difficultes_mesures", SUPABASE_CONFIG$url)
      if (!is.null(projet_id)) url <- sprintf("%s?projet_id=eq.%s", url, projet_id)
      
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer une difficulté/mesure par ID
#' @export
get_difficulte_mesure_by_id <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/difficultes_mesures?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer une difficulté/mesure
#' @export
insert_difficulte_mesure <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/difficultes_mesures", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) return(NULL)
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Difficulté/mesure insérée"), "INFO")
      return(result[[1]]$id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour une difficulté/mesure
#' @export
update_difficulte_mesure <- function(id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/difficultes_mesures?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) return(FALSE)
      log_message(sprintf("✓ Difficulté/mesure mise à jour"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer une difficulté/mesure
#' @export
delete_difficulte_mesure <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/difficultes_mesures?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) return(FALSE)
      log_message(sprintf("✓ Difficulté/mesure supprimée"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# TAUX D'EXÉCUTION - CRUD COMPLET
# ═══════════════════════════════════════════════════════════════════════════

#' Récupérer les taux d'exécution
#' @export
get_taux_execution <- function(projet_id = NULL) {
   tryCatch({
      url <- sprintf("%s/rest/v1/taux_execution_globaux", SUPABASE_CONFIG$url)
      if (!is.null(projet_id)) url <- sprintf("%s?projet_id=eq.%s", url, projet_id)
      
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      if (status_code(response) != 200) return(data.frame())
      
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(data.frame())
      
      df <- as.data.frame(do.call(rbind, lapply(result, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
      return(df)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(data.frame())
   })
}

#' Récupérer un taux d'exécution par ID
#' @export
get_taux_execution_by_id <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/taux_execution_globaux?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- GET(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 200) return(NULL)
      result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(result) == 0) return(NULL)
      return(result[[1]])
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Insérer un taux d'exécution
#' @export
insert_taux_execution <- function(data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/taux_execution_globaux", SUPABASE_CONFIG$url)
      response <- POST(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                       body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 201) return(NULL)
      result <- fromJSON(content(response, as = "text"))
      log_message(sprintf("✓ Taux d'exécution inséré"), "INFO")
      return(result[[1]]$id)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(NULL)
   })
}

#' Mettre à jour un taux d'exécution
#' @export
update_taux_execution <- function(id, data) {
   tryCatch({
      url <- sprintf("%s/rest/v1/taux_execution_globaux?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- PATCH(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"),
                        body = toJSON(data, auto_unbox = TRUE))
      
      if (status_code(response) != 200) return(FALSE)
      log_message(sprintf("✓ Taux d'exécution mis à jour"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

#' Supprimer un taux d'exécution
#' @export
delete_taux_execution <- function(id) {
   tryCatch({
      url <- sprintf("%s/rest/v1/taux_execution_globaux?id=eq.%d", SUPABASE_CONFIG$url, id)
      response <- DELETE(url, add_headers(apikey = SUPABASE_CONFIG$anon_key, `Content-Type` = "application/json"))
      
      if (status_code(response) != 204) return(FALSE)
      log_message(sprintf("✓ Taux d'exécution supprimé"), "INFO")
      return(TRUE)
      
   }, error = function(e) {
      log_message(sprintf("Erreur : %s", e$message), "ERROR")
      return(FALSE)
   })
}

# ═══════════════════════════════════════════════════════════════════════════
# ═══════════════════════════════════════════════════════════════════════════