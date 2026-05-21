# ============================================================
# global.R — Chargement packages, connexion BD, utilitaires
# ============================================================

library(shiny)
library(shinydashboard)
library(shinymanager)
library(shinyjs)
library(DBI)
library(RPostgres)
library(DT)
library(lubridate)
library(jsonlite)
library(blastula)
library(bcrypt)

# ── Paramètres de connexion PostgreSQL ──────────────────────
DB_PARAMS <- list(
  host     = Sys.getenv("DB_HOST",     "localhost"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME",     "suivioprojet"),
  user     = Sys.getenv("DB_USER",     "postgres"),
  password = Sys.getenv("DB_PASSWORD", "0023421Postgres")
)

con_params <- list(
  host     = "localhost",
  port     = 5433,
  dbname   = "suiviprojet",
  user     = "postgres",
  password = "0023421Postgres"
)
# ── Connexion BD ─────────────────────────────────────────────
get_con <- function() {
  do.call(dbConnect, c(list(drv = RPostgres::Postgres()), con_params))
}

# Test de connexion au démarrage
tryCatch({
  con_test <- get_con()
  dbDisconnect(con_test)
  message("✅ Connexion PostgreSQL OK")
}, error = function(e) {
  stop("❌ Connexion PostgreSQL échouée : ", e$message)
})

# ── Opérateur null-coalescing ─────────────────────────────────
`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1 && is.na(a)) || a == "") b else a
}

# ════════════════════════════════════════════════════════════
# FONCTIONS UTILITAIRES BD
# ════════════════════════════════════════════════════════════

# ── Utilisateurs ─────────────────────────────────────────────
db_get_user_by_email <- function(email) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM utilisateurs WHERE email = $1 AND actif = TRUE",
    params = list(email))
}

db_get_user_by_id <- function(id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM utilisateurs WHERE id = $1",
    params = list(id))
}

db_get_all_ong <- function() {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM utilisateurs WHERE role = 'ong' AND actif = TRUE ORDER BY nom_structure")
}

db_create_user <- function(nom_structure, email, mot_de_passe, role = "ong") {
  hash <- bcrypt::hashpw(mot_de_passe)
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO utilisateurs (nom_structure, email, mot_de_passe, role)
     VALUES ($1, $2, $3, $4)",
    params = list(nom_structure, email, hash, role))
}

db_verify_password <- function(email, mot_de_passe) {
  user <- db_get_user_by_email(email)
  if (nrow(user) == 0) return(NULL)
  if (bcrypt::checkpw(mot_de_passe, user$mot_de_passe)) user else NULL
}

# ── Sessions ─────────────────────────────────────────────────
db_get_all_sessions <- function() {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT s.*, u.nom_structure as createur
     FROM sessions s
     LEFT JOIN utilisateurs u ON s.created_by = u.id
     ORDER BY s.created_at DESC")
}

db_get_sessions_ouvertes <- function() {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM sessions WHERE statut = 'ouverte' ORDER BY date_ouverture DESC")
}

db_create_session <- function(label, trimestre, date_ouverture, date_fin,
                               description, created_by) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "INSERT INTO sessions (label, trimestre, date_ouverture, date_fin,
                           description, created_by)
     VALUES ($1, $2, $3, $4, $5, $6) RETURNING id",
    params = list(label, trimestre, date_ouverture, date_fin,
                  description, created_by))$id
}

db_update_session_statut <- function(session_id, statut) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE sessions SET statut = $1 WHERE id = $2",
    params = list(statut, session_id))
}

# ── Soumissions ───────────────────────────────────────────────
db_get_soumissions_user <- function(utilisateur_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT s.*, se.label as session_label, se.trimestre, se.statut as session_statut
     FROM soumissions s
     JOIN sessions se ON s.session_id = se.id
     WHERE s.utilisateur_id = $1
     ORDER BY s.created_at DESC",
    params = list(utilisateur_id))
}

db_get_soumissions_session <- function(session_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT s.*, u.nom_structure
     FROM soumissions s
     JOIN utilisateurs u ON s.utilisateur_id = u.id
     WHERE s.session_id = $1
     ORDER BY s.created_at DESC",
    params = list(session_id))
}

db_get_suivi_session <- function(session_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "WITH latest AS (
       SELECT DISTINCT ON (utilisateur_id) *
       FROM soumissions
       WHERE session_id = $1
       ORDER BY utilisateur_id, version DESC, updated_at DESC, id DESC
     ),
     projets_one AS (
       SELECT DISTINCT ON (soumission_id) id, soumission_id
       FROM projets
       ORDER BY soumission_id, updated_at DESC, id DESC
     ),
     finance AS (
       SELECT projet_id, COUNT(*) AS n FROM execution_financiere GROUP BY projet_id
     ),
     physique AS (
       SELECT projet_id, COUNT(*) AS n FROM execution_physique GROUP BY projet_id
     ),
     diff AS (
       SELECT projet_id, COUNT(*) AS n FROM difficultes GROUP BY projet_id
     ),
     reco AS (
       SELECT projet_id, COUNT(*) AS n FROM recommandations GROUP BY projet_id
     ),
     sections AS (
       SELECT l.id AS soumission_id,
              ((CASE WHEN p.id IS NOT NULL THEN 1 ELSE 0 END) +
               (CASE WHEN COALESCE(f.n, 0) > 0 THEN 1 ELSE 0 END) +
               (CASE WHEN COALESCE(ph.n, 0) > 0 THEN 1 ELSE 0 END) +
               (CASE WHEN COALESCE(d.n, 0) > 0 THEN 1 ELSE 0 END) +
               (CASE WHEN COALESCE(r.n, 0) > 0 THEN 1 ELSE 0 END)) AS sections_remplies
       FROM latest l
       LEFT JOIN projets_one p ON p.soumission_id = l.id
       LEFT JOIN finance f ON f.projet_id = p.id
       LEFT JOIN physique ph ON ph.projet_id = p.id
       LEFT JOIN diff d ON d.projet_id = p.id
       LEFT JOIN reco r ON r.projet_id = p.id
     ),
     demandes_pending AS (
       SELECT soumission_id, COUNT(*) AS n
       FROM demandes
       WHERE statut = 'en_attente'
       GROUP BY soumission_id
     )
     SELECT u.id AS utilisateur_id, u.nom_structure, u.email,
            l.id AS soumission_id, l.version, l.statut,
            l.created_at, l.updated_at,
            COALESCE(sections.sections_remplies, 0) AS sections_remplies,
            COALESCE(demandes_pending.n, 0) AS demandes_en_attente
     FROM utilisateurs u
     LEFT JOIN latest l ON l.utilisateur_id = u.id
     LEFT JOIN sections ON sections.soumission_id = l.id
     LEFT JOIN demandes_pending ON demandes_pending.soumission_id = l.id
     WHERE u.role = 'ong' AND u.actif = TRUE
     ORDER BY u.nom_structure",
    params = list(session_id))
}

db_get_activite_session <- function(session_id, limit = 12) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT *
     FROM (
       SELECT s.updated_at AS event_time,
              u.nom_structure AS structure,
              CASE WHEN s.statut = 'soumis'
                   THEN 'Soumission v' || s.version || ' enregistree'
                   ELSE 'Brouillon mis a jour'
              END AS message,
              CASE WHEN s.statut = 'soumis' THEN 'soumis' ELSE 'brouillon' END AS type
       FROM soumissions s
       JOIN utilisateurs u ON u.id = s.utilisateur_id
       WHERE s.session_id = $1

       UNION ALL

       SELECT d.created_at AS event_time,
              d.nom_structure AS structure,
              'Demande de modification envoyee' AS message,
              'demande' AS type
       FROM demandes d
       JOIN soumissions s ON s.id = d.soumission_id
       WHERE s.session_id = $1

       UNION ALL

       SELECT se.created_at AS event_time,
              'Session ouverte' AS structure,
              'Notifications envoyees aux ONG actives' AS message,
              'session' AS type
       FROM sessions se
       WHERE se.id = $1
     ) a
     ORDER BY event_time DESC
     LIMIT $2",
    params = list(session_id, limit))
}

db_get_data_explorer <- function(session_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  session_where <- if (is.null(session_id) || is.na(session_id) || session_id == 0) "" else "WHERE se.id = $1"
  params <- if (identical(session_where, "")) list() else list(session_id)

  meta_join <- "
    JOIN soumissions s ON s.id = p.soumission_id
    JOIN sessions se ON se.id = s.session_id
    JOIN utilisateurs u ON u.id = s.utilisateur_id
  "
  meta_cols <- "
    se.label AS session_label, se.trimestre,
    u.nom_structure, u.email,
    s.version, s.statut AS statut_soumission,
    s.created_at AS soumission_created_at,
    s.updated_at AS soumission_updated_at
  "

  projets <- dbGetQuery(con, paste0(
    "SELECT ", meta_cols, ",
            p.annee_cible, p.date_remplissage, p.intitule_projet,
            p.ministere_tutelle, p.siege_projet, p.objectif_global,
            p.resultats_attendus, p.secteurs_activites, p.zone_intervention,
            p.annee_demarrage, p.annee_fin, p.ref_arrete_creation,
            p.responsable_nom, p.responsable_tel, p.responsable_email
     FROM projets p ", meta_join, " ", session_where, "
     ORDER BY se.date_ouverture DESC, u.nom_structure, p.id"
  ), params = params)

  finance <- dbGetQuery(con, paste0(
    "SELECT ", meta_cols, ",
            p.intitule_projet, ef.source_financement, ef.mode_financement,
            ef.cout_total, ef.cumul_demarrage_ae, ef.cumul_demarrage_cp,
            ef.loi_finance_ae, ef.loi_finance_cp,
            ef.programme_revise_ae, ef.programme_revise_cp,
            ef.depense_ae, ef.depense_cp,
            ef.depense_demarrage_ae, ef.depense_demarrage_cp
     FROM execution_financiere ef
     JOIN projets p ON p.id = ef.projet_id ", meta_join, " ", session_where, "
     ORDER BY se.date_ouverture DESC, u.nom_structure, ef.id"
  ), params = params)

  physique <- dbGetQuery(con, paste0(
    "SELECT ", meta_cols, ",
            p.intitule_projet, ep.indicateur, ep.unite, ep.prevision,
            ep.realisation, ep.localite, ep.preuves,
            ep.taux_annuel, ep.taux_global
     FROM execution_physique ep
     JOIN projets p ON p.id = ep.projet_id ", meta_join, " ", session_where, "
     ORDER BY se.date_ouverture DESC, u.nom_structure, ep.id"
  ), params = params)

  difficultes <- dbGetQuery(con, paste0(
    "SELECT ", meta_cols, ",
            p.intitule_projet, d.difficulte, d.mesure
     FROM difficultes d
     JOIN projets p ON p.id = d.projet_id ", meta_join, " ", session_where, "
     ORDER BY se.date_ouverture DESC, u.nom_structure, d.id"
  ), params = params)

  recommandations <- dbGetQuery(con, paste0(
    "SELECT ", meta_cols, ",
            p.intitule_projet, r.recommandation, r.echeance,
            r.responsable, r.perspective
     FROM recommandations r
     JOIN projets p ON p.id = r.projet_id ", meta_join, " ", session_where, "
     ORDER BY se.date_ouverture DESC, u.nom_structure, r.id"
  ), params = params)

  list(
    projets = projets,
    finance = finance,
    physique = physique,
    difficultes = difficultes,
    recommandations = recommandations
  )
}

db_create_soumission <- function(session_id, utilisateur_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "INSERT INTO soumissions (session_id, utilisateur_id, statut)
     VALUES ($1, $2, 'brouillon') RETURNING id",
    params = list(session_id, utilisateur_id))$id
}

db_update_soumission_statut <- function(soumission_id, statut) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE soumissions SET statut = $1 WHERE id = $2",
    params = list(statut, soumission_id))
}

db_get_soumission_by_id <- function(soumission_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT s.*, se.label as session_label, se.statut as session_statut,
            u.nom_structure, u.email
     FROM soumissions s
     JOIN sessions se ON s.session_id = se.id
     JOIN utilisateurs u ON s.utilisateur_id = u.id
     WHERE s.id = $1",
    params = list(soumission_id))
}

# ── Demandes de modification ──────────────────────────────────
db_create_demande <- function(soumission_id, utilisateur_id, nom_structure,
                               email, date_soumission_originale, objet, corps) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO demandes
     (soumission_id, utilisateur_id, nom_structure, email,
      date_soumission_originale, objet, corps)
     VALUES ($1, $2, $3, $4, $5, $6, $7)",
    params = list(soumission_id, utilisateur_id, nom_structure,
                  email, date_soumission_originale, objet, corps))
}

db_get_demandes_en_attente <- function() {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT d.*, u.nom_structure as ong_nom, s.session_id,
            se.label as session_label
     FROM demandes d
     JOIN utilisateurs u ON d.utilisateur_id = u.id
     JOIN soumissions s ON d.soumission_id = s.id
     JOIN sessions se ON s.session_id = se.id
     WHERE d.statut = 'en_attente'
     ORDER BY d.created_at DESC")
}

db_traiter_demande <- function(demande_id, statut, traite_par) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE demandes
     SET statut = $1, traite_par = $2, traite_le = NOW()
     WHERE id = $3",
    params = list(statut, traite_par, demande_id))
}

# ── Notifications ─────────────────────────────────────────────
db_create_notification <- function(utilisateur_id, type, message, lien = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO notifications (utilisateur_id, type, message, lien)
     VALUES ($1, $2, $3, $4)",
    params = list(utilisateur_id, type, message, lien))
}

db_get_notifications_user <- function(utilisateur_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM notifications
     WHERE utilisateur_id = $1
     ORDER BY created_at DESC
     LIMIT 50",
    params = list(utilisateur_id))
}

db_count_notif_non_lues <- function(utilisateur_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT COUNT(*) as n FROM notifications
     WHERE utilisateur_id = $1 AND lu = FALSE",
    params = list(utilisateur_id))$n
}

db_marquer_notif_lues <- function(utilisateur_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE notifications SET lu = TRUE WHERE utilisateur_id = $1",
    params = list(utilisateur_id))
}

# ── Versions / Diff ───────────────────────────────────────────
db_save_diff <- function(soumission_id, version, diff_list, created_by) {
  diff_json <- toJSON(diff_list, auto_unbox = TRUE)
  con <- get_con(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO versions_soumissions (soumission_id, version, diff_json, created_by)
     VALUES ($1, $2, $3::jsonb, $4)",
    params = list(soumission_id, version, diff_json, created_by))
}

db_get_versions <- function(soumission_id) {
  con <- get_con(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT v.*, u.nom_structure as modifie_par
     FROM versions_soumissions v
     LEFT JOIN utilisateurs u ON v.created_by = u.id
     WHERE v.soumission_id = $1
     ORDER BY v.version DESC",
    params = list(soumission_id))
}

# ── Calcul du diff entre deux listes ─────────────────────────
compute_diff <- function(avant, apres) {
  champs <- union(names(avant), names(apres))
  diff   <- list()
  for (champ in champs) {
    v_avant <- avant[[champ]]
    v_apres <- apres[[champ]]
    if (!identical(v_avant, v_apres)) {
      diff[[champ]] <- list(avant = v_avant, apres = v_apres)
    }
  }
  diff
}

# ════════════════════════════════════════════════════════════
# CONFIGURATION MAIL (blastula)
# ════════════════════════════════════════════════════════════
MAIL_FROM    <- Sys.getenv("MAIL_FROM",    "noreply@ministere.bf")
MAIL_SMTP    <- Sys.getenv("MAIL_SMTP",    "smtp.ministere.bf")
MAIL_PORT    <- as.integer(Sys.getenv("MAIL_PORT", "587"))
MAIL_USER    <- Sys.getenv("MAIL_USER",    "")
MAIL_PASSWORD <- Sys.getenv("MAIL_PASSWORD", "")

send_mail <- function(to, subject, body_html) {
  tryCatch({
    email <- compose_email(body = md(body_html))
    smtp_send(
      email,
      to      = to,
      from    = MAIL_FROM,
      subject = subject,
      credentials = creds_anonymous(
        host = MAIL_SMTP,
        port = MAIL_PORT
      )
    )
    TRUE
  }, error = function(e) {
    warning("Erreur envoi mail : ", e$message)
    FALSE
  })
}

# Templates mail
mail_session_ouverte <- function(to, nom_structure, session_label, date_fin) {
  send_mail(
    to      = to,
    subject = paste0("📋 Nouvelle enquête ouverte : ", session_label),
    body_html = paste0(
      "Bonjour **", nom_structure, "**,\n\n",
      "Une nouvelle session d'enquête est ouverte : **", session_label, "**.\n\n",
      "Vous avez jusqu'au **", format(date_fin, "%d/%m/%Y"),
      "** pour soumettre votre formulaire.\n\n",
      "Connectez-vous sur l'application pour commencer la saisie."
    )
  )
}

mail_relance_enquete <- function(to, nom_structure, session_label, date_fin, cible = "absent") {
  detail <- if (identical(cible, "brouillon")) {
    "Votre formulaire est encore en brouillon et n'a pas été soumis."
  } else {
    "Nous n'avons pas encore détecté de début de saisie pour cette session."
  }

  send_mail(
    to      = to,
    subject = paste0("Rappel - Enquête en cours : ", session_label),
    body_html = paste0(
      "Bonjour **", nom_structure, "**,\n\n",
      detail, "\n\n",
      "Session : **", session_label, "**\n\n",
      "Date limite : **", format(date_fin, "%d/%m/%Y"), "**\n\n",
      "Merci de vous connecter à l'application pour finaliser votre saisie."
    )
  )
}

mail_correction_soumission <- function(to, nom_structure, session_label, objet, corps) {
  send_mail(
    to      = to,
    subject = paste0("Correction demandee - ", session_label),
    body_html = paste0(
      "Bonjour **", nom_structure, "**,\n\n",
      "L'administration a releve des elements a corriger dans votre soumission.\n\n",
      "**Objet :** ", objet, "\n\n",
      corps, "\n\n",
      "Merci de vous connecter a l'application pour apporter les corrections demandees."
    )
  )
}

mail_demande_recue <- function(to_admin, nom_ong, objet) {
  send_mail(
    to      = to_admin,
    subject = paste0("✉️ Demande de modification — ", nom_ong),
    body_html = paste0(
      "Une demande de modification a été soumise par **", nom_ong, "**.\n\n",
      "**Objet :** ", objet, "\n\n",
      "Connectez-vous sur l'application pour traiter cette demande."
    )
  )
}

mail_demande_traitee <- function(to, nom_structure, statut, objet) {
  emoji  <- if (statut == "accepte") "✅" else "❌"
  libelle <- if (statut == "accepte") "acceptée" else "refusée"
  send_mail(
    to      = to,
    subject = paste0(emoji, " Votre demande de modification a été ", libelle),
    body_html = paste0(
      "Bonjour **", nom_structure, "**,\n\n",
      "Votre demande de modification (**", objet, "**) a été **", libelle, "**.\n\n",
      if (statut == "accepte")
        "Vous pouvez maintenant vous connecter et modifier votre soumission."
      else
        "Pour toute question, contactez l'administration."
    )
  )
}
