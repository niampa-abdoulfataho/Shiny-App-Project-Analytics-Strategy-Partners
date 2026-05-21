# ============================================================
# modules/db_historique.R
# Fonctions BD spécifiques à l'onglet Historique
# ============================================================

# ── Sessions disponibles pour une ONG ────────────────────────
db_get_sessions_ong <- function(utilisateur_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   dbGetQuery(con,
              "SELECT DISTINCT se.id, se.label, se.trimestre,
            se.date_ouverture, se.date_fin, se.statut
     FROM sessions se
     JOIN soumissions s ON s.session_id = se.id
     WHERE s.utilisateur_id = $1
     ORDER BY se.date_ouverture DESC",
              params = list(utilisateur_id))
}

# ── Soumissions d'une ONG pour une session ────────────────────
db_get_soumissions_ong_session <- function(utilisateur_id, session_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   dbGetQuery(con,
              "SELECT s.id, s.version, s.statut, s.created_at, s.updated_at
     FROM soumissions s
     WHERE s.utilisateur_id = $1 AND s.session_id = $2
     ORDER BY s.version DESC",
              params = list(utilisateur_id, session_id))
}

# ── Vérifier si une demande en attente existe déjà ───────────
db_has_demande_en_attente <- function(soumission_id, utilisateur_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   res <- dbGetQuery(con,
                     "SELECT COUNT(*) as n FROM demandes
     WHERE soumission_id = $1 AND utilisateur_id = $2
     AND statut = 'en_attente'",
                     params = list(soumission_id, utilisateur_id))
   res$n > 0
}

# ── Données complètes d'une soumission pour affichage ─────────
db_get_soumission_affichage <- function(soumission_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   list(
      soumission = dbGetQuery(con,
                              "SELECT s.*, se.label as session_label, se.statut as session_statut,
              u.nom_structure, u.email
       FROM soumissions s
       JOIN sessions se ON s.session_id = se.id
       JOIN utilisateurs u ON s.utilisateur_id = u.id
       WHERE s.id = $1",
                              params = list(soumission_id)),
      projet = dbGetQuery(con,
                          "SELECT * FROM projets WHERE soumission_id = $1 LIMIT 1",
                          params = list(soumission_id)),
      finance = dbGetQuery(con,
                           "SELECT ef.*
                            FROM execution_financiere ef
                            JOIN projets p ON p.id = ef.projet_id
                            WHERE p.soumission_id = $1
                            ORDER BY ef.id",
                           params = list(soumission_id)),
      physique = dbGetQuery(con,
                            "SELECT ep.*
                             FROM execution_physique ep
                             JOIN projets p ON p.id = ep.projet_id
                             WHERE p.soumission_id = $1
                             ORDER BY ep.id",
                            params = list(soumission_id)),
      difficultes = dbGetQuery(con,
                               "SELECT d.*
                                FROM difficultes d
                                JOIN projets p ON p.id = d.projet_id
                                WHERE p.soumission_id = $1
                                ORDER BY d.id",
                               params = list(soumission_id)),
      recommandations = dbGetQuery(con,
                                   "SELECT r.*
                                    FROM recommandations r
                                    JOIN projets p ON p.id = r.projet_id
                                    WHERE p.soumission_id = $1
                                    ORDER BY r.id",
                                   params = list(soumission_id))
   )
}

# ── Snapshot JSON d'une soumission (pour diff) ────────────────
snapshot_soumission <- function(data) {
   toJSON(list(
      projet          = as.list(data$projet),
      finance         = lapply(seq_len(nrow(data$finance)),
                               function(i) as.list(data$finance[i,])),
      physique        = lapply(seq_len(nrow(data$physique)),
                               function(i) as.list(data$physique[i,])),
      difficultes     = lapply(seq_len(nrow(data$difficultes)),
                               function(i) as.list(data$difficultes[i,])),
      recommandations = lapply(seq_len(nrow(data$recommandations)),
                               function(i) as.list(data$recommandations[i,]))
   ), auto_unbox = TRUE, na = "null")
}
