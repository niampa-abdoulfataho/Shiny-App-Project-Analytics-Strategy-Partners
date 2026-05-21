# ============================================================
# modules/db_saisie.R — Fonctions BD pour la saisie
# Toutes les opérations INSERT/UPDATE liées à une soumission
# ============================================================

# ── Vérifier si une soumission brouillon existe déjà ─────────
db_get_soumission_brouillon <- function(session_id, utilisateur_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   dbGetQuery(con,
              "SELECT * FROM soumissions
     WHERE session_id = $1 AND utilisateur_id = $2
     AND statut IN ('brouillon','modifiable')
     ORDER BY version DESC LIMIT 1",
              params = list(session_id, utilisateur_id))
}

# ── Créer ou récupérer une soumission ────────────────────────
db_get_or_create_soumission <- function(session_id, utilisateur_id) {
   existing <- db_get_soumission_brouillon(session_id, utilisateur_id)
   if (nrow(existing) > 0) return(existing$id[1])
   db_create_soumission(session_id, utilisateur_id)
}

# ── Lire les données d'une soumission (toutes les tables) ────
db_get_soumission_complete <- function(soumission_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   list(
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

# ════════════════════════════════════════════════════════════
# TRANSACTION UNIQUE : soumettre toutes les tables en une fois
# ════════════════════════════════════════════════════════════
db_soumettre <- function(soumission_id, data) {
   con <- get_con(); on.exit(dbDisconnect(con))
   
   tryCatch({
      dbBegin(con)
      
      # ── 1. Supprimer les anciennes données (upsert simplifié) ──
      dbExecute(con, "DELETE FROM projets WHERE soumission_id = $1",
                params = list(soumission_id))
      
      # ── 2. Insérer le projet ───────────────────────────────────
      p <- data$projet
      projet_id <- dbGetQuery(con,
                "INSERT INTO projets
       (soumission_id, annee_cible, date_remplissage, intitule_projet,
        ministere_tutelle, siege_projet, objectif_global, resultats_attendus,
        secteurs_activites, zone_intervention, annee_demarrage, annee_fin,
        ref_arrete_creation, responsable_nom, responsable_tel, responsable_email)
       VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16)
       RETURNING id",
                params = list(
                   soumission_id, p$annee_cible, p$date_remplissage, p$intitule_projet,
                   p$ministere_tutelle, p$siege_projet, p$objectif_global, p$resultats_attendus,
                   p$secteurs_activites, p$zone_intervention, p$annee_demarrage, p$annee_fin,
                   p$ref_arrete_creation, p$responsable_nom, p$responsable_tel, p$responsable_email
                )
      )$id
      
      # ── 3. Insérer exécution financière (n lignes) ─────────────
      if (nrow(data$finance) > 0) {
         for (i in seq_len(nrow(data$finance))) {
            f <- data$finance[i,]
            dbExecute(con,
                      "INSERT INTO execution_financiere
           (projet_id, source_financement, mode_financement, cout_total,
            cumul_demarrage_ae, cumul_demarrage_cp, loi_finance_ae, loi_finance_cp,
            programme_revise_ae, programme_revise_cp, depense_ae, depense_cp,
            depense_demarrage_ae, depense_demarrage_cp)
           VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14)",
                      params = list(
                         projet_id, f$source_financement, f$mode_financement, f$cout_total,
                         f$cumul_demarrage_ae, f$cumul_demarrage_cp, f$loi_finance_ae, f$loi_finance_cp,
                         f$programme_revise_ae, f$programme_revise_cp, f$depense_ae, f$depense_cp,
                         f$depense_demarrage_ae, f$depense_demarrage_cp
                      )
            )
         }
      }
      
      # ── 4. Insérer exécution physique (n lignes) ───────────────
      if (nrow(data$physique) > 0) {
         for (i in seq_len(nrow(data$physique))) {
            ph <- data$physique[i,]
            dbExecute(con,
                      "INSERT INTO execution_physique
           (projet_id, indicateur, unite, prevision, realisation,
            localite, preuves, taux_annuel, taux_global)
           VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)",
                      params = list(
                         projet_id, ph$indicateur, ph$unite, ph$prevision, ph$realisation,
                         ph$localite, ph$preuves, ph$taux_annuel, ph$taux_global
                      )
            )
         }
      }
      
      # ── 5. Insérer difficultés (n lignes) ─────────────────────
      if (nrow(data$difficultes) > 0) {
         for (i in seq_len(nrow(data$difficultes))) {
            d <- data$difficultes[i,]
            dbExecute(con,
                      "INSERT INTO difficultes (projet_id, difficulte, mesure)
           VALUES ($1,$2,$3)",
                      params = list(projet_id, d$difficulte, d$mesure)
            )
         }
      }
      
      # ── 6. Insérer recommandations (n lignes) ──────────────────
      if (nrow(data$recommandations) > 0) {
         for (i in seq_len(nrow(data$recommandations))) {
            r <- data$recommandations[i,]
            dbExecute(con,
                      "INSERT INTO recommandations
           (projet_id, recommandation, echeance, responsable, perspective)
           VALUES ($1,$2,$3,$4,$5)",
                      params = list(
                         projet_id, r$recommandation, r$echeance, r$responsable, r$perspective
                      )
            )
         }
      }
      
      # ── 7. Mettre à jour le statut de la soumission ────────────
      dbExecute(con,
                "UPDATE soumissions SET statut = 'soumis', updated_at = NOW() WHERE id = $1",
                params = list(soumission_id)
      )
      
      dbCommit(con)
      list(success = TRUE, message = "Soumission enregistrée avec succès.")
      
   }, error = function(e) {
      dbRollback(con)
      list(success = FALSE, message = paste0("Erreur transaction : ", e$message))
   })
}

# ── Nouvelle version d'une soumission ─────────────────────────
db_nouvelle_version <- function(ancienne_soumission_id, utilisateur_id) {
   con <- get_con(); on.exit(dbDisconnect(con))
   
   # Récupérer la version actuelle
   ancienne <- dbGetQuery(con,
                          "SELECT * FROM soumissions WHERE id = $1",
                          params = list(ancienne_soumission_id))
   
   # Créer une nouvelle soumission avec version+1
   nouvelle_id <- dbGetQuery(con,
                             "INSERT INTO soumissions (session_id, utilisateur_id, version, statut)
     VALUES ($1, $2, $3, 'brouillon') RETURNING id",
                             params = list(
                                ancienne$session_id,
                                utilisateur_id,
                                ancienne$version + 1
                             ))$id
   
   nouvelle_id
}
