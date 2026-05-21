-- ============================================================
-- GestioProjets — Initialisation de la base de données
-- ============================================================

-- Extension pour UUID (optionnel)
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- ============================================================
-- 1. UTILISATEURS
-- ============================================================
CREATE TABLE IF NOT EXISTS utilisateurs (
  id              SERIAL PRIMARY KEY,
  nom_structure   TEXT NOT NULL,
  email           TEXT NOT NULL UNIQUE,
  mot_de_passe    TEXT NOT NULL,          -- stocké hashé (bcrypt)
  role            TEXT NOT NULL DEFAULT 'ong'
                  CHECK (role IN ('ong', 'admin')),
  actif           BOOLEAN NOT NULL DEFAULT TRUE,
  created_at      TIMESTAMP DEFAULT NOW(),
  updated_at      TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 2. SESSIONS (enquêtes trimestrielles)
-- ============================================================
CREATE TABLE IF NOT EXISTS sessions (
  id              SERIAL PRIMARY KEY,
  label           TEXT NOT NULL UNIQUE,
  trimestre       TEXT NOT NULL,          -- ex: "T1 2025"
  date_ouverture  DATE NOT NULL,
  date_fin        DATE NOT NULL,
  description     TEXT,
  statut          TEXT NOT NULL DEFAULT 'fermee'
                  CHECK (statut IN ('ouverte', 'fermee')),
  created_by      INTEGER REFERENCES utilisateurs(id),
  created_at      TIMESTAMP DEFAULT NOW(),
  updated_at      TIMESTAMP DEFAULT NOW(),
  CONSTRAINT chk_dates_session CHECK (date_fin >= date_ouverture)
);

-- ============================================================
-- 3. SOUMISSIONS
-- ============================================================
CREATE TABLE IF NOT EXISTS soumissions (
  id              SERIAL PRIMARY KEY,
  session_id      INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  utilisateur_id  INTEGER NOT NULL REFERENCES utilisateurs(id) ON DELETE CASCADE,
  version         INTEGER NOT NULL DEFAULT 1,
  statut          TEXT NOT NULL DEFAULT 'brouillon'
                  CHECK (statut IN ('brouillon', 'soumis', 'modifiable')),
  created_at      TIMESTAMP DEFAULT NOW(),
  updated_at      TIMESTAMP DEFAULT NOW(),
  UNIQUE (session_id, utilisateur_id, version)
);

-- ============================================================
-- 4. PROJETS
-- ============================================================
CREATE TABLE IF NOT EXISTS projets (
  id                    SERIAL PRIMARY KEY,
  soumission_id         INTEGER NOT NULL REFERENCES soumissions(id) ON DELETE CASCADE,
  annee_cible           INTEGER NOT NULL,
  date_remplissage      DATE NOT NULL DEFAULT CURRENT_DATE,
  intitule_projet       TEXT NOT NULL,
  ministere_tutelle     TEXT NOT NULL,
  siege_projet          TEXT,
  objectif_global       TEXT,
  resultats_attendus    TEXT,
  secteurs_activites    TEXT,
  zone_intervention     TEXT,
  annee_demarrage       INTEGER,
  annee_fin             INTEGER,
  ref_arrete_creation   TEXT,
  responsable_nom       TEXT,
  responsable_tel       TEXT,
  responsable_email     TEXT,
  created_at            TIMESTAMP DEFAULT NOW(),
  updated_at            TIMESTAMP DEFAULT NOW(),
  CONSTRAINT chk_annees_coherentes CHECK (
    annee_fin IS NULL OR annee_demarrage IS NULL OR annee_fin >= annee_demarrage
  ),
  CONSTRAINT chk_email_format CHECK (
    responsable_email IS NULL OR
    responsable_email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'
  )
);

-- ============================================================
-- 5. EXÉCUTION FINANCIÈRE
-- ============================================================
CREATE TABLE IF NOT EXISTS execution_financiere (
  id                      SERIAL PRIMARY KEY,
  projet_id               INTEGER NOT NULL REFERENCES projets(id) ON DELETE CASCADE,
  source_financement      TEXT NOT NULL,
  mode_financement        TEXT,
  cout_total              NUMERIC(15,2),
  cumul_demarrage_ae      NUMERIC(15,2),
  cumul_demarrage_cp      NUMERIC(15,2),
  loi_finance_ae          NUMERIC(15,2),
  loi_finance_cp          NUMERIC(15,2),
  programme_revise_ae     NUMERIC(15,2),
  programme_revise_cp     NUMERIC(15,2),
  depense_ae              NUMERIC(15,2),
  depense_cp              NUMERIC(15,2),
  depense_demarrage_ae    NUMERIC(15,2),
  depense_demarrage_cp    NUMERIC(15,2),
  created_at              TIMESTAMP DEFAULT NOW(),
  updated_at              TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 6. EXÉCUTION PHYSIQUE (indicateurs)
-- ============================================================
CREATE TABLE IF NOT EXISTS execution_physique (
  id                SERIAL PRIMARY KEY,
  projet_id         INTEGER NOT NULL REFERENCES projets(id) ON DELETE CASCADE,
  indicateur        TEXT,
  unite             TEXT,
  prevision         NUMERIC(15,2),
  realisation       NUMERIC(15,2),
  localite          TEXT,
  preuves           TEXT,
  taux_annuel       NUMERIC(5,2),
  taux_global       NUMERIC(5,2),
  created_at        TIMESTAMP DEFAULT NOW(),
  updated_at        TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 7. DIFFICULTÉS
-- ============================================================
CREATE TABLE IF NOT EXISTS difficultes (
  id              SERIAL PRIMARY KEY,
  projet_id       INTEGER NOT NULL REFERENCES projets(id) ON DELETE CASCADE,
  difficulte      TEXT,
  mesure          TEXT,
  created_at      TIMESTAMP DEFAULT NOW(),
  updated_at      TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 8. RECOMMANDATIONS
-- ============================================================
CREATE TABLE IF NOT EXISTS recommandations (
  id              SERIAL PRIMARY KEY,
  projet_id       INTEGER NOT NULL REFERENCES projets(id) ON DELETE CASCADE,
  recommandation  TEXT,
  echeance        TEXT,
  responsable     TEXT,
  perspective     TEXT,
  created_at      TIMESTAMP DEFAULT NOW(),
  updated_at      TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 9. DEMANDES DE MODIFICATION
-- ============================================================
CREATE TABLE IF NOT EXISTS demandes (
  id                        SERIAL PRIMARY KEY,
  soumission_id             INTEGER NOT NULL REFERENCES soumissions(id) ON DELETE CASCADE,
  utilisateur_id            INTEGER NOT NULL REFERENCES utilisateurs(id),
  nom_structure             TEXT NOT NULL,
  email                     TEXT NOT NULL,
  date_soumission_originale DATE,
  date_demande              DATE NOT NULL DEFAULT CURRENT_DATE,
  objet                     TEXT NOT NULL,
  corps                     TEXT NOT NULL,
  statut                    TEXT NOT NULL DEFAULT 'en_attente'
                            CHECK (statut IN ('en_attente', 'accepte', 'refuse')),
  traite_par                INTEGER REFERENCES utilisateurs(id),
  traite_le                 TIMESTAMP,
  created_at                TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- 10. VERSIONS / DIFF
-- ============================================================
CREATE TABLE IF NOT EXISTS versions_soumissions (
  id              SERIAL PRIMARY KEY,
  soumission_id   INTEGER NOT NULL REFERENCES soumissions(id) ON DELETE CASCADE,
  version         INTEGER NOT NULL,
  diff_json       JSONB NOT NULL,   -- champs modifiés uniquement
  created_at      TIMESTAMP DEFAULT NOW(),
  created_by      INTEGER REFERENCES utilisateurs(id)
);

-- ============================================================
-- 11. NOTIFICATIONS
-- ============================================================
CREATE TABLE IF NOT EXISTS notifications (
  id              SERIAL PRIMARY KEY,
  utilisateur_id  INTEGER NOT NULL REFERENCES utilisateurs(id) ON DELETE CASCADE,
  type            TEXT NOT NULL,    -- ex: 'session_ouverte','demande_acceptee'
  message         TEXT NOT NULL,
  lu              BOOLEAN NOT NULL DEFAULT FALSE,
  lien            TEXT,             -- ex: '#historique'
  created_at      TIMESTAMP DEFAULT NOW()
);

-- ============================================================
-- TRIGGERS : updated_at automatique
-- ============================================================
CREATE OR REPLACE FUNCTION update_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Appliquer le trigger sur toutes les tables concernées
DO $$
DECLARE
  t TEXT;
BEGIN
  FOREACH t IN ARRAY ARRAY[
    'utilisateurs','sessions','soumissions','projets',
    'execution_financiere','execution_physique',
    'difficultes','recommandations'
  ] LOOP
    EXECUTE format('
      CREATE TRIGGER trg_updated_%I
      BEFORE UPDATE ON %I
      FOR EACH ROW EXECUTE FUNCTION update_updated_at();
    ', t, t);
  END LOOP;
END;
$$;

-- ============================================================
-- ADMIN PAR DÉFAUT (mot de passe : Admin1234! — à changer)
-- ============================================================
INSERT INTO utilisateurs (nom_structure, email, mot_de_passe, role)
VALUES (
  'Ministère — Administration',
  'admin@ministere.bf',
  -- hash bcrypt de 'Admin1234!' généré via R : bcrypt::hashpw('Admin1234!')
  '$2a$12$placeholder_remplacer_par_vrai_hash',
  'admin'
) ON CONFLICT (email) DO NOTHING;
