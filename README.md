# Shiny-App-Project-Analytics-Strategy-Partners
Application de collecte et d'analyse de données du MSJE

---

Exemple Appi : https://shiny.posit.co/r/gallery/government-public-sector/soil-profiles/ 
Utilisateurs : 
ong : gopaga@gmail.bf admin123
admin : admin@admin.com 
# Guide pour la gestion de l'environnement Collaboratif

## Première utilisation

```r
# 1. Ouvrir le projet en mode projet via le fichier .Rproj
# 2. Installer renv si nécessaire: install.packages("renv")
# 3. Restaurer tous les packages du projet: renv::restore()
```
> ✅ C'est tout. Vous avez exactement les mêmes versions que l'équipe.

## Rôle des fichiers indispensables
```r
* renv.lock — le fichier le plus important, il contient la liste exacte de tous les packages et leurs versions
* .Rprofile — active automatiquement renv au démarrage du projet
* renv/activate.R — le script d'activation de renv
* renv/settings.json — les paramètres de configuration renv
```

---

## Commandes essentielles

| Commande | Quand l'utiliser |
|---|---|
| `renv::restore()` | Après un `git pull` |
| `renv::snapshot()` | Après avoir installé un package |
| `renv::status()` | Pour vérifier que tout est synchronisé |
| `renv::update()` |Met à jour un ou tous les packages |


## Workflow quotidien

---

### Après un `git pull`
```r
renv::restore()  # synchronise les packages si renv.lock a changé
```

### Après avoir installé un nouveau package
```r
install.packages("monPackage")
renv::snapshot()  # met à jour renv.lock
git add renv.lock # actualisé le renv.lock sur github
git commit -m "feat: ajout de monPackage"
```

---

## Règles d'équipe

- Ne jamais commiter le dossier `renv/library/` (déjà dans `.gitignore`)
- Toujours commiter `renv.lock` après `renv::snapshot()`
- Toujours ouvrir le projet via le fichier `.Rproj`

---