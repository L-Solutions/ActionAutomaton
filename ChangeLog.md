# Change Log

- 2019-10-19 (BF) Version initiale
- 2019-10-22 (BF) Refactorisation du code des tests
- 2019-10-23 (BF) Amélioration du module Data.Text.Color 
                  - création du typeclass Color
                  - instanciation de Color8, Color256, () 
                  - différenciation entre BG et FG color. 
- 2019-10-23 (BF) Introduction de la monade ReaderT pour AutomatonExecution
                  - une exécution s'effectue dans un environnement contenant les transitions
                  - une exécution dispose de l'état courant de l'automate 
                  - celui-ci peut être modifié
                  - tout est enrobé dans des MonadTransformer
- 2019-11-05 (BF) Suppression du main, des tests et des libs non directement relié à l'automate
                  Mise en place sur github

