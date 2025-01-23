# LISP VM

Ce projet implémente une machine virtuelle pour un compilateur LISP.

## Structure du projet

- `src/` : Contient le code source de la machine virtuelle.
- `tests/` : Contient les tests unitaires et d'intégration.

## Prérequis

- Un interpréteur CLISP

## Tests

Pour exécuter les tests, utilisez la commande suivante :

```sh
clisp ./tests/run-all-tests.lisp
```

## TODO

- [x] Structure de la machine virtuelle (registres, mémoire, flags)
- [x] Accesseurs pour les registres et la mémoire (get, set)
- [x] Instructions de memoire (load, store)
- [x] Instructions de registres (move)
- [x] Instructions d'arithmétiques (add, sub, mul, div, incr, decr)
- [x] Instructions de pile (push, pop)
- [x] Accès dans la pile (peek, poke)
- [x] Generalisation de la pile (usage sans SP remplacé par un registre)
- [x] Adresses, etiquettes, et instructions de saut inconditionnel (label, jump)
- [x] Saut avec retour (JSR, RTN)
- [x] Instructions de comparaison (cmp)
- [x] Les saut conditionnels (jeq, jne, jlt, jgt, jle, jge)
- [x] Instructions diverses (nop, halt)

## Plus tard

- [x] Reliure et chargement de code
- [x] Resolution d'adresse
- [x] Chargement de chargeur
- [ ] Execution interprete naif
- [ ] Machine virtuelle threadee
