# Projet : Compilation LISP vers VM

## Description Générale
Ce projet vise à concevoir et implanter un ensemble de modules permettant la compilation et l'exécution de programmes LISP à l'aide d'une machine virtuelle (VM) dédiée. Il s'articule autour de trois grands modules :

1. **Méta-évaluateur de LISP** : Un interpréteur capable de méta-évaluer lui-même.
2. **Transformation vers un langage intermédiaire (LI)** : Traduire du code LISP vers un langage intermédiaire adapté à la VM.
3. **Générateur de code pour la VM** : Générer du code exécutable sur la machine virtuelle.

## Objectifs du Projet
- Couvrir un sous-ensemble étendu de COMMON LISP, en privilégiant les traits nécessaires à la réflexivité (fermetures, continuations, labels).
- Assurer la méta-évaluation du méta-évaluateur lui-même et faciliter la méta-évaluation des deux autres modules.
- Implémenter une VM robuste, capable d'exécuter tout code généré, avec support pour la gestion des fichiers et des transformations.
![image](https://github.com/user-attachments/assets/964ba67d-d4af-4b8b-87fc-f61f6fe23fc7)


## Modules Principaux

### 1. Méta-évaluateur de LISP
Le méta-évaluateur sera capable d'interpréter un large sous-ensemble de COMMON LISP, tout en étant capable de méta-évaluer son propre code. Les fonctionnalités principales incluent :

- Gestion des fermetures et fonctions locales (labels).
- Support des traits vus en cours comme les continuations et les macros.
- Une attention particulière aux macros comme `loop`, qui nécessitent un traitement spécifique en raison de leur complexité d'expansion.

Extensions possibles :
- Implémentation d'un méta-évaluateur SCHEME en SCHEME ou COMMON LISP, capable de méta-évaluer lui-même.
- Méta-évaluation avec filtrage et gestion avancée des macros (inspirée de SCHEME).

### 2. Transformation vers un Langage Intermédiaire (LI)
Ce module traduit le code LISP en un langage intermédiaire simplifié, plus proche du niveau machine.

- **Traits à couvrir** : Gestion des fermetures, fonctions locales, et structures de contrôle classiques (let, if, loop, select).
- **Simplifications possibles** : Les traits complexes comme `&rest` et `&optional` peuvent être omis.

Bien que la transformation du code de ce module vers un langage intermédiaire soit facultative, elle constitue une étape intéressante pour tester les capacités de la chaîne de compilation.

### 3. Générateur de Code pour la VM
Le générateur de code traduit le langage intermédiaire (LI) en instructions exécutables par la VM.

- Support des instructions nécessaires pour compiler des structures comme les fermetures et labels.
- Compilation et chargement de programmes via un chargeur intégré.

Défis spécifiques :
- Gestion des fermetures (`function`) : une tâche complexe, mais réalisable pour les labels.
- Traitement des fichiers : Implémentation d'une fonction `mload` pour charger des fichiers LISP dans la VM.

### 4. Machine Virtuelle (VM)
La VM exécute le code ASM produit par le générateur de code. Elle doit être capable de :

- Accepter tout code syntaxiquement correct généré par le compilateur.
- Gérer les fichiers LISP via des fonctions équivalentes à `load` et `mload`.
- Charger des fichiers contenant des définitions complètes pour une exécution fluide.

## Étapes Clés du Développement
1. **Méta-évaluateur**
   - Développer un méta-évaluateur capable de se méta-évaluer lui-même.
   - Intégrer les traits comme les fermetures, continuations, et labels.
2. **Transformation vers LI**
   - Créer un langage intermédiaire avec une syntaxe simplifiée.
   - Définir des règles pour traduire les constructions LISP vers LI.
3. **Génération de Code pour VM**
   - Développer un assembleur pour traduire LI en instructions VM.
   - Tester avec des exemples comme la fonction `fibo`.
4. **Implémentation de la VM**
   - Créer une machine virtuelle capable d'interpréter le code généré.
   - Ajouter un chargeur de fichiers et des outils pour tester les programmes compilés.

## Extensions Facultatives
- **Optimisations** : Ajouter des optimisations spécifiques pour améliorer la performance de la VM.
- **Support SCHEME** : Étendre le méta-évaluateur pour gérer SCHEME.
- **Support Avancé des Macros** : Implémenter des mécanismes avancés pour traiter les macros complexes.

## Conclusion
Ce projet illustre le processus complet de compilation : de la méta-évaluation de LISP à la génération et l'exécution de code sur une machine virtuelle. En combinant théorie et pratique, il permet d'explorer des concepts fondamentaux de compilation, comme la gestion des fermetures et des macros, tout en restant ancré dans un contexte éducatif.

