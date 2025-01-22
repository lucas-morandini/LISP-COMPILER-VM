# Projet : Compilation LISP vers VM

## Description Générale
Ce projet vise à concevoir et implémenter un ensemble de modules permettant la compilation et l'exécution de programmes LISP à l'aide d'une machine virtuelle (VM) dédiée. Il s'articule autour de quatres grands modules :

1. **Méta-évaluateur de LISP** : Un interpréteur capable de méta-évaluer son propre code.  
2. **Transformation vers un langage intermédiaire (LI)** : Traduire du code LISP vers un langage intermédiaire adapté à la VM.  
3. **Générateur de code pour la VM** : Générer du code exécutable sur la machine virtuelle.  
4. **Machine Virtuelle (VM)** : Permettre l’exécution du code généré (y compris la gestion des fonctions LISP primitives et le chargement de code).

## Objectifs du Projet
- Couvrir un sous-ensemble étendu de COMMON LISP, en privilégiant les traits nécessaires à la réflexivité (fermetures, continuations, labels).
- Assurer la méta-évaluation du méta-évaluateur lui-même et faciliter la méta-évaluation des autres modules (transformation LI, génération de code).
- Implémenter une VM robuste, capable d'exécuter tout code généré, avec un support pour la gestion des fichiers et l’interface avec LISP.

## Modules Principaux

### 1. Méta-évaluateur de LISP
Le méta-évaluateur interprète un large sous-ensemble de COMMON LISP, et peut méta-évaluer son propre code. Ses fonctionnalités incluent :

- Gestion des fermetures et fonctions locales (labels).
- Support des continuations et des macros, y compris des macros complexes comme `loop`.
- Méta-évaluation du méta-évaluateur lui-même, pour démontrer la réflexivité.

### 2. Transformation vers un Langage Intermédiaire (LI)
Ce module traduit le code LISP en un langage intermédiaire simplifié (LI) :

- **Traits à couvrir** : fermetures, fonctions locales, structures de contrôle classiques (let, if, loop, select).
- Génération d'une représentation de code plus proche de l'ASM, destinée à être prise en charge par la VM.

### 3. Générateur de Code pour la VM
Le générateur de code produit un "assembleur" qui transforme le langage intermédiaire (LI) en instructions exécutables par la VM :

- Support des instructions pour gérer les fermetures, labels, et structures de contrôle.
- Production d’un fichier (ou d'une structure en mémoire) pouvant être chargé et exécuté sur la VM.
- Gestion des appels et retours de fonctions, y compris l’appel aux primitives LISP depuis la VM.

### 4. Machine Virtuelle (VM)
La VM est au cœur du projet : c’est elle qui exécute le code. Elle doit être capable de :

- Interpréter les instructions générées, incluant les accès mémoire, empilements/dépilements, sauts et comparaisons.
- Gérer les appels à des fonctions LISP primitives (interface VM/LISP).
- Charger dynamiquement du code grâce à un chargeur intégré, gérant aussi bien les labels locaux que globaux.
- Offrir un mécanisme de résolution de symboles pour les appels en avant (forward references).
- Offrir une interface de lancement (exemple : exécuter `(fibo 10)` chargé dans la VM).

## Implementation Détaillée de la VM

Cette section décrit les étapes précises d’implémentation de la machine virtuelle, selon le sujet détaillé du projet.

### 4.1 Structures de Données
1. **Création de la VM**  
   - `make-vm` : crée une machine virtuelle avec un nom, une taille de mémoire, des registres (nombre fixe ou variable), et toute autre structure nécessaire (pile, flags, etc.).

2. **Accès aux registres**  
   - `get-register` / `set-register` : pour lire ou écrire le contenu d’un registre.  
   - Les registres stockent souvent des valeurs entières ou des adresses mémoires, selon la convention retenue.

3. **Accès à la mémoire**  
   - `get-memory` / `set-memory` : permet de lire ou d'écrire dans la mémoire de la VM à une adresse précise.  
   - La mémoire peut être un tableau ou un vecteur de taille fixée à la création de la VM.

### 4.2 Exécution de la VM
L’exécution est une boucle d’interprétation des instructions présentes en mémoire :

1. **Boucle d’interprétation**  
   - La VM lit l’instruction pointée par le « programme counter » (PC).
   - Elle exécute l’action correspondante (move, load, store, etc.).
   - Elle met à jour le PC pour passer à l’instruction suivante (sauf en cas de saut).

2. **Instructions élémentaires**  
   - `move` : déplacer le contenu d’un registre ou d’une valeur immédiate vers un autre registre.  
   - `load` / `store` : transfert de données entre la mémoire et un registre (ou une valeur).  
   - `push` / `pop` : opérations sur la pile, souvent implémentées via `store` / `load` sur un registre "pile pointer".  
   - Opérations arithmétiques (add, sub, mul, div) : calcul sur registres (ou valeurs immédiates) et stockage du résultat dans un registre.  
   - `cmp` : comparaison qui positionne des flags internes (zero, negative, etc.).  
   - `jmp` (saut inconditionnel), `call` (saut avec retour), `ret` (retour) : manipulations du PC et de la pile si nécessaire.  
   - `jz`, `jnz` ou équivalents : sauts conditionnels selon les flags.

3. **Arrêt de la VM**  
   - `halt` : instruction d’arrêt qui met fin à la boucle d’interprétation.

### 4.3 Chargement de la VM
Le chargement consiste à prendre une séquence de code « ASM VM » et à la copier en mémoire, avec résolution de symboles :

1. **Étiquettes (label)**  
   - Lorsqu’on rencontre une instruction `label <sym>`, on associe `<sym>` à l’adresse mémoire courante.  
   - Si `<sym>` est déjà résolu, il faut générer une erreur (double définition).  
   - On gère aussi les références en avant (lorsqu’un saut vers `<sym>` apparaît avant la définition de `<sym>`). Les sauts sont alors résolus a posteriori.

2. **Sauts et références en avant**  
   - Pour un saut (ex. `jmp <sym>`), si `<sym>` est déjà connu, on stocke directement l’adresse.  
   - Sinon, on enregistre la référence en avant dans une table (hashtable) pour la résoudre plus tard, quand `label <sym>` sera rencontré.

3. **Tables de symboles**  
   - Deux tables de hachage :  
     - **Symbols résolus** : associe un symbole à une adresse.  
     - **Références en avant** : associe un symbole à une liste d’instructions à patcher une fois l’étiquette rencontrée.

4. **Étiquettes locales vs globales**  
   - **Locales** : n’existent que dans le cadre du code chargé. À la fin du chargement, elles doivent toutes être résolues.  
   - **Globales** : peuvent correspondre à des symboles partagés dans la VM (ex. primitives LISP).  
   - Le code de chargement doit vérifier que toutes les étiquettes locales sont résolues. Les globales peuvent rester non résolues si elles renvoient à des fonctions LISP primitives (voir ci-dessous).

### 4.4 Interface VM / LISP
Deux points importants :

1. **Appeler la VM depuis LISP**  
   - Par exemple pour lancer l’exécution d’un code chargé, ou via une fonction `vm-apply`.  
   - `vm-apply` prend des arguments LISP, les push sur la pile, et déclenche un `call` vers l’adresse correspondant à la fonction (ex. `fibo`).  
   - Après exécution, un `ret` renvoie la VM à un point précis (souvent une instruction `halt`).

2. **Appeler LISP depuis la VM**  
   - Lorsque la VM exécute un `call <sym>` dont `<sym>` est non résolu, cela signifie qu’on appelle une primitive LISP.  
   - On récupère les arguments depuis la pile, on les convertit en liste LISP, puis on appelle `apply` sur la fonction LISP correspondante.  
   - Le résultat est ensuite déposé dans un registre ou sur la pile, selon la convention d’appel établie.

## Étapes Clés du Développement

1. **Méta-évaluateur**  
   - Créer un interpréteur LISP gérant fermetures, continuations, labels, macros…  
   - Valider la méta-évaluation (le méta-évaluateur s’évalue lui-même).

2. **Transformation vers LI**  
   - Définir un format intermédiaire (LI) pour représenter simplement les structures LISP.  
   - Traduire let, if, loop, etc., en instructions plus proches de l’ASM.

3. **Génération de Code pour la VM**  
   - Écrire un assembleur LISP → code VM.  
   - Gérer spécifiquement la construction des fermetures et l’allocation de labels.

4. **Implémentation de la VM**  
   - Définir la structure de données (registres, mémoire, pile).  
   - Implémenter la boucle d’interprétation (exécution des instructions).  
   - Écrire la routine de chargement pour stocker le code et résoudre les labels.  
   - Gérer l’interface VM/LISP (appels sortants et rentrants).

## Extensions Possibles
- **Optimisations** : Ajouter des optimisations dans le code généré (raccourcis de saut, optimisation tail-call, etc.). 
- **Macros Avancées** : Mécanismes plus poussés de traitement de macros complexes.  
- **Gestion fine de la mémoire** : Implémentation d’un garbage collector pour des allocations dynamiques dans la VM.

## Conclusion
Ce projet propose une chaîne de compilation complète, depuis la méta-évaluation de LISP jusqu’à la génération et l’exécution de code sur une machine virtuelle. Les détails d’implémentation de la VM (structures de données, exécution, chargement, interface) permettent de se familiariser avec la conception d’un interpréteur, la sémantique des instructions, et la communication entre LISP et la VM. L’ensemble illustre des concepts clés de la compilation et de l’évaluation de langages Lisp.
