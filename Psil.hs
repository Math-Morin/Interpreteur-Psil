-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------
module Psil where

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint
           | Llist Ltype
           | Lfun Ltype Ltype
           deriving (Show, Eq)

data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lannot Lexp Ltype   -- Annotation de type.
          | Linvoke Lexp Lexp   -- Appel de fonction, avec un argument.
          | Lnil Ltype          -- Constructeur de liste vide.
          | Lcons Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionelle.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          -- Déclaration de fonction locale.
          | Lletrec Var [(Var, Ltype)] Ltype Lexp Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
-- Cas "atomiques"
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
-- Cas "composites". On transforme d'abord en liste Haskell
s2l sexp = s2slist2l sexp

s2slist2l :: Sexp -> Lexp
s2slist2l = slist2l . s2slist

{-
s2slist:
  Transforme la liste de Scons en liste Haskell.
  À chaque pas de récursion, le dernier élément du Scons est ajouté
  comme premier élément de la liste `slist`. Le résultat est donc une
  liste dans l'ordre naturel.
-}
s2slist :: Sexp -> [Sexp]
s2slist Snil = []
s2slist sexp = s2slist' [] sexp

s2slist' :: [Sexp] -> Sexp -> [Sexp]
s2slist' slist (Scons Snil sexp)   = sexp:slist
s2slist' slist (Scons sexp1 sexp2) = s2slist' (sexp2:slist) sexp1
s2slist' _ sexp                    = error ("Malformed Psil (s2slist'): "
                                            ++ show sexp)

{-
slist2l:
  Choisi la fonction de traitement Scons -> Lexp selon le premier
  élément de la liste de Sexp.
-}
slist2l :: [Sexp] -> Lexp
slist2l []     = error "slist2l error: sexp list can't be empty"
slist2l [sexp] = s2l sexp
slist2l (s:ss) =
  case s of
    -- Annotation de type
    Ssym ":"     -> slist2lannot ss
    -- Listes
    Ssym "nil"   -> slist2lnil ss
    Ssym "list"  -> slist2lcons (s:ss)
    Ssym "cons"  -> slist2lcons (s:ss)
    -- Case (Pattern Matching sur liste)
    Ssym "case"  -> slist2lcase ss
    -- Let et Letfn
    Ssym "let"   -> slist2llet ss
    Ssym "letfn" -> slist2lletrec ss
    -- Appel de fonction
    Ssym _       -> slist2linvoke (s:ss)
    Scons _ _    -> slist2linvoke (s:ss)
    _            -> error ("Malformed Psil (slist2l): "
                           ++ show s)


{-
slist2lannot:
  Traitement [Sexp] -> Lexp pour les annotations de type.
  En Psil valide, [Sexp] n'a toujours que 2 éléments.
  Le premier élément est toujours une expression.
  Le second élément est toujours un type.
-}
slist2lannot :: [Sexp] -> Lexp
slist2lannot [sexp1,sexp2] = Lannot (s2l sexp1) (s2t sexp2)
slist2lannot slist = error ("Malformed Psil: invalid Type Annotation sexp: "
                            ++ show slist)

{-
slist2lcons:
  Traitement [Sexp] -> Lexp pour les listes.
  Le premier élément de [Sexp] est soit `list`, soit `cons`.
  Le choix du traitement se fait en fonction de ce mot-clé.
  Forme de [Sexp]:
    Cas `cons` [Sexp] := [Ssym "cons",
                          sexp du premier élément,
                          sexp du reste de la liste]
    Ces deux sexp (premier élément et reste de liste) sont des cas traités
    récursivement par s2l.

    Cas `list` [Sexp] := [Ssym "list", type, e1, e2, ... , en]
    On utilise la fonction foldr. Le cas initial est la liste vide accompagée
    de son type. On ajoute ensuite un à un les éléments de la liste depuis la
    fin, en construisant le Lexp comme un oignon, depuis le centre. Les
    éléments de la liste sont d'abord traité par s2l avant d'être imbriqués
    dans le Lexp.
-}
slist2lcons :: [Sexp] -> Lexp
slist2lcons [Ssym "cons", sexp1, sexp2] = Lcons (s2l sexp1) (s2l sexp2)
slist2lcons (Ssym "list":t:restslist)
  = foldr (Lcons . s2l) (Lnil (s2t t)) restslist
slist2lcons slist = error ("Malformed Psil: invalid List exp: "
                           ++ show slist)

{-
slist2lnil:
  Traitement [Sexp] -> Lexp pour la liste vide.
-}
slist2lnil :: [Sexp] -> Lexp
slist2lnil [t]   = Lnil (s2t t)
slist2lnil slist = error ("Malformed Psil: invalid Empty List exp: "
                          ++ show slist)

{-
slist2lcase:
  Traitement [Sexp] -> Lexp pour les expressions `case`.
  Le premier élément de [Sexp] est la liste à comparer, alors que les
  éléments suivants sont les différentes branches. Puisque les possibilités
  ne se résument qu'à 2 cas (cas nil et cas non-nil), on construit le Lexp
  avec les premières branches de chacun de ces cas.
  Chacun des cas est obligatoire (exige l'exhaustivité).
-}
slist2lcase :: [Sexp] -> Lexp
slist2lcase (list:branches) =
  let (nilCase, (vars, nonNilCase)) = findFirstCase branches
  in uncurry (Lcase (s2l list) nilCase) vars nonNilCase
slist2lcase slist = error ("Malformed Psil: invalid Case exp: "
                           ++ show slist)

-- Trouve le premier cas 'nil' ou 'non-nil'.
-- Appelle ensuite la fonction pour trouver le cas manquant.
findFirstCase :: [Sexp] -> (Lexp, ((Var, Var), Lexp))
findFirstCase []     = error "Non-exhaustive Case exp: no case found."
findFirstCase (b:bs) =
    case s2slist b of
      [Ssym "nil", expr] -> findNonNilCase (s2l expr) bs
      [vars, expr] ->
        case s2slist vars of
          [Ssym "cons", Ssym v1, Ssym v2] -> findNilCase ((v1,v2), s2l expr) bs
          _ -> error ("Invalid variable declaration "
                    ++ "in non-nil case branch: "
                    ++ show vars)
      _ -> error ("Malformed branch in case exp: "
                ++ show b)

-- Trouve le premier cas 'nil'
findNilCase :: ((Var, Var), Lexp) -> [Sexp] -> (Lexp, ((Var, Var), Lexp))
findNilCase _ [] = error "Non-exhausitve Case exp: no nil case found"
findNilCase casNonNil (b:bs) =
  case s2slist b of
    [Ssym "nil", expr] -> verifRest (s2l expr, casNonNil) bs
    [vars, _] ->
      case s2slist vars of
        [Ssym "cons", Ssym _, Ssym _] -> findNilCase casNonNil bs
        _ -> error ("Invalid variable declaration "
                  ++ "in non-nil case branch: "
                  ++ show vars)
    _ -> error ("Malformed branch in case exp: "
                ++ show b)

-- Trouve le premier cas 'non nil'
findNonNilCase :: Lexp -> [Sexp] -> (Lexp, ((Var, Var), Lexp))
findNonNilCase _ [] = error "Non exhaustive Case exp: no non-nil case found"
findNonNilCase casNil (b:bs) =
    case s2slist b of
    [Ssym "nil", _] -> findNonNilCase casNil bs
    [vars, expr] ->
      case s2slist vars of
        [Ssym "cons", Ssym v1, Ssym v2] ->
          verifRest (casNil, ((v1,v2), s2l expr)) bs
        _ -> error ("Invalid variable declaration "
                    ++ "in non-nil case branch: "
                    ++ show vars)
    _ -> error ("Malformed branch in case exp: "
                ++ show b)

-- Verifie les branches restantes
verifRest :: (Lexp, ((Var, Var), Lexp)) -> [Sexp] -> (Lexp, ((Var, Var), Lexp))
verifRest cases []     = cases
verifRest cases (b:bs) =
    case s2slist b of
    [Ssym "nil", _] -> verifRest cases bs
    [vars, _] ->
      case s2slist vars of
        [Ssym "cons", Ssym _, Ssym _] -> verifRest cases bs
        _ -> error ("Invalid variable declaration "
                    ++ "in non-nil case branch: "
                    ++ show vars)
    _ -> error ("Malformed branch in case exp: "
                ++ show b)

{-
slist2linvoke:
  Traitement [Sexp] -> Lexp pour les appels de fonction.
  On utilise la fonction foldl. Le cas initial est le premier appel de fonction.
  S'il y a des arguments à cette fonction, on les imbriquent un à un dans la
  Lexp, depuis l'extérieur (ext) vers l'intérieur (int). Si un argument est
  lui-même le résultat d'une fonction, la récursion se charge de construire
  cette sous-Lexp.
-}
slist2linvoke :: [Sexp] -> Lexp
slist2linvoke [] = error ("slist2linvoke error: "
                          ++ "sexp list can't be empty")
slist2linvoke (sexp:restslist)
  = foldl (\int ext -> Linvoke int (s2l ext)) (s2l sexp) restslist

{-
slist2llet:
  Traitement [Sexp] -> Lexp pour les définitions de fonction.
-}
slist2llet :: [Sexp] -> Lexp
slist2llet [decs, body] = slist2llet' (s2slist decs) body
slist2llet slist        = error ("Malformed let exp: " ++ show slist)

slist2llet' :: [Sexp] -> Sexp -> Lexp
slist2llet' (dec:decs) body =
    case (s2slist dec,decs) of
      ([Ssym var, expr], [])
        -> Llet var (s2l expr) (s2l body)
      ([Ssym var, expr], _)
        ->  Llet var (s2l expr) (slist2llet' decs body)
      _ -> error ("Malformed let declaration:"
                  ++ " dec := "  ++ show dec)
slist2llet' decs body = error ("Malformed let exp: "
                               ++ show (decs, body))

{-
slist2lletrec:
  Traitement [Sexp] -> Lexp pour les définitions de fonction.
-}
slist2lletrec :: [Sexp] -> Lexp
slist2lletrec slist =
  case slist of
    [Ssym funcName, formals, returnType, body, exec]
      -> Lletrec funcName (s2flist formals) (s2t returnType)
                          (s2l body) (s2l exec)
    _ -> error ("Malformed letfn exp" ++ show slist)

s2flist :: Sexp -> [(Var, Ltype)]
s2flist = s2flist' . s2slist

-- Construction de la liste de *formals*
s2flist' :: [Sexp] -> [(Var, Ltype)]
s2flist' []                       = []
s2flist' ((Ssym var):t:restslist) = (var, s2t t):s2flist' restslist
s2flist' (sexp:restslist)         = s2flist' (s2slist sexp ++ restslist)


s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Scons Snil stype) = s2t stype
s2t (Scons (Scons Snil (Ssym "List")) stype) = Llist (s2t stype)
s2t (Scons (Scons (Scons restArgs lastArgStype) (Ssym "->")) outputStype)
  = case restArgs of
      Snil -> Lfun (s2t lastArgStype) (s2t outputStype)
      _    -> s2tFunc restArgs (Lfun (s2t lastArgStype) (s2t outputStype))
s2t (Ssym "->") = error ("Malformed Psil type: "
                         ++ "missing parenthesis in function type.\n"
                         ++ "Exemples:\n"
                         ++ "Valid  : `Int Int -> Int`\n"
                         ++ "Valid  : `Int -> (Int -> Int)`\n"
                         ++ "Invalid: `Int -> Int -> Int`")
s2t sexp = error ("Malformed Psil type: " ++ show sexp)

s2tFunc :: Sexp -> Ltype -> Ltype
s2tFunc args outputLtype =
  case args of
    (Scons Snil firstArgStype) -> Lfun (s2t firstArgStype) outputLtype
    (Scons restArgs nextArgStype)
      -> s2tFunc restArgs (Lfun (s2t nextArgStype) outputLtype)
    _ -> error ("Malformed Psil type: " ++ show args)

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = Var -> Ltype

check :: TEnv -> Lexp -> Ltype
{-
Type check pour entiers.
Retourne type Lint
Règle:
       -----------
       Γ ⊢ n : Int
-}
check _ (Lnum _) = Lint

{-
Type check pour liste vide.
Retourne le type qui lui est associé dans la Lexp.
Règle:
       ------------------------
       Γ ⊢ (nil τ ) : (List τ )
-}
check _ (Lnil t) = Llist t

{-
Type check pour variable.
Retourne son type trouvé dans l'environnement.
Règle: Γ(x) = τ
       ---------
       Γ ⊢ x : τ
-}
check tenv (Lvar v) = tenv v

{-
Type check pour annotation de type.
Vérifie si le type de l'expression est le même que celui déclaré,
puis retourne ce type si les deux types sont égaux.
Règle:     Γ ⊢ e : τ
       -----------------
       Γ ⊢ ( : e τ ) : τ
-}
check tenv (Lannot expr t)
  | check tenv expr == t = t
  | let exprType = check tenv expr,
    otherwise = error ("Type annotation type error: `"
        ++ show expr ++ "` is of type `" ++ show exprType ++
        "`, but was declared to be of type `" ++ show t ++ "` instead.")

{-
Type check pour liste.
Une liste est considérée bien typée si tous ses éléments sont du même type
que celui déclaré dans la liste vide en dernière position de la liste.
Règle: Γ ⊢ e1 : τ Γ ⊢ e2 : (List τ)
       ----------------------------
       Γ ⊢ (cons e1 e2)  : (List τ)
-}
check tenv (Lcons x xs)
  | let t = Llist (check tenv x),
        t == check tenv xs = t
  | let tElem = check tenv x
        tList = check tenv xs,
    otherwise = error ("List type error: "
        ++ "can't put `" ++ show x ++
        "` of type `" ++ show tElem ++
        "` in a list of type `" ++ show tList ++ "`.")

{-
Type check pour les appels de fonction.
Vérifie que l'argument est du type attendu par la fonction,
puis retourne le type en sortie de la fonction lorsqu'on lui passe
un argument de ce type.
Règle: Γ ⊢ e1 : (τ1 → τ2) Γ ⊢ e2 : τ1
       ------------------------------
             Γ ⊢ (e1 e2) : τ2
-}
check tenv (Linvoke func arg) =
  case (check tenv func, check tenv arg) of
    (Lfun argExpected output, argGiven)
      | argExpected == argGiven -> output
      | otherwise -> error ("Expected arg of type `" ++ show argExpected
        ++ "`, but was given `" ++ show argGiven ++ "` instead.")
    (notafunc, _) -> error ("Function call error: not a function. "
                            ++ show notafunc)

{-
Type check pour les expressions Let.
Une expression Let est bien typée si le type des variables qui y sont définies
respectent les types attendu dans l'expression contenu dans le corps du Let.
Le type retourné est celui que l'on obtient après évaluation du corps lorsqu'on
lui passe ces variables déclarées dans le Let.
Règle: Γ ⊢ e1 : τ1   Γ, x : τ1 ⊢ e2 : τ2
       ---------------------------------
          Γ ⊢ (let ((x1 e1)) e2) : τ2
-}
check tenv (Llet var def expr) =
  check (tinsert tenv var (check tenv def)) expr

{-
Type check pour les expression Case.
Une expression Case est bien typée si ses 2 branches retourne le même type
en considérant que la branche 'cas non-nil' reçoit le premier élément d'une
liste de type t1 et une liste de type t1, tous deux du même type que la liste
reçue en entrée du Case. Une liste doit être donnée en paramètre.
Le type retourné est celui que retourne les deux branches.
Règle: Γ ⊢ e : (List τ1)   Γ ⊢ en : τ2   Γ, x : τ1, xs : (List τ1) ⊢ ec : τ2
       ---------------------------------------------------------------------
                    Γ ⊢ (case e (nil en) ((cons x xs) ec)) : τ2
-}
check tenv (Lcase list nilCase var1 var2 nonNilCase) =
  let tlist = check tenv list
  in
    case tlist of
      Llist t -> let tlistElem   = t
                     tNilCase    = check tenv nilCase
                     tNonNilCase = check tenv'' nonNilCase
                     tenv'  = tinsert tenv var1 tlistElem
                     tenv'' = tinsert tenv' var2 tlist
                 in
                   if tNilCase == tNonNilCase
                   then tNilCase
                   else error ("Case type error: branches must be of same type."
                               ++ " Nil case type := " ++ show tNilCase
                               ++ " Non-nil case type := " ++ show tNonNilCase)
      _ -> error ("Case type error: list expected as parameter,"
                  ++ " but received this instead : " ++ show list
                  ++ " of type " ++ show tlist)

{-
Type check pour les expressions Letfn
Une expression Letfn est bien typée si elle se réduit au type de sortie
déclarée dans sa définition, lorsqu'on lui passe les arguments du type attendu.
Le type des arguments passés son vérifiés contre le type des arguments formels
déclarés.
Le type retournée est celui auquel se réduit l'expression `expr` lorsque les
conditions de type décrites plus haut sont respectées.
Règle:
Γ′ = Γ, f : (τ1 ... τn → τr )  Γ′, x1 : τ1, ..., xn : τn ⊢ e1 : τr  Γ′ ⊢ e2 : τ
----------------------------------------------------------------------------
            Γ ` (letfn f ((x1 τ1) ... (xn τn)) τr e1 e2) : τ
-}
check tenv (Lletrec funcName formals returnType body expr) =
  let funcType  = buildFuncType formals returnType
      bodyEnv   = tinsert (tinsertMany tenv formals) funcName funcType
      bodyCheck = check bodyEnv body
      exprEnv   = tinsert bodyEnv funcName funcType
  in
    if   bodyCheck == returnType
    then check exprEnv expr
    else error ("Letfun type error: "
               ++ " function `" ++ funcName
               ++ "` expected to return type: " ++ show returnType
               ++ ", but returns type " ++ show bodyCheck ++ " instead.")

-- Construit le type d'une fonction.
buildFuncType :: [(Var, Ltype)] -> Ltype -> Ltype
buildFuncType [] returnType = returnType
buildFuncType ((_,t):ts) returnType = Lfun t (buildFuncType ts returnType)

-- Insert plusieurs variables et leur type dans un environnement.
tinsertMany :: TEnv -> [(Var, Ltype)] -> TEnv
tinsertMany tenv list =
  case list of
    []         -> tenv
    ((v,t):ts) -> tinsertMany (tinsert tenv v t) ts


tenv0 :: TEnv
tenv0 "+" = Lfun Lint (Lfun Lint Lint)
tenv0 "-" = Lfun Lint (Lfun Lint Lint)
tenv0 "*" = Lfun Lint (Lfun Lint Lint)
tenv0 "/" = Lfun Lint (Lfun Lint Lint)
tenv0 x   = error ("Unknown variable: " ++ show x)

tlookup :: TEnv -> Var -> Ltype
tlookup env = env

tinsert :: TEnv -> Var -> Ltype -> TEnv
tinsert env var val = \x -> if (x == var) then val else tlookup env x

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vlambda (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _p Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _p _ = showString "<function>"

type VEnv = Var -> Value

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 "+"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x + y)))
env0 "*"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x * y)))
env0 "/"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x `div` y)))
env0 "-"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x - y)))
env0 x      = error ("Unknown variable: " ++ show x)

vlookup :: VEnv -> Var -> Value
vlookup env = env

vinsert :: VEnv -> Var -> Value -> VEnv
vinsert env var val = \x -> if (x == var) then val else vlookup env x

-- La fonction d'évaluation principale.
-- Cas simples
eval :: VEnv -> Lexp -> Value
eval _   (Lnum n) = Vnum n
eval _   (Lnil _) = Vnil
eval env (Lvar v) = vlookup env v

-- Evaluation des annotations de type
eval env (Lannot expr _) = eval env expr

-- Evaluation des listes
eval env (Lcons x xs) = Vcons (eval env x) (eval env xs)

-- Evaluation des appels de fonction
eval env (Linvoke func expr) =
  case eval env func of
    Vlambda x -> x (eval env expr)
    _ -> error ("Error function call: This is not a function -> "
                ++ show func)

-- Evaluation des expression Let
eval env (Llet var def expr) =
  eval (vinsert env var (eval env def)) expr

-- Evaluation des expression Case
eval env (Lcase list nilCase var1 var2 nonNilCase) =
  case eval env list of
    Vnil       -> eval env nilCase
    Vcons x xs -> eval newEnv nonNilCase
      where newEnv = vinsert env' var1 x
            env'   = vinsert env  var2 xs
    _          -> error ("Error case exp: first arg must be a list, "
                         ++ " received this instead -> " ++ show list)

-- Evaluation des expressions LetFn (récursives)
-- Cas sans déclaration de valiable
eval env (Lletrec funcName [] _ body expr) =
  let envrec = vinsert env funcName (eval envrec body)
  in eval envrec expr

-- Cas avec déclaration de variables
eval env (Lletrec funcName formals _ body expr) =
  let funcrec envrec' formals' =
        Vlambda (\actual -> let formal      = fst (head formals')
                                restFormals = tail formals'
                            in
                              if   null restFormals
                              then eval (vinsert envrec' formal actual) body
                              else funcrec
                                   (vinsert envrec' formal actual)
                                   restFormals
                )
      envrec = vinsert env funcName (funcrec envrec formals)
  in
    eval envrec expr


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left err -> error ("Parse error: " ++ show err)
                            Right es -> es
            in (concat
                (map (\ sexp -> let { lexp = s2l sexp
                                   ; ltyp = check tenv0 lexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = check tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
