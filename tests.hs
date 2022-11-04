import Psil

---------------------------------------------------------------------------
-- Tests                                                                 --
---------------------------------------------------------------------------

{- Prend une fonction et une liste de tests en entré.
   Fonctions en entrée : sexpOf, lexpOf, typeOf, valOf
   Retourne : - Nothing si résultat obtenu == résultat attendu
              - Just (string psil, résultat obtenu, résultat attendu) sinon
-}
test :: (Show b) => (String -> b) -> [(String, String, b )] -> Maybe (String , b, b)
test _ [] = Nothing
test f ((n, psil, expectedValue):tests)
  | show returnedValue == show expectedValue = test f tests
  | otherwise = Just (n, returnedValue, expectedValue)
  where returnedValue = f psil

{- Forme un message selon résultat des tests
   Si aucune erreur: imprime "OK" à droite du nom de la liste de test.
   Si erreur: imprime le # de test, le résultat obtenu et le résultat attendu.
-}
formatMsg :: (Show b) => Maybe (String, b, b) -> ([Char], [Char], [Char])
formatMsg t =
  case t of
    Nothing -> ("OK", "", "")
    Just (n, r, e) -> ("erreur au test " ++ n,
                       "\n        retourné: " ++ show r,
                       "\n        attendu : " ++ show e)

-- run tests
rt :: IO ()
rt = do
  putStrLn ("tSexp : " ++ indexSexp ++ returnedSexp ++ expectedSexp)
  putStrLn ("tLexp : " ++ indexLexp ++ returnedLexp ++ expectedLexp)
  putStrLn ("tType : " ++ indexType ++ returnedType ++ expectedType)
  putStrLn ("tVal  : " ++ indexVal  ++ returnedVal  ++ expectedVal)

(indexSexp, returnedSexp, expectedSexp) = formatMsg (test sexpOf tSexp)
(indexLexp, returnedLexp, expectedLexp) = formatMsg (test lexpOf tLexp)
(indexType, returnedType, expectedType) = formatMsg (test typeOf tType)
(indexVal,  returnedVal,  expectedVal)  = formatMsg (test valOf tVal)

runEx = run "exemples.psil"
runTs = run "tests.psil"

-- Liste de tests : Sexp
-- [(#test, string Psil, résultat attendu)]
tSexp :: [(String, String, Sexp)]
tSexp = [
  ("#1", "2", Snum 2),
  ("#2", "456", Snum 456),
  ("#3", "0", Snum 0),
  ("#4", "-0", Snum 0),
  ("#5", "-456", Snum (-456)),
  ("#6", "x", Ssym "x"),
  ("#7", "+", Ssym "+"),
  ("#8", "/", Ssym "/"),
  ("#9", "()", Snil),
  ("#10", "(+)", Scons Snil (Ssym "+")),
  ("#11", "(+ 1)", Scons (Scons Snil
                          (Ssym "+"))
                   (Snum 1)),
  ("#12", "(+ 1 2)", Scons (Scons (Scons Snil
                                   (Ssym "+"))
                             (Snum 1))
                     (Snum 2)),
  ("#13", "(+ (+ 1 2) 3)", Scons (Scons (Scons Snil
                                         (Ssym "+"))
                                  (Scons (Scons (Scons Snil
                                                 (Ssym "+"))
                                          (Snum 1))
                                   (Snum 2)))
                           (Snum 3)),
  ("#14", "(+ (+ 1 2))", Scons (Scons Snil
                                (Ssym "+"))
                         (Scons (Scons (Scons Snil
                                        (Ssym "+"))
                                 (Snum 1))
                          (Snum 2))),
  ("#15", "(* (/ 4 5) 6)", Scons (Scons (Scons Snil
                                         (Ssym "*"))
                                  (Scons (Scons (Scons Snil
                                                 (Ssym "/"))
                                          (Snum 4))
                                   (Snum 5)))
                           (Snum 6)),
  ("#16", "(* (/ 7 8))", Scons (Scons Snil
                                (Ssym "*"))
                          (Scons (Scons (Scons Snil
                                         (Ssym "/"))
                                  (Snum 7))
                           (Snum 8))),
  ("#17", "(+ 1 a)", Scons (Scons (Scons Snil
                                   (Ssym "+"))
                            (Snum 1))
                     (Ssym "a")),
  ("#18", "((+ 2) 4)", Scons (Scons Snil
                              (Scons (Scons Snil
                                      (Ssym "+"))
                               (Snum 2)))
                       (Snum 4)),
  ("#19", "(: (+ 5) (Int -> Int))", Scons (Scons (Scons Snil
                                                  (Ssym ":"))
                                           (Scons (Scons Snil
                                                   (Ssym "+"))
                                            (Snum 5)))
                                    (Scons (Scons (Scons Snil
                                                   (Ssym "Int"))
                                            (Ssym "->"))
                                     (Ssym "Int"))),
  ("#20", "(: (+) (Int Int -> Int))", Scons (Scons (Scons Snil
                                                    (Ssym ":"))
                                             (Scons Snil
                                              (Ssym "+")))
                                       (Scons (Scons (Scons (Scons Snil
                                                             (Ssym "Int"))
                                                      (Ssym "Int"))
                                               (Ssym "->"))
                                        (Ssym "Int"))),
  ("#21", "(: (+ 5 6) (Int))", Scons (Scons (Scons Snil
                                             (Ssym ":"))
                                      (Scons (Scons (Scons Snil
                                                     (Ssym "+"))
                                              (Snum 5))
                                       (Snum 6)))
                               (Scons Snil
                                (Ssym "Int"))),
  ("#22", "(nil Int)", Scons (Scons Snil
                              (Ssym "nil"))
                       (Ssym "Int")),
  ("#23", "(cons 5 (nil Int))", Scons (Scons (Scons Snil
                                              (Ssym "cons"))
                                       (Snum 5))
                                (Scons (Scons Snil
                                        (Ssym "nil"))
                                 (Ssym "Int"))),
  ("#24", "(cons 4 (cons 5 (nil Int)))", Scons (Scons (Scons Snil
                                                       (Ssym "cons"))
                                                (Snum 4))
                                         (Scons (Scons (Scons Snil
                                                        (Ssym "cons"))
                                                 (Snum 5))
                                           (Scons (Scons Snil
                                                   (Ssym "nil"))
                                            (Ssym "Int")))),
  ("#25", "(cons 3 (cons 4 (cons 5 (nil Int))))", Scons (Scons (Scons Snil
                                                                (Ssym "cons"))
                                                         (Snum 3))
                                                   (Scons (Scons (Scons Snil
                                                                  (Ssym "cons"))
                                                            (Snum 4))
                                                    (Scons (Scons (Scons Snil
                                                                   (Ssym "cons"))
                                                             (Snum 5))
                                                     (Scons (Scons Snil
                                                             (Ssym "nil"))
                                                       (Ssym "Int"))))),
  ("#26", "(list Int 3)", Scons (Scons (Scons Snil
                                        (Ssym "list"))
                                 (Ssym "Int"))
                          (Snum 3)),
  ("#27", "(list Int 3 4)", Scons (Scons (Scons (Scons Snil
                                                 (Ssym "list"))
                                          (Ssym "Int"))
                                   (Snum 3))
                            (Snum 4)),
  ("#28", "(list Int 3 4 5)", Scons (Scons (Scons (Scons (Scons Snil
                                                          (Ssym "list"))
                                                   (Ssym "Int"))
                                            (Snum 3))
                                     (Snum 4))
                              (Snum 5)),
  ("#29", "(list Int 3 4 5 6)", Scons (Scons (Scons (Scons (Scons (Scons Snil
                                                                   (Ssym "list"))
                                                             (Ssym "Int"))
                                                      (Snum 3))
                                               (Snum 4))
                                        (Snum 5))
                                (Snum 6)),
  ("#30", "(: (+) (Int -> (Int -> Int)))", Scons (Scons (Scons Snil
                                                       (Ssym ":"))
                                                 (Scons Snil
                                                  (Ssym "+")))
                                         (Scons (Scons (Scons Snil
                                                        (Ssym "Int"))
                                                  (Ssym "->"))
                                           (Scons (Scons (Scons Snil
                                                          (Ssym "Int"))
                                                    (Ssym "->"))
                                             (Ssym "Int")))),
  ("#31", "(: (list Int 2 3 4) (List Int))", Scons (Scons (Scons Snil
                                                          (Ssym ":"))
                                                    (Scons (Scons (Scons (Scons (Scons Snil
                                                                                 (Ssym "list"))
                                                                           (Ssym "Int"))
                                                                    (Snum 2))
                                                             (Snum 3))
                                                      (Snum 4)))
                                            (Scons (Scons Snil
                                                    (Ssym "List"))
                                              (Ssym "Int"))),
  ("#32", "(: (list (Int Int -> Int) - * +) (List (Int Int -> Int)))",
   Scons (Scons (Scons Snil
                 (Ssym ":"))
           (Scons (Scons (Scons (Scons (Scons Snil
                                        (Ssym "list"))
                                 (Scons (Scons (Scons (Scons Snil
                                                       (Ssym "Int"))
                                                (Ssym "Int"))
                                         (Ssym "->"))
                                  (Ssym "Int")))
                          (Ssym "-"))
                   (Ssym "*"))
            (Ssym "+")))
    (Scons (Scons Snil
            (Ssym "List"))
     (Scons (Scons (Scons (Scons Snil
                           (Ssym "Int"))
                    (Ssym "Int"))
             (Ssym "->"))
      (Ssym "Int")))),
  ("#33", "(list (Int Int -> Int) - * +)",
   Scons (Scons (Scons (Scons (Scons Snil
                                (Ssym "list"))
                          (Scons (Scons (Scons (Scons Snil
                                                (Ssym "Int"))
                                          (Ssym "Int"))
                                   (Ssym "->"))
                            (Ssym "Int")))
                   (Ssym "-"))
            (Ssym "*"))
     (Ssym "+")),
  ("#34", "(cons - (cons * (cons + (cons nil (Int -> (Int -> Int))))))",
   Scons (Scons (Scons Snil
                 (Ssym "cons"))
           (Ssym "-"))
    (Scons (Scons (Scons Snil
                   (Ssym "cons"))
             (Ssym "*"))
      (Scons (Scons (Scons Snil
                     (Ssym "cons"))
               (Ssym "+"))
        (Scons (Scons (Scons Snil
                       (Ssym "cons"))
                 (Ssym "nil"))
          (Scons (Scons (Scons Snil
                         (Ssym "Int"))
                   (Ssym "->"))
            (Scons (Scons (Scons Snil
                           (Ssym "Int"))
                     (Ssym "->"))
              (Ssym "Int"))))))),
  ("#35", "(list (List Int) (list Int 3 4) (list Int 5 6))",
   Scons (Scons (Scons (Scons Snil
                        (Ssym "list"))
                  (Scons (Scons Snil
                          (Ssym "List"))
                    (Ssym "Int")))
           (Scons (Scons (Scons (Scons Snil
                                 (Ssym "list"))
                           (Ssym "Int"))
                    (Snum 3))
             (Snum 4)))
    (Scons (Scons (Scons (Scons Snil
                          (Ssym "list"))
                    (Ssym "Int"))
             (Snum 5))
      (Snum 6))),
  ("#36", "(let ((x 5)) (* x 4))", Scons (Scons (Scons Snil
                                                 (Ssym "let"))
                                          (Scons Snil
                                           (Scons (Scons Snil
                                                   (Ssym "x"))
                                            (Snum 5))))
                                   (Scons (Scons (Scons Snil
                                                  (Ssym "*"))
                                           (Ssym "x"))
                                    (Snum 4))),
  ("#37", "(let ((x 5) (y 6)) (* x y))", Scons (Scons (Scons Snil
                                                       (Ssym "let"))
                                                (Scons (Scons Snil
                                                        (Scons (Scons Snil
                                                                (Ssym "x"))
                                                         (Snum 5)))
                                                 (Scons (Scons Snil
                                                         (Ssym "y"))
                                                  (Snum 6))))
                                         (Scons (Scons (Scons Snil
                                                        (Ssym "*"))
                                                 (Ssym "x"))
                                          (Ssym "y"))),
  ("#38", "(let ((x 5)) (let ((y 6)) (* x y)))", Scons (Scons (Scons Snil
                                                               (Ssym "let"))
                                                        (Scons Snil
                                                         (Scons (Scons Snil
                                                                 (Ssym "x"))
                                                          (Snum 5))))
                                                 (Scons (Scons (Scons Snil
                                                                (Ssym "let"))
                                                         (Scons Snil
                                                          (Scons (Scons Snil
                                                                  (Ssym "y"))
                                                           (Snum 6))))
                                                  (Scons (Scons (Scons Snil
                                                                 (Ssym "*"))
                                                          (Ssym "x"))
                                                   (Ssym "y")))),
  ("#39", "(let ((x (list Int 2 3 4))) (x)))", Scons (Scons (Scons Snil
                                                             (Ssym "let"))
                                                       (Scons Snil
                                                        (Scons (Scons Snil
                                                                (Ssym "x"))
                                                          (Scons (Scons (Scons (Scons (Scons Snil
                                                                                       (Ssym "list"))
                                                                                 (Ssym "Int"))
                                                                          (Snum 2))
                                                                   (Snum 3))
                                                            (Snum 4)))))
                                               (Scons Snil
                                                (Ssym "x"))),
  ("#40", "(case (nil Int) (nil (+ 2 3)) ((cons x y) (- 3 2)))",
   Scons (Scons (Scons (Scons Snil
                        (Ssym "case"))
                  (Scons (Scons Snil (Ssym "nil"))
                   (Ssym "Int")))
           (Scons (Scons Snil
                   (Ssym "nil"))
             (Scons (Scons (Scons Snil
                            (Ssym "+"))
                      (Snum 2))
               (Snum 3))))
    (Scons (Scons Snil (Scons (Scons (Scons Snil
                                      (Ssym "cons"))
                                (Ssym "x"))
                         (Ssym "y")))
      (Scons (Scons (Scons Snil
                     (Ssym "-"))
               (Snum 3))
        (Snum 2)))),
  ("#41", "(case (list Int 2) (nil (+ 2 3)) ((cons x y) (- 3 2)))",
   Scons (Scons (Scons (Scons Snil
                        (Ssym "case"))
                  (Scons (Scons (Scons Snil
                                 (Ssym "list"))
                          (Ssym "Int"))
                    (Snum 2)))
           (Scons (Scons Snil
                   (Ssym "nil"))
             (Scons (Scons (Scons Snil
                            (Ssym "+"))
                      (Snum 2))
               (Snum 3))))
    (Scons (Scons Snil
            (Scons (Scons (Scons Snil
                           (Ssym "cons"))
                     (Ssym "x"))
              (Ssym "y")))
      (Scons (Scons (Scons Snil
                     (Ssym "-"))
               (Snum 3))
        (Snum 2)))),
  ("#42", "(case (list Int 2 3) (nil (+ 2 3)) ((cons x y) (- x 1)))",
   Scons (Scons (Scons (Scons Snil
                        (Ssym "case"))
                  (Scons (Scons (Scons (Scons Snil
                                        (Ssym "list"))
                                  (Ssym "Int"))
                           (Snum 2))
                    (Snum 3)))
           (Scons (Scons Snil
                   (Ssym "nil"))
             (Scons (Scons (Scons Snil
                            (Ssym "+"))
                      (Snum 2))
               (Snum 3))))
    (Scons (Scons Snil
            (Scons (Scons (Scons Snil
                           (Ssym "cons"))
                     (Ssym "x"))
              (Ssym "y")))
      (Scons (Scons (Scons Snil
                     (Ssym "-"))
               (Ssym "x"))
        (Snum 1)))),
  ("#43", "(case (list Int 2 3) (nil (list Int 2 3)) ((cons x y) (y)))",
   Scons (Scons (Scons (Scons Snil
                        (Ssym "case"))
                  (Scons (Scons (Scons (Scons Snil
                                        (Ssym "list"))
                                  (Ssym "Int"))
                           (Snum 2))
                    (Snum 3)))
           (Scons (Scons Snil
                   (Ssym "nil"))
             (Scons (Scons (Scons (Scons Snil
                                   (Ssym "list"))
                             (Ssym "Int"))
                      (Snum 2))
               (Snum 3))))
    (Scons (Scons Snil
            (Scons (Scons (Scons Snil
                           (Ssym "cons"))
                     (Ssym "x"))
              (Ssym "y")))
      (Scons Snil
       (Ssym "y")))),
  ("#44", "(let ((x (list Int 2 3))) (case (x) (nil (0)) ((cons e es) (e))))",
   Scons (Scons (Scons Snil
                 (Ssym "let"))
           (Scons Snil
            (Scons (Scons Snil
                    (Ssym "x"))
              (Scons (Scons (Scons (Scons Snil
                                    (Ssym "list"))
                              (Ssym "Int"))
                       (Snum 2))
                (Snum 3)))))
    (Scons (Scons (Scons (Scons Snil
                          (Ssym "case"))
                    (Scons Snil
                     (Ssym "x")))
             (Scons (Scons Snil
                     (Ssym "nil"))
               (Scons Snil
                (Snum 0))))
      (Scons (Scons Snil
              (Scons (Scons (Scons Snil
                             (Ssym "cons"))
                       (Ssym "e"))
                (Ssym "es")))
        (Scons Snil
         (Ssym "e"))))),
  ("#45", "(letfn inc ((x Int)) Int (+ x 1) (inc 4))",
   Scons (Scons (Scons (Scons (Scons (Scons Snil
                                      (Ssym "letfn"))
                                (Ssym "inc"))
                         (Scons Snil
                          (Scons (Scons Snil
                                  (Ssym "x"))
                            (Ssym "Int"))))
                  (Ssym "Int"))
           (Scons (Scons (Scons Snil
                          (Ssym "+"))
                    (Ssym "x"))
             (Snum 1)))
    (Scons (Scons Snil
            (Ssym "inc"))
      (Snum 4))),
  ("#46", "(letfn dec ((y Int)) Int (- y 1) (dec 3))",
   Scons (Scons (Scons (Scons (Scons (Scons Snil
                                      (Ssym "letfn"))
                                (Ssym "dec"))
                         (Scons Snil
                          (Scons (Scons Snil
                                  (Ssym "y"))
                            (Ssym "Int"))))
                  (Ssym "Int"))
           (Scons (Scons (Scons Snil
                          (Ssym "-"))
                    (Ssym "y"))
             (Snum 1)))
    (Scons (Scons Snil
            (Ssym "dec"))
      (Snum 3))),
  ("#47", "(letfn dif ((x Int) (y Int)) Int (- x y) (dif 3 2))",
   Scons (Scons (Scons (Scons (Scons (Scons Snil
                                      (Ssym "letfn"))
                                (Ssym "dif"))
                         (Scons (Scons Snil
                                 (Scons (Scons Snil
                                         (Ssym "x"))
                                   (Ssym "Int")))
                           (Scons (Scons Snil
                                   (Ssym "y"))
                             (Ssym "Int"))))
                  (Ssym "Int"))
           (Scons (Scons (Scons Snil
                          (Ssym "-"))
                    (Ssym "x"))
             (Ssym "y")))
    (Scons (Scons (Scons Snil
                   (Ssym "dif"))
             (Snum 3))
      (Snum 2))),
  ("#48", "(letfn sum ((xs (List Int))) Int"
       ++ "(case xs (nil 0)"
       ++ "((cons x xs) (+ x (sum xs))))"
       ++ "(sum (list Int 5 6)))",
    Scons (Scons (Scons (Scons (Scons (Scons Snil
                                       (Ssym "letfn"))
                                 (Ssym "sum"))
                          (Scons Snil
                           (Scons (Scons Snil
                                   (Ssym "xs"))
                             (Scons (Scons Snil
                                     (Ssym "List"))
                               (Ssym "Int")))))
                   (Ssym "Int"))
            (Scons (Scons (Scons (Scons Snil
                                  (Ssym "case"))
                            (Ssym "xs"))
                     (Scons (Scons Snil
                             (Ssym "nil"))
                       (Snum 0)))
              (Scons (Scons Snil
                      (Scons (Scons (Scons Snil
                                     (Ssym "cons"))
                               (Ssym "x"))
                        (Ssym "xs")))
                (Scons (Scons (Scons Snil
                               (Ssym "+"))
                         (Ssym "x"))
                  (Scons (Scons Snil
                          (Ssym "sum"))
                    (Ssym "xs"))))))
    (Scons (Scons Snil
            (Ssym "sum"))
      (Scons (Scons (Scons (Scons Snil
                            (Ssym "list"))
                      (Ssym "Int"))
               (Snum 5))
        (Snum 6)))),
  ("#49", "(letfn sum () Int (5) (sum))",
   Scons (Scons (Scons (Scons (Scons (Scons Snil
                                      (Ssym "letfn"))
                                (Ssym "sum"))
                         Snil)
                  (Ssym "Int"))
           (Scons Snil
            (Snum 5)))
    (Scons Snil
     (Ssym "sum")))
  ]

-- Liste de tests Lexp
-- [(#test, string Psil, résultat attendu)]
tLexp :: [(String, String, Lexp)]
tLexp = [
  ("#1", "2", Lnum 2),
  ("#2", "456", Lnum 456),
  ("#3", "0", Lnum 0),
  ("#4", "-0", Lnum 0),
  ("#5", "-456", Lnum (-456)),
  ("#6", "x", Lvar "x"),
  ("#7", "+", Lvar "+"),
  ("#8", "/", Lvar "/"),
  -- ("#9", "()", Lnil), -- pas dans le langage employé seul
  ("#10", "(+)", Lvar "+"),
  ("#11", "(+ 1)", Linvoke (Lvar "+") (Lnum 1)),
  ("#12", "(+ 1 2)", Linvoke (Linvoke (Lvar "+")
                              (Lnum 1))
                     (Lnum 2)),
  ("#13", "(+ (+ 1 2) 3)", Linvoke (Linvoke (Lvar "+")
                                    (Linvoke (Linvoke (Lvar "+")
                                              (Lnum 1))
                                      (Lnum 2)))
                           (Lnum 3)),
  ("#14", "(+ (+ 1 2))", Linvoke (Lvar "+")
                          (Linvoke (Linvoke (Lvar "+")
                                    (Lnum 1))
                            (Lnum 2))),
  ("#15", "(* (/ 4 5) 6)", Linvoke (Linvoke (Lvar "*")
                                    (Linvoke (Linvoke (Lvar "/")
                                              (Lnum 4))
                                      (Lnum 5)))
                           (Lnum 6)),
  ("#16", "(* (/ 7 8))", Linvoke (Lvar "*")
                          (Linvoke (Linvoke (Lvar "/")
                                    (Lnum 7))
                            (Lnum 8))),
  ("#17", "(+ 1 a)", Linvoke (Linvoke (Lvar "+")
                              (Lnum 1))
                     (Lvar "a")),
  ("#18", "((+ 2) 4)", Linvoke (Linvoke (Lvar "+")
                                (Lnum 2))
                       (Lnum 4)),
  ("#19", "(: (+ 5) (Int -> Int))", Lannot (Linvoke (Lvar "+")
                                            (Lnum 5))
                                     (Lfun Lint Lint)),
  ("#20", "(: (+) (Int Int -> Int))", Lannot (Lvar "+")
                                          (Lfun Lint (Lfun Lint Lint))),
  ("#21", "(: (+ 5 6) (Int))", Lannot (Linvoke (Linvoke (Lvar "+")
                                                (Lnum 5))
                                       (Lnum 6))
                                Lint),
  ("#22", "(nil Int)", Lnil Lint),
  ("#23", "(cons 5 (nil Int))", Lcons (Lnum 5) (Lnil Lint)),
  ("#24", "(cons 4 (cons 5 (nil Int)))", Lcons (Lnum 4)
                                          (Lcons (Lnum 5) (Lnil Lint))),
  ("#25", "(cons 3 (cons 4 (cons 5 (nil Int))))", Lcons (Lnum 3)
                                                   (Lcons (Lnum 4)
                                                    (Lcons (Lnum 5)
                                                     (Lnil Lint)))),
  ("#26", "(list Int 3)", Lcons (Lnum 3)
                          (Lnil Lint)),
  ("#27", "(list Int 3 4)", Lcons (Lnum 3)
                            (Lcons (Lnum 4)
                             (Lnil Lint))),
  ("#28", "(list Int 3 4 5)", Lcons (Lnum 3)
                              (Lcons (Lnum 4)
                               (Lcons (Lnum 5)
                                (Lnil Lint)))),
  ("#29", "(list Int 3 4 5 6)", Lcons (Lnum 3)
                                (Lcons (Lnum 4)
                                 (Lcons (Lnum 5)
                                  (Lcons (Lnum 6)
                                   (Lnil Lint))))),
  ("#30", "(: (+) (Int -> (Int -> Int)))", Lannot (Lvar "+")
                                           (Lfun Lint (Lfun Lint Lint))),
  ("#31", "(: (list Int 2 3 4) (List Int))", Lannot (Lcons (Lnum 2)
                                                     (Lcons (Lnum 3)
                                                      (Lcons (Lnum 4)
                                                       (Lnil Lint))))
                                             (Llist Lint)),
  ("#32", "(: (list (Int Int -> Int) - * +) (List (Int Int -> Int)))",
   Lannot (Lcons (Lvar "-")
           (Lcons (Lvar "*")
            (Lcons (Lvar "+")
             (Lnil (Lfun Lint (Lfun Lint Lint))))))
    (Llist (Lfun Lint (Lfun Lint Lint)))),
  ("#33", "(list (Int Int -> Int) - * +)", Lcons (Lvar "-")
                                           (Lcons (Lvar "*")
                                            (Lcons (Lvar "+")
                                             (Lnil (Lfun Lint (Lfun Lint Lint)))))),
  ("#34", "(cons - (cons * (cons + (nil (Int -> (Int -> Int))))))",
    Lcons (Lvar "-")
    (Lcons (Lvar "*")
     (Lcons (Lvar "+")
      (Lnil (Lfun Lint (Lfun Lint Lint)))))),
  ("#35", "(list (List Int) (list Int 3 4) (list Int 5 6))",
    Lcons (Lcons (Lnum 3) (Lcons (Lnum 4) (Lnil Lint)))
    (Lcons (Lcons (Lnum 5) (Lcons (Lnum 6) (Lnil Lint)))
     (Lnil (Llist Lint)))),
  ("#36", "(let ((x 5)) (* x 4))", Llet "x" (Lnum 5)
                                   (Linvoke (Linvoke (Lvar "*")
                                             (Lvar "x"))
                                    (Lnum 4))),
  ("#37", "(let ((x 5) (y 6)) (* x y))", Llet "x" (Lnum 5)
                                         (Llet "y" (Lnum 6)
                                          (Linvoke (Linvoke (Lvar "*")
                                                    (Lvar "x"))
                                          (Lvar "y")))),
  ("#38", "(let ((x 5)) (let ((y 6)) (* x y)))", Llet "x" (Lnum 5)
                                         (Llet "y" (Lnum 6)
                                          (Linvoke (Linvoke (Lvar "*")
                                                    (Lvar "x"))
                                          (Lvar "y")))),
  ("#39", "(let ((x (list Int 2 3 4))) (x)))", Llet "x" (Lcons (Lnum 2)
                                                         (Lcons (Lnum 3)
                                                          (Lcons (Lnum 4)
                                                           (Lnil Lint))))
                                               (Lvar "x")),
  ("#40", "(case (nil Int) (nil (+ 2 3)) ((cons x y) (- 3 2)))",
   Lcase (Lnil Lint)
         (Linvoke (Linvoke (Lvar "+")
                   (Lnum 2))
           (Lnum 3))
    "x" "y"
    (Linvoke (Linvoke (Lvar "-")
              (Lnum 3))
      (Lnum 2))),
  ("#41", "(case (list Int 2) (nil (+ 2 3)) ((cons x y) (- 3 2)))",
   Lcase (Lcons (Lnum 2)
           (Lnil Lint))
         (Linvoke (Linvoke (Lvar "+")
                   (Lnum 2))
           (Lnum 3))
    "x" "y"
    (Linvoke (Linvoke (Lvar "-")
              (Lnum 3))
      (Lnum 2))),
  ("#42", "(case (list Int 2 3) (nil (+ 2 3)) ((cons x y) (- x 1)))",
   Lcase (Lcons (Lnum 2)
          (Lcons (Lnum 3)
           (Lnil Lint)))
         (Linvoke (Linvoke (Lvar "+")
                   (Lnum 2))
           (Lnum 3))
    "x" "y"
    (Linvoke (Linvoke (Lvar "-")
              (Lvar "x"))
      (Lnum 1))),
  ("#43", "(case (list Int 2 3) (nil (list Int 2 3)) ((cons x y) (y)))",
   Lcase (Lcons (Lnum 2)
          (Lcons (Lnum 3)
           (Lnil Lint)))
         (Lcons (Lnum 2)
          (Lcons (Lnum 3)
           (Lnil Lint)))
    "x" "y"
    (Lvar "y")),
  ("#44", "(let ((x (list Int 2 3))) (case (x) (nil (0)) ((cons e es) (e))))",
   Llet "x" (Lcons (Lnum 2)
             (Lcons (Lnum 3)
              (Lnil Lint)))
    (Lcase (Lvar "x")
     (Lnum 0)
    "e" "es"
    (Lvar "e"))),
  ("#45", "(letfn inc ((x Int)) Int (+ x 1) (inc 4))",
   Lletrec "inc" [("x", Lint)] Lint
   (Linvoke (Linvoke (Lvar "+")
             (Lvar "x"))
     (Lnum 1))
    (Linvoke (Lvar "inc")
     (Lnum 4))),
  ("#46", "(letfn dec ((y Int)) Int (- y 1) (dec 3))",
   Lletrec "dec" [("y", Lint)] Lint
   (Linvoke (Linvoke (Lvar "-")
             (Lvar "y"))
     (Lnum 1))
    (Linvoke (Lvar "dec")
     (Lnum 3))),
  ("#47", "(letfn dif ((x Int) (y Int)) Int (- x y) (dif 3 2))",
   Lletrec "dif" [("x", Lint), ("y", Lint)] Lint
   (Linvoke (Linvoke (Lvar "-")
             (Lvar "x"))
     (Lvar "y"))
    (Linvoke (Linvoke (Lvar "dif")
              (Lnum 3))
     (Lnum 2))),
    ("#48", "(letfn sum ((xs (List Int))) Int (case xs (nil 0) ((cons x xs) (+ x (sum xs)))) (sum (list Int 5 6)))",
     Lletrec "sum" [("xs",Llist Lint)] Lint
     (Lcase (Lvar "xs")
      (Lnum 0)
      "x" "xs" (Linvoke (Linvoke (Lvar "+")
                         (Lvar "x"))
                 (Linvoke (Lvar "sum")
                  (Lvar "xs"))))
      (Linvoke (Lvar "sum")
       (Lcons (Lnum 5)
        (Lcons (Lnum 6)
         (Lnil Lint))))),
  ("#49", "(letfn sum () Int (5) (sum))",
   Lletrec "sum" [] Lint
   (Lnum 5)
   (Lvar "sum"))
  ]

-- Liste de tests pour type check
-- [(#test, string Psil, résultat attendu)]
tType :: [(String, String, Ltype)]
tType = [
  ("#1", "2", Lint ),
  ("#2", "456", Lint),
  ("#3", "0", Lint),
  ("#4", "-0", Lint),
  ("#5", "-456", Lint),
  -- ("#6", "x", Lvar "x"), -- necessite un Let
  ("#7", "+", Lfun Lint (Lfun Lint Lint)),
  ("#8", "/", Lfun Lint (Lfun Lint Lint)),
  -- ("#9", "()", Lnil), -- pas dans le langage employé seul
  ("#10", "(+)", Lfun Lint (Lfun Lint Lint)),
  ("#11", "(+ 1)", Lfun Lint Lint),
  ("#12", "(+ 1 2)", Lint),
  ("#13", "(+ (+ 1 2) 3)", Lint),
  ("#14", "(+ (+ 1 2))", Lfun Lint Lint),
  ("#15", "(* (/ 1 2) 3)", Lint),
  ("#16", "(* (/ 1 2))", Lfun Lint Lint),
  -- ("#17", "(+ 1 a)", Lint) -- necessite un let
  ("#18", "((+ 2) 4)", Lint),
  ("#19", "(: (+ 5) (Int -> Int))", Lfun Lint Lint),
  ("#20", "(: (+) (Int Int -> Int))", Lfun Lint (Lfun Lint Lint)),
  ("#21", "(: (+ 5 6) Int)", Lint),
  ("#22", "(nil Int)", Llist Lint),
  ("#23", "(cons 5 (nil Int))", Llist Lint),
  ("#24", "(cons 4 (cons 5 (nil Int)))", Llist Lint),
  ("#25", "(cons 3 (cons 4 (cons 5 (nil Int))))", Llist Lint),
  ("#26", "(list Int 3)", Llist Lint),
  ("#27", "(list Int 3 4)", Llist Lint),
  ("#28", "(list Int 3 4 5)", Llist Lint),
  ("#29", "(list Int 3 4 5 6)", Llist Lint),
  ("#30", "(: (+) (Int -> (Int -> Int)))", Lfun Lint (Lfun Lint Lint)),
  ("#31", "(: (list Int 2 3 4) (List Int))", Llist Lint),
  ("#32", "(: (list (Int Int -> Int) - * +) (List (Int Int -> Int)))",
   Llist (Lfun Lint (Lfun Lint Lint))),
  ("#33", "(list (Int Int -> Int) - * +)", Llist (Lfun Lint (Lfun Lint Lint))),
  ("#34", "(cons - (cons * (cons + (nil (Int -> (Int -> Int))))))",
   Llist (Lfun Lint (Lfun Lint Lint))),
  ("#35", "(list (List Int) (list Int 3 4) (list Int 5 6))",
   Llist (Llist Lint)),
  ("#36", "(let ((x 5)) (* x 4))", Lint),
  ("#37", "(let ((x 5) (y 6)) (* x y))", Lint),
  ("#38", "(let ((x 5)) (let ((y 6)) (* x y)))", Lint),
  ("#39", "(let ((x (list Int 2 3 4))) (x)))", Llist Lint),
  ("#40", "(case (nil Int) (nil (+ 2 3)) ((cons x y) (- 3 2)))", Lint),
  ("#41", "(case (list Int 2) (nil (+ 2 3)) ((cons x y) (- 3 2)))", Lint),
  ("#42", "(case (list Int 2 3) (nil (+ 2 3)) ((cons x y) (- x 1)))", Lint),
  ("#43", "(case (list Int 2 3) (nil (list Int 2 3)) ((cons x y) (y)))", Llist Lint),
  ("#44", "(let ((x (list Int 2 3))) (case (x) (nil (0)) ((cons e es) (e))))", Lint),
  ("#45", "(letfn inc ((x Int)) Int (+ x 1) (inc 4))", Lint),
  ("#46", "(letfn dec ((y Int)) Int (- y 1) (dec 3))", Lint),
  ("#47", "(letfn dif ((x Int) (y Int)) Int (- x y) (dif 3 2))", Lint),
  ("#48", "(letfn sum ((xs (List Int))) Int (case xs (nil 0) ((cons x xs) (+ x (sum xs)))) (sum (list Int 5 6)))", Lint),
  ("#49", "(letfn sum () Int (5) (sum))", Lint)
  ]


-- Liste de tests pour évaluation
-- [(#test, string Psil, résultat attendu)]
tVal :: [(String, String, Value)]
tVal = [
  ("#1", "2", Vnum 2),
  ("#2", "456", Vnum 456),
  ("#3", "0", Vnum 0),
  ("#4", "-0", Vnum 0),
  -- ("#6", "x", Lvar "x"), -- necessite un Let
  ("#7", "+", Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x + y)))),
  ("#8", "/", Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x `div` y)))),
  -- ("#9", "()", Lnil), -- pas dans le langage employé seul
  ("#10", "(+)", Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x + y)))),
  ("#11", "(+ 1)", Vlambda (\(Vnum x) -> Vnum (1 + x))),
  ("#12", "(+ 1 2)", Vnum 3),
  ("#13", "(+ (+ 1 2) 3)", Vnum 6),
  ("#14", "(+ (+ 1 2))", Vlambda (\(Vnum x) -> Vnum (3 + x))),
  ("#15", "(* (/ 1 2) 3)", Vnum 0),
  ("#16", "(* (/ 1 2))", Vlambda (\(Vnum x) -> Vnum (0 * x))),
  -- ("#17", "(+ 1 a)", Lint) -- necessite un let
  ("#18", "((+ 2) 4)", Vnum 6),
  ("#19", "(: (+ 5) (Int -> Int))", Vlambda (\(Vnum x) -> Vnum (x + 5))),
  ("#20", "(: (+) (Int Int -> Int))", Vlambda (\(Vnum x) ->
                                                 Vlambda (\(Vnum y) ->
                                                            Vnum (x + y)))),
  ("#21", "(: (+ 5 6) Int)", Vnum 11),
  ("#22", "(nil Int)", Vnil),
  ("#23", "(cons 5 (nil Int))", Vcons (Vnum 5) Vnil),
  ("#24", "(cons 4 (cons 5 (nil Int)))", Vcons (Vnum 4)
                                         (Vcons (Vnum 5)
                                           Vnil)),
  ("#25", "(cons 3 (cons 4 (cons 5 (nil Int))))", Vcons (Vnum 3)
                                                  (Vcons (Vnum 4)
                                                   (Vcons (Vnum 5)
                                                     Vnil))),
  ("#26", "(list Int 3)", Vcons (Vnum 3) Vnil),
  ("#27", "(list Int 3 4)", Vcons (Vnum 3)
                            (Vcons (Vnum 4)
                             Vnil)),
  ("#28", "(list Int 3 4 5)", Vcons (Vnum 3)
                              (Vcons (Vnum 4)
                               (Vcons (Vnum 5)
                                Vnil))),
  ("#29", "(list Int 3 4 5 6)", Vcons (Vnum 3)
                                (Vcons (Vnum 4)
                                 (Vcons (Vnum 5)
                                  (Vcons (Vnum 6)
                                   Vnil)))),
  ("#30", "(: (+) (Int -> (Int -> Int)))", Vlambda (\(Vnum x) ->
                                                    Vlambda (\(Vnum y) ->
                                                               Vnum (x + y)))),
  ("#31", "(: (list Int 2 3 4) (List Int))", Vcons (Vnum 2)
                                             (Vcons (Vnum 3)
                                              (Vcons (Vnum 4)
                                               Vnil))),
  ("#32", "(: (list (Int Int -> Int) - * +) (List (Int Int -> Int)))",
   Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x - y))))
   (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x * y))))
    (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x + y))))
      Vnil))),
  ("#33", "(list (Int Int -> Int) - * +)",
   Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x - y))))
   (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x * y))))
    (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x + y))))
      Vnil))),
  ("#34", "(cons - (cons * (cons + (nil (Int -> (Int -> Int))))))",
   Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x - y))))
   (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x * y))))
    (Vcons (Vlambda (\(Vnum x) -> Vlambda (\(Vnum y) -> Vnum (x + y))))
      Vnil))),
  ("#35", "(list (List Int) (list Int 3 4) (list Int 5 6))",
   Vcons (Vcons (Vnum 3) (Vcons (Vnum 4) Vnil))
   (Vcons (Vcons (Vnum 5) (Vcons (Vnum 6) Vnil))
    Vnil)),
  ("#36", "(let ((x 5)) (* x 4))", Vnum 20),
  ("#37", "(let ((x 5) (y 6)) (* x y))", Vnum 30),
  ("#38", "(let ((x 5)) (let ((y 6)) (* x y)))", Vnum 30),
  ("#39", "(let ((x (list Int 2 3 4))) (x)))", Vcons (Vnum 2)
                                               (Vcons (Vnum 3)
                                                (Vcons (Vnum 4)
                                                 Vnil))),
  ("#40", "(case (nil Int) (nil (+ 2 3)) ((cons x y) (- 3 2)))", Vnum 5),
  ("#41", "(case (list Int 2) (nil (+ 2 3)) ((cons x y) (- 3 2)))", Vnum 1),
  ("#42", "(case (list Int 2 3) (nil (+ 2 3)) ((cons x y) (- x 1)))", Vnum 1),
  ("#43", "(case (list Int 2 3) (nil (list Int 2 3)) ((cons x y) (y)))",
   Vcons (Vnum 3) Vnil),
  ("#44", "(let ((x (list Int 2 3))) (case (x) (nil (0)) ((cons e es) (e))))", Vnum 2),
  ("#45", "(letfn inc ((x Int)) Int (+ x 1) (inc 4))", Vnum 5),
  ("#46", "(letfn dec ((y Int)) Int (- y 1) (dec 3))", Vnum 2),
  ("#47", "(letfn dif ((x Int) (y Int)) Int (- x y) (dif 3 2))", Vnum 1),
  ("#48", "(letfn sum ((xs (List Int))) Int (case xs (nil 0) ((cons x xs) (+ x (sum xs)))) (sum (list Int 5 6)))", Vnum 11),
  ("#49", "(letfn sum () Int (5) (sum))", Vnum 5)
  ]

