module Main where

import Data.Char (toLower)
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map

{-|
  Под автоматом Левенштейна для заданного слова далее подразумевается
  конечный автомат, который допускает все слова, находящиеся на некотором
  расстоянии Дамерау-Левенштейна от заданного, не превышающем 1.

  Здесь и далее будут неоднократные ссылки на следующую статью:
  Klaus U. Schulz, Stoyan Mihov (2002) // Fast String Correction with
  Levenshtein-Automata. International Journal of Document Analysis and
  Recognition No. 5, P. 67 - 85
-}

{-|
  Тип для несжатого [почти] префиксного дерева.
  На самом деле этот тип интерпретирует префиксное дерево как конечный автомат,
  что удобно для исправления ошибок с помощью автомата Левенштейна.
 -}
data Trie a = Trie { isFinal :: Bool, transitions :: Map a (Trie a) }

empty :: Trie a
empty = Trie False Map.empty

{-|
  Вставка в префиксное дерево.
  Сложность: O(log(m) * n), n - длина вставляемого слова, m - арность префиксного дерева.
-}
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] dict                             = Trie True (transitions dict)
insert (x:xs) (Trie isFinal' transitions') =
    case Map.lookup x transitions' of
      Nothing -> Trie isFinal' $ Map.insert x (insert xs empty) transitions'
      Just tr -> Trie isFinal' $ Map.insert x (insert xs tr) transitions'

{-|
  Поиск в префиксном дереве.
  Сложность: O(log(m) * n), n - длина вставляемого слова, m - арность префиксного дерева.
-}
search :: Ord a => [a] -> Trie a -> Bool
search [] dict                             = isFinal dict
search (x:xs) (Trie isFinal' transitions') =
    case Map.lookup x transitions' of
      Nothing -> False
      Just tr -> search xs tr

{-|
  Состояние детерминированного автомата Левенштейна (далее просто
  автомата Левенштейна).
  Конечно, корректнее было бы придерживаться обозначений статьи, но здесь
  они условно поделены на группы.
-}
data LevenshteinState = NoMistakes { index :: Int }
                      | Edited { index :: Int }
                      | Insert { index :: Int }
                      | Deleted { index :: Int }
                      | Replaced { index :: Int }
                      | Transpose { index :: Int }
                      | Reject 
                      deriving Eq

{-|
  Функция перехода для автомата Левенштейна.
-}
nextLevenshteinState :: [Bool] -> LevenshteinState -> LevenshteinState
nextLevenshteinState vec (NoMistakes i)
  | vec == [False, False, False] = Insert i
  | vec == [True, False, False]  = NoMistakes $ i + 1
  | vec == [False, True, False]  = Transpose i
  | vec == [False, False, True]  = Insert i
  | vec == [True, True, False]   = NoMistakes $ i + 1
  | vec == [True, False, True]   = NoMistakes $ i + 1
  | vec == [False, True, True]   = Transpose i
  | vec == [True, True, True]    = NoMistakes $ i + 1
  | vec == [False, False]        = Insert i
  | vec == [True, False]         = NoMistakes $ i + 1
  | vec == [False, True]         = Transpose i
  | vec == [True, True]          = NoMistakes $ i + 1
  | vec == [False]               = Insert i
  | vec == [True]                = NoMistakes $ i + 1
  | null vec                     = Edited i
nextLevenshteinState vec (Edited i)
  | vec == [False, False, False] = Reject
  | vec == [True, False, False]  = Edited $ i + 1
  | vec == [False, True, False]  = Reject
  | vec == [False, False, True]  = Reject
  | vec == [True, True, False]   = Edited $ i + 1
  | vec == [True, False, True]   = Edited $ i + 1
  | vec == [False, True, True]   = Reject
  | vec == [True, True, True]    = Edited $ i + 1
  | vec == [False, False]        = Reject
  | vec == [True, False]         = Edited $ i + 1
  | vec == [False, True]         = Reject
  | vec == [True, True]          = Edited $ i + 1
  | vec == [False]               = Reject
  | vec == [True]                = Edited $ i + 1
  | null vec                     = Reject
nextLevenshteinState vec (Insert i)
  | vec == [False, False, False] = Reject
  | vec == [True, False, False]  = Edited $ i + 1
  | vec == [False, True, False]  = Edited $ i + 2
  | vec == [False, False, True]  = Reject
  | vec == [True, True, False]   = Insert $ i + 1
  | vec == [True, False, True]   = Edited $ i + 1
  | vec == [False, True, True]   = Edited $ i + 2
  | vec == [True, True, True]    = Insert $ i + 1
  | vec == [False, False]        = Reject
  | vec == [True, False]         = Edited $ i + 1
  | vec == [False, True]         = Edited $ i + 2
  | vec == [True, True]          = Insert $ i + 1
  | vec == [False]               = Reject
  | vec == [True]                = Edited $ i + 1
nextLevenshteinState vec (Deleted i)
  | vec == [False, False, False] = Reject
  | vec == [True, False, False]  = Edited $ i + 1
  | vec == [False, True, False]  = Reject
  | vec == [False, False, True]  = Edited $ i + 3
  | vec == [True, True, False]   = Edited $ i + 1
  | vec == [True, False, True]   = Deleted $ i + 1
  | vec == [False, True, True]   = Edited $ i + 3
  | vec == [True, True, True]    = Deleted $ i + 1
  | vec == [False, False]        = Reject
  | vec == [True, False]         = Edited $ i + 1
  | vec == [False, True]         = Reject
  | vec == [True, True]          = Edited $ i + 1
nextLevenshteinState vec (Replaced i)
  | vec == [False, False, False] = Reject
  | vec == [True, False, False]  = Edited $ i + 1
  | vec == [False, True, False]  = Edited $ i + 2
  | vec == [False, False, True]  = Edited $ i + 3
  | vec == [True, True, False]   = Insert $ i + 1
  | vec == [True, False, True]   = Deleted $ i + 1
  | vec == [False, True, True]   = Insert $ i + 2
  | vec == [True, True, True]    = Replaced $ i + 1
  | vec == [False, False]        = Reject
  | vec == [True, False]         = Edited $ i + 1
  | vec == [False, True]         = Edited $ i + 2
  | vec == [True, True]          = Insert $ i + 1
nextLevenshteinState vec (Transpose i)
  | vec == [False, False, False] = Reject
  | vec == [True, False, False]  = Insert $ i + 1
  | vec == [False, True, False]  = Edited $ i + 2
  | vec == [False, False, True]  = Edited $ i + 3
  | vec == [True, True, False]   = Insert $ i + 1
  | vec == [True, False, True]   = Replaced $ i + 1
  | vec == [False, True, True]   = Insert $ i + 2
  | vec == [True, True, True]    = Replaced $ i + 1
  | vec == [False, False]        = Reject
  | vec == [True, False]         = Insert $ i + 1
  | vec == [False, True]         = Edited $ i + 2
  | vec == [True, True]          = Insert $ i + 1

{-|
  Функция, определяющая, является ли данное состояние автомата Левенштейна для
  заданного входа финальным.
-}
isFinalLevenshteinState :: [a] -> LevenshteinState -> Bool
isFinalLevenshteinState input state = state `elem` (finalLevenshteinStates input)
    where finalLevenshteinStates []  = [NoMistakes 0, Edited 0]
          finalLevenshteinStates [x] = [NoMistakes 0, NoMistakes 1, Edited 1, Insert 0]
          finalLevenshteinStates _   = [ NoMistakes inputLength
                                       , NoMistakes $ inputLength - 1
                                       , Edited inputLength
                                       , Insert $ inputLength - 1
                                       , Deleted $ inputLength - 2
                                       , Replaced $ inputLength - 2
                                       , Transpose $ inputLength - 2
                                       ]
          inputLength = length input

characteristicVector :: Eq a => [a] -> a -> [Bool]
characteristicVector haystack needle = map (== needle) (take 3 haystack)

{-|
  Функция, которая решает предложенную задачу.
  На вход подаются словарь (хранится в префиксном дереве) и пользовательский ввод.
  В случае, если слово не было найдено в словаре, возвращается список исправлений.

  Метод, который используется в этой функции, использует параллельный обход двух
  конечных автоматов - словаря и автомата Левенштейна для пользовательского ввода.
  В случае, если автомат Левенштейна допустил некоторое слово из словаря (которое
  является путём до финального состояния в автомате-словаре), то оно может быть
  предложено как исправление введённому слову, так как находится на расстоянии
  Дамерау-Левенштейна <= 1. Как можно понять из статьи, сам автомат
  Левенштейна вовсе необязательно синтезировать, достаточно просто его имитировать,
  что и делает эта функция.

  Сложность данного алгоритма в случае необходимости исправления для заданного слова 
  есть сложность обхода префиксного дерева (словаря) в глубину - O(|V|) (V - множество
  вершин префиксного дерева).
-}
suggestions :: Ord a => Trie a -> [a] -> Maybe [[a]]
suggestions dict input
  | search input dict = Nothing
  | otherwise         = Just (suggestionsHelper [([], dict, NoMistakes 0)] [] input)

suggestionsHelper :: Ord a => [([a], Trie a, LevenshteinState)] -> [[a]] -> [a] -> [[a]]
suggestionsHelper [] candidates _         = candidates
suggestionsHelper (x:xs) candidates input = suggestionsHelper stack' candidates' input
    where
        (prefix, dictState, levState) = x
        characteristicVector' = characteristicVector (drop (index levState) input)
        stack' = Map.foldrWithKey (\ char nextDictState ys ->
            if nextLevenshteinState (characteristicVector' char) levState /= Reject
               then ((char : prefix)
                    , nextDictState
                    , nextLevenshteinState (characteristicVector' char) levState
                    ) : ys
               else ys
                                  ) xs (transitions dictState)
        candidates' = Map.foldrWithKey (\ char nextDictState ys ->
            if isFinal nextDictState
               && isFinalLevenshteinState input (nextLevenshteinState 
                    (characteristicVector' char) levState)
               then (reverse (char : prefix)) : ys
               else ys
                                       ) candidates (transitions dictState)

main :: IO ()
main = interact (unlines . processInput . filter (not . null) . lines)

processInput :: [String] -> [String]
processInput []    = []
processInput input = map (suggestionsWrapper dict) words
    where n = (read (head input) :: Int)
          (dictLines, words) = splitAt n (tail input)
          dict = foldr (insert . map toLower) empty dictLines

suggestionsWrapper :: Trie Char -> String -> String
suggestionsWrapper dict input = input ++ what (suggestions dict input')
    where input' = map toLower input
          what Nothing   = " - ok"
          what (Just []) = " -?"
          what (Just xs) = " -> " ++ (intercalate ", " (sort xs)) 
