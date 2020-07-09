{-# LANGUAGE LambdaCase #-}

module Expressions ( pparse, evaluate ) where

import Utils
import Data.Ratio
import Data.Char ( isDigit )

data E = N (Ratio Integer) | Sum [ E ] | Prod [ E ] | E :^: E | Negated E
       | Inversed E
    deriving ( Show )

evaluate :: E -> Ratio Integer
evaluate (Sum xs) = sum (map evaluate xs)
evaluate (Prod xs) = product (map evaluate xs)
evaluate (x :^: y) = evaluate x ^ numerator (evaluate y)
evaluate (Negated x) = negate (evaluate x)
evaluate (Inversed x) = 1 / evaluate x
evaluate (N x) = x

pparse = parse . prettify

parse :: String -> E
parse x
    | null (indexesOfNonNestedChar '+' x) && head x == '-' = Negated $ parse
        (tail x)
    | null (indexesOfNonNestedChar '*' x) && head x == '/' = Inversed $ parse
        (tail x)
    | otherwise
        = if have_nested_binary x then parse_nested_sums x else parse_sums x

parse_nested_sums :: String -> E
parse_nested_sums s = case splitOnNonNested '+' s of
    [] -> error "Empty expression on lv1"
    [ x ] -> parse_nested_prods x
    xs -> Sum . map parse $ xs

parse_nested_prods :: String -> E
parse_nested_prods s = case splitOnNonNested '*' s of
    [] -> error "Empty expression on lv2"
    [ x ] -> parse_nested_powers x
    xs -> Prod . map parse $ xs

parse_nested_powers :: String -> E
parse_nested_powers s = case splitOnNonNested '^' s of
    [] -> error "Empty expression on lv2"
    [ x ] -> parse (debrackify x)
    [ x, y ] -> parse x :^: parse y

parse_sums :: String -> E
parse_sums s = case splitOn '+' s of
    [] -> error "Empty expression on lv3"
    [ x ] -> parse_prods s
    xs -> Sum $ map parse xs

parse_prods :: String -> E
parse_prods s = case splitOn '*' s of
    [] -> error "Empty expression on lv4"
    [ x ] -> parse_powers s
    xs -> Prod $ map parse xs

parse_powers :: String -> E
parse_powers s = case splitOn '^' s of
    [] -> error "Empty expression on lv4"
    [ x ] -> parse_literals s
    [ x, y ] -> parse x :^: parse y

parse_literals :: String -> E
parse_literals = \case
    ('/' : xs) -> Inversed . parse $ xs
    ('-' : xs) -> Negated . parse $ xs
    x -> N $ read x % 1

isBOperator :: Char -> Bool
isBOperator = (`elem` "+*^")

have_nested_binary :: String -> Bool
have_nested_binary = helper 0
  where
    helper _ [] = False
    helper n (x : xs)
        | n > 0 && isBOperator x = True
    helper n ('(' : xs) = helper (n + 1) xs
    helper n (')' : xs) = helper (n - 1) xs
    helper n (x : xs) = helper n xs

debrackify :: String -> String
debrackify [] = []
debrackify s = f . g $ s
  where
    f s = if head s == '(' then tail s else s

    g s = if last s == ')' then init s else s

splitOnNonNested :: Char -> String -> [ String ]
splitOnNonNested c s = splitAtList (indexesOfNonNestedChar c s) s

indexesOfNonNestedChar :: Char -> String -> [ Int ]
indexesOfNonNestedChar c s = helper 0 c (zip s [ 0 .. ])
  where
    helper :: Int -> Char -> [ ( Char, Int ) ] -> [ Int ]
    helper _ c [] = []
    helper n c (x : xs) = case fst x of
        v
            | v == c && n == 0 -> snd x : helper n c xs
        ')' -> helper (n - 1) c xs
        '(' -> helper (n + 1) c xs
        x -> helper n c xs

prettify :: String -> String
prettify = add_plus_before_binary_minus . add_mul . filter (/= ' ')
  where
    add_plus_before_binary_minus :: String -> String
    add_plus_before_binary_minus = \case
        (x : '-' : other)
            | x /= '(' -> x : '+' : '-' : add_plus_before_binary_minus other
        (x : xs) -> x : add_plus_before_binary_minus xs
        [] -> []

    add_mul :: String -> String
    add_mul = \case
        ('/' : xs) -> "*/" ++ add_mul xs
        (a : '(' : xs)
            | isDigit a || a == 'x' -> a : '*' : '(' : add_mul xs
        (x : xs) -> x : add_mul xs
        [] -> []
