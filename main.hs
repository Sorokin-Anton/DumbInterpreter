{-# LANGUAGE LambdaCase #-}

import Data.List ( intercalate, sort, groupBy, nub, isPrefixOf )
import Data.Char ( isDigit, toLower )
import Expressions
import Data.Ratio
import Utils

main = readFile "input.txt" >>= runCommands . concatMap toCommand . lines

newtype LFunc = LFunc { coef :: [ Ratio Integer ] }
    deriving ( Show )

runLFunc :: LFunc -> [ Ratio Integer ] -> LFunc
runLFunc (LFunc cs) xs = let ( f1, f2 ) = splitAt (length xs) cs
                             ( ns, n ) = ( init f2, last f2 ) in LFunc
    (ns ++ [ n + sum (zipWith (*) f1 xs) ])

freePart :: LFunc -> Ratio Integer
freePart (LFunc f) = last f

showLFunc :: LFunc -> String
showLFunc = intercalate ", " . map showR . coef

showR :: Ratio Integer -> String
showR x = case ( numerator x, denominator x ) of
    ( n, 1 ) -> show n
    ( n, m ) -> show n ++ "/" ++ show m

type Name = String

type Assignment = ( Name, LFunc )

type StrExpr = String

data Command
    = Is Name LFunc | Assign Name StrExpr | Ask Name | Do StrExpr [ Command ]
    deriving ( Show )

toCommand :: String -> [ Command ]
toCommand "" = []
toCommand s
    | "do" `isPrefixOf` s1 = [ parseDo s1 ]
    | "what" `isPrefixOf` s1 = parseWhat s1
    | "assign" `isPrefixOf` s1 = parseAssign s1
    | otherwise = [ parseIs s1 ]
  where
    s1 = init . removeExtraSpaces . concat
        . (\x -> map addSpaces (init x) ++ [ last x ]) . words . map toLower $ s

    addSpaces = \case
        "and" -> " and "
        "is" -> " is "
        "to" -> " to "
        "do" -> "do "
        "what" -> "what "
        "assign" -> "assign "
        "function" -> "function "
        "of" -> "of "
        v
            | last v `elem` ":," -> v ++ [ ' ' ]
        x -> x

    removeExtraSpaces (' ' : ' ' : xs) = removeExtraSpaces (' ' : xs)
    removeExtraSpaces x = x

    parseDo :: String -> Command
    parseDo str = let p = last $ splitWhen (== '{') str
                      [ n, cmd ] = splitWhen (== '}') p in Do n
        (toCommand (dropWhile (== ' ') cmd ++ "!"))

    parseWhat :: String -> [ Command ]
    parseWhat = map Ask . drop 2 . filter (/= "and") . words

    parseAssign :: String -> [ Command ]
    parseAssign = map (\[ y, _, x ] -> Assign x y) . splitOn "and" . tail
        . words

    parseIs :: String -> Command
    parseIs str = let name : _ : f = words str
                      func = unwords f in Is name (parseLFunc func)

    parseLFunc :: String -> LFunc
    parseLFunc s
        | "function" `isPrefixOf` s = LFunc $ map eval
            (splitOn ',' . unwords . drop 3 . words $ s)
        | otherwise = LFunc . (: []) . eval $ s

runCommands c = executeCommands c []

executeCommands :: [ Command ] -> [ Assignment ] -> IO ()
executeCommands [] _ = return ()
executeCommands (v : vs) vardict = case v of
    Ask name -> (putStrLn . showLFunc $ purify name vardict)
        >> executeCommands vs vardict
    Is name f -> executeCommands vs (( name, f ) : vardict)
    Do times cs -> let n = fromIntegral . numerator $ evilEval times vardict
                       new = concat $ replicate n cs in executeCommands
        (new ++ vs) vardict
    Assign name s -> executeCommands vs
        (( name, LFunc . (: []) $ evilEval s vardict ) : vardict)

purify :: String -> [ Assignment ] -> LFunc
purify s vardict = case getBoxesContents s of
    [ name ] -> evilLookup name vardict
    name : xs -> evilLookup name vardict `runLFunc` map (`evilEval` vardict) xs

evilEval :: String -> [ Assignment ] -> Ratio Integer
evilEval str vardict = case prepare str of
    Nothing -> eval str
    Just s -> evilEval s vardict
  where
    prepare str = case break (`elem` ([ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ])) str of
        ( _, "" ) -> Nothing
        ( p1, p2 ) -> let ( u1, u2 ) = break
                              (`notElem` ([ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ]
                                          ++ [ '0' .. '9' ])) p2
                          ( x1, x2 ) = takingBoxed u2
                          ( a1, a2 ) = ( u1 ++ x1, x2 ) in Just $ p1 ++ "("
            ++ (showR . freePart . (`purify` vardict) $ a1) ++ ")" ++ a2

evilLookup :: Name -> [ Assignment ] -> LFunc
evilLookup x xs = case lookup x xs of
    Just x'' -> x''
    Nothing -> error $ "evil: No such thing" ++ show x

takingBoxed :: String -> ( String, String )
takingBoxed = helper 0
  where
    helper _ "" = ( "", "" )
    helper n str @ (v : vs) = let f ( a, b ) = ( v : a, b ) in case ( n, v ) of
        ( n, '[' ) -> f $ helper (n + 1) vs
        ( n, ']' ) -> f $ helper (n - 1) vs
        ( 0, _ ) -> ( "", str )
        _ -> f $ helper n vs

eval :: String -> Ratio Integer
eval = evaluate . pparse . f
  where
    f :: String -> String
    f "" = ""
    f [ x ] = [ x ]
    f (x : y : xs)
        | y == '-' && x `elem` "+*/" = let ( p1, p2 ) = span isDigit xs
            in x : "(-" ++ p1 ++ ")" ++ f p2
        | otherwise = x : f (y : xs)

getBoxesContents :: String -> [ String ]
getBoxesContents s = let ( p1, p2 ) = break (== '[') s in p1 : helper 0 p2
  where
    helper :: Int -> String -> [ String ]
    helper _ [] = []
    helper n (v : vs) = let f xs = (v : head xs) : tail xs in case ( n, v ) of
        ( 1, ']' ) -> "" : helper 0 vs
        ( 0, '[' ) -> helper (n + 1) vs
        ( n, '[' ) -> f $ helper (n + 1) vs
        ( n, ']' ) -> f $ helper (n - 1) vs
        _ -> f $ helper n vs
