-- Build me with: cabal build TP3.hs
-- Execute me with: cabal run -v0 TP3.hs
-- Load me in the REPL with: cabal repl TP3.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Data.List.Split (splitOn)
import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

data Protocol = HTTP | HTTPS deriving (Enum, Eq, Show)

type Domain = String

type TLD = String

type PathSegment = [String]

data URL = MkURL
  { protocol :: Protocol,
    domains :: [Domain],
    tld :: TLD,
    path :: PathSegment
  }
  deriving (Eq, Show)

parseProtocol :: String -> Either String Protocol
parseProtocol "http" = Right HTTP
parseProtocol "https" = Right HTTPS
parseProtocol other = Left $ "Unknown protocol: " ++ other

parseDomain :: String -> Either String ([Domain], TLD)
parseDomain s =
  case splitOn "." s of
    [] -> Left "Empty domain"
    parts ->
      let tld = last parts
          domains = init parts
       in if null domains || domains == [""]
            then Left "No domain before TLD"
            else Right (domains, tld)

parsePath :: String -> PathSegment
parsePath s = splitOn "/" s

parseURL :: String -> Either String URL
parseURL s =
  case splitOn "://" s of
    [protoStr, rest] ->
      case parseProtocol protoStr of
        Right protocole -> case splitOn "/" rest of
          [] -> Left "No domain found"
          [""] -> Left "No domain found"
          (domainStr : pathParts) ->
            case parseDomain domainStr of
              Right (domains, tld) ->
                Right (MkURL protocole domains tld pathParts)
              Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid URL format"

filterURL :: URL -> URL -> Bool
filterURL (MkURL _ fDomains fTLD fPath) (MkURL _ uDomains uTLD uPath) =
  fDomains == uDomains
    && fTLD == uTLD
    && matchPath fPath uPath
  where
    matchPath [] [] = True
    matchPath ("**" : _) _ = True
    matchPath ("*" : fs) (_ : us) = matchPath fs us
    matchPath (f : fs) (u : us)
      | f == u = matchPath fs us
      | otherwise = False
    matchPath _ _ = False

-- QuickCheck and manual tests for parseURL and filterURL

-- Test parseURL with various cases
test_parseURL :: IO ()
test_parseURL = do
  let cases =
        [ ( "http://www.google.fr",
            Right (MkURL HTTP ["www", "google"] "fr" [])
          ),
          ( "http://reddit.com/",
            Right (MkURL HTTP ["reddit"] "com" [""])
          ),
          ( "http://reddit.com/r/haskell",
            Right (MkURL HTTP ["reddit"] "com" ["r", "haskell"])
          ),
          ( "ftp://example.com",
            Left "Unknown protocol: ftp"
          ),
          ( "http://google",
            Left "No domain before TLD"
          ),
          ( "http://.fr",
            Left "No domain before TLD"
          ),
          ( "http://",
            Left "No domain found"
          ),
          ( "not_a_url",
            Left "Invalid URL format"
          )
        ]
  mapM_
    ( \(input, expected) ->
        let result = parseURL input
         in putStrLn $ "" ++ if result == expected then " ✔" else " ✗ " ++ show input ++ " => " ++ show result ++ ", expected: " ++ show expected
    )
    cases

-- Test filterURL with various cases
test_filterURL :: IO ()
test_filterURL = do
  let url1 = MkURL HTTP ["reddit"] "com" ["r", "haskell"]
      url2 = MkURL HTTP ["reddit"] "com" ["r", "ocaml"]
      url3 = MkURL HTTP ["reddit"] "com" ["r", "haskell", "fun"]
      url4 = MkURL HTTP ["lemonde"] "fr" ["emploi"]
      url5 = MkURL HTTP ["lemonde"] "fr" ["politique"]
      filter1 = MkURL HTTP ["reddit"] "com" ["*", "haskell", "**"]
      filter2 = MkURL HTTP ["reddit"] "com" ["**"]
      filter3 = MkURL HTTP ["lemonde"] "fr" ["emploi"]
      filter4 = MkURL HTTP ["reddit"] "com" ["r"]
      filter5 = MkURL HTTP ["reddit"] "com" ["*"]
      filter6 = MkURL HTTP ["lemonde"] "fr" ["*"]
  let cases =
        [ (filter1, url1, True),
          (filter1, url2, False),
          (filter1, url3, True),
          (filter2, url2, True),
          (filter3, url4, True),
          (filter3, url5, False),
          (filter4, url1, False),
          (filter5, url1, False),
          (filter6, url4, True)
        ]
  mapM_
    ( \(filt, url, expected) ->
        let result = filterURL filt url
         in putStrLn $ "" ++ if result == expected then " ✔" else " ✗ " ++ show filt ++ (if expected then " should accept " else " should not accept ") ++ show url
    )
    cases

-- Run all tests
runTests :: IO ()
runTests = do
  putStrLn "Testing parseURL:"
  test_parseURL
  putStrLn ""
  putStrLn "Testing filterURL:"
  test_filterURL
  putStrLn ""

main :: IO ()
main = do
  putStrLn "TP3 is running"
  runTests

-- 1/ Define a type representing URLs you can enter in a browser address bar,
--    i.e. strings of the form:
--    - http://www.google.fr
--    - https://github.com/dmjio/miso/pulls
--    - http://reddit.com/r/haskell
-- 2/ Write a parser from String to your URL type. Its return type must be
--    Either String MyType. The 'Left' case is the error message. Write tests.
-- 3/ We now want to do URL filtering, to grant/forbid access to an URL
--    based on a policy. A filter is a string of the form:
--    - lemonde.fr/emploi
--    - reddit.com/**
--    - reddit.com/*/haskell/**
--    The second filter rules out all URLs of the form reddit.com/suffix, for any 'suffix'
--    The third filter rules out all URLs of the form reddit.com/whatever/haskell/suffix,
--    for any 'whatever' and any 'suffix'. In other words, '*' matches any single
--    segment of the URL while '**' matches all possibles suffixes.
--
--    Write a type for filters
-- 4/ Write a function that taskes an URL and a filter, and returns whether
--    the URL passes the filter. Write tests.
--
-- Use https://hoogle.haskell.org/ to find the functions you need, for example splitOn:
-- https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:splitOn
