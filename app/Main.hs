module Main where

main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml =
  makeHtml
    "Hello title"
    (h1_ "Hello, world!" <> p_ "P content")

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


html_ :: [Char] -> [Char]
html_ = el "html"

body_ :: [Char] -> [Char]
body_ = el "body"

head_ :: [Char] -> [Char]
head_ = el "head"

title_ :: [Char] -> [Char]
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

makeHtml :: [Char] -> [Char] -> [Char]
makeHtml title content = html_ (head_ (title_ title) <> body_ content)
