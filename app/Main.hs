module Main where

main :: IO ()
main = putStrLn (makeHtml "My page title" "My page content")

html_ :: [Char] -> [Char]
html_ xs = "<html>" <> xs <> "</html>"

body_ :: [Char] -> [Char]
body_ xs = "<body>" <> xs <> "</body>"

head_ :: [Char] -> [Char]
head_ xs = "<head>" <> xs <> "</head>"

title_ :: [Char] -> [Char]
title_ xs = "<title>" <> xs <> "</title>"

makeHtml title content = html_ (head_ (title_ title) <> body_ content)
