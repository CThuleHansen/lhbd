{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where
import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "04 safer construction"
    (
      (
        h1_ "h1"
        <>
        (
          (p_ "p1")
          <>
          (p_ "p2")
        )
      )
      <>
      (
        (
          ul_
              [
                p_ "ulli1"
              , p_ "ulli2"
              ] <>
            ol_
              [
                p_ "olli1"
              , p_ "olli2"
              ]
        )
        <>
        code_ "a < b"
      )
    )