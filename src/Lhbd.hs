{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lhbd 
  where
import qualified Lhbd.Html as Html
import qualified Lhbd.Markup as Markup
import qualified Lhbd.Convert as Convert
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import GHC.IO.Handle
import GHC.IO.Handle.Text

convertSingle :: Html.Title -> Handle -> Handle -> Bool -> IO ()
convertSingle title input output replace = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> Bool ->  IO ()
convertDirectory = error "Not implemented"


argsHandler :: [String] -> IO ()
argsHandler [inputFile,outputFile] = do
  putStrLn ("Processing the input file:" <> inputFile <> " and the output file: " <> outputFile)
  inputContent <- readFile inputFile 
  outputFilenameConfirmed <- confirmOutputFilename outputFile
  let writeResult = writeFile outputFile (process outputFile inputContent)
  if outputFilenameConfirmed
    then writeResult
    else putStrLn "Output filename failed confirmation"
  where 
    confirmOutputFilename :: String -> IO Bool
    confirmOutputFilename fileName = do
      exists <- doesFileExist fileName
      if exists
        then existsHandler
        else pure True
    existsHandler :: IO Bool
    existsHandler = do
      putStrLn "File already exists, do you wish to overwrite? Y for yes or N for n"
      overwrite <- getLine 
      case overwrite of
        "Y" -> pure True
        "N" -> pure False
        _ -> putStrLn "Unknown answer, try again" *> existsHandler

argsHandler _ = putStrLn "error." 

noArgsHandler :: IO ()
noArgsHandler = do
  putStrLn "Enter input html"
  content <- getContents
  putStrLn $ process "Empty Title" content


{- main :: IO ()
main = putStrLn (Html.render myhtml)
-}

myhtml :: Html.Html
myhtml =
  Html.html_
    "04 safer construction"
    (
      (
        Html.h1_ "h1"
        <>
        (
          (Html.p_ "p1")
          <>
          (Html.p_ "p2")
        )
      )
      <>
      (
        (
          Html.ul_
              [
                Html.p_ "ulli1"
              , Html.p_ "ulli2"
              ] <>
            Html.ol_
              [
                Html.p_ "olli1"
              , Html.p_ "olli2"
              ]
        )
        <>
        Html.code_ "a < b"
      )
    )

process :: Html.Title -> String -> String
process title txt = 
  let 
    markup = Markup.parse txt
    html = Convert.convert title markup
  in
    Html.render html

