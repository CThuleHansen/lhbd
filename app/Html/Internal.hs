module Html.Internal where

-- * types
newtype Html
    = Html String

newtype Structure
    = Structure String

type Title
    = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content = 
  Html
    ( el "Html" 
      ( el "Head" (el "Title" (escape title)) 
        <> el "Body" (getStructureString content ))) 

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" .escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

-- Type class instances are exported automatically
instance Semigroup Structure where
    (Structure a) <> (Structure b) = Structure (a <> b)

liElement :: Structure -> String
liElement = el "li" . getStructureString

ul_ :: [Structure] -> Structure
ul_ xs = 
    Structure  
        (el "ul" 
            ( concatMap liElement xs)
        )

ol_ :: [Structure] -> Structure
ol_ xs = 
    Structure
        (el "ol"
            ( concatMap liElement xs )
        )

-- * Render
render :: Html -> String
render (Html s) = s

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure s) = s

escape :: String -> String
escape = 
    let 
        escapeChar c = 
            case c of 
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in
        concat . map escapeChar