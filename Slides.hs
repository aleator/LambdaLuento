{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ExtendedDefaultRules#-}
{-#LANGUAGE NoMonomorphismRestriction#-}
module Slides where

import Lucid 
import Lucid.Base
import Data.Monoid
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Foldable (foldMap,Foldable)

slideMeta = do
    meta_ [charset_ "utf-8"]
    meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
    meta_ [name_ "viewport", content_ "width=1024, user-scalable=no"]
    
initialize :: Html ()
initialize = script_ "$(function() {$.deck('.slide');});"

navigation :: Html ()
navigation = div_ [makeAttribute "aria-role" "navigation"] 
                (a_ [href_ "#",class_ "deck-prev-link", title_ "previous"] "←" <>
                 a_ [href_ "#",class_ "deck-next-link", title_ "next"] "→")

includeJS :: [T.Text] -> Html ()
includeJS = foldMap (\s-> script_ [src_ s] "")

stylesheet url= 
  link_ [rel_ "stylesheet", media_ "screen", href_ url]

slide :: T.Text -> Html () -> Html ()
slide t c = section_ [class_ "slide"] (h2_ (toHtml t)<> c) 

deckStatus :: Html ()
deckStatus = p_ [class_ "deck-status",makeAttribute "aria-role" "status"]
             (span_ [class_ "deck-status-current"] "" <> "/" <> span_ [class_ "deck-status-total"] "")

boilerPlate :: T.Text -> Html () -> Html ()
boilerPlate titletext content = html_ $ do
                head_ $ do
                    slideMeta
                    title_ (toHtml titletext)
                    stylesheet "deck.js-latest/core/deck.core.css"
                    stylesheet "deck.js-latest/extensions/goto/deck.goto.css"
                    stylesheet "deck.js-latest/extensions/menu/deck.menu.css"
                    stylesheet "deck.js-latest/extensions/navigation/deck.navigation.css"
                    stylesheet "deck.js-latest/extensions/status/deck.status.css"
                    stylesheet "deck.js-latest/extensions/scale/deck.scale.css"

                    stylesheet "deck.js-latest/themes/style/swiss.css"
                    stylesheet "deck.js-latest/themes/transition/fade.css"
                    stylesheet "test.css"
                    stylesheet "tree.css"
                body_ (div_ [class_ "deck-container"] (content<>navigation<>deckStatus))
                includeJS ["deck.js-latest/jquery.min.js"
                          , "deck.js-latest/modernizr.custom.js"
                          , "deck.js-latest/core/deck.core.js"
                          , "deck.js-latest/extensions/menu/deck.menu.js"
                          , "deck.js-latest/extensions/goto/deck.goto.js"
                          , "deck.js-latest/extensions/status/deck.status.js"
                          , "deck.js-latest/extensions/navigation/deck.navigation.js"
                          , "deck.js-latest/extensions/scale/deck.scale.js"]
                initialize


--- Formatting & Helpers

bigTitle :: String -> Html ()
bigTitle t = section_ [class_ "slide"] (h1_ (toHtml t))

reduction :: [Html ()] -> Html ()
reduction []     = mempty
reduction [x]    = code_ x
reduction (x:xs) = code_ x <> br_ [] <> "⇒" <> br_ [] <> reduction xs


bullets, numbers :: Foldable f => f (Html ()) -> Html ()
bullets = ul_ . foldMap li_
numbers = ol_ . foldMap li_

hl x xs = h3_ x <> bullets xs

a <+> b = a<>" "<>b
a <!> b = a<>code_ b
a <!!> b = a<>code_ [style_ "color:#C44"] b

left = div_ [class_ "leftSide"]
right = div_ [class_ "rightSide"]

u = span_ [style_ "text-decoration:underline; text-decoration-color:#A77"]
strike = span_ [style_ "color:#F44"]

note = div_ [class_ "note"]
smallnote = div_ [class_ "note small"]

box :: Html () -> Html ()
box  = div_ [style_ "text-align:center"] . div_ [class_ "box"] . code_

rbox :: Html () -> Html ()
rbox  =  div_ [class_ "box", style_ "text-align:left;display:inline"] . code_ 

esim :: [Html ()] -> Html ()
esim ls = div_ [class_ "note example"]
           (h4_ "esim."<>
            (code_ . mconcat $ intersperse (br_ []) ls))
