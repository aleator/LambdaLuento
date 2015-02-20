{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ExtendedDefaultRules#-}
module TreeToHtml where
import Lucid
import Data.Tree
import Data.Monoid
import Data.Foldable (foldMap)

lucidify :: Tree (Html ()) -> Html ()
lucidify t = ul_ [class_ "tree"] (recur t)
 where
  recur (Node a []) = li_ (span_ (a))
  recur (Node a ns) = (li_ . label_) (cb <> span_ (a) <> ul_ (foldMap recur ns))
  cb = input_ [type_ "checkbox"]

t x = p_ (toHtml x)
testHtml t = head_ (link_ [rel_ "stylesheet", type_ "text/css", href_ "tree.css"]) <>
             body_ (h1_ "Säännöt" <>
                    "Lause voi olla jokin seuraavista muodoista" <>
                    ul_ (li_ "Muuttuja, eli x,y,z,u,w.. jne." <>
                         li_ "Abstraktio, eli (○ => ●), missä ○ on muuttuja ja ● on lause" <>
                         li_ "Applikaatio, eli ■[●], missä ■ ja ● ovat lauseita") <>
                    "Muita lauseita ei ole." <>
                    h2_ "Esimerkki" <> 
                    "Näin voidaan perustella, että seuraava merkkijono on lause:" <>
                    p_ (lucidify t))


testTree = Node "start" [
            Node "first" [Node "a" [],Node "b" []]
           ,Node "second" [Node "a" []
                          ,Node "level" [Node "1" [],Node "1" []]
                          ,Node "b" []]
           ,Node "third" [Node "a" [],Node "b" []]
            ]
