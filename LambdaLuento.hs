{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ExtendedDefaultRules#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE NoMonomorphismRestriction#-}
module Main where

import Lucid 
import Lucid.Base
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Foldable (foldMap)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad
import Data.Time.Clock

import Slides
import TreeToHtml
import Parser
import Text.Trifecta.Parser
import Text.Trifecta.Result
import qualified Data.Map as M
import Data.Tree
import Data.String
import Control.Applicative
import Text.Parser.Combinators
import Data.List hiding (insert)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Aeson as Aeson
import System.Random
import Tally

main = do
    scorecard <- newIORef mempty
    let page = renderText slides 
        updateScoreCard mcq cookie tally = atomicModifyIORef' scorecard $ \sc ->
            (insert mcq cookie tally sc,())
        getTally mcq = readIORef scorecard >>= return . tally mcq
    quickHttpServe $
        ensureCookie >> route 
         [("index.html",writeLazyText page)
         ,("tree.css", sendFile "tree.css")
         ,("test.css", sendFile "test.css")
         ,("deck.js-latest/", serveDirectory "deck.js-latest/")
         ,("/mcq/:name", method POST $ do
                Just name <- getParam "name"
                Just p <- getParam "check"
                let decodedName = T.decodeUtf8 name
                cookie <- fmap (maybe "X" cookieValue) (getCookie queryCookieName)
                case Aeson.decode (LBS.fromStrict p) of
                    Nothing -> fail "Bad request"
                    Just t -> do
                        liftIO $ updateScoreCard decodedName cookie (toTally t)
                        liftIO (getTally decodedName) 
                            >>= writeLBS . Aeson.encode . relative

         )]
         <|> serveDirectory "fonts"
         <|> ifTop (writeLazyText page)

---------------
-- Cookies

queryCookieName = "TheLambdaQueryCookie"

randomVal = liftIO (BS.pack <$> replicateM 25 (randomRIO (97,122)))
ensureCookie = do
    rc <- getCookie queryCookieName
    case rc of
        Just _ -> return ()
        Nothing -> do
            rv <- randomVal
            curTime   <- liftIO $ getCurrentTime
            let tokenExpires = addUTCTime (24*60*60) curTime  
            let newCookie = Cookie
                             queryCookieName
                             rv
                             (Just tokenExpires)
                             Nothing
                             (Just "/")
                             False
                             True
            modifyResponse (addResponseCookie newCookie)




reduce :: M.Map String (LC String) -> LC String -> Html ()
reduce defs = reduction . steps
    where steps x = map (ppR . fmap (fmap toHtml) . annotateRedex) 
                    . (x:)  
                    . unfoldr (\e -> (\x->(x,x)) <$> (evaluate e <|> expand defs e))
                    $ x

syntaxTree :: LC (Html ()) -> Html ()
syntaxTree x = lucidify (htmlTree x)


-- The Actual lecture


slides = boilerPlate "Pienin ohjelmointikieli" $ do
    bigTitle "Kaikkein pienin ohjelmointikieli"
    slide "Tämä luento" $ do
        h3_ "Miksi pidän tämä luennon?"
        bullets ["Suoritan YO-pedagogisia opintoja"
                ,"Tämä on opetuskoe"]
        h3_ "Koe: Muokattu 'peer instruction'"
        bullets ["Luennot korvataan keskustelemalla konseptikysymyksistä"
                ,"Väite: arvosanat nousevat 10-20% (E. Mazur ym.)"
                ,"Kysymys 1: miten peer instruction hajoaa jos\
                 \ asia on liian vaikeaa"
                ,"Kysymys 2: voiko 'powerpointteja' käyttää haittaamatta oppimista "
                ,span_ [style_ "font-size:small"] 
                    "Kysymys 3: Voiko luentokalvot koodata Haskellilla.."]

    slide "Mitä täällä opitaan?" $ do
        h3_ "Ehkäpä.."
        bullets ["Luodaan tyhjästä hyvin pieni ja ärhäkkä ohjelmointikieli"
                ,"Käydään läpi pari yleiskäsitettä ohjelmoinnista"
                ,strong_ "Opetellaan selviämään formaaleista kielistä" ]
        h3_ "Todennäköisesti.."
        bullets ["Tämä on koe ja kokeet harvoin onnistuvat"
                ,"40 kalvoa. Jää kesken tai mennään yliajalle."
                ,"Tuntuu ahdistavalle"]

    slide "Käytännössä" $ do
        h3_ "Järjestely"
        bullets ["Tehkää 3-4 hengen ryhmät (muuten ette ole läsnä luennolla)"
                ,"Vähintään 1 läppäri tai nettivekotin / ryhmä"
                ,"Kynät ja paperia jos ei ole nettiä"
                ,"Nettivekotin osoitteeseen:"
                  <+>a_ [href_ "http://goo.gl/0iek4a"] 
                        (code_ [style_ "color:blue"] "http://goo.gl/0iek4a")
                ]  

    bigTitle "Asiaan"

    slide "Kieli = Statiikka + Dynamiikka" $ do
     left $ do
        h3_ "Statiikka"
        h4_ "Syntaksi"
        bullets [
              "Kertoo miten ohjelman lauseet rakennetaan"
            , "Ts. Mitä saa kirjoittaa?"
            ]
        h4_ "Tyypitys" 
        bullets [
              "Asiayhteydestä riippuvat säännöt"
              ,"esim. " <!> "1+x" <+> "on sallittu, jos " <!> "x" <+> "on numero"
            ]
     right $ do
        hl "Dynamiikka " [
              "Kertoo miten ohjelma suoritetaan"
             ,"esim." 
             ,reduction ["x + 1","41 + 1","42"]<>" jos x on 41"
            ]

    bigTitle "Statiikka"

    slide "Pikkukielen Syntaksi" $ do
            h3_ "Pikkukielen ohjelma"
            bullets ["Ohjelma on yksi lause"
                    ,"Suoritettuna ohjelma tulostaa lasketun lauseen"
                    ]
            lauseenMaar
            h3_ "Muuta ei ole"
            bullets ["Kaikki muut merkkijonot hylätään"]

    slide "Esimerkki syntaksista I/III" $ do
            left $ do
                h3_ "Tämä on lause:" 
                lucidify $ htmlTree $ (x --> x) `A` y 
            right (note lauseenMaar)

    slide "Esimerkki syntaksista II/III" $ do
          left $ do
            h3_ ("Tämä ei ole lause: " <!> "(x)(y)")
            bullets [
                     "Ei ole muuttuja "<!>"x,y,z.."
                    ,"Ei ole abstraktio "<!>"{○ => ●}"
                    ,"Ei ole applikaatio "<!>"■(●)"]
          right (note lauseenMaar)

    slide "Esimerkki syntaksista III/III" $ do
            left $ do
                h3_ "Tämä on lause:" 
                syntaxTree "{x=>{y=>x(y)}}"
            right (note lauseenMaar)

    slide "Kysymys" $ do
        h3_ "Mitkä seuraavista ovat lauseita?"
        left $ mcq "onkolause" [
             code_ "x"
            ,lam "{x=>y}({x=>y})"
            ,code_ "x y"
            ,code_ "{x(y) => x}"
            ,lam "{x => x(y)}"
            ]
        right $ socrativeInfo

    slide "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["1,2 ja 5"]

    slide "Pikkukielen tyypitys" $ do
        h3_ "Yksityypitetty kieli"
        bullets ["Vain yksi tyyppi: "<!> "LAUSE"
                ,"Jokainen lause on "<!>"LAUSE"
                ,"Myöhemmin voitaisiin lisätä vaikkapa "<!>"Int"<+>"tai "<!>"String"
                ,"(Sukua pythonille, schemelle, rubylle jne..)"]

    bigTitle "Dynamiikka"

    slide "Muuttuja I/II" $ do
      left $ do  
        h3_ "Ajatus"
        bullets ["Muuttuja on paikka lauseessa"
                ,esim [lam $ x --> (V "f" `A` V "x" `A` V "z" `A` V "x") --"(x => y[x][z][x])"
                      ,"{○ => f(○)(z)(○)}"]
                ,"Muuttuja on tarkoitettu"<+> strong_ "korvattavaksi"
                ,"Nimellä ei ole väliä"
                ]
      right $ do
        h3_ "Näkyvyysalue"
        bullets ["Usealla muuttujalla voi olla sama nimi"
                ,"Sisin abstraktio \"sitoo\" muuttujan"
                ,esim [lam $  x --> (x --> (V "f" `A` V "x"))
                      ,lam $  (V "◊" --> (V "○" --> (V "f" `A` V "○")))
                      ]]

    slide "Kysymys" $ do
        h3_ "Kuinka monta eri muuttujaa on lausekkeessa"
        box $ lam "{x => x({x=>x(x)})}"
        left $ mcq "montakomuuttujaa" . reverse . map span_ $ ["Viisi","Neljä","Kolme","Kaksi","Yksi","Ei yhtään"]

    slide "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["Kaksi muuttujaa"]

    slide "Muuttuja II/II" $ do
        h3_ "Vapaus"
        bullets ["Muuttuja on"<+>strong_ "vapaa"<+>"jos se ei ole sidottu"
                ,"Vapaus on lokaali ominaisuus"
                ,code_ "x"<+>"on vapaa lauseessa "<>lam "x({y => y})"
                ,code_ "x"<+>"on on sidottu lauseessa "<>lam "{x => x({y => y})}"
                ]

    slide "Kysymys" $ do
       h3_ "Mitkä muuttujat ovat vapaita lauseessa"
       box "{x¹ => y²(x¹)}({z³=>z³(x⁴)})"
       left $ do 
        span_ "Lauseessa vapaita muuttujia ovat"
        mcq "vapaatmuuttujat" $ map span_ 
            ["x¹","y²","z³","x⁴"]
       right $ socrativeInfo

    slide "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["y² ja x⁴"]

    slide "Dynamiikka" $ do
      h3_ "Miten pienin ohjelmointikieli suoritetaan?"
      numbers ["Etsi osalause joka näyttää tältä:"<!> " {○ => ■}(●)"
              ,"Kirjoita sen paikalle lause '■' missä muuttuja '○' on korvattu"<+>
               "lauseella '●'"
              ,"Toista niin kauan kuin voit"]
      h3_ "Korvaus?"
      bullets [strong_ "Vapaan"<+>"muuttujan paikalle kirjoitetaan toinen lauseke"
              ,"esim. Korvataan "<+>lam x<+>"lauseella "<!>lam (y --> y)<+>"lauseessa "<>
               lam "x({x => x})(x)"
              ,"tulos on: "<> slam (replace "x({x => x})(x)" "x" "{y=>y}")
               ]

    slide "Esimerkki" $ do
        h3_ "Laskenta käy esimerkiksi näin:"
        reductionNote
        div_ $ reduce mempty "{x=>f(x)}(y)"
        br_ []
        h3_ "Tai näin"
        div_ $ reduce mempty "{f=>{y=>f(y)}(a)}(g)" 

    slide "Esimerkki" $ do
      h3_ "Monimutkaisempi esimerkki"
      div_ $ reduce mempty "{x=>{y=>y(x)}(x)}(u => u)"
      reductionNote

    slide "Kysymys" $ do
        h3_ $ "Mitkä seuraavista lausekkeista tuottavat saman tuloksen kuin lause "
              <!> lam "{x => {x => f(x)(x)}}(z)"<>"?"
        mcq "samatulos"
                [lam "z"
                ,lam "f(z)(z)"
                ,lam "{z => {x => f(x)(x)}}"
                ,lam "{z => {z => f(z)(z)}}"
                ,lam "{x => f(z)(x)}"
                ,lam "{x => f(x)(x)}"
                ,lam "{i => {j => f(j)(j)}}(z)"
                ,lam "{i => {j => f(i)(i)}}(z)"
                ]
        right socrativeInfo

    slide "Vastaus" $ do
        h3_ "Vastaus"
        bullets  ["3, 4, 6, ja 7"]

    slide "Kysymys" $ do
       socrativeInfo
       h3_ ("Ongelma:")
       bullets [ slam "{f => {y => f(y)}}(y)" 
                 <+> "laskettaisiin " <+> slam "{y=>y(y)}"
                , "Tuloksessa on peräkkäin kaksi"
                  <+>strong_ "eri"<+>"muuttujaa nimellä y"
                , "Vrt."<+>slam "{f => {◊ => f(◊)}}(○)" 
                  <+> "laskettaisiin " <+> slam "{◊=>○(◊)}"
               ]
       h3_ "Mitä vastauksen pitäisi olla?"
       mcq "aliasoituminen" . map lam $
            ["{y => y(y)}"
            ,"{y => a(y)}"
            ,"{a => y(a)}"
            ,"{y => y(a)}"
            ]

    slide "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["3"]

    slide "Esimerkki" $ do
      h3_ "Kolmas esimerkki"
      div_ $ reduce mempty "{f => {y => f(y)}}(y)" 
      br_ [] 
      h3_ "Merkintätapa"
      bullets ["Laitetaan pilkku muuttujan perään erottamaan eri muuttujat"]


    bigTitle "Koodaus"

    slide "Onko tässä kaikki?" $ do
        h3_ "On!"
        bullets ["Tällä kielellä voi kirjoittaa"<+>strong_ "kaikki"<+>" ohjelmat (Church, Turing, jne.)"
                ,"Osataan tuottaa ohjelmakoodia"
                ,"Osataan tulkita ohjelmia"
                ,i_ "'Selkeästi'"<+>"nähdään, että systeemin voi"<+>
                    "toteuttaa tietokoneella"]
        h3_ "Ei!"
        bullets ["Epäilemättä yhtään ohjelmaa ei synny vaikka nyt yrittäisi kuinka.."]

    slide "Mitä ohjelmointiin tarvitaan?" $ do
        left $ do
            h3_ "Statiikka + Dynamiikka:"
            bullets ["Mitä saa kirjoittaa?","Mitä ohjelma tarkoittaa?"]
        right $ do
            h3_ "Mallit + käytänteet"
            bullets ["Miten ajatuksensa voi esittää?"
                    ,"Mitä kannattaa kirjoittaa?" ]

    slide "Pidempi esimerkki" $ do
        h3_ "Tavoite"
        bullets ["Ohjelma, joka summaa listan lukuja"]
        h3_ "Osatavoitteet, tarvitaan"
        bullets ["numeroita","lista","yhteenlasku"]

    slide "Numero? I/II" $ do
        h3_ "Vaihtoehto 1"
        bullets ["Lisätään kieleen numerot"]
        h3_ "Vaihtoehto 2"
        bullets ["Keksitään miten numero esitetään lauseena"]

    slide "Numero? II/II" $ do
        h3_ "Keksitään miten numero esitetään lauseena:"
        bullets ["0 = Ei kertaakaan = "<+> (lam $ f' --> (x --> x))
                ,"1 = Kerran ="<+>   (lam $ f' --> (x --> f(x)))
                ,"2 = Kahdesti ="<+> (lam $ f' --> (x --> f(f(x))))
                ,"3 = Kolmasti ="<+> (lam $ f' --> (x --> f(f(f(x)))))
                ,"n = n kertaa ="<+> (lam $ f' --> (x --> 
                                        f((V (u "..n-kertaa f..") `A` x))))
                ]

    slide "Yhteenlasku" $ do
        h3_ "n+m?"
        bullets ["kaksi = "  <>(lam $ f' --> (x --> rf(rf(rx))))
                ,"yksi = "  <>(lam $ f' --> (x --> gf(gx)))
                ,"kaksi + yksi = "<>(lam $ f' --> (x --> rf(rf(gf(gx)))))]
        h3_ "Joten + on"
        bullets [lam "{n => {m => {f => {x => n(f)(m(f)(x))}}}}"]
                     
    slide "Yhteenlaskun ajatus" $ do
        h3_ "Näyttää monimutkaiselle, on yksinkertaista:"
        br_ []
        div_ $ reduce mempty "{n => {m => {f => {x => n(f)(m(f)(x))}}}}(yksi)(kaksi)" 
        br_ []
        bullets ["Yllä "<!>"kaksi(f)(x) on "<>lam "f(f(x))"<+>"ja"
                ,code_ "yksi(f)"<+>"on"<+>lam "{x=>f(x)}"
                ,"Eli"<+> lam "{x=>f(x)}(f(f(x)))"<+>"on"<+>u (lam "f(f(f(x)))")
                ]

    bigTitle "Älä säikähdä, mutta"

    slide "Esimerkki" $ do
        div_ $ reduce cnumbers "summa(kaksi)(yksi)"

    slide "Merkintää" $ do
        h3_ "Koska numerot osataan, jatkossa, "
        bullets [code_ (toHtml nimi) <+>"tarkoittaa lausetta:" 
                  <+> div_ (lam (fmap toHtml def))
                | (nimi,def) <- M.toList cnumbers]

    slide "Listat?" $ do
        h3_ "Keksitään miten lista lukuja esitetään lauseena:"
        bullets [ "Tyhjä =" <> div_ (lam "{c=>{n=>n}}")
                , "[kolme] =" <> div_ (lam "{c=>{n=>c(kolme)(n)}}")
                , "[kaksi, kolme] =" <> div_ (lam "{c=>{n=>c(kaksi)(c(kolme)(n))}}")
                , "[yksi,kaksi,kolme] ="
                 <> div_ (lam "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}")]

    slide "Esimerkki" $ do
        h3_ "Summataan lista [yksi,kaksi,kolme]:"
        div_ (code_ "[yksi,kaksi,kolme](summa)(nolla)")
        div_ (code_ "⇒")
        div_ (reduce mempty 
                     "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}(summa)(nolla)")
        br_ []
        div_ (code_ "eli,  1+(2+(3+(0))")
        br_ []
        div_ (code_ "= 1+2+3 = 6")

    slide "Miksi abstrahointi on erittäin tärkeää?" $ do
        h3_ "Summataan lista [yksi,kaksi,kolme]:"
        div_ [style_ "font-size:small; columns: 10ex 4; -webkit-columns: 10ex 4;"]
            (reduce cnumbers 
                     "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}(summa)(nolla)")

    bigTitle "Yhteenveto"

    slide "Yhteenveto I/III" $ do
        h3_ "Mitä?"
        bullets ["Opettelimme sietämään kaavoja ja abstraktiota"
                ,"Opimme tyypittämättömän lambdalaskennan perusteet"
                ,"Opimme uuden formaalin kielen ja yhden perustan\
                 \ ohjelmointikielille yleensä"]
    slide "Yhteenveto II/III" $ do
        h3_ "Miksi?"
        bullets ["Abstrakti ajattelu on pakollista yliopistossa"
                ,"Ohjelmointi on ajattelua ohjelmointikielellä"
                ,"Ohjelmointi on kielen suunnittelua:"
                 <> bullets ["Jokainen aliohjelma lisää uuden sanan kieleen"]
                ,"Lambdalaskenta on käyttökelpoinen työkalu"
                 <> bullets ["Ohjelmointikielten periaatteet"
                            ,"Todistusteoria:\
                             \Georges Conthierin todistus neliväriteoreemalle"
                            ,"Teoreettinen tietojenkäsittelytiede"] ]
    slide "Yhteenveto III/III" $ do
        h3_ "Mitä seuraavaksi?"
        bullets ["Kertaa tämä asia huomenna, ensi viikolla ja kuukauden päästä"]
        h3_ "Kysymyksiä?"

type Context = M.Map String (LC String)
cnumbers :: Context
cnumbers = M.fromList [ 
                       ("nolla", "{f => {x=>x}}")
                     , ("yksi", "{f => {x=>f(x)}}")
                     , ("kaksi", "{f => {x=>f(f(x))}}")
                     , ("kolme", "{f => {x=>f(f(f(x)))}}")
                     , ("summa", "{n => {m => {f => {x => n(f)(m(f)(x))}}}}")]

        
socrativeInfo :: Html () 
socrativeInfo = div_ [class_ "note socr"] $ h4_ "Vastaa valintalaatikoilla:" <>
                       numbers ["Mieti & Vastaa","Vakuuta ryhmäsi","Vastaa uudelleen","Tarkasta vastaus"]

mcq :: T.Text -> [Html ()] -> Html ()
mcq name options = do 
    div_ [class_ "mcq", id_ ("mcq_"<>name)] $ do  
        ol_ (sequence_ [li_ . label_  $
                         (input_ [type_  "checkbox"
                                 ,name_  name
                                 ,value_ (T.pack $  show i)]
                         <> (option)
                         <> div_ [style_ "background:black;width:0%;height:0.5em;\
                                         \ text-align:right; margin:3px;\
                                         \  color:white"
                                 ,class_ "resultbar"] "")
                       | (i,option) <- zip [1..] options])
        button_ [class_ "answerbutton"
                ,onclick_ (submitAnswer name)] "Vastaa"

submitAnswer :: T.Text -> T.Text    
submitAnswer qn = 
   "var checks=[];\
   \$('#mcq_"<>qn<>"')\
   \.find('input')\
   \.each(function(){checks.push($(this).is(':checked'))});\
   \$.post('/mcq/"<>qn<>"',{'check':JSON.stringify(checks)})\
   \.success(function(d){\
   \var parsed = JSON.parse(d);\
   \$('#mcq_"<>qn<>"')\
   \.find('.resultbar')\
   \.each(function(i,e){$(e).css('width',parsed[i]+'%');})\
   \});"
  
{-
-}

reductionNote :: Html ()
reductionNote = right $ note (h4_ "Merkitään" <> 
                        bullets ["poistettavat kohdat "<>strike "punaisella"
                                ,"korvattavat kohdat "<>u "alleviivauksella"
                                ,"Korvaajat "<>u (strike "punaisella ja alleviivauksella")])

lauseenMaar :: Html ()
lauseenMaar = do 
                (h3_ "Lause voi olla")
                bullets [ 
                          strong_ "Muuttuja"<>", eli "<!>"x,y,z,u,w"<>".. jne." 
                        , strong_ "Abstraktio"<>", eli"<!>"{○ => ●}"<>", missä ○ on muuttuja ja ● on lause" 
                        , strong_ "Applikaatio"<>", eli"<!>"■(●)"<>", missä ■ ja ● ovat lauseita"
                        ]

-- Nopsempi lambda
rx,gx, x,y,z,k,i,j :: LC (Html ())
x = V "x"
gx = V (span_ [style_ "color:blue" ] "x")
rx = V (span_ [style_ "color:red" ] "x") 
y = V "y"
z = V "z"
k = V "k"
i = V "i"
j = V "j"
f' = V "f"

rf,gf, f,g,h :: LC (Html ()) -> LC (Html ())
rf x = V (span_ [style_ "color:red" ] "f") `A` x
f x = V "f" `A` x
gf x = V (span_ [style_ "color:blue" ] "f") `A` x
g x = V "g" `A` x
h x = V "h" `A` x

(V x) --> f = L x f

slam :: LC (String) -> Html ()
slam x = code_ (pp $ fmap toHtml x)

lam :: LC (Html ()) -> Html ()
lam x = code_ (pp x)

pp (V a)   = a
pp (L a e) = "{"<>a<> " => "<> pp e <> "}"
pp (A a e) = pp a <> "(" <> pp e<>")"

ppR :: LC (Reduct (Html ())) -> Html ()
ppR (V (P a))      = pp a
ppR (V (Reduct a)) = u (pp a)
ppR (V (Param a))  = u (strike (pp a))
ppR (L (Arg a) e)    = "{"<>strike (a<> " => ")<> ppR e <> "}"
ppR (L (P a) e)    = "{"<>pp a<> " => "<> ppR e <> "}"
ppR (L (Reduct a) e)    = "{"<>u (pp a)<> " => "<> ppR e <> "}"
ppR (A a e)        = ppR a <> "(" <> ppR e<>")"
ppR e = error $ "Non ex:"++show e

hreason a = "Joka on muotoa" <+> a <+> "missä"
htmlTree :: LC (Html ()) -> Tree (Html ())
htmlTree (V a)   = Node (a <+> "eli muuttuja") []
htmlTree x@(L a e) = Node (pp x)
                    [Node (hreason " (○ => ●) ") []
                    ,addDescription ("○ on ") (htmlTree (V a)) 
                    ,addDescription ("● on ")  (htmlTree e)]
htmlTree x@(A a e) = Node (pp x )
                    [Node (hreason "■(●)") []
                    ,addDescription "■ on " (htmlTree a)
                    ,addDescription "● on " (htmlTree e)]


instance IsString (LC String) where
    fromString s = case parseString (lexpr<*eof) mempty s of
                    Success a -> a
                    _ -> error $ "Couldn't parse expr "  ++ s 

instance IsString (LC (Html ())) where
    fromString s = case parseString (lexpr<*eof) mempty s of
                    Success a -> fmap toHtml a
                    _ -> error $ "Couldn't parse expr "++s  
