{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ExtendedDefaultRules#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE NoMonomorphismRestriction#-}
module LambdaLuento where

import Lucid 
import Lucid.Base
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Foldable (foldMap)
import Data.List (intersperse)
import Control.Monad

import Slides
import TreeToHtml
import Parser hiding ((<:>))
import Text.Trifecta.Parser
import Text.Trifecta.Result
import qualified Data.Map as M
import Data.Tree
import Data.String
import Control.Applicative
import Text.Parser.Combinators
import Data.List hiding (insert)
import Data.Monoid
import Tally
import Polls
import Utils

reduce :: Monad m => M.Map String (LC String) -> LC String -> HtmlT m ()
reduce defs = reduction . steps
    where steps x = map (ppR . fmap (fmap toHtml) . annotateRedex) 
                    . (x:)  
                    . unfoldr (\e -> (\x->(x,x)) <$> (evaluate e <|> expand defs e))
                    $ x

syntaxTree :: Monad m => LC (HtmlT m ()) -> HtmlT m ()
syntaxTree x = lucidify (htmlTree x)


-- The Actual lecture


slide =  reveal "Pienin ohjelmointikieli" $ do
  sect "Kaikkein pienin ohjelmointikieli" $ do
    subsec "Mitä tänään opitaan?" $ do
        h3_ "Ehkäpä.."
        bullets ["Luodaan tyhjästä hyvin pieni ja ärhäkkä ohjelmointikieli"
                ,"Käydään läpi pari yleiskäsitettä ohjelmoinnista"
                ,strong_ "Opetellaan selviämään formaaleista kielistä" ]

    subsec "Käytännössä" $ do
        h3_ "Järjestely"
        bullets ["Tehkää 3-4 hengen ryhmät (muuten ette ole läsnä luennolla)"
                ,"Vähintään 1 läppäri tai nettivekotin / ryhmä"
                ,"Kynät ja paperia jos ei ole nettiä"
                ]  

  sect "Asiaan" $ do
    subsec "Kieli = Statiikka + Dynamiikka" $ do
     onLeft $ small $ do
        h3_ "Statiikka"
        h4_ "Syntaksi"
        bullets [
              "Kertoo miten ohjelman lauseet rakennetaan"
            , "Ts. Mitä saa kirjoittaa?"
            ]
        h4_ "Tyypitys" 
        bullets [
              "Asiayhteydestä riippuvat säännöt"
              ,"esim. " <:> "1+x" <-> "on sallittu, jos " <:> "x" <-> "on numero"
            ]
     onRight $ small $ do
        h3_ "Dynamiikka " 
        list [
              "Kertoo miten ohjelma suoritetaan"
             ,"esim." 
             ,reduction ["x + 1","41 + 1","42"]<>" jos x on 41"
            ]

    subsec "Pikkukielen Syntaksi" $ do
            h3_ "Pikkukielen ohjelma"
            bullets ["Ohjelma on yksi lause"
                    ,"Suoritettuna ohjelma tulostaa lasketun lauseen"
                    ]
    subsec "Pikkukielen Syntaksi" $ do
            lauseenMaar
            "Kaikki muut merkkijonot hylätään"

    subsec "Esimerkki syntaksista I/III" $ do
            onLeft $ do
                h3_ "Tämä on lause:" 
                lucidify $ htmlTree $ (x --> x) `A` y 
            onRight (note lauseenMaar)

    subsec "Esimerkki syntaksista II/III" $ do
          onLeft $ do
            h3_ ("Tämä ei ole lause: " <:> "(x)(y)")
            bullets [
                     "Ei ole muuttuja "<:>"x,y,z.."
                    ,"Ei ole abstraktio "<:>"{○ => ●}"
                    ,"Ei ole applikaatio "<:>"■(●)"]
          onRight (note lauseenMaar)

    subsec "Esimerkki syntaksista III/III" $ do
            onLeft $ do
                h3_ "Tämä on lause:" 
                syntaxTree "{x=>{y=>x(y)}}"
            onRight (note lauseenMaar)

    subsec "Kysymys" $ do
        h3_ "Mitkä seuraavista ovat lauseita?"
        onLeft $ poll "onkolause" [
             code_ "x"
            ,lam "{x=>y}({x=>y})"
            ,code_ "x y"
            ,code_ "{x(y) => x}"
            ,lam "{x => x(y)}"
            ]
        onRight $ socrativeInfo

    subsec "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["1,2 ja 5"]

    subsec "Pikkukielen tyypitys" $ do
        h3_ "Yksityypitetty kieli"
        bullets ["Vain yksi tyyppi: "<:> "LAUSE"
                ,"Jokainen lause on "<:>"LAUSE"
                ,"Myöhemmin voitaisiin lisätä vaikkapa "<:>"Int"<->"tai "<:>"String"
                ,"(Sukua pythonille, schemelle, rubylle jne..)"]

    secHead "Dynamiikka"

    subsec "Muuttuja I/II" $ do
      onLeft $ do  
        h3_ "Ajatus"
        bullets ["Muuttuja on paikka lauseessa"
                ,esim [lam $ x --> (V "f" `A` V "x" `A` V "z" `A` V "x") --"(x => y[x][z][x])"
                      ,"{○ => f(○)(z)(○)}"]
                ,"Muuttuja on tarkoitettu"<-> strong_ "korvattavaksi"
                ,"Nimellä ei ole väliä"
                ]
      onRight $ do
        h3_ "Näkyvyysalue"
        bullets ["Usealla muuttujalla voi olla sama nimi"
                ,"Sisin abstraktio \"sitoo\" muuttujan"
                ,esim [lam $  x --> (x --> (V "f" `A` V "x"))
                      ,lam $  (V "◊" --> (V "○" --> (V "f" `A` V "○")))
                      ]]

    subsec "Kysymys" $ do
        h3_ "Kuinka monta eri muuttujaa on lausekkeessa"
        box $ lam "{x => x({x=>x(x)})}"
        onLeft $ poll "montakomuuttujaa" . reverse . map span_ $ ["Viisi","Neljä","Kolme","Kaksi","Yksi","Ei yhtään"]

    subsec "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["Kaksi muuttujaa"]

    subsec "Muuttuja II/II" $ do
        h3_ "Vapaus"
        bullets ["Muuttuja on"<->strong_ "vapaa"<->"jos se ei ole sidottu"
                ,"Vapaus on lokaali ominaisuus"
                ,code_ "x"<->"on vapaa lauseessa "<>lam "x({y => y})"
                ,code_ "x"<->"on on sidottu lauseessa "<>lam "{x => x({y => y})}"
                ]

    subsec "Kysymys" $ do
       h3_ "Mitkä muuttujat ovat vapaita lauseessa"
       box "{x¹ => y²(x¹)}({z³=>z³(x⁴)})"
       onLeft $ do 
        span_ "Lauseessa vapaita muuttujia ovat"
        poll "vapaatmuuttujat" $ map span_ 
            ["x¹","y²","z³","x⁴"]
       onRight $ socrativeInfo

    subsec "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["y² ja x⁴"]

    subsec "Dynamiikka" $ do
      h3_ "Miten pienin ohjelmointikieli suoritetaan?"
      numbers ["Etsi osalause joka näyttää tältä:"<:> " {○ => ■}(●)"
              ,"Kirjoita sen paikalle lause '■' missä muuttuja '○' on korvattu"<->
               "lauseella '●'"
              ,"Toista niin kauan kuin voit"]
      h3_ "Korvaus?"
      bullets [strong_ "Vapaan"<->"muuttujan paikalle kirjoitetaan toinen lauseke"
              ,"esim. Korvataan "<->lam x<->"lauseella "<:>lam (y --> y)<->"lauseessa "<>
               lam "x({x => x})(x)"
              ,"tulos on: "<> slam (replace "x({x => x})(x)" "x" "{y=>y}")
               ]

    subsec "Esimerkki" $ do
        h3_ "Laskenta käy esimerkiksi näin:"
        reductionNote
        div_ $ reduce mempty "{x=>f(x)}(y)"
        br_ []
        h3_ "Tai näin"
        div_ $ reduce mempty "{f=>{y=>f(y)}(a)}(g)" 

    subsec "Esimerkki" $ do
      h3_ "Monimutkaisempi esimerkki"
      div_ $ reduce mempty "{x=>{y=>y(x)}(x)}(u => u)"
      reductionNote

    subsec "Kysymys" $ do
        h3_ $ "Mitkä seuraavista lausekkeista tuottavat saman tuloksen kuin lause "
              <:> lam "{x => {x => f(x)(x)}}(z)"<>"?"
        poll "samatulos"
                [lam "z"
                ,lam "f(z)(z)"
                ,lam "{z => {x => f(x)(x)}}"
                ,lam "{z => {z => f(z)(z)}}"
                ,lam "{x => f(z)(x)}"
                ,lam "{x => f(x)(x)}"
                ,lam "{i => {j => f(j)(j)}}(z)"
                ,lam "{i => {j => f(i)(i)}}(z)"
                ]
        onRight socrativeInfo

    subsec "Vastaus" $ do
        h3_ "Vastaus"
        bullets  ["3, 4, 6, ja 7"]

    subsec "Kysymys" $ do
       socrativeInfo
       h3_ ("Ongelma:")
       bullets [ slam "{f => {y => f(y)}}(y)" 
                 <-> "laskettaisiin " <-> slam "{y=>y(y)}"
                , "Tuloksessa on peräkkäin kaksi"
                  <->strong_ "eri"<->"muuttujaa nimellä y"
                , "Vrt."<->slam "{f => {◊ => f(◊)}}(○)" 
                  <-> "laskettaisiin " <-> slam "{◊=>○(◊)}"
               ]
       h3_ "Mitä vastauksen pitäisi olla?"
       poll "aliasoituminen" . map lam $
            ["{y => y(y)}"
            ,"{y => a(y)}"
            ,"{a => y(a)}"
            ,"{y => y(a)}"
            ]

    subsec "Vastaus" $ do
        h3_ "Vastaus"
        bullets $ ["3"]

    subsec "Esimerkki" $ do
      h3_ "Kolmas esimerkki"
      div_ $ reduce mempty "{f => {y => f(y)}}(y)" 
      br_ [] 
      h3_ "Merkintätapa"
      bullets ["Laitetaan pilkku muuttujan perään erottamaan eri muuttujat"]


    secHead "Koodaus"

    subsec "Onko tässä kaikki?" $ do
        h3_ "On!"
        bullets ["Tällä kielellä voi kirjoittaa"<->strong_ "kaikki"<->" ohjelmat (Church, Turing, jne.)"
                ,"Osataan tuottaa ohjelmakoodia"
                ,"Osataan tulkita ohjelmia"
                ,i_ "'Selkeästi'"<->"nähdään, että systeemin voi"<->
                    "toteuttaa tietokoneella"]
        h3_ "Ei!"
        bullets ["Epäilemättä yhtään ohjelmaa ei synny vaikka nyt yrittäisi kuinka.."]

    subsec "Mitä ohjelmointiin tarvitaan?" $ do
        onLeft $ do
            h3_ "Statiikka + Dynamiikka:"
            bullets ["Mitä saa kirjoittaa?","Mitä ohjelma tarkoittaa?"]
        onRight $ do
            h3_ "Mallit + käytänteet"
            bullets ["Miten ajatuksensa voi esittää?"
                    ,"Mitä kannattaa kirjoittaa?" ]

    subsec "Pidempi esimerkki" $ do
        h3_ "Tavoite"
        bullets ["Ohjelma, joka summaa listan lukuja"]
        h3_ "Osatavoitteet, tarvitaan"
        bullets ["numeroita","lista","yhteenlasku"]

    subsec "Numero? I/II" $ do
        h3_ "Vaihtoehto 1"
        bullets ["Lisätään kieleen numerot"]
        h3_ "Vaihtoehto 2"
        bullets ["Keksitään miten numero esitetään lauseena"]

    subsec "Numero? II/II" $ do
        h3_ "Keksitään miten numero esitetään lauseena:"
        bullets ["0 = Ei kertaakaan = "<-> (lam $ f' --> (x --> x))
                ,"1 = Kerran ="<->   (lam $ f' --> (x --> f(x)))
                ,"2 = Kahdesti ="<-> (lam $ f' --> (x --> f(f(x))))
                ,"3 = Kolmasti ="<-> (lam $ f' --> (x --> f(f(f(x)))))
                ,"n = n kertaa ="<-> (lam $ f' --> (x --> 
                                        f((V (u "..n-kertaa f..") `A` x))))
                ]

    subsec "Yhteenlasku" $ do
        h3_ "n+m?"
        bullets ["kaksi = " <>(lam $ f' --> (x --> rf(rf(rx))))
                ,"yksi = "  <>(lam $ f' --> (x --> gf(gx)))
                ,"kaksi + yksi = "<>(lam $ f' --> (x --> rf(rf(gf(gx)))))]
        h3_ "Joten + on"
        bullets [lam "{n => {m => {f => {x => n(f)(m(f)(x))}}}}"]
                     
    subsec "Yhteenlaskun ajatus" $ do
        h3_ "Näyttää monimutkaiselle, on yksinkertaista:"
        br_ []
        div_ $ reduce mempty "{n => {m => {f => {x => n(f)(m(f)(x))}}}}(yksi)(kaksi)" 
        br_ []
        bullets ["Yllä "<:>"kaksi(f)(x) on "<>lam "f(f(x))"<->"ja"
                ,code_ "yksi(f)"<->"on"<->lam "{x=>f(x)}"
                ,"Eli"<-> lam "{x=>f(x)}(f(f(x)))"<->"on"<->u (lam "f(f(f(x)))")
                ]

    secHead "Älä säikähdä, mutta"

    subsec "Esimerkki" $ do
        div_ $ reduce cnumbers "summa(kaksi)(yksi)"

    subsec "Merkintää" $ do
        h3_ "Koska numerot osataan, jatkossa, "
        bullets [code_ (toHtml nimi) <->"tarkoittaa lausetta:" 
                  <-> div_ (lam (fmap toHtml def))
                | (nimi,def) <- M.toList cnumbers]

    subsec "Listat?" $ do
        h3_ "Keksitään miten lista lukuja esitetään lauseena:"
        bullets [ "Tyhjä =" <> div_ (lam "{c=>{n=>n}}")
                , "[kolme] =" <> div_ (lam "{c=>{n=>c(kolme)(n)}}")
                , "[kaksi, kolme] =" <> div_ (lam "{c=>{n=>c(kaksi)(c(kolme)(n))}}")
                , "[yksi,kaksi,kolme] ="
                 <> div_ (lam "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}")]

    subsec "Esimerkki" $ do
        h3_ "Summataan lista [yksi,kaksi,kolme]:"
        div_ (code_ "[yksi,kaksi,kolme](summa)(nolla)")
        div_ (code_ "⇒")
        div_ (reduce mempty 
                     "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}(summa)(nolla)")
        br_ []
        div_ (code_ "eli,  1+(2+(3+(0))")
        br_ []
        div_ (code_ "= 1+2+3 = 6")

    subsec "Miksi abstrahointi on erittäin tärkeää?" $ do
        h3_ "Summataan lista [yksi,kaksi,kolme]:"
        div_ [style_ "font-size:small; columns: 10ex 4; -webkit-columns: 10ex 4;"]
            (reduce cnumbers 
                     "{c=>{n=>c(yksi)(c(kaksi)(c(kolme)(n)))}}(summa)(nolla)")

    secHead "Yhteenveto"

    subsec "Yhteenveto I/III" $ do
        h3_ "Mitä?"
        bullets ["Opettelimme sietämään kaavoja ja abstraktiota"
                ,"Opimme tyypittämättömän lambdalaskennan perusteet"
                ,"Opimme uuden formaalin kielen ja yhden perustan\
                 \ ohjelmointikielille yleensä"]
    subsec "Yhteenveto II/III" $ do
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
    subsec "Yhteenveto III/III" $ do
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

        
socrativeInfo :: Monad m => HtmlT m () 
socrativeInfo = div_ [class_ "note socr"] $ h4_ "Vastaa valintalaatikoilla:" <>
                       numbers ["Mieti & Vastaa","Vakuuta ryhmäsi","Vastaa uudelleen","Tarkasta vastaus"]

reductionNote :: Monad m=> HtmlT m ()
reductionNote = onRight $ note (h4_ "Merkitään" <> 
                        bullets ["poistettavat kohdat "<>strike "punaisella"
                                ,"korvattavat kohdat "<>u "alleviivauksella"
                                ,"Korvaajat "<>u (strike "punaisella ja alleviivauksella")])

lauseenMaar :: Monad m => HtmlT m ()
lauseenMaar = do 
                (h3_ "Lause voi olla")
                bullets [ 
                          strong_ "Muuttuja "<>", eli "<:>"x,y,z,u,w"<>".. jne." 
                        , strong_ "Abstraktio "<>", eli"<:>"{○ => ●}"<>", missä ○ on muuttuja ja ● on lause" 
                        , strong_ "Applikaatio "<>", eli"<:>"■(●)"<>", missä ■ ja ● ovat lauseita"
                        ]

-- Nopsempi lambda
rx,gx, x,y,z,k,i,j :: Monad m => LC (HtmlT m ())
x = V "x"
gx = V (span_ [style_ "color:blue" ] "x")
rx = V (span_ [style_ "color:red" ] "x") 
y = V "y"
z = V "z"
k = V "k"
i = V "i"
j = V "j"
f' = V "f"

rf,gf, f,g,h :: Monad m => LC (HtmlT m ()) -> LC (HtmlT m ())
rf x = V (span_ [style_ "color:red" ] "f") `A` x
f x = V "f" `A` x
gf x = V (span_ [style_ "color:blue" ] "f") `A` x
g x = V "g" `A` x
h x = V "h" `A` x

(V x) --> f = L x f

slam :: Monad m => LC (String) -> HtmlT m ()
slam x = code_ (pp $ fmap toHtml x)

lam :: Monad m => LC (HtmlT m ()) -> HtmlT m ()
lam x = code_ (pp x)

pp (V a)   = a
pp (L a e) = "{"<>a<> " => "<> pp e <> "}"
pp (A a e) = pp a <> "(" <> pp e<>")"

ppR :: Monad m => LC (Reduct (HtmlT m ())) -> HtmlT m ()
ppR (V (P a))      = pp a
ppR (V (Reduct a)) = u (pp a)
ppR (V (Param a))  = u (strike (pp a))
ppR (L (Arg a) e)    = "{"<>strike (a<> " => ")<> ppR e <> "}"
ppR (L (P a) e)    = "{"<>pp a<> " => "<> ppR e <> "}"
ppR (L (Reduct a) e)    = "{"<>u (pp a)<> " => "<> ppR e <> "}"
ppR (A a e)        = ppR a <> "(" <> ppR e<>")"
ppR e = error $ "Non ex:" -- ++show e

hreason a = "Joka on muotoa" <-> a <-> "missä"
htmlTree :: Monad m =>LC (HtmlT m ()) -> Tree (HtmlT m ())
htmlTree (V a)   = Node (a <-> "eli muuttuja") []
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

instance Monad m => IsString (LC (HtmlT m ())) where
    fromString s = case parseString (lexpr<*eof) mempty s of
                    Success a -> fmap toHtml a
                    _ -> error $ "Couldn't parse expr "++s  
