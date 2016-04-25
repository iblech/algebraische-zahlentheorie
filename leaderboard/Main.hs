{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Text.Blaze.Html.Renderer.String
import Text.Printf
import MyHamlet

data Algebraist = MkAlgebraist
    { name     :: String
    , code     :: String
    , nick     :: String
    , url      :: String
    , awards   :: [String]
    , points   :: [Maybe Double]
    , comments :: String
    } deriving (Show,Eq,Read)

data Config = MkConfig
    { totals      :: [Int]
    , newestSheet :: Int
    } deriving (Show,Eq,Read)

longestStreak :: Algebraist -> Int
longestStreak = maximum . streaks

currentStreak :: Algebraist -> Int
currentStreak = last . streaks

streaks :: Algebraist -> [Int]
streaks = (0:) . map (length . filter id) . group . map (maybe False (const True)) . points

strength :: Algebraist -> Algebraist -> Ordering
strength = mconcat
    [ flip (comparing longestStreak)
    , flip (comparing currentStreak)
    , comparing nick
    ]

totalPoints :: Algebraist -> Double
totalPoints = sum . map (maybe 0 id) . points

main :: IO ()
main = do
    (config,ps) <- fmap read $ getContents
    let ps'  = sortBy strength ps
        ps'' = sortBy (mconcat [ flip (comparing totalPoints), comparing nick ]) ps
    writeFile "index.html" $ renderHtml $ renderLeaderboard config ps' []
    forM_ ps $ \p -> do
        writeFile (code p ++ ".html") $ renderHtml $ renderLeaderboard config ps' [p]
    putStrLn $ renderHtml $ renderLeaderboard config ps'' ps

exportCSV :: (Config,[Algebraist]) -> IO ()
exportCSV (config,ps) = do
    let maxs = sum $ totals $ config
    forM_ ps $ \p ->
        printf "0,%s,%d\n" (name p) (round $ min 50 $ 100 * totalPoints p / fromIntegral maxs :: Integer)

renderAlgebraist config showPoints p = [hamlet|
  <tr>
    <td>
      <a href="#{url p}">#{nick p}
    <td>
      $forall sheet <- points p
        $if isJust sheet
          <span class="rect success">■
        $else
          <span class="rect failure">■
    $if showPoints
      <td>^{renderPoints config p}
    $else
      <td>
    <td>#{longestStreak p}
    <td>#{currentStreak p}
    <td>#{concat $ intersperse ", " $ awards p}
|]

renderPoints config p =
    let total  = totalPoints p
        maxs   = sum $ totals $ config
        ratio  = round $ 100 * total / fromIntegral maxs :: Int
        sheets = concat . intersperse ", " $
            zipWith3 (\i n m -> "B" ++ pad 2 '0' (show i) ++ ":" ++ [nbsp] ++ format n m)
                [1..] (points p) (map Just (totals config) ++ repeat Nothing)
        nbsp     = '\xa0'    -- NO-BREAK SPACE
        figspace = '\x2007'  -- FIGURE SPACE
        format n Nothing = init . init . init $ format n (Just 0)  -- noch schlimmerer Hack
        format n (Just m)
            | Nothing <- n
            = replicate 3 figspace ++ "–/" ++ pad 2 figspace (show m)
            | Just n' <- n
            = let n'' = floor n' :: Int  -- schlimmer Hack
              in  if fromIntegral n'' == n'
                      then pad 2 figspace (show n'') ++ ",0/" ++ pad 2 figspace (show m)
                      else pad 2 figspace (show n'') ++ ",5/" ++ pad 2 figspace (show m)
        pad k s str = replicate (k - length str) s ++ str
    in [hamlet|
<span title="#{sheets}">
    #{show ratio} %
|]

renderLeaderboard config ps ps' =
    let format = printf "%02d" :: Int -> String
        sheets = [0..newestSheet config]
    in [hamlet|
$doctype 5
<html lang="de">
  <head>
    <meta charset="utf-8">
    <title>Kommutative Algebra
    <link rel="shortcut icon" href="images/heart.ico">
    <link rel="stylesheet" type="text/css" href="http://fonts.googleapis.com/css?family=Ubuntu:regular,bold&amp;subset=latin,latin-ext">
    <style type="text/css">
      body {
        font-size: 15px;
        line-height: 24px;
        font-weight: lighter;
        font-family: Ubuntu, sans-serif;
        text-align: center;
      }
      h1 { margin: 1em 0 0.5em 0; font-weight:normal; }
      th { padding: 6px; }
      td { padding: 0px 6px 0px 6px; }
      th { background-color: #2d1630; color: white; }
      td:first-child { font-weight: bold; }
      .exam td:nth-child(2) { text-align: right; }
      table { margin-left: auto; margin-right: auto; text-align: left; }
      .moral { border: 3px solid #2d1630; width: 18em; margin-left: auto; margin-right: auto; }
      .moral em { font-style: normal; }
      .poem { width: 32em; padding: 1em; text-align: left; margin-left: auto; margin-right: auto; background-color: #e4999f; }
      .remark { width: 44em; margin-left: auto; margin-right: auto; }
      .rect { font-size: 200%; }
      .success { color: #44a340; }
      .failure { color: #eeeeee; }
  <body>
    <h1>Kommutative Algebra
    <a href="http://brownsharpie.courtneygibbons.org/?p=1253">
      <img src="images/love-commute.jpeg" alt="Love makes the diagram commute." style="width: 500px; height: 333px; border: 0">
    <p>
      <strong>
        Aktuell: <a href="https://www.youtube.com/watch?v=mA402F5K47o">Eugenia Cheng in der Late Show von Stephen Colbert</a>
    <table>
      <tr>
        <th>AlgebraikerIn
        <th>Abgaben
        <th>Punkte
        <th>längste Strähne
        <th>aktuelle Strähne
        <th>besondere Auszeichnungen
      $forall p <- ps
        ^{renderAlgebraist config (elem p ps') p}
    <p>
      Fahre über deine Prozentzahl, um die Punkte auf den einzelnen Blättern einzusehen. Null Punkte bedeuten nur: noch nicht korrigiert.
    <p>
      <em>Du willst deine Übungsblattsträhne verbessern?
      <br>
      <a href="skript.pdf">Skript
      <br>
      <a href="geometrie.pdf">Notizen zur geometrischen Visualisierung von Ringen
      <br>
      <a href="merkblatt-isos.pdf">Notizen zu wichtigen Ringisomorphismen
      <br>
      <a href="merkblatt-ideale.pdf">Notizen zu wichtigen Idealrechenregeln
      <br>
      $forall n <- sheets
        <a href="uebung#{format n}.pdf">Blatt #{show n}
        <br>
      <a href="alle.pdf">Alle Übungsblätter in einer Datei
      <br>
      <a href="http://etherpad.wikimedia.org/p/kommutative-algebra-rueckmeldung">
        Anregungen oder Kritik? Zum Kummerpad.
    <table class="exam">
      <tr><th colspan="3">Ergebnisse der Klausur</th>
      <tr><td>Bayes</td><td>84</td><td>2,0</td>
      <tr><td>blaugrün8</td><td>95</td><td>1,0</td>
      <tr><td>Dunklereli</td><td>100</td><td>1,0</td>
      <tr><td>fg172</td><td>100</td><td>1,0</td>
      <tr><td>Hoffnung</td><td>93</td><td>1,3</td>
      <tr><td>Jippie</td><td>100</td><td>1,0</td>
      <tr><td>KA_4-3-16</td><td>25</td><td>5,0</td>
      <tr><td>Lineal</td><td>98</td><td>1,0</td>
      <tr><td>norisknofun</td><td>64</td><td>3,3</td>
      <tr><td>pangalaktischer Donnergurgler</td><td>100</td><td>1,0</td>
      <tr><td>Smiley-Punkt</td><td>67</td><td>3,0</td>
      <tr><td>Torsion</td><td>58</td><td>3,7</td>
    <p class="poem">
      Das Beste an dem neuen Jahr:<br>
      Es startet gleich mit Algebra!<br>
      Schlangenlemma und das von Zorn,<br>
      da fühlt man sich wie neu geborn'n.<br>
      Aussagen zeigen, auf die ein oder andre Weise,<br>
      am besten jedoch durch direkte Beweise.<br>
      Mittwoch und Freitag, das sind die tollsten Tage.<br>
      Warum? Da ist kommutative Algebra, was soll die Frage?<br>
      Hilbertscher Basissatz, Newton-Verfahren und Korrespondenzsätze,<br>
      das sieht man auf einen Blick:<br>
      Alles gibt Sinn, das ist die Einheit der Mathematik!
    <p class="moral">
      Whenever you meet a <em>functor</em>,<br>
      ask “What is its <em>codensity monad</em>?”.
|]

{-
    <div style="text-align: center">
      <iframe name="embed_readwrite" src="http://etherpad.wikimedia.org/p/kommutative-algebra-rueckmeldung?showControls=true&amp;showChat=false&amp;showLineNumbers=false&amp;useMonospaceFont=false" style="width: 80%; height: 20em;">
-}
