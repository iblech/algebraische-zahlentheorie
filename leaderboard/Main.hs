{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.List
import Data.Monoid
import Data.Ord
import Text.Blaze.Html.Renderer.String
import Text.Printf
import MyHamlet

data Bool3 = F | T | U deriving (Show,Read,Eq)

data Algebraist = MkAlgebraist
    { name     :: String
    , nick     :: String
    , url      :: String
    , awards   :: [String]
    , sheets   :: [Bool3]
    } deriving (Show,Eq,Read)

data Config = MkConfig
    { newestSheet :: Int
    } deriving (Show,Eq,Read)

longestStreak :: Algebraist -> Int
longestStreak = maximum . streaks

currentStreak :: Algebraist -> Int
currentStreak = last . streaks

streaks :: Algebraist -> [Int]
streaks = (0:) . map (length . filter (== T)) . group . sheets

strength :: Algebraist -> Algebraist -> Ordering
strength = mconcat
    [ flip (comparing longestStreak)
    , flip (comparing currentStreak)
    , comparing nick
    ]

main :: IO ()
main = do
    (config,ps) <- fmap read $ getContents
    let ps'  = sortBy strength ps
    putStrLn $ renderHtml $ renderLeaderboard config ps'

renderAlgebraist p = [hamlet|
  <tr>
    <td>
      <a href="#{url p}">#{nick p}
    <td>
      $forall sheet <- sheets p
        $if sheet == T
          <span class="rect success">■
        $elseif sheet == U
          <span class="rect unknown">☯
        $else
          <span class="rect failure">■
    <td>#{longestStreak p}
    <td>#{currentStreak p}
    <td>#{concat $ intersperse ", " $ awards p}
|]

renderLeaderboard config ps =
    let format = printf "%02d" :: Int -> String
        shs    = [1..newestSheet config]
    in [hamlet|
$doctype 5
<html lang="de">
  <head>
    <meta charset="utf-8">
    <title>Algebraische Zahlentheorie
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
      .unknown { color: #eeeeee; font-size: 135%; }
  <body>
    <h1>Algebraische Zahlentheorie
    <a href="http://brownsharpie.courtneygibbons.org/?p=1253">
      <img src="images/love-commute.jpeg" alt="Love makes the diagram commute." style="width: 500px; height: 333px; border: 0">
    <p>
      <strong>
        Nicht mehr aktuell, aber immer noch gut: <a href="https://www.youtube.com/watch?v=mA402F5K47o">Eugenia Cheng in der Late Show von Stephen Colbert</a>
    <table>
      <tr>
        <th>ZahlentheoretikerIn
        <th>Abgaben
        <th>längste Strähne
        <th>aktuelle Strähne
        <th>besondere Auszeichnungen
      $forall p <- ps
        ^{renderAlgebraist p}
    <p>
      <em>Du willst deine Übungsblattsträhne verbessern?
      <br>
      $forall n <- shs
        <a href="uebung#{format n}.pdf">Blatt #{show n}
        <br>
      <a href="alle.pdf">Alle Übungsblätter in einer Datei
    <p class="moral" style="background-color: #dd434b; color: white; font-weight: bold">
      Bitte Kommentar im <a href="https://etherpad.wikimedia.org/p/RPUjEpIKmv">Kummerkasten</a> abgeben! Vielen Dank. :-)
|]
