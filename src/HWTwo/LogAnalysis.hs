{-# OPTIONS_GHC -Wall #-}
module HWTwo.LogAnalysis where

import HWTwo.Log

parseMessage :: String -> LogMessage
parseMessage str = 
  let wordList = words str in
  case wordList of
    ("I":ts:msg)     -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg)     -> LogMessage Warning (read ts) (unwords msg)
    ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
    _                -> Unknown (unwords wordList)

-- Thats some big brain shit
parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert m@(LogMessage _ mts _) (Node lmt t@(LogMessage _ tts _) rmt)
  | mts >= tts  = Node lmt t (insert m rmt)
  | otherwise   = Node (insert m lmt) t rmt
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf 
insert (Unknown _) mt = mt
insert _ mt = mt

build :: [LogMessage] -> MessageTree
build []    = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs m rhs) = inOrder lhs ++ [m] ++ inOrder rhs

-- helper function for grabbing the severity/message
messageSeverity :: LogMessage -> Int
messageSeverity (LogMessage (Error err) _ _) = err
messageSeverity _ = 0

badMessage :: LogMessage -> Bool
badMessage m  
  | (messageSeverity m) >= 50 = True 
  | otherwise                 = False

messageText :: LogMessage -> String
messageText (LogMessage _ _ m) = m
messageText _                  = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = (map messageText) . inOrder $ build (filter badMessage xs)







