{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read

-- Ex 1

data ParseResult = Ok LogMessage
                 | Failure
  deriving (Show)

data MessageTypeParseResult = MtOk MessageType
                            | MtFailure
  deriving (Show)

parseMessageFromList :: MessageType -> [String] -> ParseResult
parseMessageFromList m (x:xs) = case (readMaybe x :: Maybe Int) of
                                      Just a  -> Ok (LogMessage m a (unwords xs))
                                      Nothing -> Failure
parseMessageFromList _ _        = Failure

getMessageType :: String -> String -> MessageTypeParseResult
getMessageType m ecode = case m of
                           "I" -> MtOk Info
                           "W" -> MtOk Warning
                           "E" -> case (readMaybe ecode :: Maybe Int) of
                                       Just a  -> MtOk (Error a)
                                       Nothing -> MtFailure
                           _   -> MtFailure

parseStringList :: [String] -> ParseResult
parseStringList [] = Failure
parseStringList (_:[]) = Failure
parseStringList (x:(y:xs)) = case getMessageType x y of
                               MtOk messageType -> parseMessageFromList messageType (case messageType of
                                                                                     Error _ ->  xs
                                                                                     _ -> y:xs)
                               MtFailure    -> Failure

parseMessage :: String -> LogMessage
parseMessage s = case parseStringList (words s) of
                   Ok a    -> a
                   Failure -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Ex 2

data SomeTimeStamp = Some Int
                   | None

data LogMessageCompareResult = Greater
                             | Equal
                             | Lower
                             | ComparisonError

compareLogMessage :: LogMessage -> LogMessage -> LogMessageCompareResult
compareLogMessage (LogMessage _ t1 _) (LogMessage _ t2 _) = case () of _
                                                                         | t1 > t2   -> Greater
                                                                         | t2 < t1   -> Lower
                                                                         | otherwise -> Equal
compareLogMessage _ _ = ComparisonError


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt            = mt
insert lm Leaf                     = Node Leaf lm Leaf
insert lm mt@(Node left val right) = case (compareLogMessage lm val) of
                                       Greater   -> insert lm right
                                       Lower     -> insert lm left
                                       _ -> mt

-- Ex 3
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:[]) = insert x Leaf
build (x:xs) = insert x (build xs)

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left val right) = (inOrder left) ++ [val] ++ (inOrder right)

-- Ex 5
data SomeString = SomeString String
                | NoString

logMessageToString :: LogMessage -> SomeString
logMessageToString (LogMessage (Error l) _ msg) = case () of _
                                                                   | l < 50    -> NoString
                                                                   | otherwise -> SomeString msg
logMessageToString _                       = NoString

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []     = []
whatWentWrong (x:[]) = case (logMessageToString x) of
                         SomeString s -> [s]
                         _            -> []

whatWentWrong (x:xs) = case (logMessageToString x) of
                         SomeString s -> [s] ++ (whatWentWrong xs)
                         _            -> whatWentWrong xs






