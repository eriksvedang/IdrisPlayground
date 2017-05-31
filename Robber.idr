module Robber

import Control.Monad.State

data RobberLang = MkRobber String

shouldExpand : Char -> Bool
shouldExpand c = c `elem` (unpack "qwrtpsdfghjklzxcvbnmQWRTPSDFGHJKLZXCVBNM")

expandOrNot : Char -> List Char
expandOrNot c = if shouldExpand c 
                then [c, 'o', toLower c] 
                else [c]

toRobber : String -> RobberLang
toRobber = MkRobber . pack . concatMap expandOrNot . unpack      








visitOne : Char -> State String ()
visitOne c = do modify (pack . (++ (expandOrNot c)) . unpack)
                pure ()

visit : String -> State String ()
visit text = traverse_ visitOne (unpack text)

stateful : String -> RobberLang
stateful text = MkRobber $ execState (visit text) ""
