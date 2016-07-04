module Greetings where

greet :: String
greet = "Greetings" ++ " human"

hello :: String
hello = "hello"

world :: String
world = "world"

main :: IO ()
main = do
    putStrLn greet
    putStrLn helloWorld
    where helloWorld = concat [hello, " ", world]

