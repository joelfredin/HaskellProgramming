module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    case cool of
        True -> putStrLn "eyyyy. What's happenning?"
        False -> putStrLn "pssssh."
    where cool = coolness == "downright frosty yo"