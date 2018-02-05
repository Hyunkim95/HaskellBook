module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's shaking?"
  else
    putStrLn "pshhh."
  where cool =
          coolness == "downright frosty yo"

greetIfCool :: String -> IO ()
greetIfCool2 coolness =
  if cool coolness
    then putStrLn "eyyyy. What's shaking?"
  else
    putStrLn "pshhh"
  where cool v =
          v == "downright frosty yo"
