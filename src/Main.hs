module Main where

  import Data.Char (isAlphaNum, isSpace)

  newtype Password =
    Password String deriving (Eq, Show)

  newtype Error = 
    Error String deriving (Eq, Show)

  newtype Username =
    Username String deriving (Eq, Show)

  data User = User Username Password 
    deriving (Eq, Show) 

  passwordLength :: String -> Either Error Password
  passwordLength "" = Left $ Error "Your password cannot be empty"
  passwordLength password = 
    case (length password > 20) of
      True -> Left $ Error "Your password cannot be longer than 20 characters"
      False -> Right (Password password) 

  usernameLength :: String -> Either Error Username
  usernameLength username = 
    case (length username > 15) of
      True -> Left $ Error "Your username cannot be longer than 15 characters"
      False -> Right $ Username username
  
  allAlpha :: String -> Either Error String
  allAlpha "" = Left (Error "Your password cannot be empty")
  allAlpha xs = 
    case (all isAlphaNum xs) of 
      False -> Left $ Error "Your password cannot contain whitespace or special characters" 
      True -> Right xs
  
  stripSpace :: String -> Either Error String
  stripSpace "" = Left $ Error "Your password cannot be empty"
  stripSpace (x:xs) = 
    case (isSpace x) of 
      True -> stripSpace xs
      False -> Right (x:xs)

  validatePassword :: Password -> Either Error Password
  validatePassword (Password password) =
    stripSpace password 
    >>= allAlpha 
    >>= passwordLength 

  main :: IO ()
  main = do
    putStrLn "Please enter a password"
    password <- Password <$> getLine
    print (validatePassword password) 


