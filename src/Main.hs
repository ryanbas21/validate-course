module Main where

  import Data.Char (isAlphaNum, isSpace)
  import Data.Validation
  import Data.Semigroup

  newtype Password =
    Password String deriving (Eq, Show)

  newtype Error  = 
    Error [String] deriving (Eq, Show)
  
  instance Semigroup  Error where
    Error xs <> Error ys = Error $ xs <> ys
    
  newtype Username =
    Username String deriving (Eq, Show)

  data User = User Username Password 
    deriving (Eq, Show) 
  
  passwordLength :: String -> Validation Error Password
  passwordLength "" = Failure $ Error ["Your password cannot be empty"]
  passwordLength password = 
    case (length password > 20) of
      True -> Failure $ Error ["Your password cannot be longer than 20 characters"] 
      False -> Success (Password password) 

  usernameLength :: String -> Validation Error Username
  usernameLength username = 
    case (length username > 15) of
      True -> Failure $ Error ["Your username cannot be longer than 15 characters"]
      False -> Success $ Username username
  
  allAlpha :: String -> Validation Error  String
  allAlpha "" = Failure $ Error [ "Your password cannot be empty"]
  allAlpha xs = 
    case (all isAlphaNum xs) of 
      False -> Failure $ Error [ "Your password cannot contain whitespace or special characters" ]
      True -> Success xs
  
  stripSpace :: String -> Validation Error  String
  stripSpace "" = Failure $ Error  ["Cannot be left blank"]
  stripSpace (x:xs) = 
    case (isSpace x) of 
      True -> stripSpace xs
      False -> Success (x:xs)

  validatePassword :: Password -> Validation Error Password
  validatePassword (Password password) =
    case stripSpace password of
      Failure err -> Failure err
      Success password' -> 
        allAlpha password' *> passwordLength password'  

  validateUsername :: Username -> Validation Error Username
  validateUsername (Username username) = 
    case stripSpace username of 
      Failure err -> Failure err
      Success name -> 
        allAlpha name *> usernameLength name 

  passwordErrors :: Password -> Validation Error Password
  passwordErrors password = 
    case validatePassword password of
      Failure err -> Failure $ Error ["Invalid Password: "] <> err 
      Success password' -> Success password'

  usernameErrors :: Username -> Validation Error Username
  usernameErrors username =
    case validateUsername username of
      Failure err -> Failure $ Error ["Invalid username" ] <> err
      Success name -> Success name

  errorCoerce :: Error -> [String]
  errorCoerce (Error err) = err

  display :: Username -> Password -> IO ()
  display username password = 
    case makeUser username password of
      Failure err ->   putStrLn $ unlines $ errorCoerce err 
      Success (User (Username name) password) -> putStrLn ("Welcome " ++ name)


  makeUser :: Username -> Password -> Validation Error User
  makeUser name password =
    User <$> usernameErrors name 
    <*> passwordErrors password
  
  main :: IO ()
  main = do
    putStrLn "Please enter a username"
    username <- Username <$> getLine
    putStrLn "Please enter a password"
    password <- Password <$> getLine
    putStrLn ""
    display username password 
