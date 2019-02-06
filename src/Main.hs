module Main where
  import Data.Char (isAlphaNum, isSpace)
  
  maxLength :: String -> Maybe String
  maxLength "" = Nothing
  maxLength xs = 
    case (length xs > 20) of
      True -> Nothing
      False -> Just xs

  allAlpha :: String -> Maybe String
  allAlpha "" = Nothing
  allAlpha xs = 
    case (all isAlphaNum xs) of 
      False -> Nothing 
      True -> Just xs

  stripSpace :: String -> Maybe String
  stripSpace "" = Nothing
  stripSpace (x:xs) = 
    case (isSpace x) of 
      True -> stripSpace xs
      False -> Just (x:xs)

  validatePassword :: String -> Maybe String
  validatePassword password =
    stripSpace password >>= allAlpha >>= maxLength

  main :: IO ()
  main = do
    putStrLn "Please enter a password"
    password <- getLine
    print (validatePassword password) 

  checkPassword :: String -> Maybe String
  checkPassword password = 
    case stripSpace password of
     Nothing -> Nothing 
     Just password'  ->
       case allAlpha password' of
         Nothing -> Nothing
         Just password' ->
           case maxLength password' of
             Nothing -> Nothing
             Just password' -> Just password'

  -- second way
  checkPasswrd :: String -> String
  checkPasswrd password =
    case stripSpace password of
      Nothing -> "Your password cannot be empty."
      Just password' -> 
        case allAlpha password' of
          Nothing -> "Your password cannot contain white space or special charcters"
          Just password' -> 
            case maxLength password' of
              Nothing -> "Your password is too long. Your password cannot be longer than 20 characters"
              Just password' -> password'

