{-# LANGUAGE RecordWildCards #-}

module Commands.Insult
    ( run
    , Options(..)
    )
where


data Options = Options
    { insultName :: String
    , insultWith :: String
    }


run :: Options -> IO ()
run Options { .. } = putStrLn
    $ "You suck real hard, " ++ insultName ++ ", and you're " ++ insultWith
