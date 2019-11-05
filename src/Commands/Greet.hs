{-# LANGUAGE RecordWildCards #-}

module Commands.Greet
    ( run
    , Options(..)
    )
where


data Options = Options
    { name :: String
    , enthusiasm :: Int
    }


run :: Options -> IO ()
run Options { .. } =
    putStrLn $ "Hello, " ++ name ++ replicate enthusiasm '!'
