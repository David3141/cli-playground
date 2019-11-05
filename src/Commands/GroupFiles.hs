{-# LANGUAGE RecordWildCards #-}

module Commands.GroupFiles
    ( run
    , Options(..)
    )
where

import Control.Monad (mapM_, unless)
import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , listDirectory
    , renameFile
    )


newtype Options = Options{optDir :: String}


{-|
  Given the path to an existing directory, this groups files that share the
  same name (up to their respective file extension) into folders
  named after the shared name. Everything after the first '.' character in a
  file name currently counts as extension and files that don't share their
  name with other files are moved nonetheless.

  Example:
  The following file structure:
  - melons.csv
  - melons.txt
  - someScript
  - soup
  - soup.txt

  Results in:
  - melons/
    - melons.csv
    - melons.txt
  - someScript
    - someScript
  - soup/
    - soup
    - soup.txt
-}
run :: Options -> IO ()
run Options { .. } = do
    isExisting <- doesDirectoryExist optDir

    if isExisting then do
        listDirectory optDir >>= mapM_ (liftIntoFolderWithSameName optDir)
        print $ "Grouped files in " ++ optDir ++ "."

    else
        print $ "Directory " ++ optDir ++ " does not exist."


liftIntoFolderWithSameName :: FilePath -> FilePath -> IO ()
liftIntoFolderWithSameName workingDir file = do
    let newDir = workingDir ++ "/" ++ takeWhile (/= '.') file

    isExisting <- doesDirectoryExist newDir
    unless isExisting $ createDirectory newDir

    renameFile
        (workingDir ++ "/" ++ file)
        (newDir ++ "/" ++ file)
