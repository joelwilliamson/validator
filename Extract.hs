-- Handle ZIP archives.

{-# LANGUAGE UnicodeSyntax #-}

module Extract where

import Codec.Archive.Zip(Archive,ZipOption(OptDestination),extractFilesFromArchive)
import System.IO.Temp(withSystemTempDirectory)

-- | Extract an archive to a newly created temporary directory. Return the path
-- | to the directory. The directory is in the system temp dir. The caller is
-- | responsible for cleaning up the directory.
extractArchiveToTemp :: Archive → IO FilePath
extractArchiveToTemp arch = withSystemTempDirectory "validator-mod"
                            $ (\dir → extractFilesFromArchive [OptDestination dir] arch >> return dir)
  
