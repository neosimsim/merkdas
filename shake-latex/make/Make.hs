module Main
  ( main
  ) where

import           Control.Monad              (when)
import           Data.Maybe                 (fromMaybe)
import           Development.Shake
import           Development.Shake.FilePath (dropDirectory1, dropExtension,
                                             dropFileName, takeFileName, (<.>),
                                             (</>))
import           System.Exit                (ExitCode (ExitSuccess))

buildDir :: String
buildDir = "build"

main :: IO ()
main =
  shakeArgs shakeOptions {shakeFiles = buildDir, shakeThreads = 0} $ do
    want [buildDir </> "document.pdf", buildDir </> "nested/document.pdf"]
    phony "clean" $ do
      putInfo "Cleaning files in _build"
      removeFilesAfter "build" ["//*"]
    buildDir ++
      "//*.pdf" %> \out -> do
        let jobname = dropDirectory1 $ dropExtension out
        let src = jobname <.> "tex"
        let targetDir = dropFileName out
        need [src]
        need ["references.bib"] -- TODO should be customizable
        texCmd <- fromMaybe "xelatex" <$> getEnv "TEX"
        texFlags <- fromMaybe "" <$> getEnv "TEXFLAGS"
        let mkTex =
              cmd_
                texCmd
                "-jobname"
                [jobname]
                "-output-directory"
                [buildDir]
                (words texFlags)
                [src]
        mkTex
        biberNeeded <-
          (ExitSuccess ==) . fromExit <$>
          cmd
            "grep"
            "-q"
            ["Please (re)run Biber on the file:"]
            [buildDir </> jobname <.> "log"]
        -- The heuristic is not perfect. E.g. when metadata of references get
        -- changed this will not hold, only if new references are added.
        when biberNeeded $ do
          cmd_
            "biber"
            "--input-directory"
            [targetDir]
            "-output-directory"
            [targetDir]
            [takeFileName jobname]
          mkTex
        undefinedReferenecs <-
          (ExitSuccess ==) . fromExit <$>
          cmd
            "grep"
            "-q"
            ["LaTeX Warning: There were undefined references\\."]
            [targetDir </> jobname <.> "log"]
        when undefinedReferenecs mkTex
