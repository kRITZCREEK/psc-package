{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           PscPackage
import           Turtle hiding (fold)
import           Data.Foldable (fold)
import           Data.Version (showVersion)
import qualified Options.Applicative as Opts
import qualified Paths_psc_package as Paths
import           System.Environment (getArgs)
import qualified System.IO as IO

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    join $ Opts.handleParseResult . execParserPure opts =<< getArgs
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "Manage package dependencies"
    footerInfo  = Opts.footer $ "psc-package " ++ showVersion Paths.version

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands = (Opts.subparser . fold)
        [ Opts.command "init"
            (Opts.info (initialize <$> optional ((,) <$> (fromString <$> set)
                                                     <*> optional (fromString <$> source))
                                   Opts.<**> Opts.helper)
            (Opts.progDesc "Create a new psc-package.json file"))
        , Opts.command "uninstall"
            (Opts.info (uninstall <$> pkg Opts.<**> Opts.helper)
            (Opts.progDesc "Uninstall the named package"))
        , Opts.command "install"
            (Opts.info (install <$> optional pkg Opts.<**> Opts.helper)
            (Opts.progDesc "Install/update the named package and add it to 'depends' if not already listed. If no package is specified, install/update all dependencies."))
        , Opts.command "build"
            (Opts.info (exec ["purs", "compile"]
                        <$> onlyDeps "Compile only the package's dependencies"
                        <*> passthroughArgs "purs compile"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Install dependencies and compile the current package"))
        , Opts.command "repl"
            (Opts.info (exec ["purs", "repl"]
                        <$> onlyDeps "Load only the package's dependencies"
                        <*> passthroughArgs "purs repl"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Open an interactive environment for PureScript"))
        , Opts.command "dependencies"
            (Opts.info (pure listDependencies)
            (Opts.progDesc "List all (transitive) dependencies for the current package"))
        , Opts.command "sources"
            (Opts.info (pure listSourcePaths)
            (Opts.progDesc "List all (active) source paths for dependencies"))
        , Opts.command "available"
            (Opts.info (listPackages <$> sorted Opts.<**> Opts.helper)
            (Opts.progDesc "List all packages available in the package set"))
        , Opts.command "updates"
            (Opts.info (checkForUpdates <$> apply <*> applyMajor Opts.<**> Opts.helper)
            (Opts.progDesc "Check all packages in the package set for new releases"))
        , Opts.command "verify"
            (Opts.info (verify <$>
                        ((Package . fromString <$> pkg)
                         <|> (VerifyAll <$> optional (fromString <$> after)))
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Verify that the named package builds correctly. If no package is specified, verify that all packages in the package set build correctly."))
        , Opts.command "add-from-bower"
            (Opts.info (addFromBower <$> pkg Opts.<**> Opts.helper)
            (Opts.progDesc "Add a package from the Bower registry to the package set. This requires Bower to be installed on your system."))
        , Opts.command "format"
            (Opts.info (pure formatPackageFile)
            (Opts.progDesc "Format the packages.json file for consistency"))
        , Opts.command "install-all"
            (Opts.info (pure (runCmd installAll))
            (Opts.progDesc "Install all packages available in the package set."))
        , Opts.command "lint"
            (Opts.info (pure (runCmd lint))
            (Opts.progDesc "Format the packages.json file for consistency"))
        , Opts.command "dhall"
            (Opts.info (pure readDhallPackageSet)
            (Opts.progDesc "Format the packages.json file for consistency"))
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"

        source = Opts.strOption $
             Opts.long "source"
          <> Opts.help "The Git repository for the package set"

        set = Opts.strOption $
             Opts.long "set"
          <> Opts.help "The package set tag name"

        apply = Opts.switch $
             Opts.long "apply"
          <> Opts.help "Apply all minor package updates"

        applyMajor = Opts.switch $
             Opts.long "apply-breaking"
          <> Opts.help "Apply all major package updates"

        onlyDeps help = Opts.switch $
             Opts.long "only-dependencies"
          <> Opts.short 'd'
          <> Opts.help help

        passthroughArgs cmd = many $ Opts.strArgument $
             Opts.help ("Options passed through to " <> cmd <> "; use -- to separate")
          <> Opts.metavar ("`" <> cmd <> "`" <> "-options")

        sorted = Opts.switch $
             Opts.long "sort"
          <> Opts.short 's'
          <> Opts.help "Sort packages in dependency order"

        after = Opts.strOption $
             Opts.long "after"
          <> Opts.help "Skip packages before this package during verification"
