{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


import Annotations.Bounds
import Annotations.MultiRec.Annotated
import Annotations.MultiRec.Yield
import Annotations.MultiRec.ShowFam
import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import Generics.MultiRec.Base
import qualified Generics.MultiRec.Show as GS
-- import Generics.MultiRec.Zipper
import Annotations.MultiRec.Annotated

import System.IO
import Control.Monad

import Generics.MultiRec.GHC.GHCTHUseAlt
import Generics.MultiRec.GHC.Instances

import GHC.Paths ( libdir )
import System.Directory


-----------------


import qualified DynFlags              as GHC
import qualified GHC                   as GHC
import qualified MonadUtils            as GHC
import qualified Outputable            as GHC


-- ---------------------------------------------------------------------

data MyAnn = MyAnn String
           deriving (Eq,Show)

-- ---------------------------------------------------------------------

main = do
  Just renamed <- getStuff
  return ()



-- ---------------------------------------------------------------------

targetFile = "./examples/Foo.hs"

getStuff :: IO (Maybe GHC.RenamedSource)
getStuff =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                           [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

            dflags'' = dflags' { GHC.importPaths = ["./src/","./test/testdata/","../test/testdata/"] }

            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                   GHC.ghcLink =  GHC.LinkInMemory }

        _ <- GHC.setSessionDynFlags dflags'''
        GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        modSum <- GHC.getModSummary $ GHC.mkModuleName "Foo"
        p <- GHC.parseModule modSum
        t <- GHC.typecheckModule p

        GHC.setContext [GHC.IIModule (GHC.moduleName $ GHC.ms_mod modSum)]

        g <- GHC.getModuleGraph
        gs <- mapM GHC.showModule g
        GHC.liftIO (putStrLn $ "modulegraph=" ++ (Prelude.show gs))
        let ps  = GHC.pm_parsed_source p
        GHC.liftIO $ putStrLn $ "got parsed source"


        -- RenamedSource -----------------------------------------------
        GHC.liftIO $ putStrLn $ "about to show renamedSource"

        -- GHC.liftIO (putStrLn $ "renamedSource(Ppr)=" ++ (showGhc $ GHC.tm_renamed_source t))
        -- GHC.liftIO (putStrLn $ "\nrenamedSource(showData)=" ++ (SYB.showData SYB.Renamer 0 $ GHC.tm_renamed_source t))

        return (GHC.tm_renamed_source t)



pwd :: IO FilePath
pwd = getCurrentDirectory

