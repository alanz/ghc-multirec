{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import Generics.MultiRec.Base
import Generics.MultiRec.Compos
import qualified Generics.MultiRec.Show as GS
import Generics.MultiRec.Zipper
import System.IO
import Control.Monad

import Generics.MultiRec.GHC.GHCTHUseAlt
import Generics.MultiRec.GHC.Instances

import Bag
import Bag(Bag,bagToList)
-- import Data.Generics
import FastString(FastString)
import GHC.Paths ( libdir )
import RdrName
import OccName
import qualified OccName(occNameString)
import System.Directory


-----------------

import qualified Data.Generics         as SYB
import qualified Data.Generics.Schemes as SYB
import qualified Data.Generics.Aliases as SYB
import qualified GHC.SYB.Utils         as SYB

import Var
import qualified CoreFVs               as GHC
import qualified CoreSyn               as GHC
import qualified DynFlags              as GHC
import qualified ErrUtils              as GHC
import qualified Exception             as GHC
import qualified FastString            as GHC
import qualified GHC                   as GHC
import qualified HscTypes              as GHC
import qualified HsLit                 as GHC
import qualified Lexer                 as GHC
import qualified MonadUtils            as GHC
import qualified Outputable            as GHC
import qualified SrcLoc                as GHC
import qualified StringBuffer          as GHC

-- Working with the GHC AST.

-- * Instantiating the library for GHC AST, starting with RenamedSource

-- ---------------------------------------------------------------------

main = do
  Just renamed <- getStuff
  let renamed' = addOne renamed
  let foo = testZipper renamed'
  -- putStrLn $ "\nfoo=" ++ (SYB.showData SYB.Renamer 0 $ foo)
  putStrLn $ "\nfoo=" ++ showGhc foo
  putStrLn $ "\nrenamed'=" ++ showGhc renamed'
  startEditor renamed
  return ()

-- ---------------------------------------------------------------------
-- playing with compos
{-
-- | Renaming variables using 'compos'

renameVar :: Expr String -> Expr String
renameVar = renameVar' Expr
  where
    renameVar' :: AST String a -> a -> a
    renameVar' Var x = x ++ "_"
    renameVar' p   x = compos renameVar' p x

-- | Test for 'renameVar'

testRename :: Expr String
testRename = renameVar example
-}

addOne :: GHC.RenamedSource -> GHC.RenamedSource
addOne = addOne' RenamedSourceIt
  where
    addOne' :: AST a -> a -> a
    addOne' OverLitValIt (GHC.HsIntegral x) = (GHC.HsIntegral (x+1))
    addOne' p x = compos addOne' p x

-- ---------------------------------------------------------------------

-- | Call this to start the navigation demo.
startEditor :: GHC.RenamedSource -> IO ()
startEditor ast@(g,i,e,d) =
  do
    intro
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    -- loop $ enter HsGroupIt g
    loop $ enter RenamedSourceIt ast

-- ---------------------------------------------------------------------

-- | Main loop. Prints current location, asks for a command and executes
-- a navigation operation depending on that command.
-- loop :: Loc AST I0 (GHC.HsGroup GHC.Name) -> IO ()
loop :: Loc AST I0 (GHC.RenamedSource) -> IO ()
loop l =
  do
    putStr $ (showZipper l) ++ " {" ++ {- typeOfFocus l ++ -}  "}"
    cmd <- getChar
    putStr "\r\ESC[2K"
    when (cmd == 'q') $ putStrLn ""
    when (cmd /= 'q') $ do
      let op = case cmd of
                'j'  -> down
                'l'  -> right
                'h'  -> left
                'k'  -> up
                ' '  -> dfnext
                'n'  -> dfnext
                'b'  -> dfprev
                _    -> return
      case op l of
        Nothing -> loop l
        Just l' -> loop l'

-- ---------------------------------------------------------------------

testZipper2 :: GHC.RenamedSource -> Maybe (GHC.HsGroup GHC.Name)
testZipper2 renamed@(g,i,e,d) =
    -- enter LImportDeclIt   >>>
    enter HsGroupIt   >>>
    dfnext         >=>
    update solve >>>
    leave        >>>
    return        $  g
  where
    solve :: AST ix -> ix -> ix
    -- solve OveerLit _ = Const 42
    solve HsValBindsLRIt (GHC.ValBindsOut [x1,x2] y) = GHC.ValBindsOut [x1] y
    solve _    x = error "foo" --  x

-- ---------------------------------------------------------------------

-- | Show the current location, with the focus being highlighted in red.
-- showZipper :: Loc AST I0 (GHC.HsGroup GHC.Name) -> String
showZipper :: Loc AST I0 (GHC.RenamedSource) -> String
showZipper l = (GS.spaces $ map ($ 0) $ unK0 (foldZipper focus (\ p x -> K0 (GS.hShowsPrecAlg p x)) l)) ""
  where focus :: AST ix -> ix -> K0 ([Int -> ShowS]) ix
        focus ix x = K0 [\ n -> ("\ESC[01;31m" ++) . GS.showsPrec ix n x . ("\ESC[00m" ++)]

-- ---------------------------------------------------------------------

testZipper :: GHC.RenamedSource -> Maybe (GHC.HsGroup GHC.Name)
testZipper renamed@(g,i,e,d) =
    -- enter LImportDeclIt   >>>
    enter HsGroupIt   >>>
    down         >=>
    -- down         >=>
    -- right        >=>
    update solve >>>
    leave        >>>
    return        $  g
  where
    solve :: AST ix -> ix -> ix
    -- solve OveerLit _ = Const 42
    -- solve HsValBindsLRIt (GHC.ValBindsOut [x1,x2] y) = GHC.ValBindsOut [x1] y
    solve HsValBindsLRIt (GHC.ValBindsOut [x1,x2] y) = GHC.ValBindsOut [x2] y
    solve _    x = error "foo" --  x

-- ---------------------------------------------------------------------

-- | Introductory help message.
intro :: IO ()
intro =
  putStrLn "h: left, j: down, k: up, l: right, q: quit, n,[space]: df lr traversal, b: df rl traversal"

-- ---------------------------------------------------------------------

targetFile = "./examples/Foo.hs"

-- getStuff :: IO ()

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

