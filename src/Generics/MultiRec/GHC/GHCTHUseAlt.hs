{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Generics.MultiRec.GHC.GHCTHUseAlt where

import Generics.MultiRec.Base
import Generics.MultiRec.TH.Alt

-- GHC stuff
import Bag
import BasicTypes
import CoreSyn
import CostCentre
import DynFlags
import FastString
import GHC
import HsSyn
import Name
import Outputable
import TcEvidence
import TypeRep
import Var

-- * Instantiating the library for AST using TH

-- ** Index type

{-
data AST :: * -> * -> * where
  -- HsGroup    :: AST (GHC.HsGroup a)
  -- HsValBinds :: AST (GHC.HsValBinds a)
  -- HsBindLR   :: AST a (HsBindLR Name Name)
  OverLitVal :: AST a (OverLitVal)
  -- HsExpr     :: AST a (HsExpr Name)
  -- HsIPName     :: AST a  HsIPName
-}
data AST :: * -> * where
  ArithSeqInfoIt    :: AST (ArithSeqInfo Name)
  BoxityIt          :: AST (Boxity)
  -- FastStringIt      :: AST (FastString)
  FixityDirectionIt :: AST (FixityDirection)
  FixityIt          :: AST (HsSyn.Fixity)
  FractionalLitIt   :: AST (FractionalLit)
  GRHSsIt           :: AST (GRHSs Name)
  GenLocatedIt      :: AST (GenLocated SrcSpan (HsBindLR Name Name))
  HsArrAppTypeIt    :: AST (HsArrAppType)
  HsBracketIt       :: AST (HsBracket Name)
  -- HsExprIt          :: AST (HsExpr Name)
  HsGroupIt         :: AST (HsGroup Name)
  HsIPBindsIt       :: AST (HsIPBinds Name)
  HsIPNameIt        :: AST (HsIPName)
  HsLitIt           :: AST (HsLit)
  HsLocalBindsIt    :: AST (HsLocalBinds Name)
  HsMatchContextIt  :: AST (HsMatchContext Name)
  HsOverLitIt       :: AST (HsOverLit Name)
  HsQuasiQuoteIt    :: AST (HsQuasiQuote Name)
  HsRecordBindsIt   :: AST (HsRecordBinds Name)
  HsSpliceIt        :: AST (HsSplice Name)
  HsStmtContextIt   :: AST (HsStmtContext Name)
  HsValBindsLRIt    :: AST (HsValBindsLR Name Name)
  LHsBindsLRIt      :: AST (LHsBindsLR Name Name)
  LHsCmdTopIt       :: AST (LHsCmdTop Name)
  LHsExprIt         :: AST (LHsExpr Name)
  LHsTypeIt         :: AST (LHsType Name)
  LIPBindIt         :: AST (LIPBind Name)
  LImportDeclIt     :: AST (LImportDecl Name)
  LMatchIt          :: AST (LMatch Name)
  LPatIt            :: AST (LPat Name)
  LSigIt            :: AST (LSig Name)
  LocatedIt         :: AST (Located Name)
  MatchGroupIt      :: AST (MatchGroup Name)
  MatchIt           :: AST (Match Name)
  NameIt            :: AST (Name)
  OverLitValIt      :: AST (OverLitVal)
  PatIt             :: AST (Pat Name)
  PostTcExprIt      :: AST PostTcExpr
  PostTcTypeIt      :: AST PostTcType
  SrcSpanIt         :: AST (SrcSpan)
  SyntaxExprIt      :: AST (SyntaxExpr Name)
  TcEvBindsIt       :: AST (TcEvBinds)
  TickishIt         :: AST (Tickish Name)


$(deriveEverything
  (DerivOptions {
   familyTypes =
        [ ( [t| Boxity                 |], "BoxityIt" )
        , ( [t| ArithSeqInfo Name      |], "ArithSeqInfoIt"   )
        -- , ( [t| FastString             |], "FastStringIt" )
        , ( [t| FixityDirection        |], "FixityDirectionIt" )
        , ( [t| FractionalLit          |], "FractionalLitIt" )
        , ( [t| GRHSs Name             |], "GRHSsIt" )
        , ( [t| GenLocated SrcSpan (HsBindLR Name Name) |], "GenLocatedIt" )
        , ( [t| HsArrAppType           |], "HsArrAppTypeIt"   )
        , ( [t| HsBracket Name         |], "HsBracketIt" )
        -- , ( [t| HsExpr Name            |], "HsExpr"   )
        , ( [t| HsGroup Name           |], "HsGroupIt" )
        , ( [t| HsIPBinds Name         |], "HsIPBindsIt" )
        , ( [t| HsIPName               |], "HsIPNameIt" )
        , ( [t| HsLit                  |], "HsLitIt"   )
        , ( [t| HsLocalBinds Name      |], "HsLocalBindsIt" )
        , ( [t| HsMatchContext Name    |], "HsMatchContextIt" )
        , ( [t| HsOverLit Name         |], "HsOverLitIt"   )
        , ( [t| HsQuasiQuote Name      |], "HsQuasiQuoteIt"   )
        , ( [t| HsRecordBinds Name     |], "HsRecordBindsIt" )
        , ( [t| HsSplice Name          |], "HsSpliceIt"   )
        , ( [t| HsStmtContext Name     |], "HsStmtContextIt" )
        , ( [t| HsSyn.Fixity           |], "FixityIt" )
        , ( [t| HsValBindsLR Name Name |], "HsValBindsLRIt" )
        , ( [t| LHsBindsLR Name Name   |], "LHsBindsLRIt" )
        , ( [t| LHsCmdTop Name         |], "LHsCmdTopIt" )
        , ( [t| LHsExpr Name           |], "LHsExprIt" )
        , ( [t| LHsType Name           |], "LHsTypeIt" )
        , ( [t| LIPBind Name           |], "LIPBindIt" )
        , ( [t| LImportDecl Name       |], "LImportDeclIt"   )
        , ( [t| LMatch Name            |], "LMatchIt" )
        , ( [t| LPat Name              |], "LPatIt" )
        , ( [t| LSig Name              |], "LSigIt" )
        , ( [t| Located Name           |], "LocatedIt"   )
        , ( [t| Match Name             |], "MatchIt" )
        , ( [t| MatchGroup Name        |], "MatchGroupIt" )
        , ( [t| OverLitVal             |], "OverLitValIt"   )
        , ( [t| Pat Name               |], "PatIt" )
        , ( [t| PostTcExpr             |], "PostTcExprIt" )
        , ( [t| PostTcType             |], "PostTcTypeIt" )
        , ( [t| SrcSpan                |], "SrcSpanIt" )
        , ( [t| SyntaxExpr Name        |], "SyntaxExprIt" )
        , ( [t| TcEvBinds              |], "TcEvBindsIt" )
        , ( [t| Tickish Name           |], "TickishIt"   )
        ],
   indexGadtName = "AST",
   constructorNameModifier = defaultConstructorNameModifier,
   patternFunctorName = "ThePF",
   -- verbose = True,
   verbose = False,
   sumMode = Balanced
  }
 ))

type instance PF AST = ThePF

-- ---------------------------------------------------------------------

-- Show instances

-- |Show a GHC API structure
showGhc :: (Outputable a) => a -> String
showGhc x = showSDoc tracingDynFlags $ ppr x


instance Show TyLit where
    show = showGhc

instance Show TyCon where
    show = showGhc

instance (Outputable a) => Show (Bag a) where
    show = showGhc

instance Show TcEvBinds where
    show = showGhc

instance Show EvBindsVar where
    show = showGhc

instance (OutputableBndr a) => Show (HsType a) where
    show = showGhc

instance (HasOccName a, OutputableBndr a) => Show (ImportDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsCmdTop a) where
    show = showGhc

instance (OutputableBndr a) => Show (Sig a) where
    show = showGhc

instance (OutputableBndr a) => Show (IPBind a) where
    show = showGhc

instance (OutputableBndr a, OutputableBndr b) => Show (HsBindLR a b) where
    show = showGhc

instance (Outputable a) => Show (HsWithBndrs a) where
    show = showGhc

instance (Outputable a) => Show (GenLocated SrcSpan a) where
    show = showGhc

instance (OutputableBndr a) => Show (Match a) where
    show (Match pats mtyp rhs) = "(Match " ++ show pats ++ " " ++ show mtyp ++ " " ++ show rhs ++ ")"

instance (OutputableBndr a) => Show (GRHSs a) where
    -- show (GRHSs rhs local) = "(GRHSs " ++ show rhs ++ " " ++ show local ++ ")"
    show (GRHSs rhs local) = "(GRHSs rhs " ++ show local ++ ")"

instance (OutputableBndr a) => Show (HsLocalBinds a) where
    show = showGhc

instance (OutputableBndr a) => Outputable (Match a) where
    ppr (Match pats mtyp rhs) = ptext (sLit "Match")  <+> ppr pats <+> ppr mtyp <+> ppr rhs

instance (OutputableBndr a) => Outputable (GRHSs a) where
    ppr (GRHSs rhs local) = ptext (sLit "GRHSs") <+> ppr rhs <+> ppr local

instance (OutputableBndr a) =>  Outputable (GRHS a) where
    ppr (GRHS stmts expr) = ptext (sLit "GRHS") <+> ppr stmts <+> ppr expr

-- May need to extend this one
instance Show (HsConDetails a b) where
    show (PrefixCon _)  = "PrefixCon"
    show (RecCon _)     = "RecordCon"
    show (InfixCon _ _) = "InfixCon"

instance (OutputableBndr a, Outputable b) => Show (HsRecField a b) where
    show = showGhc

instance (OutputableBndr a, Outputable b) => Show (HsRecFields a b) where
    show = showGhc

instance (Outputable a) => Show (Tickish a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsQuasiQuote a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsSplice a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsBracket a) where
    show = showGhc

instance (OutputableBndr a) => Show (ArithSeqInfo a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsExpr a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsTupArg a) where
    show = showGhc

instance (OutputableBndr a) => Outputable (HsTupArg a) where
    ppr (Present expr) = ptext (sLit "Present") <+> ppr expr
    ppr (Missing typ) = ptext (sLit "Missing") <+> ppr typ

instance (OutputableBndr a) => Show (MatchGroup a) where
    show = showGhc

instance (OutputableBndr a) => Outputable (MatchGroup a) where
    ppr (MatchGroup matches typ) = ptext (sLit "MatchGroup") <+> ppr matches <+> ppr typ

instance (OutputableBndr a) => Show (HsOverLit a) where
    show = showGhc

instance Show Var where
    show = showGhc

instance Show (HsWrapper) where
    show = showGhc

instance Show (HsSyn.Fixity) where
    show = showGhc

instance Show (Type) where
    show = showGhc

instance Show (DataCon) where
    show = showGhc

instance Show (RecFlag) where
    show = showGhc

instance Show (Name) where
    show = showGhc

instance Show (Module) where
    show = showGhc

instance Show (CostCentre) where
    show = showGhc
