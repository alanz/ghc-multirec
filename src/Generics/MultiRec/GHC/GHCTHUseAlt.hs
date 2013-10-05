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
import HsExpr
import HsPat
import HsSyn
import Name
import Outputable
import TcEvidence
import TypeRep
import UniqFM
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
  ABExportIt        :: AST (ABExport Name)
  ArithSeqInfoIt    :: AST (ArithSeqInfo Name)
  BoxityIt          :: AST (Boxity)
  FixityDirectionIt :: AST (FixityDirection)
  FixityIt          :: AST (HsSyn.Fixity)
  FractionalLitIt   :: AST (FractionalLit)
  GRHSsIt           :: AST (GRHSs Name)
  HsArrAppTypeIt    :: AST (HsArrAppType)
  HsBindLRIt        :: AST (HsBindLR Name Name)
  HsBracketIt       :: AST (HsBracket Name)
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
  IEIt              :: AST (IE Name)
  ImportDeclIt      :: AST (ImportDecl Name)
  LGRHSIt           :: AST (LGRHS Name)
  LHsBindLRIt       :: AST (LHsBindLR Name Name)
  LHsBindsLRIt      :: AST (LHsBindsLR Name Name)
  LHsBindsIt        :: AST (LHsBinds Name)
  LHsCmdTopIt       :: AST (LHsCmdTop Name)
  LHsDeclIt         :: AST (LHsDecl Name)
  LHsExprIt         :: AST (LHsExpr Name)
  LHsTypeIt         :: AST (LHsType Name)
  LIEIt             :: AST (LIE Name)
  LIPBindIt         :: AST (LIPBind Name)
  -- LImportDeclIt     :: AST (LImportDecl Name)
  LInstDeclIt       :: AST ( LInstDecl Name)
  LMatchIt          :: AST (LMatch Name)
  LPatIt            :: AST (LPat Name)
  LSigIt            :: AST (LSig Name)
  LStmtLRIt         :: AST (LStmtLR Name Name)
  LocatedIt         :: AST (Located Name)
  MatchGroupIt      :: AST (MatchGroup Name)
  MatchIt           :: AST (Match Name)
  NameIt            :: AST (Name)
  OverLitValIt      :: AST (OverLitVal)
  PatIt             :: AST (Pat Name)
  PostTcExprIt      :: AST PostTcExpr
  PostTcTypeIt      :: AST PostTcType
  RenamedSourceIt   :: AST RenamedSource
  StmtLRIt          :: AST (StmtLR Name Name)
  SyntaxExprIt      :: AST (SyntaxExpr Name)
  TcEvBindsIt       :: AST (TcEvBinds)
  TickishIt         :: AST (Tickish Name)

  -- Maybe elements
  MaybeBoolLIENamesIt :: AST (Maybe ((Bool, [LIE Name])))

  -- List elements
  ABExportListIt    :: AST [ABExport Name]
  LGRHSListIt       :: AST [LGRHS Name]
  LHsBindLRListIt   :: AST [LHsBindLR Name Name]
  LHsCmdTopListIt   :: AST [LHsCmdTop Name]
  LHsDeclListIt     :: AST [LHsDecl Name]
  LHsExprListIt     :: AST [LHsExpr Name]
  LIEListIt         :: AST [LIE Name]
  LIPBindListIt     :: AST [LIPBind Name]
  LImportDeclListIt :: AST [LImportDecl Name]
  LInstDeclListIt   :: AST [LInstDecl Name]
  LMatchListIt      :: AST [LMatch Name]
  LPatListIt        :: AST [LPat Name]
  LSigListIt        :: AST [LSig Name]
  LStmtListIt       :: AST [LStmt Name]
  LTyClDeclListIt   :: AST [LTyClDecl Name]
  RecBindsListIt    :: AST [HsRecField Name (GenLocated SrcSpan (HsExpr Name))]
  ValBindsOutListIt :: AST [(RecFlag, LHsBinds Name)]

$(deriveEverything
  (DerivOptions {
   familyTypes =
        [ ( [t| ABExport Name          |], "ABExportIt" )
        , ( [t| Boxity                 |], "BoxityIt" )
        , ( [t| ArithSeqInfo Name      |], "ArithSeqInfoIt"   )
        , ( [t| FixityDirection        |], "FixityDirectionIt" )
        , ( [t| FractionalLit          |], "FractionalLitIt" )
        , ( [t| GRHSs Name             |], "GRHSsIt" )
        , ( [t| LGRHS Name             |], "LGRHSIt" )
        , ( [t| HsArrAppType           |], "HsArrAppTypeIt"   )
        , ( [t| HsBindLR Name Name     |], "HsBindLRIt" )
        , ( [t| LHsBindLR Name Name    |], "LHsBindLRIt" )
        , ( [t| HsBracket Name         |], "HsBracketIt" )
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
        , ( [t| IE Name                |], "IEIt" )
        , ( [t| LIE Name               |], "LIEIt" )
        , ( [t| LHsBinds Name          |], "LHsBindsIt" )
        -- , ( [t| LHsBindsLR Name Name   |], "LHsBindsLRIt" )
        , ( [t| LHsCmdTop Name         |], "LHsCmdTopIt" )
        , ( [t| LHsDecl Name           |], "LHsDeclIt" )
        , ( [t| LHsExpr Name           |], "LHsExprIt" )
        , ( [t| LHsType Name           |], "LHsTypeIt" )
        , ( [t| LInstDecl Name         |],   "LInstDeclIt" )
        , ( [t| LIPBind Name           |], "LIPBindIt" )
        -- , ( [t| LImportDecl Name       |], "LImportDeclIt"   )
        , ( [t| ImportDecl Name        |], "ImportDeclIt"   )
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
        , ( [t| RenamedSource          |], "RenamedSourceIt" )
        , ( [t| (LStmtLR Name Name)    |], "LStmtLRIt" )
        , ( [t| (StmtLR Name Name)     |], "StmtLRIt" )
        , ( [t| SyntaxExpr Name        |], "SyntaxExprIt" )
        , ( [t| TcEvBinds              |], "TcEvBindsIt" )
        , ( [t| Tickish Name           |], "TickishIt"   )

        -- List elements
        , ( [t| [(RecFlag, LHsBinds Name)] |], "ValBindsOutListIt"   )
        , ( [t| [ABExport Name]       |],   "ABExportListIt" )
        , ( [t| [LGRHS Name]          |],   "LGRHSListIt" )
        , ( [t| [LHsDecl Name]        |],   "LHsDeclListIt" )
        , ( [t| [LHsExpr Name]        |],   "LHsExprListIt" )
        , ( [t| [LIPBind Name]        |],   "LIPBindListIt" )
        , ( [t| [LInstDecl Name]      |],   "LInstDeclListIt" )
        , ( [t| [LSig Name]           |],   "LSigListIt" )
        , ( [t| [LStmt Name]          |],   "LStmtListIt" )
        , ( [t| [LTyClDecl Name]      |],   "LTyClDeclListIt" )
        , ( [t| [LPat Name]           |],   "LPatListIt"        )
        , ( [t| [HsRecField Name (GenLocated SrcSpan (HsExpr Name))] |],   "RecBindsListIt"    )
        , ( [t| [LMatch Name]         |],   "LMatchListIt"      )
        , ( [t| [LHsCmdTop Name]      |],   "LHsCmdTopListIt"   )
        , ( [t| [LHsBindLR Name Name] |],   "LHsBindLRListIt"   )
        , ( [t| [LImportDecl Name]    |],   "LImportDeclListIt" )
        , ( [t| [LIE Name]            |],   "LIEListIt" )
        -- Maybe elements
        , ( [t| Maybe ((Bool, [LIE Name])) |],   "MaybeBoolLIENamesIt" )
        ],
   indexGadtName = "AST",
   constructorNameModifier = defaultConstructorNameModifier,
   patternFunctorName = "ThePF",
   verbose = True,
   -- verbose = False,
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

instance (OutputableBndr a) => Show (Match a) where
    show (Match pats mtyp rhs) = "(Match " ++ show pats ++ " " ++ show mtyp ++ " " ++ show rhs ++ ")"

instance (OutputableBndr a) => Show (GRHSs a) where
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

instance Show DataCon where
    show = showGhc

instance Show (RecFlag) where
    show = showGhc

instance Show (Name) where
    show = showGhc

instance Show (Module) where
    show = showGhc

instance Show (CostCentre) where
    show = showGhc

instance Show ModuleName where
    show = showGhc

instance (OutputableBndr a) => Show (ABExport a) where
    show = showGhc

instance (OutputableBndr a) => Show (UniqFM a) where
    show = showGhc

instance (OutputableBndr a) => Show (HsDecl a) where
    show = showGhc

instance Show TransForm where
    show = showGhc

instance Outputable TransForm where
    ppr ThenForm  = ptext (sLit "ThenForm")
    ppr GroupForm = ptext (sLit "GroupForm")

instance (OutputableBndr a, OutputableBndr b) => Show (ParStmtBlock a b) where
    show = showGhc

instance Show TcSpecPrags where
    show = showGhc

instance Outputable TcSpecPrags where
    ppr IsDefaultMethod = ptext (sLit "IsDefaultMethod")
    ppr (SpecPrags sps) = ptext (sLit "SpecPrags") <+> ppr sps

instance (OutputableBndr a) => Show (GRHS a) where
    show = showGhc

instance (OutputableBndr a) => Show (InstDecl a) where
    show = showGhc

instance (Outputable a) => Show (Located a) where
    show (GHC.L l x) = "(" ++ showGhc l ++ ":" ++ showGhc x ++ ")"
