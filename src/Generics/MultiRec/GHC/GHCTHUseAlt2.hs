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
-- import Generics.MultiRec.TH.Alt
import Generics.MultiRec.TH

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
import SrcLoc

-- * Instantiating the library for AST using TH

-- ** Index type

data AST :: * -> * -> * where
  -- ABExport        :: AST a (ABExport Name)
{-
  ArithSeqInfoIt    :: AST a (ArithSeqInfo Name)
  BoxityIt          :: AST a (Boxity)
  FixityDirectionIt :: AST a (FixityDirection)
  FixityIt          :: AST a (HsSyn.Fixity)
  FractionalLitIt   :: AST a (FractionalLit)
  GRHSsIt           :: AST a (GRHSs Name)
  GRHSIt            :: AST a (GRHS Name)
  HsArrAppTypeIt    :: AST a (HsArrAppType)
  HsBindLRIt        :: AST a (HsBindLR Name Name)
  HsBracketIt       :: AST a (HsBracket Name)
  HsGroupIt         :: AST a (HsGroup Name)
  HsIPBindsIt       :: AST a (HsIPBinds Name)
  HsIPNameIt        :: AST a (HsIPName)
-}
  -- HsLit             :: AST a (HsSyn.HsLit)
{-
  HsLocalBindsIt    :: AST a (HsLocalBinds Name)
  HsMatchContextIt  :: AST a (HsMatchContext Name)
  HsOverLitIt       :: AST a (HsOverLit Name)
  HsQuasiQuoteIt    :: AST a (HsQuasiQuote Name)
  HsRecordBindsIt   :: AST a (HsRecordBinds Name)
  HsSpliceIt        :: AST a (HsSplice Name)
  HsStmtContextIt   :: AST a (HsStmtContext Name)
  HsValBindsLRIt    :: AST a (HsValBindsLR Name Name)
  IEIt              :: AST a (IE Name)
  ImportDeclIt      :: AST a (ImportDecl Name)
  LGRHSIt           :: AST a (LGRHS Name)
  LHsBindLRIt       :: AST a (LHsBindLR Name Name)
  LHsBindsIt        :: AST a (LHsBinds Name)
  LHsCmdTopIt       :: AST a (LHsCmdTop Name)
  LHsDeclIt         :: AST a (LHsDecl Name)
  LHsExprIt         :: AST a (LHsExpr Name)
  LHsTypeIt         :: AST a (LHsType Name)
-}
  LIEIt             :: AST a (LIE Name)
{-
  LIPBindIt         :: AST a (LIPBind Name)
  LImportDeclIt     :: AST a (LImportDecl Name)
  LInstDeclIt       :: AST a ( LInstDecl Name)
  LMatchIt          :: AST a (LMatch Name)
  LPatIt            :: AST a (LPat Name)
  LSigIt            :: AST a (LSig Name)
  LStmtLRIt         :: AST a (LStmtLR Name Name)

  LFixitySigIt      :: AST a (LFixitySig Name)
  LAnnDeclIt        :: AST a (LAnnDecl Name)
  LDefaultDeclIt    :: AST a (LDefaultDecl Name)
  LDerivDeclIt      :: AST a (LDerivDecl Name)
  LForeignDeclIt    :: AST a (LForeignDecl Name)
  LRuleDeclIt       :: AST a (LRuleDecl Name)
  LTyClDeclIt       :: AST a (LTyClDecl Name)
  LVectDeclIt       :: AST a (LVectDecl Name)
  LWarnDeclIt       :: AST a (LWarnDecl Name)
  LDocDecl          :: AST a LDocDecl

  LocatedIt         :: AST a (Located Name)


  MatchGroupIt      :: AST a (MatchGroup Name)
  MatchIt           :: AST a (Match Name)
  NameIt            :: AST a (Name)
  OverLitValIt      :: AST a (OverLitVal)
  PatIt             :: AST a (Pat Name)
  PostTcExprIt      :: AST a PostTcExpr
  -- PostTcTypeIt      :: AST PostTcType
  RenamedSourceIt   :: AST a RenamedSource
  StmtLRIt          :: AST a (StmtLR Name Name)
  SyntaxExprIt      :: AST a (SyntaxExpr Name)
  TcEvBindsIt       :: AST a (TcEvBinds)
  TickishIt         :: AST a (Tickish Name)

  -- Maybe elements
  MaybeBoolLIENamesIt :: AST a (Maybe ((Bool, [LIE Name])))

  -- List elements
  ABExportListIt    :: AST a [ABExport Name]
  LGRHSListIt       :: AST a [LGRHS Name]
  LHsBindLRListIt   :: AST a [LHsBindLR Name Name]
  LHsCmdTopListIt   :: AST a [LHsCmdTop Name]
  LHsDeclListIt     :: AST a [LHsDecl Name]
  LHsExprListIt     :: AST a [LHsExpr Name]
  LIEListIt         :: AST a [LIE Name]
  LIPBindListIt     :: AST a [LIPBind Name]
  LImportDeclListIt :: AST a [LImportDecl Name]
  LInstDeclListIt   :: AST a [LInstDecl Name]
  LMatchListIt      :: AST a [LMatch Name]
  LPatListIt        :: AST a [LPat Name]
  LSigListIt        :: AST a [LSig Name]
  LStmtListIt       :: AST a [LStmt Name]
  LTyClDeclListIt   :: AST a [LTyClDecl Name]
  -- RecBindsListIt    :: AST [HsRecField Name (GenLocated SrcSpan (HsExpr Name))]
  RecBindsListIt    :: AST a [HsRecField Name (LHsExpr Name)]
  ValBindsOutListIt :: AST a [(RecFlag, LHsBinds Name)]

  LTyClDeclLitListIt :: AST a [[LTyClDecl Name]]
  LDerivDeclListIt   :: AST a [LDerivDecl Name]
  LFixitySigLitsIt   :: AST a [LFixitySig Name]
  LDefaultDeclListIt :: AST a [LDefaultDecl Name]
  LForeignDeclListIt :: AST a [LForeignDecl Name]
  LWarnDeclListIt    :: AST a [LWarnDecl Name]
  LAnnDeclListIt     :: AST a [LAnnDecl Name]
  LRuleDeclListIt    :: AST a [LRuleDecl Name]
  LVectDeclListIt    :: AST a [LVectDecl Name]
  LDocDeclListIt     :: AST a [LDocDecl]


  -- Tuple elements
  TupRecFlagLHsBindsIt :: AST a (RecFlag, LHsBinds Name)
-}

$(deriveAll ''AST)

{-
$(deriveEverything
  (DerivOptions {
   familyTypes =
        [ ( [t| ABExport Name          |], "ABExportIt" )
        , ( [t| ArithSeqInfo Name      |], "ArithSeqInfoIt"   )

        , ( [t| Boxity                 |], "BoxityIt" )
        , ( [t| FixityDirection        |], "FixityDirectionIt" )
        , ( [t| FractionalLit          |], "FractionalLitIt" )
        , ( [t| GRHS Name              |], "GRHSIt" )
        , ( [t| GRHSs Name             |], "GRHSsIt" )
        , ( [t| HsArrAppType           |], "HsArrAppTypeIt"   )
        , ( [t| HsBindLR Name Name     |], "HsBindLRIt" )
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
        , ( [t| ImportDecl Name        |], "ImportDeclIt"   )
        , ( [t| LAnnDecl Name          |], "LAnnDeclIt" )
        , ( [t| LDefaultDecl Name      |], "LDefaultDeclIt" )
        , ( [t| LDerivDecl Name        |], "LDerivDeclIt" )
        , ( [t| LDocDecl               |], "LDocDecl" )
        , ( [t| LFixitySig Name        |], "LFixitySigIt" )
        , ( [t| LForeignDecl Name      |], "LForeignDeclIt" )
        , ( [t| LGRHS Name             |], "LGRHSIt" )
        , ( [t| LHsBindLR Name Name    |], "LHsBindLRIt" )
        , ( [t| LHsBinds Name          |], "LHsBindsIt" )
        , ( [t| LHsCmdTop Name         |], "LHsCmdTopIt" )
        , ( [t| LHsDecl Name           |], "LHsDeclIt" )
        , ( [t| LHsExpr Name           |], "LHsExprIt" )
        , ( [t| LHsType Name           |], "LHsTypeIt" )
        , ( [t| LIE Name               |], "LIEIt" )
        , ( [t| LIPBind Name           |], "LIPBindIt" )
        , ( [t| LImportDecl Name       |], "LImportDeclIt"   )
        , ( [t| LInstDecl Name         |],   "LInstDeclIt" )
        , ( [t| LMatch Name            |], "LMatchIt" )
        , ( [t| LPat Name              |], "LPatIt" )
        , ( [t| LRuleDecl Name         |], "LRuleDeclIt" )
        , ( [t| LSig Name              |], "LSigIt" )
        , ( [t| LStmtLR Name Name      |], "LStmtLRIt" )
        , ( [t| LTyClDecl Name         |], "LTyClDeclIt" )
        , ( [t| LVectDecl Name         |], "LVectDeclIt" )
        , ( [t| LWarnDecl Name         |], "LWarnDeclIt" )
        , ( [t| Located Name           |], "LocatedIt"   )
        , ( [t| Match Name             |], "MatchIt" )
        , ( [t| MatchGroup Name        |], "MatchGroupIt" )
        , ( [t| OverLitVal             |], "OverLitValIt"   )
        , ( [t| Pat Name               |], "PatIt" )
        , ( [t| PostTcExpr             |], "PostTcExprIt" )
        , ( [t| RenamedSource          |], "RenamedSourceIt" )
        , ( [t| StmtLR Name Name       |], "StmtLRIt" )
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
        -- , ( [t| [HsRecField Name (GenLocated SrcSpan (HsExpr Name))] |],   "RecBindsListIt"    )
        , ( [t| [HsRecField Name (LHsExpr Name)] |],   "RecBindsListIt"    )
        , ( [t| [LMatch Name]         |],   "LMatchListIt"      )
        , ( [t| [LHsCmdTop Name]      |],   "LHsCmdTopListIt"   )
        , ( [t| [LHsBindLR Name Name] |],   "LHsBindLRListIt"   )
        , ( [t| [LImportDecl Name]    |],   "LImportDeclListIt" )
        , ( [t| [LIE Name]            |],   "LIEListIt" )
        , ( [t| [[LTyClDecl Name]]    |],   "LTyClDeclLitListIt" )
        , ( [t| [LDerivDecl Name]    |],   "LDerivDeclListIt" )
        , ( [t| [LFixitySig Name]    |],   "LFixitySigLitsIt" )
        , ( [t| [LDefaultDecl Name]    |],   "LDefaultDeclListIt" )
        , ( [t| [LForeignDecl Name]    |],   "LForeignDeclListIt" )
        , ( [t| [LWarnDecl Name]    |],   "LWarnDeclListIt" )
        , ( [t| [LAnnDecl Name]    |],   "LAnnDeclListIt" )
        , ( [t| [LRuleDecl Name]    |],   "LRuleDeclListIt" )
        , ( [t| [LVectDecl Name]    |],   "LVectDeclListIt" )
        , ( [t| [LDocDecl]    |],   "LDocDeclListIt" )


        -- Maybe elements
        , ( [t| Maybe ((Bool, [LIE Name])) |],   "MaybeBoolLIENamesIt" )
        -- Tuple elements
        , ( [t| (RecFlag, LHsBinds Name) |],   "TupRecFlagLHsBindsIt" )

        ],
   indexGadtName = "AST",
   constructorNameModifier = defaultConstructorNameModifier,
   patternFunctorName = "ThePF",
   verbose = True,
   -- verbose = False,
   sumMode = Balanced
  }
 ))

type instance PF (AST a) = ThePF
-}

-- ---------------------------------------------------------------------
