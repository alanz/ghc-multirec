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
import SrcLoc

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
  GRHSIt            :: AST (GRHS Name)
  GRHSsIt           :: AST (GRHSs Name)
  HsArrAppTypeIt    :: AST (HsArrAppType)
  HsBindLRIt        :: AST (HsBindLR Name Name)
  HsBracketIt       :: AST (HsBracket Name)
  HsCmdTopIt        :: AST (HsCmdTop Name)
  HsConPatDetailsIt :: AST (HsConPatDetails Name)
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
  HsTupArgIt        :: AST (HsTupArg Name)
  HsTypeIt          :: AST (HsType Name)
  HsValBindsLRIt    :: AST (HsValBindsLR Name Name)
  IEIt              :: AST (IE Name)
  ImportDeclIt      :: AST (ImportDecl Name)
  IPBindIt          :: AST (IPBind Name)
  LAnnDeclIt        :: AST (LAnnDecl Name)
  LDefaultDeclIt    :: AST (LDefaultDecl Name)
  LDerivDeclIt      :: AST (LDerivDecl Name)
  LDocDecl          :: AST LDocDecl
  LFixitySigIt      :: AST (LFixitySig Name)
  LForeignDeclIt    :: AST (LForeignDecl Name)
  LGRHSIt           :: AST (LGRHS Name)
  LHsBindLRIt       :: AST (LHsBindLR Name Name)
  LHsBindsIt        :: AST (LHsBinds Name)
  LHsCmdTopIt       :: AST (LHsCmdTop Name)
  LHsDeclIt         :: AST (LHsDecl Name)
  LHsExprIt         :: AST (LHsExpr Name)
  LHsTypeIt         :: AST (LHsType Name)
  LIEIt             :: AST (LIE Name)
  LIPBindIt         :: AST (LIPBind Name)
  LImportDeclIt     :: AST (LImportDecl Name)
  LInstDeclIt       :: AST (LInstDecl Name)
  LMatchIt          :: AST (LMatch Name)
  LPatIt            :: AST (LPat Name)
  LRuleDeclIt       :: AST (LRuleDecl Name)
  LSigIt            :: AST (LSig Name)
  LStmtLRIt         :: AST (LStmtLR Name Name)
  LTyClDeclIt       :: AST (LTyClDecl Name)
  LVectDeclIt       :: AST (LVectDecl Name)
  LWarnDeclIt       :: AST (LWarnDecl Name)
  LocatedIt         :: AST (Located Name)
  MatchGroupIt      :: AST (MatchGroup Name)
  MatchIt           :: AST (Match Name)
  NameIt            :: AST (Name)
  OverLitValIt      :: AST (OverLitVal)
  ParStmtBlockIt    :: AST (ParStmtBlock Name Name)
  PatIt             :: AST (Pat Name)
  PostTcExprIt      :: AST PostTcExpr
  RenamedSourceIt   :: AST RenamedSource
  StmtLRIt          :: AST (StmtLR Name Name)
  SyntaxExprIt      :: AST (SyntaxExpr Name)
  TcEvBindsIt       :: AST (TcEvBinds)
  TickishIt         :: AST (Tickish Name)
  TyClDeclIt        :: AST (TyClDecl Name)

  -- Maybe elements
  MaybeBoolLIENamesIt :: AST (Maybe ((Bool, [LIE Name])))
  MaybeLHsExprIt      :: AST (Maybe (LHsExpr Name))
  MaybeLHsTypeIt      :: AST (Maybe (LHsType Name)) -- Includes LHsKind
  MaybeSyntaxExprIt   :: AST (Maybe (SyntaxExpr Name))

  -- List elements
  ABExportListIt     :: AST [ABExport Name]
  LAnnDeclListIt     :: AST [LAnnDecl Name]
  LDefaultDeclListIt :: AST [LDefaultDecl Name]
  LDerivDeclListIt   :: AST [LDerivDecl Name]
  LDocDeclListIt     :: AST [LDocDecl]
  LFixitySigLitsIt   :: AST [LFixitySig Name]
  LForeignDeclListIt :: AST [LForeignDecl Name]
  LGRHSListIt        :: AST [LGRHS Name]
  LHsBindLRListIt    :: AST [LHsBindLR Name Name]
  LHsCmdTopListIt    :: AST [LHsCmdTop Name]
  LHsDeclListIt      :: AST [LHsDecl Name]
  LHsExprListIt      :: AST [LHsExpr Name]
  LIEListIt          :: AST [LIE Name]
  LIPBindListIt      :: AST [LIPBind Name]
  LImportDeclListIt  :: AST [LImportDecl Name]
  LInstDeclListIt    :: AST [LInstDecl Name]
  LMatchListIt       :: AST [LMatch Name]
  LPatListIt         :: AST [LPat Name]
  LRuleDeclListIt    :: AST [LRuleDecl Name]
  LSigListIt         :: AST [LSig Name]
  LStmtListIt        :: AST [LStmt Name]
  LTyClDeclListIt    :: AST [LTyClDecl Name]
  LTyClDeclLitListIt :: AST [[LTyClDecl Name]]
  LVectDeclListIt    :: AST [LVectDecl Name]
  LWarnDeclListIt    :: AST [LWarnDecl Name]
  RecBindsListIt     :: AST [HsRecField Name (LHsExpr Name)]
  ValBindsOutListIt  :: AST [(RecFlag, LHsBinds Name)]
  VarListIt           :: AST [Var] -- Includes TyVar and EvVar
  HsTupArgListIt      :: AST [HsTupArg Name]
  DataConListIt       :: AST [DataCon]
  PendingSpliceListIt :: AST [PendingSplice]
  ParStmtBlockListIt  :: AST [ParStmtBlock Name Name]

  -- Tuple elements
  TupRecFlagLHsBindsIt :: AST (RecFlag, LHsBinds Name)
  TupBoolLIEListIt     :: AST (Bool, [LIE Name])

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
        , ( [t| HsCmdTop Name          |], "HsCmdTopIt" )
        , ( [t| HsConPatDetails Name   |], "HsConPatDetailsIt" )
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
        , ( [t| HsTupArg Name          |], "HsTupArgIt" )
        , ( [t| HsType Name            |], "HsTypeIt" )
        , ( [t| HsValBindsLR Name Name |], "HsValBindsLRIt" )
        , ( [t| IE Name                |], "IEIt" )
        , ( [t| ImportDecl Name        |], "ImportDeclIt"   )
        , ( [t| IPBind Name            |], "IPBindIt" )
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
        , ( [t| ParStmtBlock Name Name |], "ParStmtBlockIt" )
        , ( [t| Pat Name               |], "PatIt" )
        , ( [t| PostTcExpr             |], "PostTcExprIt" )
        , ( [t| RenamedSource          |], "RenamedSourceIt" )
        , ( [t| StmtLR Name Name       |], "StmtLRIt" )
        , ( [t| SyntaxExpr Name        |], "SyntaxExprIt" )
        , ( [t| TcEvBinds              |], "TcEvBindsIt" )
        , ( [t| Tickish Name           |], "TickishIt"   )
        , ( [t| (TyClDecl Name)        |], "TyClDeclIt" )

        -- List elements
        , ( [t| [(RecFlag, LHsBinds Name)] |], "ValBindsOutListIt"   )
        , ( [t| [ABExport Name]            |],   "ABExportListIt" )
        , ( [t| [DataCon]                  |],   "DataConListIt" )
        , ( [t| [HsRecField Name (LHsExpr Name)] |],   "RecBindsListIt"    )
        , ( [t| [HsTupArg Name]            |],   "HsTupArgListIt" )
        , ( [t| [LAnnDecl Name]            |],   "LAnnDeclListIt" )
        , ( [t| [LDefaultDecl Name]        |],   "LDefaultDeclListIt" )
        , ( [t| [LDerivDecl Name]          |],   "LDerivDeclListIt" )
        , ( [t| [LDocDecl]                 |],   "LDocDeclListIt" )
        , ( [t| [LFixitySig Name]          |],   "LFixitySigLitsIt" )
        , ( [t| [LForeignDecl Name]        |],   "LForeignDeclListIt" )
        , ( [t| [LGRHS Name]               |],   "LGRHSListIt" )
        , ( [t| [LHsBindLR Name Name]      |],   "LHsBindLRListIt"   )
        , ( [t| [LHsCmdTop Name]           |],   "LHsCmdTopListIt"   )
        , ( [t| [LHsDecl Name]             |],   "LHsDeclListIt" )
        , ( [t| [LHsExpr Name]             |],   "LHsExprListIt" )
        , ( [t| [LIE Name]                 |],   "LIEListIt" )
        , ( [t| [LIPBind Name]             |],   "LIPBindListIt" )
        , ( [t| [LImportDecl Name]         |],   "LImportDeclListIt" )
        , ( [t| [LInstDecl Name]           |],   "LInstDeclListIt" )
        , ( [t| [LMatch Name]              |],   "LMatchListIt"      )
        , ( [t| [LPat Name]                |],   "LPatListIt"        )
        , ( [t| [LRuleDecl Name]           |],   "LRuleDeclListIt" )
        , ( [t| [LSig Name]                |],   "LSigListIt" )
        , ( [t| [LStmt Name]               |],   "LStmtListIt" )
        , ( [t| [LTyClDecl Name]           |],   "LTyClDeclListIt" )
        , ( [t| [LVectDecl Name]           |],   "LVectDeclListIt" )
        , ( [t| [LWarnDecl Name]           |],   "LWarnDeclListIt" )
        , ( [t| [ParStmtBlock Name Name]   |],   "ParStmtBlockListIt" )
        , ( [t| [PendingSplice]            |],   "PendingSpliceListIt" )
        , ( [t| [Var]                      |],   "VarListIt" )
        , ( [t| [[LTyClDecl Name]]         |],   "LTyClDeclLitListIt" )

        -- Maybe elements
        , ( [t| Maybe ((Bool, [LIE Name])) |],   "MaybeBoolLIENamesIt" )
        , ( [t| Maybe (LHsExpr Name)       |],   "MaybeLHsExprIt" )
        -- , ( [t| Maybe (LHsKind Name)       |],   "MaybeLHsKindIt" )
        , ( [t| Maybe (LHsType Name)       |],   "MaybeLHsTypeIt" )
        , ( [t| Maybe (SyntaxExpr Name)    |],   "MaybeSyntaxExprIt" )

        -- Tuple elements
        , ( [t| (RecFlag, LHsBinds Name) |],   "TupRecFlagLHsBindsIt" )
        , ( [t| (Bool, [LIE Name])       |],   "TupBoolLIEListIt" )

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

