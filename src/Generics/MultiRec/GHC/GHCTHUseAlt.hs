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
import Coercion
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
  ABExportVarIt     :: AST (ABExport Var)
  AnnDeclIt         :: AST (AnnDecl Name)
  AnnProvenanceIt   :: AST (AnnProvenance Name)
  ArithSeqInfoIt    :: AST (ArithSeqInfo Name)
  ArithSeqInfoVarIt :: AST (ArithSeqInfo Var)
  BoxityIt          :: AST (Boxity)
  CoercionIt        :: AST (Coercion.Coercion)
  ConDeclIt         :: AST (ConDecl Name)
  ConDeclVarIt      :: AST (ConDecl Var)
  DefaultDeclIt     :: AST (DefaultDecl Name)
  DefaultDeclVarIt  :: AST (DefaultDecl Var)
  EvBindsVarIt      :: AST (EvBindsVar)
  DerivDeclIt       :: AST (DerivDecl Name)
  DerivDeclVarIt    :: AST (DerivDecl Var)
  FamInstDeclIt     :: AST (FamInstDecl Name)
  ForeignExportIt   :: AST (ForeignExport)
  ForeignImportIt   :: AST (ForeignImport)
  FixityDirectionIt :: AST (FixityDirection)
  FixityIt          :: AST (HsSyn.Fixity)
  FixitySigIt       :: AST (FixitySig Name)
  ForeignDeclIt     :: AST (ForeignDecl Name)
  FractionalLitIt   :: AST (FractionalLit)
  GRHSIt            :: AST (GRHS Name)
  GRHSsIt           :: AST (GRHSs Name)
  GRHSsVarIt        :: AST (GRHSs Var)
  HsArrAppTypeIt    :: AST (HsArrAppType)
  HsBindLRIt        :: AST (HsBindLR Name Name)
  HsBindLRVarIt     :: AST (HsBindLR Var Var)
  HsBracketIt       :: AST (HsBracket Name)
  HsBracketVarIt    :: AST (HsBracket Var)
  HsCmdTopIt        :: AST (HsCmdTop Name)
  HsCmdTopVarIt     :: AST (HsCmdTop Var)
  HsDeclIt          :: AST (HsDecl Name)
  HsConPatDetailsIt :: AST (HsConPatDetails Name)
  HsConPatDetailsVarIt :: AST (HsConPatDetails Var)
  HsGroupIt         :: AST (HsGroup Name)
  HsGroupVarIt      :: AST (HsGroup Var)
  HsIPBindsIt       :: AST (HsIPBinds Name)
  HsIPBindsVarIt    :: AST (HsIPBinds Var)
  HsIPNameIt        :: AST (HsIPName)
  HsLitIt           :: AST (HsLit)
  HsLocalBindsIt    :: AST (HsLocalBinds Name)
  HsLocalBindsVarIt :: AST (HsLocalBinds Var)
  HsMatchContextIt  :: AST (HsMatchContext Name)
  HsOverLitIt       :: AST (HsOverLit Name)
  HsOverLitVarIt    :: AST (HsOverLit Var)
  HsQuasiQuoteIt    :: AST (HsQuasiQuote Name)
  HsQuasiQuoteVarIt :: AST (HsQuasiQuote Var)
  HsRecFieldsNameLPatIt :: AST (HsRecFields Name (LPat Name))
  HsRecFieldsVarLPatIt  :: AST (HsRecFields Var (LPat Var))
  RecFieldNameLHsExprIt :: AST (HsRecField Name (LHsExpr Name))
  RecFieldVarLHsExprIt :: AST (HsRecField Var (LHsExpr Var))
  HsRecordBindsIt   :: AST (HsRecordBinds Name)
  HsRecordBindsVarIt :: AST (HsRecordBinds Var)
  HsSpliceIt        :: AST (HsSplice Name)
  HsSpliceVarIt     :: AST (HsSplice Var)
  HsStmtContextIt   :: AST (HsStmtContext Name)
  HsTupArgIt        :: AST (HsTupArg Name)
  HsTypeIt          :: AST (HsType Name)
  HsTyDefnIt        :: AST (HsTyDefn Name)
  HsTypeVarIt       :: AST (HsType Var)
  HsWithBndrsIt     :: AST (HsWithBndrs Name)
  HsWithBndrsLHsTypeIt :: AST (HsWithBndrs (LHsType Name))
  HsWithBndrsLHsTypeVarIt :: AST (HsWithBndrs (LHsType Var))
  HsWithBndrsLHsTypeListIt :: AST (HsWithBndrs ([LHsType Name]))
  HsValBindsLRIt    :: AST (HsValBindsLR Name Name)
  HsValBindsLRVarIt :: AST (HsValBindsLR Var Var)
  IEIt              :: AST (IE Name)
  ImportDeclIt      :: AST (ImportDecl Name)
  InstDeclIt        :: AST (InstDecl Name)
  IPBindIt          :: AST (IPBind Name)
  LAnnDeclIt        :: AST (LAnnDecl Name)
  LAnnDeclVarIt     :: AST (LAnnDecl Var)
  LConDeclIt        :: AST (LConDecl Name)
  LDefaultDeclIt    :: AST (LDefaultDecl Name)
  LDefaultDeclVarIt :: AST (LDefaultDecl Var)
  LDerivDeclIt      :: AST (LDerivDecl Name)
  LDerivDeclVarIt   :: AST (LDerivDecl Var)
  LDocDecl          :: AST LDocDecl
  LFamInstDeclIt    :: AST (LFamInstDecl Name)
  LFamInstDeclVarIt :: AST (LFamInstDecl Var)
  LFixitySigIt      :: AST (LFixitySig Name)
  LFixitySigVarIt   :: AST (LFixitySig Var)
  LForeignDeclIt    :: AST (LForeignDecl Name)
  LForeignDeclVarIt :: AST (LForeignDecl Var)
  LGRHSIt           :: AST (LGRHS Name)
  LGRHSVarIt        :: AST (LGRHS Var)
  LHsBindLRIt       :: AST (LHsBindLR Name Name)
  LHsBindLRVarIt    :: AST (LHsBindLR Var Var)
  LHsBindsLRVarIt   :: AST (LHsBindsLR Var Var)
  LHsBindsIt        :: AST (LHsBinds Name)
  LHsCmdTopIt       :: AST (LHsCmdTop Name)
  LHsCmdTopVarIt    :: AST (LHsCmdTop Var)
  LHsContextIt      :: AST (LHsContext Name)
  LHsContextVarIt   :: AST (LHsContext Var)
  LHsDeclIt         :: AST (LHsDecl Name)
  LHsDeclVarIt      :: AST (LHsDecl Var)
  LHsExprIt         :: AST (LHsExpr Name)
  LHsExprVarIt      :: AST (LHsExpr Var)
  -- LHsTyOpIt         :: AST (LHsTyOp Name)
  LHsTypeIt         :: AST (LHsType Name)
  LHsTypeVarIt      :: AST (LHsType Var)
  LHsTyVarBndrIt   :: AST (LHsTyVarBndr Name)
  LHsTyVarBndrVarIt :: AST (LHsTyVarBndr Var)
  LHsTyVarBndrsIt   :: AST (LHsTyVarBndrs Name)
  LHsTyVarBndrsVarIt :: AST (LHsTyVarBndrs Var)
  LIEIt             :: AST (LIE Name)
  LIPBindIt         :: AST (LIPBind Name)
  LIPBindVarIt      :: AST (LIPBind Var)
  LImportDeclIt     :: AST (LImportDecl Name)
  LInstDeclIt       :: AST (LInstDecl Name)
  LInstDeclVarIt    :: AST (LInstDecl Var)
  LMatchIt          :: AST (LMatch Name)
  LMatchVarIt       :: AST (LMatch Var)
  LPatIt            :: AST (LPat Name)
  LPatVarIt         :: AST (LPat Var)
  LRuleDeclIt       :: AST (LRuleDecl Name)
  LRuleDeclVarIt    :: AST (LRuleDecl Var)
  LSigIt            :: AST (LSig Name)
  LSigVarIt         :: AST (LSig Var)
  LStmtLRIt         :: AST (LStmtLR Name Name)
  LStmtLRVarIt      :: AST (LStmtLR Var Var)
  LTyClDeclIt       :: AST (LTyClDecl Name)
  LVectDeclIt       :: AST (LVectDecl Name)
  LVectDeclVarIt    :: AST (LVectDecl Var)
  LWarnDeclIt       :: AST (LWarnDecl Name)
  LWarnDeclVarIt    :: AST (LWarnDecl Var)
  LocatedIt         :: AST (Located Name)
  MatchGroupIt      :: AST (MatchGroup Name)
  MatchGroupVarIt   :: AST (MatchGroup Var)
  MatchIt           :: AST (Match Name)
  NameIt            :: AST (Name)
  OverLitValIt      :: AST (OverLitVal)
  ParStmtBlockIt    :: AST (ParStmtBlock Name Name)
  PatIt             :: AST (Pat Name)
  PatVarIt          :: AST (Pat Var)
  PostTcExprIt      :: AST PostTcExpr
  RenamedSourceIt   :: AST RenamedSource
  RuleDeclIt        :: AST (RuleDecl Name)
  SigIt             :: AST (Sig Name)
  SpliceDeclIt      :: AST (SpliceDecl Name)
  StmtLRIt          :: AST (StmtLR Name Name)
  StmtLRVarIt       :: AST (StmtLR Var Var)
  SyntaxExprIt      :: AST (SyntaxExpr Name)
  TcEvBindsIt       :: AST (TcEvBinds)
  TcSpecPragsIt     :: AST (TcSpecPrags)
  TickishIt         :: AST (Tickish Name)
  TickishVarIt      :: AST (Tickish Var)
  TyClDeclIt        :: AST (TyClDecl Name)
  VectDeclIt        :: AST (VectDecl Name)
  WarnDeclIt        :: AST (WarnDecl Name)

  -- Maybe elements
  MaybeBoolLIENamesIt :: AST (Maybe ((Bool, [LIE Name])))
  MaybeLIENamesIt     :: AST (Maybe [LIE Name])
  MaybeLHsExprIt      :: AST (Maybe (LHsExpr Name))
  MaybeLHsTypeIt      :: AST (Maybe (LHsType Name)) -- Includes LHsKind
  MaybeLHsTypeListIt  :: AST (Maybe [LHsType Name]) -- Includes LHsKind
  MaybeSyntaxExprIt   :: AST (Maybe (SyntaxExpr Name))

  -- List elements
  ABExportListIt        :: AST [ABExport Name]
  ABExportListVarIt     :: AST [ABExport Var]
  CoercionListIt        :: AST [Coercion.Coercion]
  ConDeclFieldListIt    :: AST [ConDeclField Name]
  ConDeclFieldListVarIt :: AST [ConDeclField Var]
  LAnnDeclListIt        :: AST [LAnnDecl Name]
  LAnnDeclListVarIt     :: AST [LAnnDecl Var]
  LConDeclListIt        :: AST [LConDecl Name]
  LDefaultDeclListIt    :: AST [LDefaultDecl Name]
  LDefaultDeclListVarIt :: AST [LDefaultDecl Var]
  LDerivDeclListIt      :: AST [LDerivDecl Name]
  LDerivDeclListVarIt   :: AST [LDerivDecl Var]
  LDocDeclListIt        :: AST [LDocDecl]
  LFamInstDeclListIt    :: AST [LFamInstDecl Name]
  LFamInstDeclListVarIt :: AST [LFamInstDecl Var]
  LFixitySigLitsIt      :: AST [LFixitySig Name]
  LFixitySigLitsVarIt   :: AST [LFixitySig Var]
  LForeignDeclListIt    :: AST [LForeignDecl Name]
  LForeignDeclListVarIt :: AST [LForeignDecl Var]
  LGRHSListIt           :: AST [LGRHS Name]
  LGRHSListVarIt        :: AST [LGRHS Var]
  LHsBindLRListIt       :: AST [LHsBindLR Name Name]
  LHsBindLRListVarIt    :: AST [LHsBindLR Var Var]
  LHsCmdTopListIt       :: AST [LHsCmdTop Name]
  LHsCmdTopListVarIt    :: AST [LHsCmdTop Var]
  LHsDeclListIt         :: AST [LHsDecl Name]
  LHsDeclListVarIt      :: AST [LHsDecl Var]
  LHsExprListIt         :: AST [LHsExpr Name]
  LHsExprListVarIt      :: AST [LHsExpr Var]
  LHsTypeListIt         :: AST [LHsType Name]
  LHsTypeListVarIt      :: AST [LHsType Var]
  LIEListIt             :: AST [LIE Name]
  LIPBindListIt         :: AST [LIPBind Name]
  LIPBindListVarIt      :: AST [LIPBind Var]
  LImportDeclListIt     :: AST [LImportDecl Name]
  LInstDeclListIt       :: AST [LInstDecl Name]
  LInstDeclListVarIt    :: AST [LInstDecl Var]
  LMatchListIt          :: AST [LMatch Name]
  LMatchListVarIt       :: AST [LMatch Var]
  LPatListIt            :: AST [LPat Name]
  LPatListVarIt         :: AST [LPat Var]
  LRuleDeclListIt       :: AST [LRuleDecl Name]
  LRuleDeclListVarIt    :: AST [LRuleDecl Var]
  LSigListIt            :: AST [LSig Name]
  LSigListVarIt         :: AST [LSig Var]
  LStmtListIt           :: AST [LStmt Name]
  LStmtListVarIt        :: AST [LStmt Var]
  LTcSpecPragListIt     :: AST [LTcSpecPrag]
  LTyClDeclListIt       :: AST [LTyClDecl Name]
  LTyClDeclLitListIt    :: AST [[LTyClDecl Name]]
  LTyClDeclLitListVarIt :: AST [[LTyClDecl Var]]
  LVectDeclListIt       :: AST [LVectDecl Name]
  LVectDeclListVarIt    :: AST [LVectDecl Var]
  LWarnDeclListIt       :: AST [LWarnDecl Name]
  LWarnDeclListVarIt    :: AST [LWarnDecl Var]
  RecFieldNameLHsExprListIt   :: AST [HsRecField Name (LHsExpr Name)]
  RecFieldVarLHsExprListVarIt :: AST [HsRecField Var (LHsExpr Var)]
  RuleBndrListIt        :: AST [RuleBndr Name]
  ValBindsOutListIt     :: AST [(RecFlag, LHsBinds Name)]
  ValBindsOutListVarIt  :: AST [(RecFlag, LHsBinds Var)]
  VarListIt             :: AST [Var] -- Includes TyVar and EvVar
  HsTupArgListIt        :: AST [HsTupArg Name]
  HsTupArgListVarIt     :: AST [HsTupArg Var]
  DataConListIt         :: AST [DataCon]
  PendingSpliceListIt   :: AST [PendingSplice]
  ParStmtBlockListIt    :: AST [ParStmtBlock Name Name]
  LHsTyVarBndrListIt    :: AST [LHsTyVarBndr Name]
  LHsTyVarBndrListVarIt :: AST [LHsTyVarBndr Var]

  -- Tuple elements
  TupRecFlagLHsBindsIt :: AST (RecFlag, LHsBinds Name)
  TupBoolLIEListIt     :: AST (Bool, [LIE Name])
  TupNameLHsExprVarIt  :: AST (Name, LHsExpr Var)

$(deriveEverything
  (DerivOptions {
   familyTypes =
        [ ( [t| ABExport Name          |], "ABExportIt" )
        , ( [t| ABExport Var           |], "ABExportVarIt" )
        , ( [t| AnnDecl Name           |], "AnnDeclIt")
        , ( [t| AnnProvenance Name     |], "AnnProvenanceIt")
        , ( [t| ArithSeqInfo Name      |], "ArithSeqInfoIt"   )
        , ( [t| ArithSeqInfo Var       |], "ArithSeqInfoVarIt"   )
        , ( [t| Boxity                 |], "BoxityIt" )
        , ( [t| Coercion.Coercion      |], "CoercionIt")
        , ( [t| ConDecl Name           |], "ConDeclIt" )
        , ( [t| ConDecl Var            |], "ConDeclVarIt" )
        , ( [t| DefaultDecl Name       |], "DefaultDeclIt" )
        , ( [t| DefaultDecl Var        |], "DefaultDeclVarIt" )
        , ( [t| DerivDecl Name         |], "DerivDeclIt" )
        , ( [t| DerivDecl Var          |], "DerivDeclVarIt" )
        , ( [t| FamInstDecl Name       |], "FamInstDeclIt")
        , ( [t| LFamInstDecl Name      |], "LFamInstDeclIt")
        , ( [t| LFamInstDecl Var       |], "LFamInstDeclVarIt")
        , ( [t| ForeignExport          |], "ForeignExportIt")
        , ( [t| ForeignImport          |], "ForeignImportIt")
        , ( [t| EvBindsVar             |], "EvBindsVarIt")
        , ( [t| FixityDirection        |], "FixityDirectionIt" )
        , ( [t| FractionalLit          |], "FractionalLitIt" )
        , ( [t| GRHS Name              |], "GRHSIt" )
        , ( [t| GRHSs Name             |], "GRHSsIt" )
        , ( [t| GRHSs Var              |], "GRHSsVarIt" )
        , ( [t| HsArrAppType           |], "HsArrAppTypeIt"   )
        , ( [t| HsBindLR Name Name     |], "HsBindLRIt" )
        , ( [t| HsBindLR Var Var       |], "HsBindLRVarIt" )
        , ( [t| HsBracket Name         |], "HsBracketIt" )
        , ( [t| HsBracket Var          |], "HsBracketVarIt" )
        , ( [t| HsCmdTop Name          |], "HsCmdTopIt" )
        , ( [t| HsCmdTop Var           |], "HsCmdTopVarIt" )
        , ( [t| HsDecl Name            |], "HsDeclIt" )
        , ( [t| HsConPatDetails Name   |], "HsConPatDetailsIt" )
        , ( [t| HsConPatDetails Var    |], "HsConPatDetailsVarIt" )
        , ( [t| HsGroup Name           |], "HsGroupIt" )
        , ( [t| HsGroup Var            |], "HsGroupVarIt" )
        , ( [t| HsIPBinds Name         |], "HsIPBindsIt" )
        , ( [t| HsIPBinds Var          |], "HsIPBindsVarIt" )
        , ( [t| HsIPName               |], "HsIPNameIt" )
        , ( [t| HsLit                  |], "HsLitIt"   )
        , ( [t| HsLocalBinds Name      |], "HsLocalBindsIt" )
        , ( [t| HsLocalBinds Var       |], "HsLocalBindsVarIt" )
        , ( [t| HsMatchContext Name    |], "HsMatchContextIt" )
        , ( [t| HsOverLit Name         |], "HsOverLitIt"   )
        , ( [t| HsOverLit Var          |], "HsOverLitVarIt"   )
        , ( [t| HsQuasiQuote Name      |], "HsQuasiQuoteIt"   )
        , ( [t| HsQuasiQuote Var       |], "HsQuasiQuoteVarIt"   )
        -- , ( [t| HsRecFields Name (LPat Name)   |] "HsRecFieldsNameLPatIt")
        -- , ( [t| HsRecFields Var (LPat Var)     |] "HsRecFieldsVarLPatIt")
        , ( [t| HsRecField Name (LHsExpr Name) |], "RecFieldNameLHsExprIt")
        , ( [t| HsRecField Var  (LHsExpr Var)  |], "RecFieldVarLHsExprIt")
        , ( [t| HsRecordBinds Name     |], "HsRecordBindsIt" )
        , ( [t| HsRecordBinds Var      |], "HsRecordBindsVarIt" )
        , ( [t| HsSplice Name          |], "HsSpliceIt"   )
        , ( [t| HsSplice Var           |], "HsSpliceVarIt"   )
        , ( [t| HsStmtContext Name     |], "HsStmtContextIt" )
        , ( [t| HsSyn.Fixity           |], "FixityIt" )
        , ( [t| FixitySig Name         |], "FixitySigIt")
        , ( [t| ForeignDecl Name       |], "ForeignDeclIt" )
        , ( [t| HsTupArg Name          |], "HsTupArgIt" )
        , ( [t| HsTyDefn Name          |], "HsTyDefnIt")
        , ( [t| HsType Name            |], "HsTypeIt" )
        , ( [t| HsType Var             |], "HsTypeVarIt" )
        , ( [t| HsWithBndrs Name       |], "HsWithBndrsIt")
        , ( [t| HsWithBndrs (LHsType Name) |], "HsWithBndrsLHsTypeIt")
        , ( [t| HsWithBndrs (LHsType Var ) |], "HsWithBndrsLHsTypeVarIt")
        , ( [t| HsWithBndrs [LHsType Name] |], "HsWithBndrsLHsTypeListIt")
        , ( [t| HsValBindsLR Name Name |], "HsValBindsLRIt" )
        , ( [t| HsValBindsLR Var Var   |], "HsValBindsLRVarIt" )
        , ( [t| IE Name                |], "IEIt" )
        , ( [t| ImportDecl Name        |], "ImportDeclIt"   )
        , ( [t| InstDecl Name          |], "InstDeclIt" )
        , ( [t| IPBind Name            |], "IPBindIt" )
        , ( [t| LAnnDecl Name          |], "LAnnDeclIt" )
        , ( [t| LAnnDecl Var           |], "LAnnDeclVarIt" )
        , ( [t| LDefaultDecl Name      |], "LDefaultDeclIt" )
        , ( [t| LDefaultDecl Var       |], "LDefaultDeclVarIt" )
        , ( [t| LDerivDecl Name        |], "LDerivDeclIt" )
        , ( [t| LDerivDecl Var         |], "LDerivDeclVarIt" )
        , ( [t| LDocDecl               |], "LDocDecl" )
        , ( [t| LFixitySig Name        |], "LFixitySigIt" )
        , ( [t| LFixitySig Var         |], "LFixitySigVarIt" )
        , ( [t| LForeignDecl Name      |], "LForeignDeclIt" )
        , ( [t| LForeignDecl Var       |], "LForeignDeclVarIt" )
        , ( [t| LGRHS Name             |], "LGRHSIt" )
        , ( [t| LGRHS Var              |], "LGRHSVarIt" )
        , ( [t| LHsBindLR Name Name    |], "LHsBindLRIt" )
        , ( [t| LHsBindLR Var Var      |], "LHsBindLRVarIt" )
        , ( [t| LHsBindsLR Var Var     |], "LHsBindsLRVarIt" )
        , ( [t| LHsBinds Name          |], "LHsBindsIt" )
        , ( [t| LHsCmdTop Name         |], "LHsCmdTopIt" )
        , ( [t| LHsCmdTop Var          |], "LHsCmdTopVarIt" )
        , ( [t| LHsContext Name        |], "LHsContextIt")
        , ( [t| LHsContext Var         |], "LHsContextVarIt")
        , ( [t| LHsDecl Name           |], "LHsDeclIt" )
        , ( [t| LHsDecl Var            |], "LHsDeclVarIt" )
        , ( [t| LHsExpr Name           |], "LHsExprIt" )
        , ( [t| LHsExpr Var            |], "LHsExprVarIt" )
        -- , ( [t| LHsTyOp Name           |], "LHsTyOpIt")
        , ( [t| LHsType Name           |], "LHsTypeIt" )
        , ( [t| LHsType Var            |], "LHsTypeVarIt" )
        , ( [t| LHsTyVarBndr Name      |], "LHsTyVarBndrIt")
        , ( [t| LHsTyVarBndr Var       |], "LHsTyVarBndrVarIt")
        , ( [t| LHsTyVarBndrs Name     |], "LHsTyVarBndrsIt")
        , ( [t| LHsTyVarBndrs Var      |], "LHsTyVarBndrsVarIt")
        , ( [t| LIE Name               |], "LIEIt" )
        , ( [t| LIPBind Name           |], "LIPBindIt" )
        , ( [t| LIPBind Var            |], "LIPBindVarIt" )
        , ( [t| LImportDecl Name       |], "LImportDeclIt"   )
        , ( [t| LInstDecl Name         |], "LInstDeclIt" )
        , ( [t| LInstDecl Var          |], "LInstDeclVarIt" )
        , ( [t| LMatch Name            |], "LMatchIt" )
        , ( [t| LMatch Var             |], "LMatchVarIt" )
        , ( [t| LPat Name              |], "LPatIt" )
        , ( [t| LPat Var               |], "LPatVarIt" )
        , ( [t| LRuleDecl Name         |], "LRuleDeclIt" )
        , ( [t| LRuleDecl Var          |], "LRuleDeclVarIt" )
        , ( [t| LSig Name              |], "LSigIt" )
        , ( [t| LSig Var               |], "LSigVarIt" )
        , ( [t| LStmtLR Name Name      |], "LStmtLRIt" )
        , ( [t| LStmtLR Var Var        |], "LStmtLRVarIt" )
        , ( [t| LTyClDecl Name         |], "LTyClDeclIt" )
        , ( [t| LVectDecl Name         |], "LVectDeclIt" )
        , ( [t| LVectDecl Var          |], "LVectDeclVarIt" )
        , ( [t| LWarnDecl Name         |], "LWarnDeclIt" )
        , ( [t| LWarnDecl Var          |], "LWarnDeclVarIt" )
        , ( [t| Located Name           |], "LocatedIt"   )
        , ( [t| Match Name             |], "MatchIt" )
        , ( [t| MatchGroup Name        |], "MatchGroupIt" )
        , ( [t| MatchGroup Var         |], "MatchGroupVarIt" )
        , ( [t| OverLitVal             |], "OverLitValIt"   )
        , ( [t| ParStmtBlock Name Name |], "ParStmtBlockIt" )
        , ( [t| Pat Name               |], "PatIt" )
        , ( [t| Pat Var                |], "PatVarIt" )
        , ( [t| PostTcExpr             |], "PostTcExprIt" )
        , ( [t| RenamedSource          |], "RenamedSourceIt" )
        , ( [t| RuleDecl Name          |], "RuleDeclIt" )
        , ( [t| Sig Name               |], "SigIt")
        , ( [t| SpliceDecl Name        |], "SpliceDeclIt")
        , ( [t| StmtLR Name Name       |], "StmtLRIt" )
        , ( [t| StmtLR Var Var         |], "StmtLRVarIt" )
        , ( [t| SyntaxExpr Name        |], "SyntaxExprIt" )
        , ( [t| TcEvBinds              |], "TcEvBindsIt" )
        , ( [t| TcSpecPrags            |], "TcSpecPragsIt")
        , ( [t| Tickish Name           |], "TickishIt"   )
        , ( [t| Tickish Var            |], "TickishVarIt"   )
        , ( [t| TyClDecl Name          |], "TyClDeclIt" )
        , ( [t| VectDecl Name          |], "VectDeclIt" )
        , ( [t| WarnDecl Name          |], "WarnDeclIt" )

        -- List elements
        , ( [t| [(RecFlag, LHsBinds Name)] |], "ValBindsOutListIt" )
        , ( [t| [(RecFlag, LHsBinds Var)]  |], "ValBindsOutListVarIt" )
        , ( [t| [ABExport Name]            |],   "ABExportListIt" )
        , ( [t| [ABExport Var]             |],   "ABExportListVarIt" )
        , ( [t| [Coercion.Coercion]        |],   "CoercionListIt")
        , ( [t| [ConDeclField Name]        |],   "ConDeclFieldListIt")
        , ( [t| [ConDeclField Var]         |],   "ConDeclFieldListVarIt")
        , ( [t| [DataCon]                  |],   "DataConListIt" )
        , ( [t| [HsRecField Name (LHsExpr Name)] |], "RecFieldNameLHsExprListIt" )
        , ( [t| [HsRecField Var  (LHsExpr Var)]  |], "RecFieldVarLHsExprListVarIt" )
        , ( [t| [RuleBndr Name]            |],   "RuleBndrListIt")
        , ( [t| [HsTupArg Name]            |],   "HsTupArgListIt" )
        , ( [t| [HsTupArg Var]             |],   "HsTupArgListVarIt" )
        , ( [t| [LAnnDecl Name]            |],   "LAnnDeclListIt" )
        , ( [t| [LAnnDecl Var]             |],   "LAnnDeclListVarIt" )
        , ( [t| [LConDecl Name]            |],   "LConDeclListIt")
        , ( [t| [LDefaultDecl Name]        |],   "LDefaultDeclListIt" )
        , ( [t| [LDefaultDecl Var]         |],   "LDefaultDeclListVarIt" )
        , ( [t| [LDerivDecl Name]          |],   "LDerivDeclListIt" )
        , ( [t| [LDerivDecl Var]           |],   "LDerivDeclListVarIt" )
        , ( [t| [LDocDecl]                 |],   "LDocDeclListIt" )
        , ( [t| [LFamInstDecl Name]        |],   "LFamInstDeclListIt")
        , ( [t| [LFamInstDecl Var]         |],   "LFamInstDeclListVarIt")
        , ( [t| [LFixitySig Name]          |],   "LFixitySigLitsIt" )
        , ( [t| [LFixitySig Var]           |],   "LFixitySigLitsVarIt" )
        , ( [t| [LForeignDecl Name]        |],   "LForeignDeclListIt" )
        , ( [t| [LForeignDecl Var]         |],   "LForeignDeclListVarIt" )
        , ( [t| [LGRHS Name]               |],   "LGRHSListIt" )
        , ( [t| [LGRHS Var]                |],   "LGRHSListVarIt" )
        , ( [t| [LHsBindLR Name Name]      |],   "LHsBindLRListIt"   )
        , ( [t| [LHsBindLR Var Var]        |],   "LHsBindLRListVarIt"   )
        , ( [t| [LHsCmdTop Name]           |],   "LHsCmdTopListIt"   )
        , ( [t| [LHsCmdTop Var]            |],   "LHsCmdTopListVarIt"   )
        , ( [t| [LHsDecl Name]             |],   "LHsDeclListIt" )
        , ( [t| [LHsDecl Var]              |],   "LHsDeclListVarIt" )
        , ( [t| [LHsExpr Name]             |],   "LHsExprListIt" )
        , ( [t| [LHsExpr Var]              |],   "LHsExprListVarIt" )
        , ( [t| [LHsType Name]             |],   "LHsTypeListIt" )
        , ( [t| [LHsType Var]              |],   "LHsTypeListVarIt" )
        , ( [t| [LIE Name]                 |],   "LIEListIt" )
        , ( [t| [LIPBind Name]             |],   "LIPBindListIt" )
        , ( [t| [LIPBind Var]              |],   "LIPBindListVarIt" )
        , ( [t| [LImportDecl Name]         |],   "LImportDeclListIt" )
        , ( [t| [LInstDecl Name]           |],   "LInstDeclListIt" )
        , ( [t| [LInstDecl Var]            |],   "LInstDeclListVarIt" )
        , ( [t| [LMatch Name]              |],   "LMatchListIt" )
        , ( [t| [LMatch Var]               |],   "LMatchListVarIt" )
        , ( [t| [LPat Name]                |],   "LPatListIt" )
        , ( [t| [LPat Var]                 |],   "LPatListVarIt" )
        , ( [t| [LRuleDecl Name]           |],   "LRuleDeclListIt" )
        , ( [t| [LRuleDecl Var]            |],   "LRuleDeclListVarIt" )
        , ( [t| [LSig Name]                |],   "LSigListIt" )
        , ( [t| [LSig Var]                 |],   "LSigListVarIt" )
        , ( [t| [LStmt Name]               |],   "LStmtListIt" )
        , ( [t| [LStmt Var]                |],   "LStmtListVarIt" )
        , ( [t| [LTcSpecPrag]              |], "LTcSpecPragListIt")
        , ( [t| [LTyClDecl Name]           |],   "LTyClDeclListIt" )
        , ( [t| [LVectDecl Name]           |],   "LVectDeclListIt" )
        , ( [t| [LVectDecl Var]            |],   "LVectDeclListVarIt" )
        , ( [t| [LWarnDecl Name]           |],   "LWarnDeclListIt" )
        , ( [t| [LWarnDecl Var]            |],   "LWarnDeclListVarIt" )
        , ( [t| [ParStmtBlock Name Name]   |],   "ParStmtBlockListIt" )
        , ( [t| [PendingSplice]            |],   "PendingSpliceListIt" )
        , ( [t| [Var]                      |],   "VarListIt" )
        , ( [t| [[LTyClDecl Name]]         |],   "LTyClDeclLitListIt" )
        , ( [t| [[LTyClDecl Var]]          |],   "LTyClDeclLitListVarIt" )
        , ( [t| [LHsTyVarBndr Name]        |], "LHsTyVarBndrListIt")
        , ( [t| [LHsTyVarBndr Var]         |], "LHsTyVarBndrListVarIt")

        -- Maybe elements
        , ( [t| Maybe ((Bool, [LIE Name])) |],   "MaybeBoolLIENamesIt" )
        , ( [t| (Maybe [LIE Name])         |],   "MaybeLIENamesIt" )
        , ( [t| Maybe (LHsExpr Name)       |],   "MaybeLHsExprIt" )
        -- , ( [t| Maybe (LHsKind Name)       |],   "MaybeLHsKindIt" )
        , ( [t| Maybe (LHsType Name)       |],   "MaybeLHsTypeIt" )
        , ( [t| Maybe [LHsType Name]       |],   "MaybeLHsTypeListIt" )
        , ( [t| Maybe (SyntaxExpr Name)    |],   "MaybeSyntaxExprIt" )

        -- Tuple elements
        , ( [t| (RecFlag, LHsBinds Name) |],   "TupRecFlagLHsBindsIt" )
        , ( [t| (Bool, [LIE Name])       |],   "TupBoolLIEListIt" )
        , ( [t| (Name, LHsExpr Var)      |], "TupNameLHsExprVarIt")

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

