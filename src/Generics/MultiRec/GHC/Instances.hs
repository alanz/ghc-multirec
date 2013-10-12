{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Generics.MultiRec.GHC.Instances where

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
import UniqFM
import Var


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

-- Note: Type and PostTcType are synonyms. Do not show either for now
instance Show (Type) where
    -- show = showGhc
    show _ = "Type/PostTcType"

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
    -- show (GHC.L l x) = "(" ++ showGhc l ++ ":" ++ showGhc x ++ ")"
    show (GHC.L l x) = "(" ++ "loc" ++ ":" ++ showGhc x ++ ")"


instance Show DocDecl where
    show = showGhc

instance (OutputableBndr a) => Show (WarnDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (VectDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (TyClDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (RuleDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (ForeignDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (DerivDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (DefaultDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (AnnDecl a) where
    show = showGhc

instance (OutputableBndr a) => Show (FixitySig a) where
    show = showGhc

-- instance Show (GHC.HsRecField a e) where
--    -- show (HsRecField n a p) = "(HsRecField " ++ showGhc n ++ " " ++ showGhc a ++ " " ++ showGhc p ++ ")"
--    show = showGhc
