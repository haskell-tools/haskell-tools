{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
             
module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.MagicHashChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

{-# ANN module "HLint: ignore Redundant bracket" #-}

chkMagicHashLiteral :: CheckNode Literal
chkMagicHashLiteral = conditional chkMagicHashLiteral' MagicHash

chkMagicHashNamePart :: CheckNode NamePart
chkMagicHashNamePart = conditional chkMagicHashNamePart' MagicHash

chkMagicHashKind :: CheckNode Kind
chkMagicHashKind = conditional chkMagicHashKind' MagicHash




chkMagicHashLiteral' :: CheckNode Literal
chkMagicHashLiteral' l@(PrimIntLit _)    = addOccurence MagicHash l
chkMagicHashLiteral' l@(PrimWordLit _)   = addOccurence MagicHash l
chkMagicHashLiteral' l@(PrimFloatLit _)  = addOccurence MagicHash l
chkMagicHashLiteral' l@(PrimDoubleLit _) = addOccurence MagicHash l
chkMagicHashLiteral' l@(PrimCharLit _)   = addOccurence MagicHash l
chkMagicHashLiteral' l@(PrimStringLit _) = addOccurence MagicHash l
chkMagicHashLiteral' l = return l


chkMagicHashNamePart' :: CheckNode NamePart
chkMagicHashNamePart' n@(NamePart name) =
  if (last name == '#') then addOccurence MagicHash n
                        else return n

-- NOTE: is this really needed?
chkMagicHashKind' :: CheckNode Kind
chkMagicHashKind' k@UnboxKind = addOccurence MagicHash k
chkMagicHashKind' k = return k

{- Name can be reached from:
  UIESpec
  USubSpec
  UInjectivityAnn
  URuleVar
  UTopLevelPragma
  UAnnotationSubject
  UMinimalFormula

  UDecl  DONE
  UClassElement DONE
  UDeclHead DONE
  UGadtConDecl DONE
  UPatSynLhs  DONE
  UPatternTypeSignature DONE
  UFunDep DONE
  UConDecl DONE
  UFieldDecl DONE
  UInstanceHead DONE
  UTypeSignature DONE
  UMatchLhs DONE
  UTyVar DONE
  UType DONE
  UKind DONE
  UAssertion DONE
  UExpr DONE
  UFieldUpdate DONE
  UPattern DONE
  UPatternField DONE
  USplice
  UQuasiQuote

  UPromoted t
-}

{- QualifiedName can be reached from:
  UDecl DONE
  UOperator DONE
  UName (obviously) DONE
-}
