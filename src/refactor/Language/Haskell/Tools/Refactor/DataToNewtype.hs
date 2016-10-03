module Language.Haskell.Tools.Refactor.DataToNewtype (dataToNewtype) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import Language.Haskell.Tools.Refactor (tryRefactor)

tryItOut moduleName = tryRefactor (localRefactoring $ dataToNewtype) moduleName

dataToNewtype :: Domain dom => LocalRefactoring dom
dataToNewtype (Ann mi mod@(Module { _modDecl = AnnList li ls })) 
  = return (Ann mi mod { _modDecl = AnnList li (map changeDeclaration ls) })

changeDeclaration :: Ann Decl dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
changeDeclaration (Ann a dd@(DataDecl {})) 
  | annLength (_declCons dd) == 1 
      && annLength (_conDeclArgs (_element (_annListElems (_declCons dd) !! 0))) == 1
  = Ann a dd{ _declNewtype = mkNewtypeKeyword }
changeDeclaration decl = decl