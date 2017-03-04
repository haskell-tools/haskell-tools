{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Language.Haskell.Tools.Transform.FixCPPSpans where

import Control.Monad.State
import Control.Reference
import qualified Data.Text as Txt

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.SourceTemplate (SourceTemplateElem(..), sourceTemplateNodeElems, srcTmpSeparators)

fixCPPSpans :: Ann UModule dom SrcTemplateStage -> Ann UModule dom SrcTemplateStage
-- the state will be used to pass the removed separators to the correct child
fixCPPSpans = flip evalState [] . (sourceInfoTraverseDown (SourceInfoTrf trfElem trfList pure) (modify ([]:)) (modify tail))
  where trfList ls = do ret <- srcTmpSeparators&traversal !~ removeClosing $ ls
                        modify $ (\(act:rest) -> ("" : act ++ [""]):rest)
                        return ret
        trfElem elem = do st <- get
                          case st of
                            -- add the moved element to the end
                            []:(bef:aft:rest):stack -> do
                              put $ []:rest:stack
                              return $ sourceTemplateNodeElems .- ((TextElem bef :) . (++ [TextElem aft])) $ elem
                            _ -> return elem

        removeClosing :: String -> State [[String]] String
        removeClosing sep = let (aftPrev,rest) = Txt.breakOnEnd "#endif" (Txt.pack sep)
                                (remaining,befNext) = Txt.breakOn "#if" (rest)
                             in if Txt.null aftPrev || Txt.null befNext
                                  then do modify (\(act:rest) -> (act ++ [Txt.unpack aftPrev, Txt.unpack befNext]) : rest)
                                          return $ Txt.unpack remaining
                                  else return sep
