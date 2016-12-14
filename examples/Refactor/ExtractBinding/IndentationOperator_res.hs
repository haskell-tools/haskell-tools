module Refactor.ExtractBinding.IndentationOperator where

f aaa bbb = extracted $
    aaa
      ++ bbb
  where extracted = id . id