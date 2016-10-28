-- In this example we present how comments are attached to elements
-- of the syntax tree.

-- TODO: use [Ctrl, UAlt and/or Shift + O] organize the imports in this module.
-- The second import is redundant. See which comments are kept.
module Features.KeepsFormatting where

-- This comment is just inside the module

-- | This kind of comment is attached to the next element
-- Comments in the previous line attach to the definition in the next line
import Control.Monad -- In line comments are attached to the definition defined in the line
-- ^ This kind of comment is attached to the previous element

-- | comment on the second import
-- comment on the second import
import Control.Monad as Monad -- comment on the second import
-- ^ comment on the second import


