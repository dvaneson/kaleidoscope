-- An update to the Emit module that uses the LLVM IR builder API instead of Codegen
{-# LANGUAGE OverloadedStrings #-}

module Emit (codegen, emptyModule) where

import Data.String
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import Control.Monad.State

import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Internal.SnocList

import LLVM.AST hiding (function)
import LLVM.AST.Global
import LLVM.AST.Typed (typeOf)
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import JIT
import qualified Syntax as S


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- For easily using conditionals
one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

-- Produce a local operand with a name
local ::  Name -> Operand
local = LocalReference T.double

-- Produce a constant operand
cons :: C.Constant -> Operand
cons = ConstantOperand

-- Function type that returns a double and only has double type for arguments
doublef :: Int -> Type
doublef n = FunctionType T.double args False
  where
    args = replicate n T.double

-- Create a function pointer type with the given number of arguments
fnPtr :: Int -> Name -> Operand
fnPtr n = ConstantOperand . C.GlobalReference (T.ptr $ doublef n)

-- Converts a list of argument names to a list of double type arguments
toSig :: [String] -> [(Type, ParameterName)]
toSig = map (\x -> (T.double, ParameterName $ fromString x))

lt 
  :: MonadIRBuilder m
  => Operand 
  -> Operand 
  -> m Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp test T.double

-- Map of binary operators
binops 
  :: MonadIRBuilder m
  => Map.Map String (Operand -> Operand -> m Operand)
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

-- Gets an operator from the operator list based on the name provided
getop 
  :: MonadIRBuilder m
  => [Operand]         -- List of available operators
  -> Name              -- Name of operator to find
  -> m Operand         -- Found operator
getop [] var = error $ "Local variable not in scope: " ++ show var
getop (LocalReference ty nm:xs) var = 
  if nm == var
     then pure $ LocalReference ty nm
     else getop xs var

-- Gets the name of the PartialBlock (current block being added to)
currName :: MonadIRBuilder m => m Name
currName = do
  mbb <- liftIRState $ gets builderBlock
  case mbb of
    Nothing -> pure $ mkName ""
    Just pb -> pure $ partialBlockName pb

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = fromString label }

-- Updates a module using the defintions from a ModuleBuilder
updateModule 
  :: Module            -- The module to update
  -> ModuleBuilder a   -- The module builder to update with
  -> Module            -- The updated module
updateModule mod modb = mod { moduleDefinitions = defs ++ newdefs }
    where
      defs    = moduleDefinitions mod
      newdefs = execModuleBuilder emptyModuleBuilder modb

-------------------------------------------------------------------------------
-- Emitters
-------------------------------------------------------------------------------

-- Emits toplevel constructions in modules
topgen 
  :: MonadModuleBuilder m 
  => S.Expr 
  -> m Operand
topgen (S.Function name args body) = do
  function label argtys retty buildf
  where
    label  = mkName name
    argtys = toSig args
    retty  = T.double
    buildf = \xs ->
      opgen xs body >>= ret

topgen (S.Extern name args) = do
  extern label argtys retty
  where
    label  = mkName name
    argtys = [T.double | arg <- args]
    retty  = T.double

topgen exp = do
  function label [] retty buildf
  where
    label  = mkName "main"
    retty  = T.double
    buildf = \xs ->
      opgen xs exp >>= ret

-- Expression level generator that recurisvely walks the AST
opgen 
  :: MonadIRBuilder m
  => [Operand]    -- The list of in use operands
  -> S.Expr       -- The to parse and convert into an operand
  -> m Operand    -- The resulting operand
opgen ops (S.UnaryOp op a) = do
  opgen ops $ S.Call ("unary" ++ op) [a]

opgen ops (S.BinaryOp "=" (S.Var var) val) = do
  ptr <- getop ops (mkName var)
  cval <- opgen ops val
  store ptr 0 cval
  pure cval

opgen ops (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- opgen ops a
      cb <- opgen ops b
      f ca cb
    Nothing -> error "No such operator"

opgen ops (S.Var x) = getop ops (mkName x)

opgen _ (S.Float n) = pure $ cons $ C.Float (F.Double n)

opgen ops (S.Call fn args) = do
  largs <- mapM (opgen ops) args
  let len   = length largs
  let rargs = [(arg, []) | arg <- largs]
  call ptr rargs
    where
      len = length args
      ptr = fnPtr len (mkName fn)

opgen ops (S.If c t f) = do
  -- Produce fresh names for the if/then/else blocks
  ifthen <- frsh "if.then"
  ifelse <- frsh "if.else"
  ifexit <- frsh "if.exit"

  -- entry block
  cond <- opgen ops c
  test <- fcmp FP.ONE false cond
  condBr test ifthen ifelse       -- Branch based on condition

  -- if.then block
  emitBlockStart ifthen
  tval <- opgen ops t             -- Generate code for true branch
  br ifexit                       -- Branch to the merge block
  ifthen <- currName              -- Block may have changed from opgen, 
                                  -- i.e. nested if/else/then

  -- if.else block
  emitBlockStart ifelse
  fval <- opgen ops f             -- Generate code for false branch
  br ifexit                       -- Branch to merge block
  ifelse <- currName              -- Block may have changed
  
  -- if.exit block
  emitBlockStart ifexit
  phi [(tval, ifthen), (fval, ifelse)]
    where
      frsh = freshName . fromString
      
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

-- Generates string representation of the LLVM IR from the AST
-- Uses JIT to generate LLVM IR with an optimization pass
-- Uses topgen and opgen to generate from the AST
codegen :: Module -> [S.Expr] -> IO Module
codegen mod fns = do
    newast <- runJIT oldast
    pure newast
  where
    modb    = mapM topgen fns
    oldast  = updateModule mod modb


