-- This module adds support for JIT compilation and optimization passes
{-# LANGUAGE ForeignFunctionInterface #-}

module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )
import qualified Data.ByteString.Char8 as B

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import qualified LLVM.ExecutionEngine as EE

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

foreign import ccall "dynamic" haskFun 
  :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

-- Execution Engine to give JIT compilation
jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- Optimization level
    model    = Nothing -- code model (Default)
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

-- Default curated optimization pass 
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

-- Run the JIT compilation for the module
runJIT :: AST.Module -> IO AST.Module
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          verify m
          -- Produce and output IR
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          B.putStrLn s

          -- Execute with the JIT execution engine
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.mkName "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> pure ()

          pure optmod

