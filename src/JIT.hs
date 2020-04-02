-- This module adds support for JIT compilation and optimization passes

module JIT (runJIT) where

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

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

-- Default curated optimization pass 
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO AST.Module
runJIT mod = do
  withContext $ \context ->
    withModuleFromAST context mod $ \m ->
      withPassManager passes $ \pm -> do
        runPassManager pm m
        optmod <- moduleAST m
        s <- moduleLLVMAssembly m
        B.putStrLn s
        pure optmod

