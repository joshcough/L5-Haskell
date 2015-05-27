{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module L.Runners where

import L.Compiler
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4
import L.L5.L5
import L.Parser.SExpr
import L.Primitives (X86)

withCompiler :: Extension -> (forall i. (FromSExpr i, Show i) => Compiler i X86 -> a) -> a
withCompiler ".L1" go = go l1Compiler
withCompiler ".L2" go = go l2Compiler
withCompiler ".L3" go = go l3Compiler
withCompiler ".L4" go = go l4Compiler
withCompiler ".L5" go = go l5Compiler
withCompiler bad _ = error $ "invalid L extension: " ++ bad

withLanguage :: Extension -> (forall i. (FromSExpr i, Show i) => Language i X86 -> a) -> a
withLanguage ".L1" go = go l1Language
withLanguage ".L2" go = go l2Language
withLanguage ".L3" go = go l3Language
withLanguage ".L4" go = go l4Language
withLanguage ".L5" go = go l5Language
withLanguage bad _ = error $ "invalid L extension: " ++ bad

withLanguage1 :: Extension -> (forall i o. (FromSExpr i, Show i) => Language1 i o -> a) -> a
withLanguage1 ".L1" go = go l1Language1
withLanguage1 ".L2" go = go l2Language1
withLanguage1 ".L3" go = go l3Language1
withLanguage1 ".L4" go = go l4Language1
withLanguage1 ".L5" go = go l5Language1
withLanguage1 bad _ = error $ "invalid L extension: " ++ bad

withInterp    :: Extension -> (forall i. (FromSExpr i, Show i) => Interpreter i -> a) -> a
withInterp    ".L1" go = go l1Interpreter
withInterp    ".L2" go = go l2Interpreter
withInterp    ".L3" go = go l3Interpreter
withInterp    ".L4" go = go l4Interpreter
withInterp    ".L5" go = go l5Interpreter
withInterp    bad _ = error $ "invalid L extension: " ++ bad
