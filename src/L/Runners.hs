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
import L.Primitives (X86)

withCompiler :: Extension -> (forall i. Compiler i X86 -> a) -> a
withCompiler ".L1" go = go l1Compiler
withCompiler ".L2" go = go l2Compiler
withCompiler ".L3" go = go l3Compiler
withCompiler ".L4" go = go l4Compiler
withCompiler ".L5" go = go l5Compiler
withCompiler bad _ = error $ "invalid L extension: " ++ bad

withLanguage :: Extension -> (forall i. Language i X86 -> a) -> a
withLanguage ".L1" go = go l1Language
withLanguage ".L2" go = go l2Language
withLanguage ".L3" go = go l3Language
withLanguage ".L4" go = go l4Language
withLanguage ".L5" go = go l5Language
withLanguage bad _ = error $ "invalid L extension: " ++ bad

--withCompiler ".L2" go = go $ Constrained $ Constrained l2Compiler
--withCompiler ".L3" go = go $ Constrained $ Constrained l3Compiler
--withCompiler ".L4" go = go $ Constrained $ Constrained l4Compiler
--withCompiler ".L5" go = go $ Constrained $ Constrained l5Compiler

--withLang :: Extension -> (forall i o. Thrist (Show :=> Language1) i o -> a) -> a
--withLang ".L1" go = go $ Constrained l1Language
--withLang ".L2" go = go $ Constrained l2Language
--withLang ".L3" go = go $ Constrained l3Language
--withLang ".L4" go = go $ Constrained l4Language
--withLang ".L5" go = go $ Constrained l5Language
----    g bad  = error $ "LC: bad input file: " ++ file

--withLang1 :: (forall i o. Language1 i o -> a) -> Extension -> a
--withLang1 go ".L1" = go l1Language1
--withLang1 go ".L2" = go l2Language1
--withLang1 go ".L3" = go l3Language1
--withLang1 go ".L4" = go l4Language1
--withLang1 go ".L5" = go l5Language1
--
--withInterp :: (forall i. Interpreter i -> a) -> Extension -> a
--withInterp go ".L1" = go l1Interpreter
--withInterp go ".L2" = go l1Interpreter
--withInterp go ".L3" = go l1Interpreter
--withInterp go ".L4" = go l1Interpreter
--withInterp go ".L5" = go l1Interpreter