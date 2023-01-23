module Dataflow where

data Node = Data | Execute

data AtomicInstruction = Add | Sub | Mul | Div | Copy
