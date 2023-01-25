module Dataflow where

data Node = Data | Execute

data AtomicInstruction = Add | Sub | Mul | Div | Copy | Run

-- Node is of N bits
-- all data is represented in N bits, even instructions themselves
-- absolute addressing used in instructions and data
-- no absolute addressing when compiling down
-- fixed size allocator for multiples of 2 for zero external fragmentation

-- an execute node is basically a list of AtomicInstruction
-- a data node is basically a list of data 64 bits each
-- instructions take data (from compute nodes or data nodes) and write to compute nodes and data nodes
-- system IO is represented as data nodes I guess
-- the more modular the nodes the better each specific subfield can be cached

-- executeInstruction :: AtomicInstruction
-- executeInstruction =

-- I still dont know how you can run everything concurrently and "when the data is available"
-- maybe it runs each time the data changes?

