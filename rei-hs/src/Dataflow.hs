module Dataflow where
import qualified Data.Map as Map

data Node = Data | Execute
-- data NNode = NNode Node Bool

data AtomicInstruction = Add | Sub | Mul | Div | Copy | Run
data BinaryInstruction = BinaryInstruction AtomicInstruction Source Source Dest
data UnaryInstruction = UnaryInstruction AtomicInstruction Source Dest
type Operand = Int
type Source = Operand
type Dest = Operand
type ProgramCounter = Int
type Stack = Node

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

-- global clock
-- if data changes, nodes affected will be run

-- MODEL A': attached node as an address or 64 bit ID, so usually 2 64 bit IDs
-- executeInstruction ins | Add = +

-- always start at a node, use that node to CAM in?
-- no like an entire map of nodes

nodes :: Map.Map String Node
nodes = Map.empty

data Context = Context Node Stack ProgramCounter
