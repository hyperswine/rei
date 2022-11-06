#*
    Silicon Compiler and Simulator
*#

// generate an optimised silicon chip using values like number of interconnect layers
// fabrication process

// ...now "default values" on enum children
// maybe super.var = ?
// or a fn that returns it?

default_fab_process: (fab_process: FabProcess) -> FabProcessSize {
    match fab_process {
        ElectronBeam => 5
    }
}

FabProcessSize: Nm

FabProcess: enum {
    ElectronBeam
}

// generate interconnects

// basically, just a bunch of wires and stuff here and there that join together points
// maybe each continuous wire is a list of 2d equations or a list of ((x0, y0), (x1, y1))
ContinuousWire: Vec[LinearEquation]

Interconnect: {
    layer: Vec[ContinuousWire]
}

gen_interconnect: () -> Interconnect {}
