#*
    A Minimal and Extendible ECS Framework
*#

ECSBackend: {}

// defined in prei or rei core? like rust's once_cell, but wrapped around in a lock free ring buffer
// all once cells can be accessed by anything else in the direct module
once RingBuffer { mut global_ecs = ECSBackend() }

// Unlike macros, annotations dont change the input stream/return a modified one
// Instead, they control access to global systems and resources

# A system makes the ECS backend know of the fn and callable from the main system thread
export system: annotation (fn_expr: Fn) {
    // all systems are functions that take in components
    global_ecs.register(fn_expr)
}

// Could use a hardware accel backend for this, by @compute. Which gets compiler to try and make all invocations a compute shader instead and schedule on a proper hardware accelerator
gravity: system (dt: f32, velocities: &mut [Vel3D], gravitational_constant: Accel3D) {
    velocities.for_each(v => v += gravitational_constant * dt)
}

F32A3: [f32; 3]

Pos3D: F32A3

Vel3D: F32A3

Accel3D: F32A3

Player3D: {
    pos3d: Pos3D
}

// Never write an impl like in rust for a struct
// Always do the fn do_something_on(T, ...) style
// Associated style in systems
// Methods in systems
