#*
    A Minimal and Extendible ECS Framework
*#

ECSBackend: {}

// fine, but not exportable and can only be mutated in unsafe contexts
// to allow complex state management, export an unsafe fn that mutates the value 
// or an impure fn with a local unsafe block
mut global_ecs = ECSBackend()

// Unlike macros, annotations dont change the input stream/return a modified one
// Instead, they control access to global systems and resources

# A system. Registering a function with @sys makes the ECS backend know of the system and callable from the main system thread
export sys: annotation (fn_expr:Fn) {
    // all systems are functions that take in components
    global_ecs.register(fn_expr)
}

// Could use a hardware accel backend for this, by @compute. Which gets compiler to try and make all invocations a compute shader instead and schedule on a proper hardware accelerator
@compute
gravity: system (dt: f32, velocities: &mut [Vel3D], gravitational_constant: Accel3D) {
    velocities.for_each(v => {
        v += gravitational_constant * dt
    })
}

F32A3: [f32; 3]

@component
Pos3D: F32A3

@component
Vel3D: F32A3

@component
Accel3D: F32A3

@component
Player3D: {
    pos3d: Pos3D
}

// Never write an impl like in rust for a struct
// Always do the fn do_something_on(T, ...) style
// Associated style in systems
// Methods in systems
