name = "rei"
author = "Jasen Qin"
description = "Rei Implementation & Utilities"
license = "LGPL-3.0"

rei = "2022-v1"

// Dont link against std, we are std. Core and tests auto included but explicit here. As well as any other useful no-std stuff
require = {
    std = { version = "0.1" }
}

// ensure prelude is enabled for prei
prelude = {
    enabled = true
}

// modules are implicit in prei, if you dont want them, dont create them or do @hide

// this is a rough way of checking for compatibility of target. Best to use target instead
// scripts are good for defining your own build hooks
// and are executed in order of definition
script = {
    // depending on your platform, configs and builds for your arch extensions, os, etc.
    supported_platforms = ["macos", "linux", "neutron", "freebsd"]

    if !(exec("platform") in supported_platforms) => exit(Fail)
}

Features: enum {
    Core
    // if std is on, then the lib's export prelude will be linked automatically by prei
    Std: {
        depends: Features::Core
        features: enum {
            Graphics: enum { Shader }
            MachineLearning
            Spectro
        }
    }
}

// building the entire project with the defaults will also build reic and prei binaries
// if you just want the library, then include rei in your prei project
