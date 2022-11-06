// Dont link against std, we are std. Core and tests auto included but explicit here. As well as any other useful no-std stuff
require = {
    std
}

prelude = {
    enabled = true
}

// modules are implicit in prei, if you dont want them, dont create them or do @hide

script = {
    // depending on your platform, configs and builds for your arch extensions, os, etc.
    supported_platforms = ["macos", "linux", "neutron", "freebsd"]

    if exec("platform") in supported_platforms: exec("rei build")
    else exit(FAIL)
}

Features: enum {
    // if std is on, then the lib will be linked
    Std
    Graphics: enum => {
        Shader
    }
    Ml
    Spectro
}
