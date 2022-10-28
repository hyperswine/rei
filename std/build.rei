// Dont link against std, we are std. Core and tests auto included but explicit here. As well as any other useful no-std stuff
require = {
    core = { path = "../core" }
    test = { path = "../reitest" }
}

@reish
script = {
    // depending on your platform, configs and builds for your arch extensions, os, etc.
    supported_platforms = ["macos", "linux", "neutron", "freebsd"]

    if exec("platform") in supported_platforms: exec("rei build")
    else exit(FAIL)
}

Features: enum {
    Graphics: enum => {
        Shader
    }
    Ml
    Spectro
}
