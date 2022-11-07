# For filesystem related functionalities

@target(os = "neutron")
use neutronapi

export read_file: (file: &str) -> String | String {
    // propagate Ok, take Err
    @target(os = "neutron")
    let err = neutronapi::read(file)!

    Err("System error: $err. Couldn't read file from path: $file")
}

// can also do import pkg::common::{Byte,Size}
use pkg::common::[Byte Size]

export write: (file: &str, buf: &[Byte]) -> () | String {
    @target(os = "neutron")
    neutronapi::write(file, buf)!?
}
