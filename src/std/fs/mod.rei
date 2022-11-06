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
use pkg::common::Byte,Size

// use () if you specifically want to return empty
// otherwise _ for template deducTION!!!@$!!T@SGG

// when you have a duo union type, we always assume the first type is the "Ok" and the second type is the "Err"
// duo union types allow sugar like ! and ? to work seamlessly

export write: (file: &str, buf: &[Byte]) -> () | String {
    @target(os = "neutron")
    neutronapi::write(file, buf)!?
}
