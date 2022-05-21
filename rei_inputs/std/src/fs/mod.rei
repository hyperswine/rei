# For filesystem related functionalities

@target(os = "neutron")
use neutronapi

@target(os = "neutron")
pub fn read_file(file: &str) -> <String, &str> {
    let res = neutronapi::read(file)

    // all return statements must return an <String, &str>
    return match res {
        // I dont really like 'Some'
        Exist(r) => {
            <r>
        }
        None => {
            <"Couldn't read file", file>
        }
    }
}
