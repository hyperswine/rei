#*
    Neutron specific functions
*#
@!cfg(target_os = neutron)

use neutronapi

export neutronapi::prelude::*

File: u64

// read a file (whether already opened or not is managed by kernel)
export(super) read: (path: &str) -> ServiceResult[Bytes] {
    let res = open(path: &str)?
    
    neutronapi::read(res)?
}

export(super) read: (fileobj: File) -> ServiceResult[Bytes] {
    neutronapi::read(fileobj as HandleNumber)?
}

export(super) write: (path: &str, write_bytes: &mut Bytes) -> ServiceResult[()] {
    let res = open(path: &str)?

    neutronapi::write(res)?
}

export(super) open: (path: &str) -> ServiceResult[HandleNumber] {
    file_open(path)?
}

export(super) close: (handle_number: HandleNumber) -> ServiceResult[()] {
    neutronapi::close(handle_number)
}
