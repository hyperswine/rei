#*
    Neutron API
*#

export enum SyscallNumber {
    // Handles
    Close = 0
    Duplicate
    Read
    Write

    // Files
    FileOpen

    // Objects
    ObjectInfo
    ObjectSetProperties
    ObjectSignal
    ObjectWait

    // Memory
    VMOCreate
    VMOTransfer
    VMOMap

    // Tasks
    TaskSpawn
    TaskSuspend
    TaskKill
    ThreadCreate
    ThreadKill

    // IPC
    PortCreate
    PortClose
    PortConnect
}

@cfg(target_arch = "riscv64") {
    sys_number_reg = asm::riscv::s0
    syscall = asm::riscv::ecall
    result_reg = asm::riscv::s0
}

export type HandleNumber = Size
export type ServiceResult<T> = |T|

export fn close(handle_number: HandleNumber) -> ServiceResult<()> {
    @asm {
        sys_number_reg = SyscallNumber::Close
        a0 = handle_number
        syscall
    }

    result_reg?
}

// core::types::Bytes is auto exported to core::prelude::Bytes

export fn read(handle_number: HandleNumber, buf: &mut Bytes, offset: Size) -> ServiceResult<()> {
    // asm is an annotation defined in the core lib
    // nice reference: https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md
    @asm {
        // load regs
        sys_number_reg = SyscallNumber::Read
        // params
        a0 = handle_number
        a1 = buf
        a2 = offset
        syscall
    }

    // return result in result_reg
    result_reg?
}

export fn write(handle_number: HandleNumber, write_bytes: &mut Bytes, offset: Size) -> ServiceResult<()> {
    @asm {
        sys_number_reg = SyscallNumber::Read
        a0 = handle_number
        a1 = write_bytes
        a2 = offset
        syscall
    }

    result_reg?
}

export fn file_open(filepath: &str) -> ServiceResult<HandleNumber> {
    @asm {
        sys_number_reg = SyscallNumber::FileOpen
        a0 = handle_number
        syscall
    }

    result_reg?
}

export fn task_spawn() -> ServiceResult<HandleNumber> {
    @asm {
        sys_number_reg = SyscallNumber::TaskSpawn
        syscall
    }

    result_reg?
}

export fn task_kill(task_id: HandleNumber) -> ServiceResult<()> {
    @asm {
        sys_number_reg = SyscallNumber::TaskKill
        a0 = task_id
        syscall
    }

    result_reg?
}

export fn port_create() -> ServiceResult<HandleNumber> {
    @asm {
        sys_number_reg = SyscallNumber::TaskKill
        syscall
    }

    result_reg?
}

export fn port_close(port_id: HandleNumber) -> ServiceResult<HandleNumber> {
    @asm {
        sys_number_reg = SyscallNumber::PortClose
        syscall
    }

    result_reg?
}

// @test should be highlighted as orange, different from other stuff like @asm, @arcen, etc. Core lib hook onto lang server
@test(expect = "fail")
fn test_ports() {
    // set process to a non privileged task, if cant set privilege, then already an unpriv task
    set_privilege(0)

    // you cant close task 0 (the kernel) through a non privileged task
    assert(port_close(0).is_ok())
}
