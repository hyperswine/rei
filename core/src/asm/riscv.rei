#*
    Risc V Registers and Etc
*#
@!cfg(target_arch = "riscv64")

// NOTE: the export(mod_name) exports the item to the mod name
// export by itself makes it available
// export(root) puts it at the root

export(super) GeneralPurposeReg: enum {
    x0

    ra
    sp
    gp
    tp

    @infer t0..t6

    @infer s0..s11

    @infer a0..a7
}

export(super) FloatingPointReg: enum {
    @infer f0..f31
}
