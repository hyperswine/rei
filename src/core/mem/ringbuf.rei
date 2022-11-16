// RING BUFFER (MPMC)

# MPMC, Lock Free
export default RingBuffer[T, N]: {
    data: [T; N]

    () -> Self => Self()
}
