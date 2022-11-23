#*
    Variety of Sorting Tools
*#

// find_bound[T]: (val: T, buckets: &BinaryTree[T]) -> T {
//     // find the bucket where val < bucket
//     buckets.find(b => val < b)
// }

// what does sample look like
// NOTE: T* means local generics within the signature, refer to them as T within the body
sample: (array: [T*; N*], n: Size) {
    // method 1: use a hardware rng to keep generating n values where rand(a, b) basically does (rand + a) % b
    [val for val in 0..n => rand(0, N)]
}

// O(log2(n!)) time complexity
// sort an array on a functional computer with P-wide SIMD executors or G * P-wide SIMT executors
// think of samplesort as an enlightened quicksort with a better way of getting the distribution of values
@cfg(target_arch = Functional::ExecutorS3)
samplesort[T, P, K, S]: (array: &[T]) -> &[T] {
    static(lazy) threshold = core::info::executors_per_cluster()
    let n = array.len()
    if n/K < threshold => quicksort(array)

    // basically, choose S random indicies without replacement. With hardware PRNG, should be fast
    // O(n/p + log(p))
    let sample = arr.sample(n=S)
    let sample = quicksort(sample)
    let splitters = sample[..(P-1)]

    // template should be preconstructed at compile time
    let buckets = BinaryTree[T, P](splitters)
    // sort the array into buckets by scheduling another executor
    array.parallel_for_each(val, index => {
        let bucket = buckets.search_lower_bound(val)
        buckets[bucket][index] = val
    })

    // recursively sort each bucket and combine them
    buckets.parallel_map(bucket => samplesort(bucket))
}

// in place samplesort, using hyper types

// when n > 10m
// and p > 1000
// pretty good
