/// Merge sort using recursion

/// Merge two sorted subarrays into a single array.
/// `m` is the middle point index with original array size less than 2^33
/// This version follows the pseudo code in chap2.3 of Algorithms by Jeff Erickson
pub fn merge1<T: Ord + Clone + Default>(src_arr: &mut [T], m: usize) {
    let n = src_arr.len();
    let mut i = 0usize;
    let mut j = m + 1;
    let mut dest = Vec::<T>::with_capacity(n);
    for _ in 0..n {
        dest.push(T::default());
    }
    for k in 0..n {
        if j >= n {                       // right subarray is empty
            dest[k] = src_arr[i].clone(); // src_arr[i] is the smallest
            i += 1;
        }
        else if i > m {                   // left subarray is empty
            dest[k] = src_arr[j].clone(); // src_arr[j] is the smallest
            j += 1;
        }
        else if src_arr[i] < src_arr[j] {
            dest[k] = src_arr[i].clone();
            i += 1;
        } else {
            dest[k] = src_arr[j].clone();
            j += 1;
        }
    }
    // update the source array
    src_arr.clone_from_slice(&dest);
}

// This fn must return either a merged-sorted subarray or the passed array as is;
// Otherwise the algorithm gets stuck because this fn essentially does nothing.
// The pseudocode does not indicate this point but the real code must do so
pub fn mergesort1<T: Ord + Clone + Default>(src_arr: &mut [T])-> Vec<T> {
    let n = src_arr.len();
    if n > 1 {
        let m = n / 2;
        let mut left = mergesort1(&mut src_arr[0..m]);
        let mut right = mergesort1(&mut src_arr[m..]);
        left.append(&mut right);
        merge1(&mut left, m-1); // middle point here needs to be rounded down
        left.to_vec()
    } else {
        src_arr.to_vec()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mergesort_v1() {
        // let mut src_arr = vec![8, 3];
        // assert_eq!([3, 8], dest.as_slice());
        let mut src_arr1 = vec![3, 8, 1, 7, 2, 5,];
        let dest1 = mergesort1(&mut src_arr1);
        assert_eq!([1, 2, 3, 5, 7, 8], dest1.as_slice());

        let mut src_arr2 = vec![3, 8, 1, 7, 2, 5, 10];
        let dest2 = mergesort1(&mut src_arr2);
        assert_eq!([1, 2, 3, 5, 7, 8, 10], dest2.as_slice());
    }
}
