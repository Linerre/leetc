pub fn quick_sort1(arr: &mut Vec<i32>, l: usize, r: usize)  {
    if l >= r {
        return;
    }

    // Since std::random is nightly-only feature, here we always use
    // the last item as if it was picked up randomly
    let x = *(arr.last().unwrap());
    let mid = partition1(arr, l, r, x);
    quick_sort1(arr, l, mid - 1);
    quick_sort1(arr, l, mid + 1);
}

/// Given a vector of numbers, left/start and right/end, and a pivot
/// number, partition it as follows:
/// [any number <= pivot, pivot, any number > pivot]
/// return the index of pivot
pub fn partition1(arr: &mut Vec<i32>, l: usize, r: usize, x: i32) -> usize {
    // a represents the boundary where values starting at arr[a] > x
    // thus range of arr[l, a-1] contains values <= x
    // in the beginning, there is no value known to be <= x
    // the task is to move a from left to right until the cond meets
    let mut a = l;
    let mut xi = 0usize;        // the number at arr[a-1] in the end

    // there are 2 code paths:
    // 1. arr[i] <= x, then swap(a, i) and inc both a and i
    // 2. arr[i] > x, just inc i
    for i in l..=r {
        let curr = arr[i];
        if curr <= x {
            arr.swap(a, i);
            // record the number that is equal to x because
            // it will be placed at a - 1 in the end
            if arr[a] == x {
                xi = a;
            }
            a += 1;
        }
    }
    // place number that is equal to x at a-1
    arr.swap(xi, a-1);
    a - 1
}

/// Optimized parition1 which has two bourdary sentinels: a and b
/// numbers left to arr[a] all < x
/// numbers right to arr[b] all > x
/// 1. arr[i] < x, swap(a, i); a++; i++
/// 2. arr[i] ==x, i++;
/// 3. arr[i] > x, swap(b, i); b--;
pub fn partition2(arr: &mut Vec<i32>, l: usize, r: usize, x: i32) -> (usize, usize) {
    let mut a: usize = l;
    let mut b: usize = r;
    let mut i: usize = l;
    while i <= b {
        let curr = arr[i];
        if i <= b {
            if curr == x {
                i += 1;
            } else if curr < x {
                arr.swap(a, i);
                a += 1;
                i += 1;
            } else {
                arr.swap(b, i);
                b -= 1;
            }
        }
    }
    (a, b)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_partition1() {
        let mut arr: Vec<i32> = vec![2, 5, 4, 3, 1, 4, 7, 9, 10, 8];
        let b = partition1(&mut arr, 0, 9, 4);
        println!("{}", b);
        assert_eq!(&arr, &[2,4,3,1,4,5,7,9,10,8]);
        assert_eq!(b, 4)
    }

    #[test]
    fn test_partition2() {
        let mut arr: Vec<i32> = vec![3,2,5,2,4,3,5,6,7,9,6];
        let (_a, _b) = partition2(&mut arr, 0, 10, 5);
        assert_eq!(&arr, &[3,2,2,4,3,5,5,7,9,6,6]);
    }
}
