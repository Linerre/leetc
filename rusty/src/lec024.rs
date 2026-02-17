/// Similar to `partition2` in mode lec023
pub fn partition(arr: &mut Vec<i32>, l: usize, r: usize, x: i32) -> (usize, usize) {
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

pub fn find_kth_largest(mut nums: Vec<i32>, k: usize) -> i32 {
    let mut ans: i32 = 0;
    let mut l: usize = 0;
    let mut r: usize = nums.len() - 1;
    for _ in l..=r {
        // FIXME: use a real random number
        let random = *(nums.last().unwrap());
        let (first, last) = partition(&mut nums, l, r, random);
        if k < first {
            r = first - 1;
        } else if k > last {
            l = last + 1;
        } else {
            ans = nums[k];
            break;
        }
    }
    ans
}
