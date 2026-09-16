export function findNumber(nums: number[], target: number): boolean {
  if (nums.length === 0) return false;

  let l = 0;
  let r = nums.length - 1;
  let m = 0;
  while (l <= r) {
    m = Math.floor((l+r)/2);
    if (nums[m] === target) return true;
    else if (nums[m] > target) r = m - 1;
    else l = m + 1
  }
  return false;
}

// Find the leftmost number >= target in a sorted array and return its index
export function findLeft(nums: number[], target: number): number {
  let l = 0;
  let r = nums.length - 1;
  let m = 0;
  let ans = -1;
  while (l <= r) {
    m = l + (r - l >> 1);       // equivalent to m = Math.floor((l + r) / 2) but avoid overflow
    if (nums[m] >= target) {
      ans = m;                  // remember this index
      r = m - 1;                // move left and see if there is any more
    } else {
      l = m + 1                 // move right
    }
  }
  return ans;
}

// Find the rightmost number <= target in a sorted array and return its index
export function findRight(nums: number[], target: number): number {
  let l = 0;
  let r = nums.length - 1;
  let m = 0;
  let ans = -1;
  while (l <= r) {
    m = l + (r - l >> 1);       // equivalent to m = Math.floor((l + r) / 2) but avoid overflow
    if (nums[m] <= target) {
      ans = m;                  // remember this index
      l = m + 1;                // move right and see if there is any more
    } else {
      r = m - 1                 // move left
    }
  }
  return ans;
}
