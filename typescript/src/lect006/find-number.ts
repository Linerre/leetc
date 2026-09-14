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
