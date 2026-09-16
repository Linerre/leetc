
export function randomArray(n = 100, v = 1000): number[] {
  const arr = Array<number>(n);
  for (let i = 0; i < n; i++) {
    arr[i] = Math.floor(Math.random() * v) + 1;
  }
  return arr;
}

export function findNumberLinear(nums: number[], target: number): boolean {
  for (const n of nums) {
    if (n === target) return true;
  }
  return false;
}

export function findLeftLinear(nums: number[], target: number): number {
  for (let i = 0; i < nums.length; i++) {
    if (nums[i] >= target)
      return i;
  }
  return -1;
}

export function findRightLinear(nums: number[], target: number): number {
  for (let i = nums.length - 1; i >= 0; i--) {
    if (nums[i] <= target)
      return i;
  }
  return -1;
}
