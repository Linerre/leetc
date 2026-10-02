export function subsetsWithDup(nums: number[]): number[][] {
  const ans: number[][] = [];
  // sort the array so the same numbers are grouped
  nums.sort();
  f(nums, 0, 0, Array<number>(), ans);
  return ans;
};

function f(nums: number[], i: number, size: number, path: number[], ans: number[][]): void {
  // path is full
  if (i === nums.length) {
    const p: number[] = [];
    for (let j = 0; j < size; j++) {
      p.push(path[j]);
    }
    ans.push(p);
  } else {
    let j = i + 1;
    // find the 1st number of next group
    while (j < nums.length && nums[i] === nums[j]) j++;
    // if current number is excluded
    f(nums, j, size, path, ans);
    // if current number is included
    for (; i < j; i++) {
      path[size++] = nums[i];
      f(nums, j, size, path, ans);
    }
  }
}
