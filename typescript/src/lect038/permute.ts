// Medium 46: https://leetcode.cn/problems/permutations/description/
function permute(nums: number[]): number[][] {
  const ans: number[][] = [];
  f(nums, 0, ans);
  return ans;
};

function f(nums: number[], i: number, ans: number[][]): void {
  if (i === nums.length) {
    const comb: number[] = [];
    ans.push(nums.map(n => n)); 
  } else {
    for (let j = i; j < nums.length; j++) {
      swap(nums, i, j);
      f(nums, i+1, ans);
      swap(nums, i, j);
    }
  }
}

function swap(nums: number[], i: number, j: number): void {
  const tmp = nums[i];
  nums[i] = nums[j];
  nums[j] = tmp;
}
