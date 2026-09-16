## Question
I have a typescript implementation as follows, but the test sometimes succeed and sometimes fail

```typescript
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
      r = m - 1;                // move left
    }
  }
  return ans;
}

export function findRightLinear(nums: number[], target: number): number {
  for (let i = nums.length - 1; i >= 0; i--) {
    if (nums[i] <= target)
      return i;
  }
  return -1;
}

Deno.test({
  name: 'Test findRight',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort();
    assertEquals(findRight(nums, target), findRightLinear(nums, target));
  }
});
```

Find the potential bug and fix it

## Response
Found it — the bug is in the test, not in `findRight` itself.

```typescript
nums.sort();
```

JavaScript/TypeScript's `Array.prototype.sort()` **defaults to lexicographic (string) ordering** when no comparator is given. So `[2, 10, 1]` sorts to `[1, 10, 2]` instead of `[1, 2, 10]`. Your array isn't actually numerically sorted, which violates the precondition of both `findRight` (binary search requires a truly sorted array) and `findRightLinear`.

This explains the flaky behavior: the test only fails when the random array happens to contain values whose lexicographic order differs from numeric order (e.g., contains something like `9` and `10`), which happens some of the time but not always.

**Fix:**

```typescript
nums.sort((a, b) => a - b);
```

Full corrected test:

```typescript
Deno.test({
  name: 'Test findRight',
  timeout: 1000,
  fn: () => {
    const V = 1000;
    const nums = randomArray();
    const target = Math.floor(Math.random() * V);
    nums.sort((a, b) => a - b); // numeric sort, not lexicographic
    assertEquals(findRight(nums, target), findRightLinear(nums, target));
  }
});
```

Side note: the `m = l + (r - l >> 1)` line is fine — in JS, `-` binds tighter than `>>`, so it correctly evaluates as `l + ((r - l) >> 1)`, matching the Java version. That part isn't the issue.
