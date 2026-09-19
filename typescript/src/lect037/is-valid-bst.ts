import { TreeNode } from '../lect036/util.ts';

// Medium 98:  https://leetcode.cn/problems/validate-binary-search-tree/description/
export function isValidBST(root: TreeNode | null): boolean {
  if (root === null) return true;

  let size = 0;
  let prev: TreeNode | null = null;
  const stack = Array<TreeNode>(10001);
  // prev shows inOrder traversal and wil become left, mid, right in that order

  while (size > 0 || root) {
    if (root) {
      // put entire left edge of current head/root into stack
      stack[size++] = root;
      root = root.left;
    } else {
      // pop node from stack and compare with previous one
      // if previous one is null, skip
      // if previous one is left, current is mid
      // if previous one is mid, current is right
      root = stack[--size];
      if (prev && root && prev.val >= root.val) return false;

      prev = root;
      root = root.right;
    }
  }

  return true;
};
