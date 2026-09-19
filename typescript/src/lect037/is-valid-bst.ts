import { TreeNode } from '../lect036/util.ts';

function isValidBST(root: TreeNode | null): boolean {
  if (root === null) return true;

  let size = 0;
  let prev: TreeNode | null = null;
  const stack = Array<TreeNode>(10001);
  // prev shows inOrder traversal and wil become left, mid, right in that order

  while (size > 0 || root) {
    if (root) {
      // keep moving downward to the leftmost leaf of current node
      stack[size++] = root;
      root = root.left;
    } else {
      root = stack[--size];
      if (prev && root && prev.val >= root.val) return false;

      prev = root;
      root = root.right;
    }
  }
  
  return true;
};
