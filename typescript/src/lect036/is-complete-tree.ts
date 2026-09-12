import { TreeNode } from './util.ts';

// Medium 958: https://leetcode.cn/problems/check-completeness-of-a-binary-tree/
export function isCompleteTree(root: TreeNode | null): boolean {
  // according to problem description, this case is considered true
  if (root === null) return true;

  // use BFS
  const queue = Array<TreeNode | null>(101);
  let l = 0;
  let r = 0;
  queue[r++] = root;
  // mark if there is a leaf when doing BFS
  let leaf = false;
  while (l < r) {
    const node = queue[l++];
    // Any of the below two conditions is broken, not a complete tree
    // 1. if a node has right child, it must has left child
    // 2. if a node has only left child, all other following nodes must be leaves (no children)
    if ((node && node.right && node.left === null ) || (leaf && node && (node.left || node.right)))
      return false;

    // continue BFS
    if (node && node.left) queue[r++] = node.left;
    if (node && node.right) queue[r++] = node.right;

    // when hit a leaf node, turn on left
    if (node && (node.left === null || node.right === null))
      leaf = true;
  }

  return true;
};
