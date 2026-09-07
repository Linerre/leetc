import { TreeNode } from './util.ts';

export function maxDepth(root: TreeNode | null): number {
  return root === null ? 0 : Math.max(maxDepth(root.left), maxDepth(root.right)) + 1;
};

export function minDepth(root: TreeNode | null): number {
  if (root === null) return 0;
  if (root.left === null && root.right === null) return 1;

  let leftDepth = Number.MAX_SAFE_INTEGER;
  let rightDepth = Number.MAX_SAFE_INTEGER;

  if (root.left) leftDepth = minDepth(root.left);
  if (root.right) rightDepth = minDepth(root.right);

  return Math.min(leftDepth, rightDepth) + 1;
};
