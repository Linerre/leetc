import { TreeNode } from '../lect036/util.ts';

export function trimBST(root: TreeNode | null, low: number, high: number): TreeNode | null {
  if (root === null) return null;

  if (root.val < low) return trimBST(root.right, low, high);
  if (root.val > high) return trimBST(root.left, low, high);

  // root's val is between low and high
  root.left = trimBST(root.left, low, high);
  root.right = trimBST(root.right, low, high);

  return root;
};
