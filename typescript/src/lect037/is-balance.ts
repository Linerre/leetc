import { TreeNode } from '../lect036/util.ts';

export function isBalanced(root: TreeNode | null): boolean {
  const balance = true;
  const [_, nb] = height(root, balance);
  return nb;
};

function height(node: TreeNode | null, balance: boolean): [number, boolean] {
  if (!balance || node === null) return [0, balance];
  const [lh, lb] = height(node.left, balance);
  const [rh, rb] = height(node.right, lb);

  if (!lb || !rb) return [0, false];

  if (Math.abs(lh - rh) > 1) balance = false;
  return [Math.max(lh, rh) + 1, balance];
}
