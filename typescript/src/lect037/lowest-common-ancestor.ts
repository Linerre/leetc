import { TreeNode } from '../lect036/util.ts';

// Medium 236: https://leetcode.cn/problems/lowest-common-ancestor-of-a-binary-tree/description/
export function lowestCommonAncestor(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null,
): TreeNode | null {
  // when p or q found, return p or q accordingly
  if (root === null || root === p || root === q) return root;

  // try to find p and q in left tree
  const l: TreeNode | null = lowestCommonAncestor(root.left, p, q);
  // try to find p and q in right tree
  const r: TreeNode | null = lowestCommonAncestor(root.right, p, q);

  // if p and q can be found in both branches, root must be the common ancestor
  if (l && r) return root;
  // if p and q cannot be found in either branches, no common ancestor
  if (l === null && r === null) return null;
  // if only one of p or q can be found, the one found is the common
  // ancestor (p contains q or the opposite)
  return l ?? r;
}
