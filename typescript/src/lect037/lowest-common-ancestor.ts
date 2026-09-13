import { TreeNode } from '../lect036/util.ts';

function lowestCommonAncestor(
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
  if (l === null && r === null) return null;
  return l ?? r;
}
