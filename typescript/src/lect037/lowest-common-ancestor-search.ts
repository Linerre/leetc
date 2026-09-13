import { TreeNode } from '../lect036/util.ts';

// Medium  235: https://leetcode.cn/problems/lowest-common-ancestor-of-a-binary-search-tree/description/

/**
 * In top-down order, starting with root of the entire tree,
 * if root comes to p first, p is the lowest common ancestor;
 * if root comes to q first, q is the lowest common ancestor;
 * if root comes to a node whose value lies in between p's and q's, this node is lowest common anccestor;
 * if root comes to a node whose value is smaller than min(p, q), move root to right
 * if root comes to a node whose value is larger than max(p, q), move root to left
 */
export function lowestCommonAncestor(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null,
): TreeNode | null {
  while (root && p && q && root.val !== p.val && root.val !== q.val) {
    if (Math.min(p.val, q.val) < root.val && root.val < Math.max(p.val, q.val)) break;

    root = root.val < Math.min(p.val, q.val) ? root.right : root.left;
  }
  return root;
}
