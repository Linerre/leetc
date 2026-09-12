import { TreeNode } from './util.ts';

// Medium 222: https://leetcode.cn/problems/count-complete-tree-nodes/
function countNodes(root: TreeNode | null): number {
  if (root === null) return 0;
  // get the entire tree's height
  const h = leftMostLevel(root, 1);
  return count(root, 1, h);
};

// Given a node at level, with height of entire tree known, count
// nodes of with the given node as root.
function count(node: TreeNode | null, level: number, height: number): number {
  if (node === null) return 0;
  if (level === height) return 1; // leaf node

  // find whether current node's right child's leftmost child reaches
  // height; if so, its left subtree is a full complete binary tree,
  // whose total number of nodes is 2^(h - level) and the right
  // substree can be recursively counted. If not so, its right subtree
  // is a full complete binary tree with total number of nodes being
  // 2^(h - level - 1) and its left substree will need recursive
  // counting.
  if (leftMostLevel(node.right, level + 1) === height) {
    return (1 << (height - level)) + count(node.right, level + 1, height);
  } else {
    return (1 << (height - level - 1)) + count(node.left, level + 1, height)
  }
}
// Given a node at level l, find the level of its leftmost child
function leftMostLevel(node: TreeNode | null, l: number): number {
  while (node) {
    l++;
    node = node.left;
  }
  // if node is leaf node, the while loop inc l one more time, so in
  // the end, before return, dec l
  return l - 1;
}
