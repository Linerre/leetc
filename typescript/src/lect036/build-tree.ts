import { TreeNode } from './util.ts';

// Medium 105: https://leetcode.cn/problems/construct-binary-tree-from-preorder-and-inorder-traversal/description
export function buildTree(preorder: number[], inorder: number[]): TreeNode | null {
  if (
    preorder.length === 0 || inorder.length === 0 ||
    preorder.length !== inorder.length
  ) return null;

  const map = new Map<number, number>();
  for (let i = 0; i < inorder.length; i++) map.set(inorder[i], i);

  return build(
    preorder,
    0,
    preorder.length - 1,
    inorder,
    0,
    inorder.length - 1,
    map,
  );
}

function build(
  preo: number[],
  l1: number,
  r1: number,
  ino: number[],
  l2: number,
  r2: number,
  map: Map<number, number>,
): TreeNode | null {
  if (l1 > r1) return null;
  const head = new TreeNode(preo[l1]);
  // one node only
  if (l1 === r1) return head;
  // find the head index k in inOrder array
  const k = map.get(preo[l1]);
  if (k === undefined) return null;

  // find the left and right substrees accordingly
  // For preOrder array:
  // left substree starts at l1 + 1 and ends at k - l2 + l1 
  // right substree starts at k - l2 + l1 + 1 and ends at r1
  // For inOrder array:
  // left substree starts at l2 and ends at k - 1 
  // right substree starts at k + 1 and ends at r2
  head.left = build(preo, l1 + 1, k - l2 + l1, ino, l2, k - 1, map);
  head.right = build(preo, k - l2 + l1 + 1, r1, ino, k + 1, r2, map);
  return head;
}
