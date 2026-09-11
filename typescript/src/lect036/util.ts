import { Queue } from '@datastructures-js/queue';
/**
 * Definition for a binary tree node.
 */
export class TreeNode {
  val: number;
  left: TreeNode | null;
  right: TreeNode | null;
  constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
    this.val = (val===undefined ? 0 : val);
    this.left = (left===undefined ? null : left);
    this.right = (right===undefined ? null : right);
  }
}

export type NodeVal = number | null;

export function makeBTreeFromArray(vals: NodeVal[]): TreeNode {
  const queue = new Queue<TreeNode | null>();
  const n = vals.length;
  const root = new TreeNode(vals[0] ?? 0);
  queue.enqueue(root);

  let i = 1;
  while (i < n) {
    const parent = queue.dequeue();
    if (parent !== null) {
      const leftVal = vals[i++];
      const rightVal = vals[i++];
      const leftNode = leftVal != undefined ? new TreeNode(leftVal) : null;
      const rightNode = rightVal != undefined ? new TreeNode(rightVal) : null;
      parent.left = leftNode;
      parent.right = rightNode;
      queue.enqueue(leftNode);
      queue.enqueue(rightNode);
    }
  }
  return root;
}

export function treeToArray(head: TreeNode | null): NodeVal[] {
  const arr = Array<NodeVal>();
  if (head) {
    const queue = new Queue<TreeNode | null>();
    queue.enqueue(head);
    while (!queue.isEmpty()) {
      const node = queue.dequeue();
      if (node) {
        arr.push(node.val);
        queue.enqueue(node.left);
        queue.enqueue(node.right);
      } else {
        arr.push(null);
      }
    }
  }

  // clean up trailing nulls of leaf nodes
  while (arr.length > 0 && arr[arr.length - 1] === null) {
    arr.pop();
  }
  return arr;
}
