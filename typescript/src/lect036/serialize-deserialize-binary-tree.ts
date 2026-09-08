import { TreeNode } from './util.ts';

// Hard 297: https://leetcode.cn/problems/serialize-and-deserialize-binary-tree/

/**
 * Only preOrder or postOrder can serialize and/or deserialze a binary
 * tree.  It is impoosible to do inOrder seriliazation as two trees
 * can have the same serialized result.
 */


/*
 * Encodes a tree to a single string.
 */
export function serialize(root: TreeNode | null): string {
  const ser = Array<string>();
  _ser(root, ser);
  return ser.join();
};

function _ser(node: TreeNode | null, arr: string[]): void {
  if (node === null) {
    arr.push('#');
  } 
  else {
    arr.push((node.val).toString());
    _ser(node.left, arr);
    _ser(node.right, arr);
  }
}

/*
 * Decodes your necoded data to tree.
 */
export function deserialize(data: string): TreeNode | null {
  const vals = data.split(',');
  return _der(vals);
};

function _der(vals: string[]): TreeNode | null {
  const val = vals.shift();
  if (val === undefined || val === '#') {
    return null;
  } else {
    const node = new TreeNode(Number.parseInt(val));
    node.left = _der(vals);
    node.right = _der(vals);
    return node;
  }
}
