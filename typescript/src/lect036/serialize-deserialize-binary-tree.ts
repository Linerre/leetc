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
export function serialize1(root: TreeNode | null): string {
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
export function deserialize1(data: string): TreeNode | null {
  const vals = data.split(',');
  return _des(vals);
};

function _des(vals: string[]): TreeNode | null {
  const val = vals.shift();
  if (val === undefined || val === '#') {
    return null;
  } else {
    const node = new TreeNode(Number.parseInt(val));
    node.left = _des(vals);
    node.right = _des(vals);
    return node;
  }
}


/**
 * This implementation does not rely on recursion
 */
export function serialize2(root: TreeNode | null): string {
  const ser = Array<string>();
  const queue = Array<TreeNode>(10001);

  if (root) {
    let l = 0;
    let r = 0;

    // serialize and push into queue
    ser.push((root.val).toString());
    queue[r++] = root;

    // repeat until queue becomes empty (l == r)
    while (l < r) {
      const node = queue[l++];
      if (node && node.left) {
        ser.push((node.left.val).toString());
        queue[r++] = node.left;
      } else {
        ser.push('#');
      }

      if (node && node.right) {
        ser.push((node.right.val).toString());
        queue[r++] = node.right;
      } else {
        ser.push('#');
      }
    }
  }
  return ser.join();
}

export function deserialize2(data: string): TreeNode | null {
  if (data.length === 0) return null;
  let index = 0;                // for consuming vals
  let l = 0;                    // for consuming queue
  let r = 0;
  const queue = Array<TreeNode>(10001)
  const vals = data.split(',');
  const root = generate(vals[index++]);
  if (root) queue[r++] = root;

  while (l < r) {
    const node = queue[l++];
    node.left = generate(vals[index++]);
    node.right = generate(vals[index++]);
    if (node.left) queue[r++] = node.left;
    if (node.right) queue[r++] = node.right;
  }
  return root;
}

function generate(val: string): TreeNode | null {
  return val === '#' ? null : new TreeNode(Number.parseInt(val));
}


