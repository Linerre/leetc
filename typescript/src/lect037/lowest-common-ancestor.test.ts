import { assertEquals } from '@std/assert/equals';
import { lowestCommonAncestor } from './lowest-common-ancestor.ts';
import { makeBTreeFromArray } from '../lect036/util.ts';

Deno.test({
  name: 'Test lowestCommonAncestor 1',
  timeout: 1000,
  fn: () => {
    const input = [3,5,1,6,2,0,8,null,null,7,4];
    const root = makeBTreeFromArray(input);
    const p = root!.left;
    const q = root!.right;
    assertEquals(lowestCommonAncestor(root, p, q), root);
  }
});

Deno.test({
  name: 'Test lowestCommonAncestor 2',
  timeout: 1000,
  fn: () => {
    const input = [3,5,1,6,2,0,8,null,null,7,4];
    const root = makeBTreeFromArray(input);
    const p = root!.left;
    const q = p!.right!.right;
    assertEquals(lowestCommonAncestor(root, p, q), p);
  }
});

Deno.test({
  name: 'Test lowestCommonAncestor 3',
  timeout: 1000,
  fn: () => {
    const input = [1,2];
    const root = makeBTreeFromArray(input);
    const p = root;
    const q = root!.left;
    assertEquals(lowestCommonAncestor(root, p, q), root);
  }
});
