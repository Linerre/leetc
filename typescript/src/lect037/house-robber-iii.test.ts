import { assertEquals } from '@std/assert/equals';
import { rob } from './house-robber-iii.ts';
import { makeBTreeFromArray } from '../lect036/util.ts';

Deno.test({
  name: 'Test rob 1',
  timeout: 1000,
  fn: () => {
    const input = [3,2,3,null,3,null,1];
    const output = 7;
    const root = makeBTreeFromArray(input);
    assertEquals(rob(root), output);
  }
});

Deno.test({
  name: 'Test rob 2',
  timeout: 1000,
  fn: () => {
    const input = [3,4,5,1,3,null,1];
    const output = 9;
    const root = makeBTreeFromArray(input);
    assertEquals(rob(root), output);
  }
});
