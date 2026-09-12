import { assertEquals } from '@std/assert/equals';
import { countNodes } from './count-nodes.ts';
import { makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test countNodes 1',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3,4,5,6];
    const root = makeBTreeFromArray(input);
    assertEquals(countNodes(root), 6);
  }
});

Deno.test({
  name: 'Test countNodes 2',
  timeout: 1000,
  fn: () => {
    const input: number[] = [];
    const root = makeBTreeFromArray(input);
    assertEquals(countNodes(root), 0);
  }
});

Deno.test({
  name: 'Test countNodes 3',
  timeout: 1000,
  fn: () => {
    const input = [1];
    const root = makeBTreeFromArray(input);
    assertEquals(countNodes(root), 1);
  }
});

Deno.test({
  name: 'Test countNodes 4',
  timeout: 1000,
  fn: () => {
    const input = [1,2,3,4];
    const root = makeBTreeFromArray(input);
    assertEquals(countNodes(root), 4);
  }
});
