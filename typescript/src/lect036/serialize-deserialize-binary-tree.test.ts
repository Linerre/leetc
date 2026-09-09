import { assertEquals } from '@std/assert/equals';
import {
  serialize1,
  serialize2,
  deserialize1,
  deserialize2,
} from './serialize-deserialize-binary-tree.ts';
import { makeBTreeFromArray } from './util.ts';

Deno.test({
  name: 'Test ser/des 1',
  timeout: 1000,
  fn: () => {
    const input1 = [1,2,3,null,null,4,5];
    const root1 = makeBTreeFromArray(input1);
    const ser1 = serialize1(root1);
    assertEquals(deserialize1(ser1), root1);

    const input2: number[] = [];
    const root2 = makeBTreeFromArray(input2);
    const ser2 = serialize1(root2);
    assertEquals(deserialize1(ser2), root2);
  }
});

Deno.test({
  name: 'Test ser/des 2',
  timeout: 1000,
  fn: () => {
    const input1 = [1,2,3,null,null,4,5];
    const root1 = makeBTreeFromArray(input1);
    const ser1 = serialize2(root1);
    assertEquals(deserialize2(ser1), root1);

    const input2: number[] = [];
    const root2 = makeBTreeFromArray(input2);
    const ser2 = serialize2(root2);
    assertEquals(deserialize2(ser2), root2);
  }
})
