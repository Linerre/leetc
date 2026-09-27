import { assert, assertEquals } from '@std/assert';
import { generatePermuation } from './generate-permuation.ts';

Deno.test({
  name: 'Test generatePermutation 1',
  timeout: 1000,
  fn: () => {
    const input = 'ab';
    const output = ["","a","ab","b"];
    const ans = generatePermuation(input);
    assertEquals(ans.length, output.length);
    for (const combo of ans) {
      assert(ans.includes(combo));
    }
  }
});

Deno.test({
  name: 'Test generatePermutation 2',
  timeout: 1000,
  fn: () => {
    const input = 'dbcq';
    const output = ["","b","bc","bcq","bq","c","cq","d","db","dbc","dbcq","dbq","dc","dcq","dq","q"];
    const ans = generatePermuation(input);
    assertEquals(ans.length, output.length);
    for (const combo of ans) {
      assert(ans.includes(combo));
    }
  }
});

Deno.test({
  name: 'Test generatePermutation 3',
  timeout: 1000,
  fn: () => {
    const input = 'aab';
    const output = ["","a","aa","aab","ab","b"];
    const ans = generatePermuation(input);
    assertEquals(ans.length, output.length);
    for (const combo of ans) {
      assert(ans.includes(combo));
    }
  }
});
