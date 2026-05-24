import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { Queue } from './main.ts';


Deno.test('Test array-based Queue', () => {
    const q = new Queue(20);
    const nums = [10,85,1,-2,3,5,66,21];
    assert(q.isEmpty());

    for (const num of nums) q.offer(num);
    assertEquals(q.size(), nums.length);
    assertEquals(q.head(), 10);
    assertEquals(q.tail(), 21);
    assertEquals(q.poll(), 10);
    assertEquals(q.size(), nums.length - 1);
})
