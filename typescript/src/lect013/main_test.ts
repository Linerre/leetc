import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import { Queue, Stack } from './main.ts';


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

Deno.test('Test array-based Stack', () => {
    const s = new Stack(20);
    const nums = [10,85,1,-2,3,5,66,21];
    assert(s.isEmpty());

    for (const num of nums) s.push(num);
    assertEquals(s.size(), nums.length);
    assertEquals(s.peek(), 21);
    assertEquals(s.pop(), 21);
    assertEquals(s.peek(), 66);
    assertEquals(s.size(), nums.length - 1);
    assertEquals(s.pop(), 66);
})
