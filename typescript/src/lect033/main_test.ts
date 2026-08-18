import { assertEquals } from 'jsr:@std/assert';
import { add, sub, multi, div } from './main.ts';

Deno.test('Test bitwise addition', () => {
    const a = 110;
    const b = 123;
    assertEquals(add(a,b), 233);
})

Deno.test('Test bitwise subtraction', () => {
    const a = 433;
    const b = 200;
    assertEquals(sub(a,b), 233);
})
