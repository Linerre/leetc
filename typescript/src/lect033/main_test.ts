import { assertEquals } from 'jsr:@std/assert';
import { add, sub, multi, div, divide } from './main.ts';

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

Deno.test('Test bitwise muplitpliaton', () => {
    const a = 4;
    const b = 25;
    assertEquals(multi(a,b), 100);
})

Deno.test('Test bitwise div1', () => {
    const a = 25;
    const b = 4;
    assertEquals(div(a,b), 6);
})


Deno.test('Test bitwise div2', () => {
    const a = -10;
    const b = 2;
    assertEquals(div(a,b), -5);
})

Deno.test('Test bitwise div3', () => {
    const a = -Math.pow(2, 31);
    const b = Math.pow(2, 20);
    assertEquals(divide(a,b), -Math.pow(2,11));
})
