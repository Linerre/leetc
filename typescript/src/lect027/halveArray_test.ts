import { assertEquals } from 'jsr:@std/assert';
import { halveArray1 } from './halveArray.ts';

Deno.test('halveArray-1', () => {
    const nums = [5,19,8,1];
    assertEquals(halveArray1(nums), 3);
})

Deno.test('halveArray-2', () => {
    const nums = [3,8,20];
    assertEquals(halveArray1(nums), 3);
})


Deno.test('halveArray-3', () => {
    const nums = [6,58,10,84,35,8,22,64,1,78,86,71,77];
    assertEquals(halveArray1(nums), 9);
})


Deno.test('halveArray-4', () => {
    const nums = [32,98,23,14,67,40,26,9,96,96,91,76,4,40,42,2,31,13,16,37,62,2,27,25,100,94,14,3,48,56,64,59,33,10,74,47,73,72,89,69,15,79,22,18,53,62,20,9,76,64];
    assertEquals(halveArray1(nums), 36);
})
