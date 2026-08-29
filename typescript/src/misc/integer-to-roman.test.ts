import { assertEquals } from '@std/assert/equals';
import { intToRoman } from './integer-to-roman.ts';

Deno.test({
  name: 'Test intToRoman',
  timeout: 1000,
  fn: () => {
    const num1 = 3749;
    const roman1 = 'MMMDCCXLIX';
    assertEquals(intToRoman(num1), roman1);
    const num2 = 58;
    const roman2 = 'LVIII';
    assertEquals(intToRoman(num2), roman2);
    const num3 = 1994;
    const roman3 = 'MCMXCIV';
    assertEquals(intToRoman(num3), roman3);
  },
});
