import { assertEquals } from '@std/assert/equals';
import { calculate } from './basic-calculater-iii.ts';

Deno.test({
  name: 'Test calculate 1',
  timeout: 1000,
  fn: () => {
    const expr = '36-(4*(3+2*(1-6))+5)+17';
    const ans = calculate(expr);
    assertEquals(ans, 36-(-23)+17);
  }
});
