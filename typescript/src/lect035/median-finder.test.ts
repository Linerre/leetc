import { assertEquals } from '@std/assert/equals';
import { MedianFinder } from './median-finder.ts';

Deno.test({
  name: 'Test MedianFinder 1',
  timeout: 1000,
  fn: () => {
    const mf = new MedianFinder();
    mf.addNum(1);
    assertEquals(mf.findMedian(), 1.0);
  }
  
});


Deno.test({
  name: 'Test MedianFinder 2',
  timeout: 1000,
  fn: () => {
    const mf = new MedianFinder();
    mf.addNum(-1);
    assertEquals(mf.findMedian(), -1.0);
    mf.addNum(-2);
    assertEquals(mf.findMedian(), -1.5);
    mf.addNum(-3);
    assertEquals(mf.findMedian(), -2.0);
    mf.addNum(-4);
    assertEquals(mf.findMedian(), -2.5);
    mf.addNum(-5);
    assertEquals(mf.findMedian(), -3.0);
  }
  
});
