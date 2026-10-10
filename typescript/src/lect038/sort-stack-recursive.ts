import { Stack } from '@datastructures-js/stack';

export function sort(stack: Stack<number>): void {
  let d = depth(stack);
  while (d > 0) {
    const max = maxItem(stack, d);
    const k = maxCount(stack, d, max);
    pushDown(stack, d, max, k);
    d -= k;
  }
}

function depth(stack: Stack<number>): number {
  if (stack.isEmpty()) return 0;
  const num = stack.pop();
  const stackDepth = depth(stack) + 1;
  if (typeof num === 'number') stack.push(num);
  return stackDepth;
}

function maxItem(stack: Stack<number>, depth: number): number {
  if (depth === 0) return Number.MIN_SAFE_INTEGER;
  const num = stack.pop();
  if (num === null) throw new Error('[maxItem]: unexpected stack top: null');
  const nextMax = maxItem(stack, depth - 1);
  const realMax = Math.max(num, nextMax);
  stack.push(num);
  return realMax;
}

function maxCount(stack: Stack<number>, depth: number, max: number): number {
  if (depth === 0) return 0;
  const num = stack.pop();
  if (num === null) throw new Error('[maxCount]: unexpected stack top: null');
  const nextCount = maxCount(stack, depth - 1, max);
  const count = nextCount + (num === max ? 1 : 0);
  stack.push(num);
  return count;
}

function pushDown(stack: Stack<number>, depth: number, max: number, k: number): void {
  if (depth === 0) {
    for (let i = 0; i < k; i++) {
      stack.push(max);
    }
  } else {
    const num = stack.pop();
    if (num === null) throw new Error('[pushDown]: unexpected stack top: null');
    pushDown(stack, depth - 1, max, k);
    if (num !== max) {
      stack.push(num);
    }
  }
}
