import { Stack } from '@datastructures-js/stack';

export function reverse(stack: Stack<number>): void {
  if (stack.isEmpty()) {
    return;
  } else {
    const num = bottomOut(stack);
    reverse(stack);
    stack.push(num);
  }
}

// Pop/return the bottom item in the stack. Other items fall down with
// the order unchanged. For example, [a,b,c] -> bottomOut -> c, [a,b]
function bottomOut(stack: Stack<number>): number {
  const bottom = stack.pop();
  if (typeof bottom === 'number' && stack.isEmpty()) {
    return bottom;
  } else {
    const last = bottomOut(stack);
    if (typeof bottom === 'number') stack.push(bottom);
    return last;
  }
}

