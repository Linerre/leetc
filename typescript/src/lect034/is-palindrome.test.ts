import { assert, assertFalse } from 'jsr:@std/assert';
import { isPalindrome } from './is-palindrome.ts';
import { makeListFromArray } from './util.ts';

Deno.test({
    name: 'Test isPalindrome 1',
    timeout: 3000,
    fn: () =>  {
        const list = [1,2,2,1];
        const head = makeListFromArray(list);
        assert(isPalindrome(head));
    },
});

Deno.test({
    name: 'Test isPalindrome 2',
    timeout: 3000,
    fn: () =>  {
        const list = [1,2];
        const head = makeListFromArray(list);
        assertFalse(isPalindrome(head));
    },
});

Deno.test({
    name: 'Test isPalindrome 3',
    timeout: 3000,
    fn: () =>  {
        const list = [1,3,5,7,5,3,1];
        const head = makeListFromArray(list);
        assert(isPalindrome(head));
    },
});
