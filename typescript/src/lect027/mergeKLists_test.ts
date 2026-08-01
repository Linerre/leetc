import { assertEquals } from 'jsr:@std/assert';
import { ListNode, mergeKLists } from './mergeKLists.ts';

Deno.test('mergeKLists-1', () => {
    const lists: ListNode[] = [
        { val: 1, next: { val: 4, next: { val: 5, next: null }}},
        { val: 1, next: { val: 3, next: { val: 4, next: null }}},
        { val: 2, next: { val: 6, next: null }},
    ];
    const expected = {
        val: 1,
        next: {
            val: 1,
            next: {
                val: 2,
                next: {
                    val: 3,
                    next: {
                        val: 4,
                        next: {
                            val: 4,
                            next: {
                                val: 5,
                                next: {
                                    val: 6,
                                    next: null
                                }
                            }
                        }
                    }
                }
            }
        }
    };
    assertEquals(mergeKLists(lists), expected);
})
