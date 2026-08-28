import { assertEquals } from '@std/assert/equals';
import { SetAllMap } from './setallmap.ts';

// see: https://www.nowcoder.com/practice/7c4559f138e74ceb9ba57d76fd169967
Deno.test({
    name: 'SetAllMap test',
    timeout: 5000,
    fn: () => {
        const map = new SetAllMap();
        // opt1
        map.put(1,2);
        assertEquals(map.get(1), 2);
        // opt2
        assertEquals(map.get(1), 2);
        // opt3
        assertEquals(map.get(2), -1);
        // opt4
        map.setAll(4);
        // opt5
        assertEquals(map.get(1), 4);
        // opt6
        assertEquals(map.get(2), -1);
    }
});

Deno.test({
    name: 'SetAllMap test',
    timeout: 5000,
    fn: () => {
        const m = new SetAllMap();
        m.put(5, 17);
        m.put(6, 100);
        m.setAll(9);
        assertEquals(m.get(5), 9);
        m.put(19, 54);
        assertEquals(m.get(19), 54);
    }
});
