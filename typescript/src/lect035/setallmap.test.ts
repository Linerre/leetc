import { assert, assertEquals } from 'jsr:@std/assert';
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
