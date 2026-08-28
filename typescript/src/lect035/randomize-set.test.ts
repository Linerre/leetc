import { assert, assertEquals, assertFalse } from "@std/assert";
import { RandomizedSet } from "./random-set.ts";

Deno.test({
  name: "Test RandomizeSet",
  timeout: 1000,
  fn: () => {
    const rs = new RandomizedSet();
    assert(rs.insert(1));
    assertFalse(rs.remove(2));
    assert(rs.insert(2));
    const rn = rs.getRandom();
    assert([1, 2].includes(rn));
    assert(rs.remove(1));
    assertFalse(rs.insert(2));
    assertEquals(rs.getRandom(), 2);
  },
});
