// Simple 13: https://leetcode.cn/problems/roman-to-integer/description/

// ["IV", 4]
// ["IX", 9]
// ["XL", 40]
// ["XC", 90]
// ["CD", 400]
// ["CM", 900]
export function romanToInt(s: string): number {
  const symToVal = new Map([
    ['I', 1],
    ['V', 5],
    ['X', 10],
    ['L', 50],
    ['C', 100],
    ['D', 500],
    ['M', 1000],
  ]);
  const n = s.length;
  let sum = 0;
  for (let i = n - 1; i >= 0; i--) {
    const char = s.charAt(i);
    const val = symToVal.get(char);
    if (i === n - 1) {
      if (val) sum += val;
    } else {
      const prevChar = s.charAt(i + 1);
      switch (char.concat(prevChar)) {
        case 'IV':
          sum += 4 - 5;
          break;
        case 'IX':
          sum += 9 - 10;
          break;
        case 'XL':
          sum += 40 - 50;
          break;
        case 'XC':
          sum += 90 - 100;
          break;
        case 'CD':
          sum += 400 - 500;
          break;
        case 'CM':
          sum += 900 - 1000;
          break;
        default:
          sum += val ? val : 0;
      }
    }
  }
  return sum;
}
