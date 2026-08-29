// Medium 12:  https://leetcode.cn/problems/integer-to-roman/description/
// 1 <= num <= 3999
export function intToRoman(num: number): string {
  let roman = '';
  let diff = 0;
  let offset = 1000;
  while (num != 0) {
    let digit = Math.floor(num / offset);
    // switch true also works but I want to rely on fall-through
    switch (offset) {
      case 1000:
        switch (digit) {
          case 0:
            break;
          case 1:
          case 2:
          case 3:
            while (digit !== 0) {
              roman = roman.concat('M');
              digit--;
            }
            break;
          default:
            break;
        }
        break;

      case 100:
        switch (digit) {
          case 9:
            roman = roman.concat('CM');
            break;
          case 8:
          case 7:
          case 6:
            roman = roman.concat('D');
            diff = digit - 5;
            while (diff !== 0) {
              roman = roman.concat('C');
              diff--;
            }
            break;
          case 5:
            roman = roman.concat('D');
            break;
          case 4:
            roman = roman.concat('CD');
            break;
          case 3:
          case 2:
          case 1:
            while (digit !== 0) {
              roman = roman.concat('C');
              digit--;
            }
            break;
          default:
            break;
        }
        break;
      case 10:
        switch (digit) {
          case 9:
            roman = roman.concat('XC');
            break;
          case 8:
          case 7:
          case 6:
            roman = roman.concat('L');
            diff = digit - 5;
            while (diff !== 0) {
              roman = roman.concat('X');
              diff--;
            }
            break;
          case 5:
            roman = roman.concat('L');
            break;
          case 4:
            roman = roman.concat('XL');
            break;
          case 3:
          case 2:
          case 1:
            while (digit !== 0) {
              roman = roman.concat('X');
              digit--;
            }
            break;
          default:
            break;
        }
        break;
      case 1:
        switch (digit) {
          case 9:
            roman = roman.concat('IX');
            break;
          case 8:
          case 7:
          case 6:
            roman = roman.concat('V');
            diff = digit - 5;
            while (diff !== 0) {
              roman = roman.concat('I');
              diff--;
            }
            break;
          case 5:
            roman = roman.concat('V');
            break;
          case 4:
            roman = roman.concat('IV');
            break;
          case 3:
          case 2:
          case 1:
            while (digit !== 0) {
              roman = roman.concat('I');
              digit--;
            }
            break;
          default:
            break;
        }
        break;
      default:
        break;
    }

    num = num - Math.floor(num / offset) * offset;
    offset /= 10;
  }
  return roman;
}
