const fs = require("fs");

const lines = fs.readFileSync("./input.txt").toString("utf8").split("\n");
const data = lines.reduce((arr, line) => {
  const [left, right] = line.split("|");
  arr.push({
    left: left.split(/ +/).filter(x => x),
    right: right.split(/ +/).filter(x => x),
  })
  return arr;
}, [])

function partA() {
  const count = data.reduce((c, d) => {
    c += d.right.map(x => x.length == 2 ? 1 : 0).reduce((x, s) => x + s);
    c += d.right.map(x => x.length == 3 ? 1 : 0).reduce((x, s) => x + s);
    c += d.right.map(x => x.length == 4 ? 1 : 0).reduce((x, s) => x + s);
    c += d.right.map(x => x.length == 7 ? 1 : 0).reduce((x, s) => x + s);
    return c;
  }, 0)

  console.log("partA:", count)
}

function partB() {
  const count = data.reduce((c, d) => {
    const left = d.left.map(x => x.split('').sort().join(''));
    const oneStr = left.find(x => x.length == 2);
    const sevenStr = left.find(x => x.length == 3);
    const fourStr = left.find(x => x.length == 4);
    const eightStr = left.find(x => x.length == 7);

    const zeroSixNineStrs = left.filter(x => x.length == 6);
    const twoThreeFiveStrs = left.filter(x => x.length == 5);
    const topLeftAndMiddle = fourStr.split("").reduce((c, x) => !oneStr.includes(x) ? c+x : c, '');

    let middle = topLeftAndMiddle.split("").reduce((c, x) => !zeroSixNineStrs[0].includes(x) ? c+x : c, '');
    if (!middle)
      middle = topLeftAndMiddle.split("").reduce((c, x) => !zeroSixNineStrs[1].includes(x) ? c+x : c, '');
    if (!middle)
      middle = topLeftAndMiddle.split("").reduce((c, x) => !zeroSixNineStrs[2].includes(x) ? c+x : c, '');

    const topLeft = topLeftAndMiddle.split("").reduce((c, x) => !middle.includes(x) ? c+x : c, '');
    const zeroStr = zeroSixNineStrs.find(x => !x.includes(middle));
    const nineStr = zeroSixNineStrs.filter(x => x.includes(middle)).find(x => x.includes(oneStr[0]) && x.includes(oneStr[1]));
    const sixStr = zeroSixNineStrs.filter(x => x.includes(middle)).find(x => x.includes(oneStr[0]) != x.includes(oneStr[1]));

    const threeStr = twoThreeFiveStrs.find(x => x.includes(oneStr[0]) && x.includes(oneStr[1]));
    const twoFiveStr = twoThreeFiveStrs.filter(x => x.includes(oneStr[0]) != x.includes(oneStr[1]));
    const fiveStr = twoFiveStr.find(x => x.includes(topLeft));
    const twoStr = twoFiveStr.find(x => !x.includes(topLeft));

    const numbers = [zeroStr, oneStr, twoStr, threeStr, fourStr, fiveStr, sixStr, sevenStr, eightStr, nineStr];

    c += d.right.reduce((sum, x) => {
      x = x.split('').sort().join('');
      const n = numbers.indexOf(x);
      if (n < 0) {
        console.log(x, n, numbers)
        throw new Error();
      }
      sum = sum * 10 + n;
      return sum;
    }, 0);

    return c;
  }, 0)

  console.log("partB:", count)
}

partA();
partB();
