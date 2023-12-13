# Advent of Code 2023 Haskell 解题代码

**内含解题代码**，我只会在每道题的排行榜满了后再上传代码。

## 运行方式

在安装了 [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 的环境下，使用 `stack run day1` 运行 day1 的解题代码，以此类推。

## 代码结构

`data` 目录下是输入数据，`src` 目录下是解题代码。

## 目录

### Day1
```
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
```
[输入数据](data/day1.txt) [解题代码](src/day1.hs)

这道题的需求太罕见了，导致不得不使用我一般极少会用到的 `breakOn` 函数来做子串搜索。绝大多数类似需求在 Haskell 中会通过简单 `split` 一下解决，或者写正经的 parser。这道题巧妙地要求我反向搜索，考虑到有重叠的可能性（例如 `nineight` 正向搜索会先看到 `nine`，反向搜索会先看到 `eight`），我放弃了写 parser 的想法。

### Day2
```
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
```
[输入数据](data/day2.txt) [解题代码](src/day2.hs)

写 parser 啦！

### Day3
```
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
```
[输入数据](data/day3.txt) [解题代码](src/day3.hs)

写 List monad 啦！

### Day4
```
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
```
[输入数据](data/day4.txt) [解题代码](src/day4.hs)

第二问最好是要有一个可以随机读写的内存，才比较方便实现。不然的替代方案就是做个状态机，用 `foldl`/`foldl'` 不断更新一个表示内存的状态。我用了 `STArray`、`HashMap`、`Integer -> Integer` 三种表示内存的方案各写了一遍，逻辑是完全相同的，可以相互对照。

### Day5
```
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
```
[输入数据](data/day5.txt) [解题代码 1](src/day5.hs) [解题代码 2](src/day5b.hs)

要是有一些现成的和区间打交道的类型和函数就好了。

### Day6
```
Time:      7  15   30
Distance:  9  40  200
```
[输入数据](data/day6.txt) [解题代码](src/day6.hs)

过于简单。

### Day7
```
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
```
[输入数据](data/day7.txt) [解题代码 1](src/day7.hs) [解题代码 2](src/day7b.hs)

用 Haskell 写这道题挺爽的，我知道第二问的 `handType` 可以写得更好一点，不过当时赶了一下时间。

### Day8
```
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
```
[输入数据](data/day8.txt) [解题代码](src/day8.hs)

赶时间，写得比较 dirty。第二问先是写成了把 `startPositions` 中的每一项分别 `map` 成后续序列，然后整个 `List.transpose` 一下，看第几项正好全以 Z 结尾，结果发现跑不完，改成了 `lcm` 的方案。

### Day9
```
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
```
[输入数据](data/day9.txt) [解题代码](src/day9.hs)

过于简单。

### Day10
```
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
```
[输入数据](data/day10.txt) [解题代码](src/day10.hs)

这道题正好可以用图形学中常用的求面积的算法来做，走一圈就能把周长和面积都求出来，两问需要的答案都能只用周长和面积算出来。不过用递归写循环确实体验不是很好，以及我常常希望如果 `Direction`、`opposite`、`step` 等类型和函数不用自己写就好了。求面积的这个算法是每次向右走时，加一次纵坐标，每次向左走时减一次纵坐标，最终的结果取绝对值就是面积，符号表示旋转的方向。注意到这个算法的另一种表述是，找出所有横着走的部分，每一段如果是向右走的，就加上这条横线的长度乘以纵坐标，如果是向左走的，就减去。

### Day11
```
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
```
[输入数据](data/day11.txt) [解题代码](src/day11.hs)

把水平和垂直方向的距离分开算，假设有 6 个星系，中间就有 5 段距离，假设分别是 a、b、c、d、e，那么总距离就是 `1*5*a + 2*4*b + 3*3*c + 4*2*d + 5*1*e`。考虑到膨胀的影响，算每段距离的时候要变换一下，例如对于第一问，变换规则是 `0->0, 1->1, 2->3, 3->5, 4->7, 5->9, ...`。

### Day12
```
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
```
[输入数据](data/day12.txt) [解题代码 1](src/day12.hs) [解题代码 2](src/day12b.hs)

不用 memo 写动态规划实在不是很爽。要显式定义状态和状态转移过程也不是很爽，我更喜欢把涉及状态机的需求写成 parser 或 async 代码，当 parser 解析了一部分输入但还没结束时，或者 async 代码暂停在一个 yield 处时，就自动保存了状态。然而这样定义出来的状态和状态转移过程是次优的，分叉后等价的状态无法再合并，也就是说只能实现剪枝带来的性能提升。另外，这道题更好的写法是，不要把 runs 和 springs 存进 cache，只存下标进去，这样省空间，hash 也更快，但意味着就不能用列表了，应该用 Vector，而且递归代码也会变得更丑。
