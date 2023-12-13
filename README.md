# Advent of Code 2023 Haskell 解题代码

**内含解题代码**，我只会在每道题的排行榜满了后再上传代码。

## 运行方式

在安装了 [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 的环境下，使用 `stack run day1` 运行 day1 的解题代码，以此类推。

## 代码结构

`data` 目录下是输入数据，`src` 目录下是解题代码。

## 额外想法

- **Day1**：这道题的需求太罕见了，导致不得不使用我一般极少会用到的 `breakOn` 函数来做子串搜索。绝大多数类似需求在 Haskell 中会通过简单 `split` 一下解决，或者写正经的 parser。这道题巧妙地要求我反向搜索，考虑到有重叠的可能性（例如 `nineight` 正向搜索会先看到 `nine`，反向搜索会先看到 `eight`），我放弃了写 parser 的想法。
- **Day2**：写 parser 啦！
- **Day3**：写 List monad 啦！
- **Day4**：第二问最好是要有一个可以随机读写的内存，才比较方便实现。不然的替代方案就是做个状态机，用 `foldl`/`foldl'` 不断更新一个表示内存的状态。我用了 `STArray`、`HashMap`、`Integer -> Integer` 三种表示内存的方案各写了一遍，逻辑是完全相同的，可以相互对照。
- **Day5**：要是有一些现成的和区间打交道的类型和函数就好了。
- **Day6**：过于简单。
- **Day7**：用 Haskell 写这道题挺爽的，我知道第二问的 `handType` 可以写得更好一点，不过当时赶了一下时间。
- **Day8**：赶时间，写得比较 dirty。第二问先是写成了把 `startPositions` 中的每一项分别 `map` 成后续序列，然后整个 `List.transpose` 一下，看第几项正好全以 Z 结尾，结果发现跑不完，改成了 `lcm` 的方案。
- **Day9**：过于简单。
- **Day10**：这道题正好可以用图形学中常用的求面积的算法来做，走一圈就能把周长和面积都求出来，两问需要的答案都能只用周长和面积算出来。不过用递归写循环确实体验不是很好，以及我常常希望如果 `Direction`、`opposite`、`step` 等类型和函数不用自己写就好了。求面积的这个算法是每次向右走时，加一次纵坐标，每次向左走时减一次纵坐标，最终的结果取绝对值就是面积，符号表示旋转的方向。注意到这个算法的另一种表述是，找出所有横着走的部分，每一段如果是向右走的，就加上这条横线的长度乘以纵坐标，如果是向左走的，就减去。
- **Day11**：把水平和垂直方向的距离分开算，假设有 6 个星系，中间就有 5 段距离，假设分别是 a、b、c、d、e，那么总距离就是 `1*5*a + 2*4*b + 3*3*c + 4*2*d + 5*1*e`。考虑到膨胀的影响，算每段距离的时候要变换一下，例如对于第一问，变换规则是 `0->0, 1->1, 2->3, 3->5, 4->7, 5->9, ...`。
- **Day12**：不用 memo 写动态规划实在不是很爽。要显式定义状态和状态转移过程也不是很爽，我更喜欢把涉及状态机的需求写成 parser 或 async 代码，当 parser 解析了一部分输入但还没结束时，或者 async 代码暂停在一个 yield 处时，就自动保存了状态。然而这样定义出来的状态和状态转移过程是次优的，分叉后等价的状态无法再合并，也就是说只能实现剪枝带来的性能提升。另外，这道题更好的写法是，不要把 runs 和 springs 存进 cache，只存下标进去，这样省空间，hash 也更快，但意味着就不能用列表了，应该用 Vector，而且递归代码也会变得更丑。
