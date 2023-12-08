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
