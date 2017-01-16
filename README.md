# 代码执行方式
本项目使用 stack 管理，只需在根目录下 stack build 即可构建；配合stack exec 可以执行主程序。
可执行运行接口符合作业说明中的要求
``` shell
sih -repl # 进入 REPL
sih -i <源文件> [-o <输出文件>] # 解释程序，输出求值结果
sih -t <源文件> [-o <输出文件>] # 解析程序，输出抽象语法树
```
# 语法总览
- 逻辑表达式
```
True | False
| (not <expr>)
| (and <expr> <expr>)
| (or <expr> <expr>)
```
- 浮点算术表达式
```
<number>
| (+ <expr> <expr>)
| (- <expr> <expr>)
| (* <expr> <expr>)
| (/ <expr> <expr>)
| (= <expr> <expr>)
| (< <expr> <expr>)
| (<= <expr> <expr>)
| (> <expr> <expr>)
| (>= <expr> <expr>)
```
- 字符串，列表
```
nil | <char-lit> | <string-lit>
| (cons <expr> <expr>)
| (car <expr>)
| (cdr <expr>)
```
- FP
```
<variable> (alpha char only)
-- 文法作用域
| (let variable <expr> <expr>)
-- 高阶函数
| (lambda <variable> <expr>)
```
- 条件表达式(if 语句)
```
(if <expr> <expr> <expr>)
```
