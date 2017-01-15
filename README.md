# 功能点(5*****)
- 独立主程序
- 解释器
- 文法解析
- Pretty-printer

# 语言特性(11***********)
- 逻辑表达式
True | False
| (not <expr>)
| (and <expr> <expr>)
| (or <expr> <expr>)
- 浮点算术表达式
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
- 字符串，列表
nil | <char-lit> | <string-lit>
| (cons <expr> <expr>)
| (car <expr>)
| (cdr <expr>)
- FP
<variable> (alpha char only)
-- 文法作用域
| (let variable <expr> <expr>)
-- 高阶函数
| (lambda <variable> <expr>)
- conditional expression(if)
(if <expr> <expr> <expr>)
