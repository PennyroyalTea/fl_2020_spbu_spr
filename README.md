# Допзадание 11

Запуск приложения: `python3 build_tree.py file`, где `file` -- адрес файла с программой.

Для работы требуются библиотеки [`PLY`](https://github.com/dabeaz/ply) и [`tree-format`](https://github.com/jml/tree-format)

Грамматика:
```
program → goal
program → relation goal
relation → atom .
relation → atom . relation
relation → atom :- body .
relation → atom :- body . relation
goal → ?- .
goal → ?- body .
atom → ID
atom → ID ( args )
args → atom
args → VAR
args → atom , args
args → VAR , args
body → atom
body → atom , body
```