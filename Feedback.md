## Ольга Шиманская

```
<S>           -> <RULES>
<RULES>       -> <RULE> /endl <RULES> | <eps>
<RULE>        -> <NONTERM> /arrow <NONEMPTYRHS>
<NONEMPTYRHS> -> <NONTERM> <RHS> | <TERM> <RHS> | <RHS> /or <RHS>
<RHS>         -> <NONEMPTYRHS> | <eps>
<NONTERM>     -> /lb <UPPER> /rb
<TERM>        -> /term <LOWER> | /lb /eps /rb
<UPPER>       -> <ULETTER> | <ULETTER> <UPPER>
<LOWER>       -> <LLETTER> | <LLETTER> <LOWER>
<DIGIT>         -> /0 | /1 | /2 | /3 | /4 | /5 | /6 | /7 | /8 | /9
<LLETTER>       -> /a | /b | /c | /d | /e | /f | /g | /h | /i | /j | /k | /l | /m | /n | /o | /p | /q | /r | /s | /t | /u | /v | /w | /x | /y | /z
<ULETTER>       -> /A | /B | /C | /D | /E | /F | /G | /H | /I | /J | /K | /L | /M | /N | /O | /P | /Q | /R | /S | /T | /U | /V | /W | /X | /Y | /Z
```
К сожалению, при попытке проверить возникает ошибка, так как не поддерживаются переносы строк из Windows (Parser error:line 1:24 extraneous input '\r' expecting {<EOF>, ' ', NEWLINE}).
Поддерживаются заглавные буквы и цифры, что является плюсом, но специальные символы пришлось заменить на названия.


## Андрей Кислицын

```
<RULES>       -> <RULE> "endl" <RULES> | <RULE> "or" "eps" "endl" <RULES> | _
<RULE>        -> <NONTERM> "arrow" <NONEMPTYRHS>
<NONEMPTYRHS> -> <NONTERM> <RHS> | <TERM> <RHS> | <RHS> "or" <RHS>
<RHS>         -> <NONEMPTYRHS> | _
<NONTERM>     -> "lnt" <UPPER> "rnt"
<TERM>        -> "lt" <LOWER> "rt"
<UPPER>       -> <ULETTER> | <ULETTER> <UPPER>
<LOWER>       -> <LLETTER> | <LLETTER> <LOWER>
<LETTER>      -> "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<ULETTER>     -> "u" <LLETTER>
```

Парсер корректно распознает грамматику. Не поддерживаются никакие символы, кроме строчных букв, поэтому символы заменены на схематичные названия.




