# Часть 3. Полнота по Тьюрингу шаблонов C++ 
После демонстрации построения некоторых полезных конструкций в предыдущих двух частях возникает вопрос об ограничениях системы шаблонов C++. В этой части мы продемонстрируем, что система шаблонов является полной по Тьюрингу, то есть с помощью шаблонов можно вычислить любую частичную $\mu$-рекурсивную функцию

## Метод доказательства
Доказывать полноту по Тьюрингу можно различными способами. Мы покажем, что к шаблонам можно свести другой Тьюринг-полный язык (Brainfuck), то есть напишем его интерпретатор.

## Спецификация языка Brainfuck
Brainfuck-машина представляет из себя бесконечную ленту, по которой перемещается головка. В каждой ячейке ленты могут быть записаны значения от 0 до 255. Язык состоит из 8 инструкций:
- `+` -- инкремент значения в ячейке
- `-` -- декремент значения в ячейке
- `>` -- сдвиг головки вправо
- `<` -- сдвиг головки влево
- `,` -- считывание символа из потока ввода
- `.` -- запись символа в поток вывода
- `[` -- начало цикла
- `]` -- конец цикла

Циклы выполняются, пока значение в текущей ячейке на момент начала цикла является ненулевым. При переполнении во время инкремента/декремента происходит wraparound.

## Типы данных
Для начала определим типы, которые будет использовать наш интерпретатор. Первый тип, который нам понадобится -- тип для значений в ячейках ленты. Его можно представить как простой Enum из 256 значений:

```c++
struct CharBase {};

template <typename T>
concept Char = std::is_base_of<CharBase, T>::value;

// Type declaration

struct Char0 : CharBase {};
struct Char1 : CharBase {};
struct Char2 : CharBase {};
struct Char3 : CharBase {};
...
struct Char255 : CharBase {};
```

Кроме того, на этом типе необходимо определить операции инкремента и декремента. Это можно сделать, используя сопсотавление с образцом:

```c++
template <Char n>
struct Inc {};

template<>
struct Inc<Char0> {
    using result = Char1;
};

template<>
struct Inc<Char1> {
    using result = Char2;
};

...

template<>
struct Inc<Char255> {
    using result = Char0;
};


template <Char n>
struct Dec {};

template<>
struct Dec<Char0> {
    using result = Char255;
};

template<>
struct Dec<Char1> {
    using result = Char0;
};

...

template<>
struct Dec<Char255> {
    using result = Char254;
};
```

*Замечание: понятно, что писать такое руками -- дело тяжелое и неблагодарное. Поэтому автор статьи сгенерировал это все. Кроме того, были сгенерированы и value printers для вывода результатов вычислений в консоль.*

Следующий тип -- лента, на которую записываются символы. Лента будет в некотором смысле "ленивой" -- изначально на ней будет только один символ, а новые будут добавляться по мере необходимости. Такую ленту можно представить в виде типа-произведения, состоящего из текущей ячейки и двух списков: символов, что идут до текущего символа и после него:

```c++
struct TapeBase {};

template <typename T>
concept Tape = std::is_base_of<TapeBase, T>::value;

template <List prev, Char cur, List next>
struct TapeCtor : TapeBase {};
```

Сразу объявим и тип для состояния интерпретатора. Состояние мы опишем, как произведение списка входных символов, списка выходных символов и ленты:

```c++
struct StateBase {};

template <typename T> 
concept State = std::is_base_of<StateBase, T>::value;

template <List input, List output, Tape tape>
struct StateCtor : StateBase {};
```

Кроме того, наш интерпретатор должен в каком-то виде принимать программу. Будем передавать ее в виде списка операторов. Само собой, придется объявить и тип для операторов:

```c++
struct ProgramSymbolBase {};

template <typename T>
concept ProgramSymbol = std::is_base_of<ProgramSymbolBase, T>::value;

struct Increment : ProgramSymbolBase {};

struct Decrement : ProgramSymbolBase {};

struct Left : ProgramSymbolBase {};

struct Right : ProgramSymbolBase {};

struct Input : ProgramSymbolBase {};

struct Output : ProgramSymbolBase {};

struct LoopStart : ProgramSymbolBase {};

struct LoopEnd : ProgramSymbolBase {};
```

На самом деле, нам будет удобнее работать с разобраной программой -- символы для начала и конца списка добавляют сложности, и было бы удобно заменить их одним типом, в котором конструктор цикла будет своим параметром принимать цикл. Заведём тип для разобранной программы

```c++
struct InstructionBase {};

template <typename T>
concept Instruction = std::is_base_of<InstructionBase, T>::value;

struct IncrementCell : InstructionBase {};

struct DecrementCell : InstructionBase {};

struct GoLeft : InstructionBase {};

struct GoRight : InstructionBase {};

struct Read : InstructionBase {};

struct Write : InstructionBase {};

template <List loopBody>
struct Loop : InstructionBase {};
```

Кажется, теперь мы готовы приступить к работе над интерпретатором.

## Принцип работы интерпретатора.

Основная идея -- каждую операцию можео представить как функцию `State -> State`. Тогда из списка операций можно собрать список таких функций, а их композиция будет исполнять программу. Общий алгоритм таков:

1. Разобрать программу: `List ProgramSymbol -> List Instruction`
2. Преобразовать инструкции в функции: `List Instruction -> List (State -> State)`
3. Взять композицию всех инструкций: `List (State -> State) -> State -> State`
4. Вычислить функцию на некотором начальном `State`

## Композиция и списки функций

На данный момент у нашего списка есть фундаментальная пробле