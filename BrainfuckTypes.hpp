#ifndef BRAINFUCK_TYPES_HPP_
#define BRAINFUCK_TYPES_HPP_

#include <type_traits>

#include "GeneratedTypes.hpp"

// Program encoding

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


// Instructions for brainfuck machine

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

template <Instruction op>
struct InstructionPrinter {};

template <>
struct InstructionPrinter<IncrementCell> {
  static void print() {
    printf("+");
  }
};

template <>
struct InstructionPrinter<DecrementCell> {
  static void print() {
    printf("-");
  }
};

template <>
struct InstructionPrinter<GoLeft> {
  static void print() {
    printf("<");
  }
};

template <>
struct InstructionPrinter<GoRight> {
  static void print() {
    printf(">");
  }
};

template <>
struct InstructionPrinter<Write> {
  static void print() {
    printf(".");
  }
};

template <>
struct InstructionPrinter<Read> {
  static void print() {
    printf(",");
  }
};

template <List loopBody>
struct InstructionPrinter<Loop<loopBody>> {
  static void print() {
    printf("[\n");
    PrintAuxillary<InstructionPrinter, EmptySep, loopBody>::print();
    printf("\n]");
  }
};

// Brainfuck machine tape

struct TapeBase {};

template <typename T>
concept Tape = std::is_base_of<TapeBase, T>::value;

template <List prev, Char cur, List next>
struct TapeCtor : TapeBase {};


// Brainfuck machine state

struct StateBase {};

template <typename T> 
concept State = std::is_base_of<StateBase, T>::value;

template <List input, List output, Tape tape>
struct StateCtor : StateBase {};

#endif