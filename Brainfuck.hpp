#ifndef BRAINFUCK_HPP_
#define BRAINFUCK_HPP_

#include "../part1/Nat.hpp"
#include "../part2/List.hpp"
#include "BrainfuckTypes.hpp"
#include "FunctionList.hpp"
#include "Tuple.hpp"

// Misc stuff

// f \circ g
template <template <typename> typename f, template <typename> typename g>
struct Compose {
  template <typename x> 
  using result = typename f<typename g<x>::result>::result;
};

template <FuncList fs>
struct ComposeAll {};

template <template <typename> typename f>
struct ComposeAll<FuncCons<f, FuncNil>> {
  template <typename x> 
  using result = typename f<x>::result;
};

template <template <typename> typename f, template <typename> typename g>
struct ComposeAll<FuncCons<f, FuncCons<g, FuncNil>>> {
  template <typename x> 
  using result = typename Compose<g, f>::template result<x>;
};

template <template <typename> typename f, FuncList fs>
struct ComposeAll<FuncCons<f, fs>> {
  template <typename x> 
  struct FsComposed {
    using result = typename ComposeAll<fs>::template result<x>;
  };

  template <typename x>
  using result = typename Compose<FsComposed, f>::template result<x>;
};

template <Tape t>
struct ToList {};

template <List prev, Char cur, List next>
struct ToList<TapeCtor<prev, cur, next>> {
  using result = typename Foldl<Append, Cons<cur, next>, prev>::result;
};

// Functions that modify state

template <State st>
struct TapeMoveLeft {};

template <List input, List output, Char cur, List next>
struct TapeMoveLeft<StateCtor<input, output, TapeCtor<Nil, cur, next>>> {
  using result = StateCtor<input, output, TapeCtor<Nil, Char0, Cons<cur, next>>>;
};

template <List input, List output, Char x, List prev, Char cur, List next>
struct TapeMoveLeft<StateCtor<input, output, TapeCtor<Cons<x, prev>, cur, next>>> {
  using result = StateCtor<input, output, TapeCtor<prev, x, Cons<cur, next>>>;
};

template <State st>
struct TapeMoveRight {};

template <List input, List output, List prev, Char cur>
struct TapeMoveRight<StateCtor<input, output, TapeCtor<prev, cur, Nil>>> {
  using result = StateCtor<input, output, TapeCtor<Cons<cur, prev>, Char0, Nil>>;
};

template <List input, List output, List prev, Char cur, Char x, List next>
struct TapeMoveRight<StateCtor<input, output, TapeCtor<prev, cur, Cons<x, next>>>> {
  using result = StateCtor<input, output, TapeCtor<Cons<cur, prev>, x, next>>;
};

template <State st>
struct TapeInc {};

template <List input, List output, List prev, Char cur, List next>
struct TapeInc<StateCtor<input, output, TapeCtor<prev, cur, next>>> {
  using result = StateCtor<input, output, TapeCtor<prev, typename Inc<cur>::result, next>>;
};

template <State st>
struct TapeDec {};

template <List input, List output, List prev, Char cur, List next> 
struct TapeDec<StateCtor<input, output, TapeCtor<prev, cur, next>>> {
  using result = StateCtor<input, output, TapeCtor<prev, typename Dec<cur>::result, next>>;
};

template <State st>
struct TapeRead {};

template <Char cur, List input, List output, List prev, Char old, List next>
struct TapeRead<StateCtor<Cons<cur, input>, output, TapeCtor<prev, old, next>>> {
  using result = StateCtor<input, output, TapeCtor<prev, cur, next>>;
};

template <State st>
struct TapeWrite {};

template <List input, List output, List prev, Char cur, List next>
struct TapeWrite<StateCtor<input, output, TapeCtor<prev, cur, next>>> {
  using result = StateCtor<input, Cons<cur, output>, TapeCtor<prev, cur, next>>;
};

template <State st, template <State> typename F> 
struct TapeLoop {};

template <List input, List output, List prev, List next, template <State> typename F>
struct TapeLoop<StateCtor<input, output, TapeCtor<prev, Char0, next>>, F> {
  using result = StateCtor<input, output, TapeCtor<prev, Char0, next>>;
};

template <List input, List output, Tape t, template <State> typename F>
struct TapeLoop<StateCtor<input, output, t>, F> {
  using result = typename TapeLoop<typename F<StateCtor<input, output, t>>::result, F>::result;
};

// Parse program input

template <List prog>
struct CodeToInstructionList {};

template <>
struct CodeToInstructionList<Nil> {
  using result = Nil;
};

template <List prog>
struct CodeToInstructionList<Cons<Increment, prog>> {
  using result = Cons<IncrementCell, typename CodeToInstructionList<prog>::result>;
};

template <List prog>
struct CodeToInstructionList<Cons<Decrement, prog>> {
  using result = Cons<DecrementCell, typename CodeToInstructionList<prog>::result>;
};

template <List prog>
struct CodeToInstructionList<Cons<Left, prog>> {
  using result = Cons<GoLeft, typename CodeToInstructionList<prog>::result>;
};

template <List prog>
struct CodeToInstructionList<Cons<Right, prog>> {
  using result = Cons<GoRight, typename CodeToInstructionList<prog>::result>;
};

template <List prog>
struct CodeToInstructionList<Cons<Input, prog>> {
  using result = Cons<Read, typename CodeToInstructionList<prog>::result>;
};

template <List prog>
struct CodeToInstructionList<Cons<Output, prog>> {
  using result = Cons<Write, typename CodeToInstructionList<prog>::result>;
};

template <List prog, List acc, Nat depth>
struct ExtractLoopBody {};

template <List prog, List acc, Nat depth>
struct ExtractLoopBody<Cons<LoopStart, prog>, acc, depth> {
  using result = typename ExtractLoopBody<prog, Cons<LoopStart, acc>, Succ<depth>>::result;
};

template <List prog, List acc, Nat depth>
struct ExtractLoopBody<Cons<LoopEnd, prog>, acc, Succ<depth>> {
  using result = typename ExtractLoopBody<prog, Cons<LoopEnd, acc>, depth>::result;
};

template <List prog, List acc>
struct ExtractLoopBody<Cons<LoopEnd, prog>, acc, Z> {
  using result = Tuple2Ctor<prog, typename Reverse<acc>::result>;
};

template <ProgramSymbol cur, List prog, List acc, Nat depth>
struct ExtractLoopBody<Cons<cur, prog>, acc, depth> {
  using result = typename ExtractLoopBody<prog, Cons<cur, acc>, depth>::result;
};

template <List prog>
struct CodeToInstructionList<Cons<LoopStart, prog>> {
  using ExtrRes = typename ExtractLoopBody<prog, Nil, Z>::result;
  using Prog = typename Fst<ExtrRes>::result;
  using LoopBody = typename Snd<ExtrRes>::result;

  using result = Cons<Loop<typename CodeToInstructionList<LoopBody>::result>, typename CodeToInstructionList<Prog>::result>;
};

// Wrap function into struct

template <template <typename> typename f>
struct Wrap {
  struct Wrapped {
    template <typename x>
    using result = typename f<x>::result;
  };

  using result = Wrapped;
};

// Converting instruction into wrapped function

template <typename x>
struct Id {
  using result = x;
};

// Compose (wrapped) functions

template <typename f, typename g>
struct ComposeWrapped {
  template <typename x>
  struct UnwrappedComposition {
    using result = typename g::template result<typename f::template result<x>>;
  };
  using result = typename Wrap<UnwrappedComposition>::result;
};

// Convert list of wrapped functions into single wrapped function

template <List wrapped>
struct FunctionListToFunction {
  using result = typename Foldl<ComposeWrapped, typename Wrap<Id>::result, wrapped>::result;
};


template <Instruction op>
struct InstructionToFunction {};

// Convert instruction list into (wrapped) function list

template <List prog>
struct InstructionListToFunctionList {
  using result = typename Fmap<InstructionToFunction, prog>::result;
};

template <>
struct InstructionToFunction<IncrementCell> {
  using result = typename Wrap<TapeInc>::result; 
};

template <>
struct InstructionToFunction<DecrementCell> {
  using result = typename Wrap<TapeDec>::result;
};

template <>
struct InstructionToFunction<GoRight> {
  using result = typename Wrap<TapeMoveRight>::result;
};

template <>
struct InstructionToFunction<GoLeft> {
  using result = typename Wrap<TapeMoveLeft>::result;
};

template <>
struct InstructionToFunction<Read> {
  using result = typename Wrap<TapeRead>::result;
};

template <>
struct InstructionToFunction<Write> {
  using result = typename Wrap<TapeWrite>::result;
};

template <List loopBody>
struct InstructionToFunction<Loop<loopBody>> {
  template <State st>
  struct TapeLoopApplied {
    template <State st_>
    struct Unwrapped {
      using result = typename FunctionListToFunction<typename InstructionListToFunctionList<loopBody>::result>::result::template result<st_>;
    };

    using result = typename TapeLoop<st, Unwrapped>::result;
  };
  
  using result = typename Wrap<TapeLoopApplied>::result;
};

template <List prog>
struct InstructionListToFunction {
  using result = typename FunctionListToFunction<typename InstructionListToFunctionList<prog>::result>::result;
};


// Converting instruction list into function list

template <List prog>
struct InstructionListToFuncList  {};

template <> 
struct InstructionListToFuncList<Nil> {
  using result = FuncNil;
};

template <List prog>
struct InstructionListToFuncList<Cons<IncrementCell, prog>> {
  using result = FuncCons<TapeInc, typename InstructionListToFuncList<prog>::result>;
};

template <List prog>
struct InstructionListToFuncList<Cons<DecrementCell, prog>> {
  using result = FuncCons<TapeDec, typename InstructionListToFuncList<prog>::result>;
};

template <List prog>
struct InstructionListToFuncList<Cons<GoRight, prog>> {
  using result = FuncCons<TapeMoveRight, typename InstructionListToFuncList<prog>::result>;
};

template <List prog>
struct InstructionListToFuncList<Cons<GoLeft, prog>> {
  using result = FuncCons<TapeMoveLeft, typename InstructionListToFuncList<prog>::result>;
};

template <List prog>
struct InstructionListToFuncList<Cons<Read, prog>> {
  using result = FuncCons<TapeRead, typename InstructionListToFuncList<prog>::result>;
};

template <List prog>
struct InstructionListToFuncList<Cons<Write, prog>> {
  using result = FuncCons<TapeWrite, typename InstructionListToFuncList<prog>::result>;
};

template <List prog, List loopBody>
struct InstructionListToFuncList<Cons<Loop<loopBody>, prog>> {
  template <State st>
  struct TapeLoopApplied {
    template <State st_>
    struct LoopBodyFunc {
      using result = typename ComposeAll<typename InstructionListToFuncList<loopBody>::result>::template result<st_>;
    };


    using result = typename TapeLoop<st, LoopBodyFunc>::result;
  };

  using result = FuncCons<TapeLoopApplied, typename InstructionListToFuncList<prog>::result>;
};


// Converting instruction list into function State -> State

template <List prog> 
struct InstructionListToFunc {
  template <State st>
  using result = typename ComposeAll<typename InstructionListToFuncList<prog>::result>::template result<st>;
};


// Converting code into function 

template <List prog> 
struct CodeToFunc {
  template <State st> 
  using result = typename InstructionListToFunction<typename CodeToInstructionList<prog>::result>::result::template result<st>;
};

// Run code on an empty tape

template <List prog, List input>
struct RunProgram {
  using InitState = StateCtor<input, Nil, TapeCtor<Nil, Char0, Nil>>;
  
  template <State st>
  struct Program {
    using result = typename CodeToFunc<prog>::template result<st>;
  };

  using result = typename Program<InitState>::result;
};


template <State st>
struct GetOutput {};

template <List input, List output, Tape t>
struct GetOutput<StateCtor<input, output, t>> {
  using result = typename Reverse<output>::result;
};

#endif