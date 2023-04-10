import System.IO

genTypes :: [String]
genTypes = do
  n <- [0..255]
  return $ "struct Char" ++ show n ++ " : CharBase {};\n"

genInc' :: [String]
genInc' = do
  n <- [0..255]
  return $ "template<>\nstruct Inc<Char" ++ show n ++ "> {\n    using result = Char" ++ show (succ n `mod` 256) ++ ";\n};\n\n"

genInc :: [String]
genInc = ("template <Char n>\nstruct Inc {};\n\n") : genInc'

genDec' :: [String]
genDec' = do
  n <- [0..255]
  return $ "template<>\nstruct Dec<Char" ++ show n ++ "> {\n    using result = Char" ++ show (pred n `mod` 256) ++ ";\n};\n\n"

genDec :: [String]
genDec = ("template <Char n>\nstruct Dec {};\n\n") : genDec'

genPrintChar' :: [String]
genPrintChar' = do
  n <- [0..255]
  return $ "template<> struct CharPrinter<Char" ++ show n ++ "> {\n    static void print() {\n        putc(" ++ show n ++ ", stdout);\n    }\n};\n\n"

genPrintChar :: [String]
genPrintChar = ("template <Char n>\n struct CharPrinter {};\n\n") : genPrintChar' 

genPrintInt' :: [String]
genPrintInt' = do
  n <- [0..255]
  return $ "template<>\nstruct CharValPrinter<Char" ++ show n ++ "> {\n    static void print() {\n        printf(\"%d\", " ++ show n ++ ");\n    }\n};\n\n"

genPrintInt :: [String]
genPrintInt = ("template <Char n>\n struct CharValPrinter {};\n\n") : genPrintInt' 

main :: IO ()
main = do
  handle <- openFile "GeneratedTypes.hpp" ReadWriteMode 
  
  hPutStrLn handle "#ifndef GENERATED_TYPES_HPP_"
  hPutStrLn handle "#define GENERATED_TYPES_HPP_\n"

  hPutStrLn handle "#include <cstdio>"
  hPutStrLn handle "#include <type_traits>\n"

  hPutStrLn handle "struct CharBase {};\n"

  hPutStrLn handle "template <typename T>\nconcept Char = std::is_base_of<CharBase, T>::value;\n"

  hPutStrLn handle "// Type declaration\n"
  hPutStr handle (concat genTypes)
  
  hPutStrLn handle "\n// Increment definition\n"
  hPutStr handle (concat genInc)

  hPutStrLn handle "\n// Decrement definition\n"
  hPutStr handle (concat genDec)

  hPutStrLn handle "\n// Character printer"
  hPutStr handle (concat genPrintChar)

  hPutStrLn handle "\n// Charcode printer"
  hPutStr handle (concat genPrintInt)

  hPutStrLn handle "#endif"
  hClose handle