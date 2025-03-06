#!/bin/bash

# Проверка наличия аргумента
if [ -z "$1" ]; then
  echo "Использование: $0 <файл.ml>"
  exit 1
fi

# Проверка существования файла
if [ ! -f "$1" ]; then
  echo "Ошибка: Файл '$1' не существует."
  exit 1
fi

# Запуск llvm_demo.exe с перенаправлением ввода из файла и перенаправлением stderr в stdout
./llvm_demo.exe 2>&1 < "$1"

# Проверка кода возврата llvm_demo.exe
if [ $? -ne 0 ]; then
  echo "Ошибка: llvm_demo.exe завершился с ошибкой."
  exit 1
fi


# Просмотр содержимого текущей директории
ls

# Обработка файла out.ll
if [ -f "out.ll" ]; then
  cat out.ll | grep -E 'source_filename|target datalayout|ModuleID' --invert-match
else
  echo "Файл out.ll не был создан."
fi

exit 0
