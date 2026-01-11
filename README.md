# ALFIO Language Documentation
**A Lightweight Compiled Systems Language for Integer Operations**

## Overview 

Alfio is a minimal, compiled systems programming language targeting simple math operations. It is a highly unoptimized compiler with a small stdlib written in Alfio. 
Alfio is in a pre-alpha with loose support for x86_64 unix like systems, in other words it follows the system V abi and generates x86_64. There is the start of an aarch65 implementation but that has not been kept instep with the main feature set of the x86 compiler. 

### Design Philosophy 

- the programmer is never wrong...
- less is more, lesser is more-er
- if you know what a syscall is, you can call it. 
- typed pointers are for nerds.
- type casting is good... manual pointer math is more gooder.
- structs are just byte arrays with math and casting
- if you need dynamic allocation, roll your own malloc...

## Installation 

### Quick Installation 

Requirements: zig 0.15, gcc (for linking)

``` bash
git clone https://github.com/satdarn/ALFIO
cd ALFIO
zig build
# Optional: move ./zig-out/bin/alc-x86_64 to path 
```

## Quick start 
``` alfio 
# The entry point for all programs is the main function
fn main(): void { 
    int n = 10; 

    # print is a stdlib function which accepts an integer and prints it to stdout with a new line
    print(n);

    # words are just 64 bit integers, we treat it as a pointer...
    word ptr_n = &n;

    # but it lacks any type information about what it points to, much like a null pointer 
    # so you need to always type cast during a dereference
    print(*(int)ptr_n);

    # this is a byte array, in this case we set it to a null terminated string
	char[14] msg = "Hello World!\n"; # 13 chars + null terminator = 14
   	
	# print_string takes a word that points to a null terminated string of bytes and prints it to stdout
	print_string(&msg);
}
```

Compile and run: 

```bash 
alc-x86_64 main.aio -o main 
./main
10
10
Hello World!

```

## Language reference

### Types:

- void: this is for return types only more or less, other than that its just going to give you compiler headaches 
- bool: 8 bit unsigned integer, treated as false when 0, and true otherwise
- char: 8 bit unsigned integer 
- char[n]: char array of size n 
- int: 32 bit signed integer 
- word: 64 bit unsigned integer 

Thats it :)

## STDLIB

The compiler embeds the Alfio standard library (stdlib.aio) at build time. During compilation (using Zig's comptime), the stdlib is compiled to an object file and embedded into the binary. Changes to the stdlib source require a full recompile of the compiler.

Purpose: Provide basic I/O, memory, and string operations
Note: All functions use raw syscalls and manual memory management

```alfio
exit(err_code: int): void
open(file_name: word, flags: int, mode: word): int 
close(fd: int): void 
write(fd: int, buf: word, count: int): void
read(fd: int, buf: word, count: int): word
mmap(addr: word, length: word, prot: int, flags: int, fd: int, offset: word): word
munmap(addr: word, length: word): word
byte_out(out: char): void
byte_in(): char
read_line(buffer: word, max_len: int): int
strlen(s: word): int
strcmp(sa: word, sb: word): int
strcpy(dest: word, src: word): void
memcpy(dest: word, src: word, n: int): void
memset(dest: word, value: char, n: int): void
memeql(a: word, b: word, n: int): bool
print(num: int): void
print_int(n: int): void
print_hex(n: int): void
print_string(str: word): void
is_digit(c: char): bool
ascii_to_integer(str: word): int
integer_to_ascii(n: int, buffer: word, base: int): word
reverse(buff: word, count: int): void
exp(base: int, power: int): int
```

