# ALFIO: The Arithmetic Integer Operations Language

## What is this? (Seriously, I'm asking)

Its a compiled language with statically typed variables, simple byte based input output, bare minimum control flow, and a lack of dynamic memory allocation.

Imagine if C and a calculator had a weird, minimalist baby that only speaks in integers and single characters. That's ALFIO. It's not quite a "real" programming language, but it *thinks* it is, and honestly, that's half the charm.

## Features (We Use That Word Loosely)

### What It *Can* Do:
- **Math!** Like, actual arithmetic! (Unless you try to divide by zero, then it gets sad)
- **Print numbers!** (Only numbers though, don't ask it to print your feelings)
- **Read/write characters!** (One at a time, because we're nostalgic for 1970s terminals)
- **Strings... Kinda** (They are null terminated character arrays, because if its good enough for C)
- **Functions!** (They're like little minions that do your bidding, dont ask if they can take strings as args)
- **Loops!** (For when you really want to do the same wrong thing multiple times)
- **If statements!** (Because I'm not trusting you with gotos)

### What It *Can't* Do:
- Floating point math (decimals are for people with standards)
- Dynamic memory (malloc who? Never met her)
- Pointers (too scary)
- Strings longer than what fits in your predetermined array
- Any form of debugging support (c'mon, where's the fun in that?)
- Make your compiler design professor proud


## Installation Steps 
Download the pre-compiled binary from a release and add it to your path

If your brave enough, you can build it from source 

Prerequisites 
You'll need:
1. **Zig compiler** (v0.15.0 or later) if you plan to build from source
   - If you don't have it: `sudo apt install zig` (Ubuntu-ish)
   - Or download from https://ziglang.org/download/

2. **A C compiler or x86 assembler** specifically GCC 

```
git clone https://github.com/satdarn/ALFIO.git
cd ALPHIO
zig build main.zig
mv zig-bin/bin/alc $YOUR_PATH$
```

## Usage 

### Step 1: Realize What You're Getting Into
Are you sure? Like, *really* sure? This isn't Python. This isn't even C.

### Step 2: Write Some "Code"
```c
# This is a comment. ALFIO supports comments! 
# See? We're practically a modern language.

fn main() : void {
    int x = 5;  # Wow, a variable!
    int y = 10; # Another one! We're on fire!
    
    if (x < y) {
        print(x + y);  # Prints 15! Mind. Blown.
    }
}
```

### Step 3: Try to Compile It
``` bash
$ aic my_program.aio
```
you should get either a series of confusing runtime errors from zig.. or some select hand crafted compilier errors from my twisted mind 

### Step 4: Question Your Life Choices

"Why am I using a language that doesn't even have for loops?"
"Wait, why did I make this?"
"Is this even useful?"

### Step 5: Keep Going Anyway
Because at this point, you're invested, thier 

### "Documentation" (I Wrote Some Things Down)

The Type System™ (It's Basically Binary)

int: Stores numbers. Like, all the numbers (up to 2^64-1 anyway)

char: A single character. One. Uno. Singular.

char[N]: N characters. That's it. No resizing. Deal with it.

### Syntax Highlights (Lowlights?)

Everything needs semicolons. EVERYTHING.

Curly braces are mandatory. Your high school English teacher would approve.

Functions must declare return types, even if it's void
