# ALFIO: The "Almost Like a Functional" Integer Operations Language

## What is this? (Seriously, I'm asking)

Imagine if C and a calculator had a weird, minimalist baby that only speaks in integers and single characters. That's ALFIO. It's not quite a "real" programming language, but it *thinks* it is, and honestly, that's half the charm.

## Features (We Use That Word Loosely)

### What It *Can* Do:
- **Math!** Like, actual arithmetic! (Unless you try to divide by zero, then it gets sad)
- **Print numbers!** (Only numbers though, don't ask it to print your feelings)
- **Read/write characters!** (One at a time, because we're nostalgic for 1970s terminals)
- **Functions!** (They're like little math minions that do your bidding)
- **Loops!** (For when you really want to do the same wrong thing multiple times)
- **If statements!** (Because I'm not trusting you with gotos)

### What It *Can't* Do:
- Floating point math (decimals are for people with standards)
- Dynamic memory (malloc who? Never met her)
- Pointers (too scary)
- Strings longer than what fits in your predetermined array
- Any form of debugging support (c'mon, where's the fun in that?)
- Make your compiler design professor proud

## "Getting Started" (A Strong Term)

### Step 1: Realize What You're Getting Into
Are you sure? Like, *really* sure? This isn't Python. This isn't even C. This is... ALFIO.

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
# ... Several confusing error messages later ...
# ... Maybe it works? Who knows!
```

### Step 4: Question Your Life Choices

"Why am I using a language that doesn't even have for loops?"
"Wait, why did I make this?"
"Is this even useful?"

### Step 5: Keep Going Anyway
Because at this point, you're invested.

### "Documentation" (I Wrote Some Things Down)

The Type System™ (It's Basically Binary)

int: Stores numbers. Like, all the numbers (up to 2^64-1 anyway)

char: A single character. One. Uno. Singular.

char[N]: N characters. That's it. No resizing. Deal with it.

### Syntax Highlights (Lowlights?)

Everything needs semicolons. EVERYTHING.

Curly braces are mandatory. Your high school English teacher would approve.

Functions must declare return types, even if it's void
