# MemDB

## Overview

This is an in-memory, disk- backed NoSQL database that is 100% written in C.
All communication is done via Google Protocol Buffers so that any client
that confirms to the specifications can talk to it.

## Primitive Data Types
- String
- Integer

## Advance Data Types
- Stacks
- Queues

These are all collections of the same type of primitive data type.


## Requests

### Get

The key is specified and returned is the value at the given location.

Possible errors:

- `NO_VALUE`: if there is no value at the given key

- `WRONG_VALUE`: if the value at the location is not a primitive data type

### PUT

A new value is set into that location in memory. This will override any
existing value, no matter the data type.

### Remove

Removes any existing value associated with the given key. This will remove
any type, including stacks and queues.

Possible errors:

- `NO_VALUE`: if there is no value associated with the given key

### InitStack

Creates a new stack and assigns it to the new key. The key must not
currently be in use.

Possible errors:

- `KEY_IN_USE`: occurs if the key is already in use

### Push

Pushes an element onto a stack. The element being pushed on has to be of the
same type as the stack.

Possible errors:

- `NO_VALUE`: if the key is not in use

- `WRONG_VALUE`: if the value at the location is not a stack

- `WRONG_TYPE`: if the value to push on is not of the same type as the stack

### Pop

Pops an element off of the stack at the given key.

Possible errors:

- `NO_VALUE`: if there key given is not in use

- `WRONG_VALUE`: if the value is not a stack

- `EMPTY`: if the stack is empty

### InitQueue

Creates a new queue and assigns it to the given key. The key must not currently
be in use.

Possible errors:

- `KEY_IN_USE`: if the given key is already in use

### Enqueue

Adds an element onto the end of the queue. The value being added has to be of
same primitive type as the rest of the queue.

Possible errors:

- `NO_VALUE`: the key is not in use

- `WRONG_VALUE`: the value at the key is not a stack

- `WRONG_TYPE`: the queue is not of the same type as of the value being added

### Dequeue

Gets the next element from the queue.

Possible errors:

- `NO_VALUE`: the key is not in use

- `WRONG_VALUE`: the value at the location is not a queue

- `EMPTY`: the queue is empty

### Peek

Gets the next element in the stack or queue without removing the value from it.

Possible errors:

- `NO_VALUE`: the key is not in use

- `WRONG_VALUE`: the value at the location is not a stack or queue

- `EMPTY`: the stack or queue is empty

### Size

Gets the size of the stack or queue for the given key. The size is stored as
metadata so this is an O(1) operation.

- `NO_VALUE`: the given location is empty

- `WRONG_TYPE`: the value at the location is not a stack or queue















