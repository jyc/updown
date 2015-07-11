# updown

Proof-of-concept script. Someone has probably done this already.
Collects standard output of a child process and buffers it into different lines
on the screen based on a prefix format.

# Dependencies

- CHICKEN Scheme (`pacman -S chicken`, `apt-get install chicken`, `brew install chicken`...)
- args egg (`chicken-install -s args`)

# Example

In one terminal:

    ./updown -- nc -l 8080

In a second terminal:

    nc localhost 8080

Type into the second terminal:

    Hi there!

In the first, you should see:

    Hi there!

If you then type:

    Hello world!

You will see:

    Hi there!
    Hello world!

If you then type:

    [STATUS] What's up?

You will now see:

    Hi there!
    Hello world!
    [STATUS] What's up?

Finally, type:

    [STATUS] The sky.

You should now see:

    Hi there!
    Hello world!
    [STATUS] The sky.
