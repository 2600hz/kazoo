Writing parsers
===============

There are three kinds of protocols:

 *  Text protocols
 *  Schema-less binary protocols
 *  Schema-based binary protocols

This chapter introduces the first two kinds. It will not cover
more advanced topics such as continuations or parser generators.

This chapter isn't specifically about Ranch, we assume here that
you know how to read data from the socket. The data you read and
the data that hasn't been parsed is saved in a buffer. Every
time you read from the socket, the data read is appended to the
buffer. What happens next depends on the kind of protocol. We
will only cover the first two.

Parsing text
------------

Text protocols are generally line based. This means that we can't
do anything with them until we receive the full line.

A simple way to get a full line is to use `binary:split/{2,3}`.

``` erlang
case binary:split(Buffer, <<"\n">>) of
    [_] ->
        get_more_data(Buffer);
    [Line, Rest] ->
        handle_line(Line, Rest)
end.
```

In the above example, we can have two results. Either there was
a line break in the buffer and we get it split into two parts,
the line and the rest of the buffer; or there was no line break
in the buffer and we need to get more data from the socket.

Next, we need to parse the line. The simplest way is to again
split, here on space. The difference is that we want to split
on all spaces character, as we want to tokenize the whole string.

``` erlang
case binary:split(Line, <<" ">>, [global]) of
    [<<"HELLO">>] ->
        be_polite();
    [<<"AUTH">>, User, Password] ->
        authenticate_user(User, Password);
    [<<"QUIT">>, Reason] ->
        quit(Reason)
    %% ...
end.
```

Pretty simple, right? Match on the command name, get the rest
of the tokens in variables and call the respective functions.

After doing this, you will want to check if there is another
line in the buffer, and handle it immediately if any.
Otherwise wait for more data.

Parsing binary
--------------

Binary protocols can be more varied, although most of them are
pretty similar. The first four bytes of a frame tend to be
the size of the frame, which is followed by a certain number
of bytes for the type of frame and then various parameters.

Sometimes the size of the frame includes the first four bytes,
sometimes not. Other times this size is encoded over two bytes.
And even other times little-endian is used instead of big-endian.

The general idea stays the same though.

``` erlang
<< Size:32, _/bits >> = Buffer,
case Buffer of
    << Frame:Size/binary, Rest/bits >> ->
        handle_frame(Frame, Rest);
    _ ->
        get_more_data(Buffer)
end.
```

You will then need to parse this frame using binary pattern
matching, and handle it. Then you will want to check if there
is another frame fully received in the buffer, and handle it
immediately if any. Otherwise wait for more data.
