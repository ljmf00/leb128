module leb128;

import std.traits;
import std.algorithm;
import std.array;
import std.range;
import std.typecons;
import std.meta;

// alias to short storage class enum name
private alias STC = ParameterStorageClass;

struct LEB128(T)
    if(isIntegral!T)
{
    this() @disable;
    this(this) @disable;

    static struct Encoder(R)
        if((isStaticArray!R || isInputRange!R) && is(ForeachType!R == T))
    {
        R range;

        size_t length() const
        {
            size_t ret;

            foreach(T value; range)
            {
                static if(isSigned!T)
                {
                    while (1)
                    {
                        ubyte b = value & 0x7F;

                        value >>= 7;
                        if ((value == 0 && !(b & 0x40)) ||
                            (value == -1 && (b & 0x40)))
                        {
                            ++ret;
                            break;
                        }

                        ++ret;
                    }
                } else {
                    do ++ret;
                    while (value >>= 7);
                }
            }

            return ret;
        }

        // Issue: https://issues.dlang.org/show_bug.cgi?id=23116
        int opApply(Dg : int delegate(ref ubyte))(scope Dg dg)
        {
            enum attrs = functionAttributes!Dg;
            alias nonRefDg = SetFunctionAttributes!(int delegate(ubyte), functionLinkage!Dg, attrs);
            foreach(ubyte i; &this.opApply!(nonRefDg)) // use the correct overload
                if(auto ret = dg(i)) return ret;

            return 0;
        }

        int opApply(Dg : int delegate(ubyte))(scope Dg dg)
        {
            foreach(ref T value; range)
            {
                static if(isSigned!T)
                {
                    while (1)
                    {
                        ubyte b = value & 0x7F;

                        value >>= 7;
                        if ((value == 0 && !(b & 0x40)) ||
                            (value == -1 && (b & 0x40)))
                        {
                            if(auto ret = dg(b))
                                return ret;
                            break;
                        }

                        if(auto ret = dg(b | 0x80))
                            return ret;
                    }
                } else {
                    do
                    {
                        ubyte b = value & 0x7F;

                        value >>= 7;
                        if (value)
                            b |= 0x80;

                        if(auto ret = dg(b))
                            return ret;
                    } while (value);
                }
            }
            return 0;
        }
    }

    static auto encoder(T value)
    {
        return Encoder!(T[1])([value]);
    }

    static ubyte[] encode(T value)
    {
        return encode([value]);
    }

    static auto encoder(R)(R range)
        if(is(ForeachType!R == T))
    {
        return Encoder!(R)(range);
    }

    static ubyte[] encode(R)(R range)
        if(is(ForeachType!R == T))
    {
        static if (isStaticArray!R)
        {
            enum maxLength = (() {
                R lo, hi;

                fill(lo[], T.min);
                fill(hi[], T.max);

                return max(
                    encoder(lo).length,
                    encoder(hi).length,
                );
            })();

            ubyte[] ret;
            ret.reserve(maxLength);
        } else {
            Appender!(ubyte[]) ret;
        }

        foreach(ubyte b; encoder(range))
            ret ~= b;

        return ret[];
    }

    static struct Decoder(R)
        if((isStaticArray!R || isInputRange!R) && is(ForeachType!R == ubyte))
    {
        R range;

        // Issue: https://issues.dlang.org/show_bug.cgi?id=23116
        int opApply(Dg : int delegate(ref Nullable!T))(scope Dg dg)
        {
            enum attrs = functionAttributes!Dg;
            alias nonRefDg = SetFunctionAttributes!(int delegate(Nullable!T), functionLinkage!Dg, attrs);
            foreach(Nullable!T i; &this.opApply!(nonRefDg)) // use the correct overload
                if(auto ret = dg(i)) return ret;

            return 0;
        }

        int opApply(Dg : int delegate(Nullable!T))(scope Dg dg)
        {
            while(!range.empty)
            {
                OriginalType!T result = 0;
                size_t shift;
                ubyte b = void;

                static if(isSigned!T)
                {
                    do {
                        if(range.empty)
                        {
                            if(auto ret = dg(Nullable!T.init))
                                return ret;
                            return 0;
                        }
                        b = range.front;
                        range.popFront;

                        result |= (b & 0x7F) << shift;
                        shift += 7;
                    } while ((b & 0x80) != 0);

                    if ((shift < T.sizeof) && (b & 0x40))
                        result |= (~0 << shift);

                    if(auto ret = dg(nullable!T(cast(T)result)))
                        return ret;
                } else {
                    while (true) {
                        if(range.empty)
                        {
                            if(auto ret = dg(Nullable!T.init))
                                return ret;
                            return 0;
                        }
                        b = range.front;
                        range.popFront;

                        result |= (b & 0x7F) << shift;
                        if ((b & 0x80) == 0)
                        {
                            if(auto ret = dg(nullable!T(cast(T)result)))
                                return ret;
                            break;
                        }
                        shift += 7;
                    }
                }
            }
            return 0;
        }
    }

    static auto decoder(R)(R range)
        if(is(ForeachType!R == ubyte))
    {
        return Decoder!(R)(range);
    }

    static Tuple!(T[], bool) decode(R)(R range)
        if(is(ForeachType!R == ubyte))
    {
        // TODO: Optimize this to pre allocate
        Appender!(T[]) ret;

        foreach(Nullable!T val; decoder(range))
        {
            if (val.isNull)
                return tuple(ret[], false);
            ret ~= val.get();
        }

        return tuple(ret[], true);
    }
}

ubyte[] toLEB128(T)(T t)
{
    static if(isIntegral!T)
        return LEB128!T.encode(t);
    else
        return LEB128!(ForeachType!T).encode(t);
}

Tuple!(T[], bool) fromLEB128(T, R)(R range)
    if((isStaticArray!R || isInputRange!R) && is(ForeachType!R == ubyte))
{
    return LEB128!T.decode(range);
}

auto leb128Encoder(T)(T t)
{
    static if(isIntegral!T)
        return LEB128!T.encoder(t);
    else
        return LEB128!(ForeachType!R).encoder(t);
}

auto leb128Decoder(T, R)(R range)
    if((isStaticArray!R || isInputRange!R) && is(ForeachType!R == ubyte))
{
    return LEB128!T.decoder!T(range);
}

///
@safe pure nothrow
unittest
{
    assert(toLEB128(int(100)) == [228u, 0u]);
    assert(toLEB128(int(10)) == [10u]);

    assert(toLEB128([int(100)]) == [228u, 0u]);
    assert(toLEB128([int(100)].staticArray) == [228u, 0u]);

    assert(fromLEB128!int(cast(ubyte[])[228u, 0u]) == tuple([100], true));
    assert(fromLEB128!int(cast(ubyte[])[10u]) == tuple([10], true));

    assert(toLEB128(uint(300)) == [172u, 2u]);
    assert(toLEB128(uint(10)) == [10u]);

    assert(fromLEB128!uint(cast(ubyte[])[172u, 2u]) == tuple([300], true));
    assert(fromLEB128!uint(cast(ubyte[])[10u]) == tuple([10], true));
}

///
@safe pure nothrow
unittest
{
    auto e = leb128Encoder(int(100));
    assert(e.length == 2);

    e = leb128Encoder(int(10));
    assert(e.length == 1);

    auto e2 = leb128Encoder(uint(300));
    assert(e2.length == 2);
    e2 = leb128Encoder(uint(10));
    assert(e2.length == 1);
}

@safe pure nothrow
unittest
{
    assert(fromLEB128!int(cast(ubyte[])[228u, 0u, 228u]) == tuple([100], false));
    assert(fromLEB128!int(cast(ubyte[])[228u]) == tuple([], false));

    assert(fromLEB128!uint(cast(ubyte[])[172u]) == tuple([], false));
    assert(fromLEB128!uint(cast(ubyte[])[172u, 2u, 172u]) == tuple([300], false));
}
