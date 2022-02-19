-------------------------------------------------------------------------------
-- gembrowse-url-punycode.adb
--
-- Copyright 2022 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Gembrowse.URL.Punycode with SPARK_Mode is

    -- bootstring parameters
    base         : constant := 36;
    tmin         : constant := 1;
    tmax         : constant := 26;
    skew         : constant := 38;
    damp         : constant := 700;
    initial_bias : constant := 72;
    initial_n    : constant := 16#80#;
    delimiter    : constant := 16#2D#;
    delimitChar  : constant Wide_Wide_Character := '-';

    maxint : constant Unsigned_32 := Unsigned_32'Last;

    ----------------------------------------------------------------------------
    -- Return True if character is a basic code point.
    ----------------------------------------------------------------------------
    function basic (c : Wide_Wide_Character) return Boolean is
    begin
        return Wide_Wide_Character'Pos (c) < 16#80#;
    end basic;

    ----------------------------------------------------------------------------
    -- Return True if character is a delimiter.
    ----------------------------------------------------------------------------
    function delim (c : Wide_Wide_Character) return Boolean is
    begin
        return Wide_Wide_Character'Pos (c) = delimiter;
    end delim;

    ----------------------------------------------------------------------------
    -- decodeDigit
    -- return numeric value of a basic code point in range 0 to base-1, or base
    -- if cp does not represent a value.
    ----------------------------------------------------------------------------
    function decodeDigit (c : Wide_Wide_Character) return Unsigned_32 is
        cp : Unsigned_32 := Wide_Wide_Character'Pos(c);
    begin
        -- if cp - 48 < 10 then
        --     return cp - 22;
        -- elsif cp - 65 < 26 then
        --     return cp - 65;
        -- elsif cp - 97 < 26 then
        --     return cp - 97;
        -- else
        --     return base;
        -- end if;
        case c is
            when 'A'..'Z' =>
                return cp - Wide_Wide_Character'Pos('A');
            when 'a'..'z' =>
                return cp - Wide_Wide_Character'Pos('a');
            when '0'..'9' =>
                return cp - Wide_Wide_Character'Pos('0') + 26;
            when others =>
                return base;
        end case;
    end decodeDigit;

    ----------------------------------------------------------------------------
    -- encodeDigit
    -- Always assumes lowercase.
    ----------------------------------------------------------------------------
    function encodeDigit (d : Unsigned_32) return Wide_Wide_Character with
        Pre => d < 36
    is
        -- dp : Unsigned_32 := (if d < 26 then 1 else 0);
    begin
        -- return d + 22 + 75 * dp;
        if d >= 0 and d <= 25 then
            return Wide_Wide_Character'Val (d + Wide_Wide_Character'Pos ('a'));
        elsif d > 25 and d <= 35 then
            return Wide_Wide_Character'Val (d + Wide_Wide_Character'Pos ('0') - 26);
        else
            return Wide_Wide_Character'Val (0);
        end if;
    end encodeDigit;

    ----------------------------------------------------------------------------
    -- flagged
    -- Return True if basic code point is flagged (uppercase)
    ----------------------------------------------------------------------------
    function flagged (bcp : Wide_Wide_Character) return Boolean with 
        Pre => bcp in 'a'..'z' or bcp in 'A'..'Z'
    is
    begin
        case bcp is
            when 'a'..'z' => return False;
            when 'A'..'Z' => return True;
            when others => return False;    -- should never get here.
        end case;
    end flagged;

    ----------------------------------------------------------------------------
    -- encodeBasic
    -- Force a basic code point to lowercase.
    ----------------------------------------------------------------------------
    function encodeBasic (bcp : Wide_Wide_Character) return Wide_Wide_Character with
        Pre => bcp in 'a'..'z' or bcp in 'A'..'Z'
    is
    begin
        case bcp is 
            when 'A'..'Z' => return Wide_Wide_Character'Val (Wide_Wide_Character'Pos (bcp) + 32);
            when 'a'..'z' => return bcp;
            when others => return bcp;
        end case;
    end encodeBasic;

    ----------------------------------------------------------------------------
    -- bias adaptation function
    ----------------------------------------------------------------------------
    function adapt (inDelta, numpoints : Unsigned_32; firstTime : Boolean) return Unsigned_32 with
        Pre => numpoints /= 0
    is
        k    : Unsigned_32 := 0;
        delt : Unsigned_32 := inDelta;
    begin
        delt := (if firstTime then delt / damp else delt / 2);
        delt := delt + delt / numpoints;

        while delt > ((base - tmin) * tmax) / 2 loop
            delt := delt / (base - tmin);
            k := k + base;
        end loop;

        return k + (base - tmin + 1) * delt / (delt + skew);
    end adapt;

    ----------------------------------------------------------------------------
    -- Convenience method for getting the numeric value of the character at a
    -- specific index in a Bounded_Wide_Wide_String
    ----------------------------------------------------------------------------
    function ElementAt (bwws : CodePoints.Bounded_Wide_Wide_String; idx : Positive) return Unsigned_32 is
        use CodePoints;
    begin
        return Unsigned_32(Wide_Wide_Character'Pos (Element (bwws, idx)));
    end ElementAt;

    ----------------------------------------------------------------------------
    -- encode
    ----------------------------------------------------------------------------
    procedure encode (input  : CodePoints.Bounded_Wide_Wide_String;
                      output : out CodePoints.Bounded_Wide_Wide_String;
                      result : out PunycodeStatus) with
        Pre => CodePoints.Length (input) <= CodePoints.Max_Length
    is
        use CodePoints;

        subtype CodePointsCount is Natural range 0..CodePoints.Max_Length;

        n      : Unsigned_32 := initial_n;
        delt   : Unsigned_32 := 0;
        
        -- number of code points that have been handled
        h      : CodePointsCount;

        -- number of basic code points
        b      : CodePointsCount;

        -- number of chars that have been output
        outlen : CodePointsCount := 0;

        -- maximum number of chars we can output
        -- maxlen : Unsigned_32 := Unsigned_32(CodePoints.Max_Length);
        
        bias   : Unsigned_32 := initial_bias;
        m      : Unsigned_32;
        q      : Unsigned_32;
        k      : Unsigned_32;
        t      : Unsigned_32;
    begin
        output := CodePoints.Null_Bounded_Wide_Wide_String;

        -- handle basic code points, just append these to our output string.
        for j in 1..Length (input) loop
            pragma Loop_Invariant (outlen < j);

            if basic (Element (input, j)) then
                Append (output, Element (input, j));
                outlen := outlen + 1;
            end if;
        end loop;

        h := outlen;
        b := outlen;

        -- use a dash to separate the basic chars from the encoded unicode stuff
        if b > 0 then
            Append (output, '-');
        end if;

        -- main encoding loop
        Assert (h = outlen);

        while h < Length (input) loop
            pragma Loop_Invariant (h < CodePoints.Max_Length);

            -- all non-basic code points < n have been handled, find next larger one
            m := maxint;
            for j in 1..Length (input) loop
                if ElementAt (input, j) >= n and
                   ElementAt (input, j) < m then
                   m := ElementAt (input, j);
                end if;
            end loop;

            -- increase delta enough to advance decoder's <n,i> state to <m,0>
            -- but guard against overflow
            Assert (h < CodePoints.Max_Length);
            Assert (h + 1 /= 0);

            if m - n > (maxint - delt) / Unsigned_32(h + 1) then
                result := Overflow;
                return;
            end if;

            delt := delt + (m - n) * Unsigned_32(h + 1);
            n := m;

            -- no need to check whether ElementAt (input, j) is basic
            for j in 1..Length (input) loop
                pragma Loop_Invariant (h < CodePoints.Max_Length);

                if ElementAt (input, j) < n then
                    if delt = Unsigned_32'Last then
                        result := Overflow;
                        return;
                    else
                        delt := delt + 1;
                    end if;
                end if;

                if ElementAt (input, j) = n then
                    -- represent delta as a generalized variable-length integer
                    q := delt;
                    k := base;

                    loop
                        if outlen >= CodePoints.Max_Length then
                            result := BigOutput;
                            return;
                        end if;

                        if k <= bias then
                            t := tmin;
                        elsif k >= bias + tmax then
                            t := tmax;
                        else
                            t := k - bias;
                        end if;

                        exit when q < t;

                        Append (output, encodeDigit (t + (q - t) mod (base - t)));
                        outlen := outlen + 1;

                        q := (q - t) / (base - t);

                        k := k + base;

                    end loop;

                    Append (output, encodeDigit (q));
                    outlen := outlen + 1;

                    bias := adapt (delt, Unsigned_32(h) + 1, h = b);
                    delt := 0;

                    h := h + 1;
                end if;
            end loop;

            delt := delt + 1;
            n := n + 1;

        end loop;

        result := Success;
    end encode;

    ---------------------------------------------------------------------------
    -- To_Ascii
    ---------------------------------------------------------------------------
    procedure To_Ascii (input  : LabelStrings.Bounded_String;
                        output : out LabelStrings.Bounded_String;
                        result : out Boolean) is
        use CodePoints;
        use LabelStrings;

        in32  : CodePoints.Bounded_Wide_Wide_String;
        out32 : CodePoints.Bounded_Wide_Wide_String;
        res   : PunycodeStatus;
    begin
        -- For SPARK we need to drop chars rather than throw an error, however
        -- here we make sure the strings are the same length so nothing is dropped.

        in32 := CodePoints.To_Bounded_Wide_Wide_String (
            Source => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (
                        LabelStrings.To_String (input)),
            Drop => Ada.Strings.Right);

        encode (in32, out32, res);

        if res /= Success then
            output := LabelStrings.Null_Bounded_String;
            result := False;
        else
            output := LabelStrings.To_Bounded_String (
                Source => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
                            CodePoints.To_Wide_Wide_String (out32)),
                Drop => Ada.Strings.Right);
            result := True;
        end if;
    end To_Ascii;                       

    ----------------------------------------------------------------------------
    -- decode
    ----------------------------------------------------------------------------
    procedure decode (input : CodePoints.Bounded_Wide_Wide_String;
                      output : out CodePoints.Bounded_Wide_Wide_String;
                      result : out PunycodeStatus) is
        use CodePoints;

        subtype CodePointsCount is Natural range 0..CodePoints.Max_Length;

        n         : Unsigned_32 := initial_n;
        outlen    : CodePointsCount := 0;           -- number of code points in output
        i         : Unsigned_32 := 0;
        bias      : Unsigned_32 := initial_bias;
        lastDelim : CodePointsCount := 0;
        inIdx     : Natural;
        oldi      : Unsigned_32;
        w         : Unsigned_32;
        k         : Unsigned_32;
        digit     : Unsigned_32;
        t         : Unsigned_32;
    begin
        output := CodePoints.Null_Bounded_Wide_Wide_String;

        -- handle basic code points. b = number of input code points before the
        -- last delimiter, or 0 if none, then copy first b code points to
        -- output.
        for j in 1..Length (input) loop
            pragma Loop_Invariant (lastDelim <= j);

            if delim (Element (input, j)) then
                lastDelim := j;
            end if;
        end loop;

        -- This can't really happen with the way we've structured our inputs/outputs,
        -- but here for completeness.
        if lastDelim >= CodePoints.Max_Length then
            result := BigOutput;
            return;
        end if;

        Assert (lastDelim < CodePoints.Max_Length);

        -- copy the basic code points to the output
        for j in 1..lastDelim - 1 loop
            pragma Loop_Invariant (outlen <= j);

            if not basic (Element (input, j)) then
                result := BadInput;
                return;
            end if;

            Append (output, Element (input, j));
            outlen := outlen + 1;
        end loop;

        -- Main decoding loop. Start just after the last delimiter if any basic
        -- code points were copied, otherwise start at the beginning.
        inIdx := (if lastDelim > 1 then lastDelim + 1 else 1);

        Assert (Length (input) <= CodePoints.Max_Length);

        while inIdx <= Length (input) loop
            --decode a generalized variable-length integer into delta, which
            -- gets added to i. The overflow checking is easier if we increase
            -- i as we go, then subtract off its starting value at the end to
            -- obtain delta.
            oldi := i;
            w := 1;
            k := base;

            loop
                pragma Loop_Invariant (w /= 0);

                if inIdx > Length (input) then
                    result := BadInput;
                    return;
                end if;

                digit := decodeDigit (Element (input, inIdx));
                inIdx := inIdx + 1;

                if digit >= base then
                    result := BadInput;
                    return;
                end if;

                if digit > (maxint - i) / w then
                    result := Overflow;
                    return;
                end if;

                i := i + digit * w;

                if k <= bias then
                    t := tmin;
                elsif k >= bias + tmax then
                    t := tmax;
                else
                    t := k - bias;
                end if;

                exit when digit < t;

                if w > maxint / (base - t) then
                    result := Overflow;
                    return;
                end if;

                w := w * (base - t);
                k := k + base;
            end loop;

            bias := adapt (i - oldi, Unsigned_32(outlen + 1), oldi = 0);

            -- i was supposed to wrap around from out+1 to 0, incrementing n
            -- each time, so we'll fix that now
            if i / Unsigned_32(outlen + 1) > maxint - n then
                result := Overflow;
                return;
            end if;

            n := n + (i / Unsigned_32(outlen + 1));
            i := i mod Unsigned_32(outlen + 1);

            if outlen >= CodePoints.Max_Length then
                result := BigOutput;
                return;
            end if;

            -- need i+1 here for Ada's 1-based indexing.
            CodePoints.Insert (output, Natural(i + 1), "" & Wide_Wide_Character'Val (n));
            i := i + 1;

            outlen := outlen + 1;
        end loop;

        result := Success;
    end decode;

    ---------------------------------------------------------------------------
    -- To_Unicode
    ---------------------------------------------------------------------------
    procedure To_Unicode (input  : LabelStrings.Bounded_String;
                          output : out LabelStrings.Bounded_String;
                          result : out Boolean) is
        use CodePoints;
        use LabelStrings;

        in32  : CodePoints.Bounded_Wide_Wide_String;
        out32 : CodePoints.Bounded_Wide_Wide_String;
        res   : PunycodeStatus;
    begin
        -- For SPARK we need to drop chars rather than throw an error, however
        -- here we make sure the strings are the same length so nothing is dropped.

        in32 := CodePoints.To_Bounded_Wide_Wide_String (
            Source => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (
                        LabelStrings.To_String (input)),
            Drop => Ada.Strings.Right);

        decode (in32, out32, res);

        if res /= Success then
            output := LabelStrings.Null_Bounded_String;
            result := False;
        else
            output := LabelStrings.To_Bounded_String (
                Source => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (
                            CodePoints.To_Wide_Wide_String (out32)),
                Drop => Ada.Strings.Right);
            result := True;
        end if;
    end To_Unicode;

end Gembrowse.URL.Punycode;
