import Vector :: * ;

// Reference functions that use Bluespec's '*' operator
function Bit#(TAdd#(n,n)) multiply_unsigned( Bit#(n) a, Bit#(n) b );
    UInt#(n) a_uint = unpack(a);
    UInt#(n) b_uint = unpack(b);
    UInt#(TAdd#(n,n)) product_uint = zeroExtend(a_uint) * zeroExtend(b_uint);
    return pack( product_uint );
endfunction

function Bit#(TAdd#(n,n)) multiply_signed( Bit#(n) a, Bit#(n) b );
    Int#(n) a_int = unpack(a);
    Int#(n) b_int = unpack(b);
    Int#(TAdd#(n,n)) product_int = signExtend(a_int) * signExtend(b_int);
    return pack( product_int );
endfunction

function Bit#(TAdd#(n,1)) add_unsigned( Bit#(n) a, Bit#(n) b );
    UInt#(n) a_uint = unpack(a);
    UInt#(n) b_uint = unpack(b);
    UInt#(TAdd#(n,1)) add_uint = zeroExtend(a_uint) + zeroExtend(b_uint);
    return pack( add_uint );
endfunction

function Bit#(n) shr_signed( Bit#(n) a, Integer x );
    Int#(n) a_int = unpack(a);
    Int#(n) shr_int = a_int >> x;
    return pack( shr_int );
endfunction


// Multiplication by repeated addition
function Bit#(TAdd#(n,n)) multiply_by_adding( Bit#(n) a, Bit#(n) b );
    Vector#(TAdd#(n, 1), Bit#(n)) product_upper = newVector;
    Bit#(n) product_lower = 0;
    product_upper[0] = 0;

    for (Integer i = 0; i < valueOf(n); i = i + 1) begin
        let adder = b[i] == 0 ? 0 : a;
        let sum = add_unsigned( product_upper[i], adder );

        product_lower[i] = sum[0];
        product_upper[i + 1] = sum[valueOf(n):1];
    end

    return { product_upper[valueOf(n)], product_lower };
endfunction



// Multiplier Interface
interface Multiplier#( numeric type n );
    method Bool start_ready();
    method Action start( Bit#(n) a, Bit#(n) b );
    method Bool result_ready();
    method ActionValue#(Bit#(TAdd#(n,n))) result();
endinterface



// Folded multiplier by repeated addition
module mkFoldedMultiplier( Multiplier#(n) );
    // You can use these registers or create your own if you want
    Reg#(Bit#(n)) a <- mkRegU();
    Reg#(Bit#(n)) b <- mkRegU();
    Reg#(Bit#(n)) prod <- mkRegU();
    Reg#(Bit#(n)) tp <- mkRegU();
    Reg#(UInt#(TAdd#(TLog#(n), 1))) i <- mkReg( unpack('1) );

    rule mulStep( i < fromInteger(valueOf(n)) );
        let adder = b[i] == 0 ? 0 : a;
        let sum = add_unsigned( tp, adder );

        prod[i] <= sum[0];
        tp <= sum[valueOf(n):1];

        i <= i + 1;
    endrule

    method Bool start_ready();
        return i == unpack('1);
    endmethod

    method Action start( Bit#(n) aIn, Bit#(n) bIn );
        a <= aIn;
        b <= bIn;
        // prod <= 0; // prod does not require initialization
        tp <= 0;
        i <= 0;
    endmethod

    method Bool result_ready();
        return i == fromInteger(valueOf(n));
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        i <= unpack('1);

        return { tp, prod };
    endmethod
endmodule



// Booth Multiplier
module mkBoothMultiplier( Multiplier#(n) );
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) p <- mkRegU;
    Reg#(UInt#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)+1) );

    rule mul_step( i < fromInteger(valueOf(n)) );
        let sum = case (p[1:0])
                      2'b01: return p + m_pos;
                      2'b10: return p + m_neg;
                      default: return p;
                  endcase;

        p <= shr_signed(sum, 1);
        i <= i+1;
    endrule

    method Bool start_ready();
        return i == fromInteger(valueOf(n)+1);
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r );
        m_pos <= {m, 0};
        m_neg <= {(-m), 0};
        p <= {0, r, 1'b0};
        i <= 0;
    endmethod

    method Bool result_ready();
        return i == fromInteger(valueOf(n));
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        i <= i+1;
        return p[?:1];
    endmethod
endmodule



//  Current Bits | Previous Bits | Original Booth Encoding | Radix-4 Booth Encoding
// --------------+---------------+-------------------------+------------------------
//       00      |       0       |           00            |          00
//       00      |       1       |           0+            |          0+
//       01      |       0       |           +-            |          0+
//       01      |       1       |           +0            |          +0
//       10      |       0       |           -0            |          -0
//       10      |       1       |           -+            |          0-
//       11      |       0       |           0-            |          0-
//       11      |       1       |           00            |          00
// Radix-4 Booth Multiplier
module mkBoothMultiplierRadix4( Multiplier#(n) );
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) p <- mkRegU;
    Reg#(UInt#(TLog#(n))) i <- mkReg( fromInteger(valueOf(n)/2+1) );

    rule mul_step( i < fromInteger(valueOf(n)/2) );
        let sum = case (p[2:0])
                      3'b001, 3'b010: return p + m_pos;
                      3'b101, 3'b110: return p + m_neg;
                      3'b011: return p + (m_pos << 1);
                      3'b100: return p + (m_neg << 1);
                      default: return p;
                  endcase;

        p <= shr_signed(sum, 2);
        i <= i+1;
    endrule

    method Bool start_ready();
        return i == fromInteger(valueOf(n)/2+1);
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r );
        m_pos <= {m[valueOf(n)-1], m, 0};
        m_neg <= {(-m)[valueOf(n)-1], (-m), 0};
        p <= {0, r, 1'b0};
        i <= 0;
    endmethod

    method Bool result_ready();
        return i == fromInteger(valueOf(n)/2);
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result();
        i <= i + 1;
        return p[?:1];
    endmethod
endmodule

