import Multiplexer::*;

// Full adder functions

function Bit#(1) fa_sum( Bit#(1) a, Bit#(1) b, Bit#(1) c_in );
    return xor1( xor1( a, b ), c_in );
endfunction

function Bit#(1) fa_carry( Bit#(1) a, Bit#(1) b, Bit#(1) c_in );
    return or1( and1( a, b ), and1( xor1( a, b ), c_in ) );
endfunction

// 4 Bit full adder

function Bit#(5) add4( Bit#(4) a, Bit#(4) b, Bit#(1) c_in );
    Bit#(4) sum = 0;
    Bit#(5) carry = {?, c_in};
    for (Integer i = 0; i < 4; i = i + 1) begin
        sum[i] = fa_sum(a[i], b[i], carry[i]);
        carry[i + 1] = fa_carry(a[i], b[i], carry[i]);
    end
    return { carry[4], sum };
endfunction

// Adder interface

interface Adder8;
    method ActionValue#( Bit#(9) ) sum( Bit#(8) a, Bit#(8) b, Bit#(1) c_in );
endinterface

// Adder modules

// RC = Ripple Carry
module mkRCAdder( Adder8 );
    method ActionValue#( Bit#(9) ) sum( Bit#(8) a, Bit#(8) b, Bit#(1) c_in );
        Bit#(2) carry;
        Bit#(8) s;

        let lower_result = add4( a[3:0], b[3:0], c_in );
        carry[0] = lower_result[4];
        s[3:0] = lower_result[3:0];

        let upper_result = add4( a[7:4], b[7:4], lower_result[4] );
        carry[1] = upper_result[4];
        s[7:4] = upper_result[3:0];

        return { carry[1], s };
    endmethod
endmodule

// CS = Carry Select
module mkCSAdder( Adder8 );
    method ActionValue#( Bit#(9) ) sum( Bit#(8) a, Bit#(8) b, Bit#(1) c_in );
        Bit#(8) s;

        let lower_result = add4( a[3:0], b[3:0], c_in );
        let carry = lower_result[4];
        s[3:0] = lower_result[3:0];

        let without_carry = add4( a[7:4], b[7:4], 0 );
        let carry0 = without_carry[4];
        let sum0 = without_carry[3:0];

        let with_carry = add4( a[7:4], b[7:4], 1 );
        let carry1 = with_carry[4];
        let sum1 = with_carry[3:0];

        let sum_selector = carry;
        let carry_selector = carry;

        let carry_out = multiplexer_n( carry_selector, carry0, carry1 );
        s[7:4] = multiplexer_n( sum_selector, sum0, sum1 );

        return { carry_out, s };
    endmethod
endmodule
