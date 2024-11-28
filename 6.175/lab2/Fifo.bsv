import Ehr::*;
import Vector::*;
import FIFO::*;

interface Fifo#(numeric type n, type t);
    method Action enq(t x);
    method Action deq;
    method t first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

// Exercise 1
// Completes the code in Fifo.bsv to implements a 3-elements fifo with properly
// guarded methods. Feel free to take inspiration from the class slides.
// The interface defined in Fifo.bsv tells you the type of the methods
// (enq, deq, first) that your module should define.
module mkFifo(Fifo#(3,t)) provisos (Bits#(t,tSz));
    Vector#(3, Reg#(t)) data <- replicateM( mkRegU );
    Vector#(3, Reg#(Bool)) valid <- replicateM( mkReg( False ) );

    rule canonicalize;
        if ( !valid[0] && valid[2] ) begin
            valid[0] <= True;
            valid[2] <= False;

            data[0] <= data[2];
        end else if ( !valid[1] && valid[2] ) begin
            valid[1] <= True;
            valid[2] <= False;

            data[1] <= data[2];
        end
    endrule

    method Action enq(t x) if ( !valid[2] );
        valid[2] <= True;
        data[2] <= x;
    endmethod

    method Action deq() if ( valid[0] );
        // erase state `010` and `011`
        if ( valid[1] ) begin
            valid[1] <= False;
            data[0] <= data[1];
        end
        // Removing this branch does not affect correctness
        // else if ( valid[2] ) begin valid[2] <= False; data[0] <= data[2]; end
        else
            valid[0] <= False;
    endmethod

    // First if there's a valid data
    method t first() if (valid[0]);
        return data[0];
    endmethod

    // Check if fifo's empty
    method Bool notEmpty();
        return valid[0];
    endmethod

    method Bool notFull();
       return !valid[2];
    endmethod
endmodule


// Two elements conflict-free fifo given as black box
module mkCFFifo( Fifo#(2, t) ) provisos (Bits#(t, tSz));
    Ehr#(2, t) da <- mkEhr(?);
    Ehr#(2, Bool) va <- mkEhr(False);
    Ehr#(2, t) db <- mkEhr(?);
    Ehr#(2, Bool) vb <- mkEhr(False);

    rule canonicalize;
        if( vb[1] && !va[1] ) begin
            da[1] <= db[1];
            va[1] <= True;
            vb[1] <= False;
        end
    endrule

    method Action enq(t x) if(!vb[0]);
        db[0] <= x;
        vb[0] <= True;
    endmethod

    method Action deq() if(va[0]);
        va[0] <= False;
    endmethod

    method t first if (va[0]);
        return da[0];
    endmethod

    method Bool notEmpty();
        return va[0];
    endmethod

    method Bool notFull();
        return !vb[0];
    endmethod

endmodule

module mkCF3Fifo(Fifo#(3,t)) provisos (Bits#(t, tSz));
    FIFO#(t) bsfif <-  mkSizedFIFO(3);
    method Action enq( t x);
        bsfif.enq(x);
    endmethod

    method Action deq();
        bsfif.deq();
    endmethod

    method t first();
        return bsfif.first();
    endmethod

    method Bool notEmpty();
        return True;
    endmethod

    method Bool notFull();
        return True;
    endmethod

endmodule
