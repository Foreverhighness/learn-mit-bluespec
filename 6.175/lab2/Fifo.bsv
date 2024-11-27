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
    // define your own 3-elements fifo here.
    Vector#(3, Reg#(t)) data <- replicateM( mkRegU );
    Reg#(UInt#(2)) front <- mkReg(0);
    Reg#(UInt#(2)) rear <- mkReg(0);
    Reg#(Bool) valid <- mkReg(False);

    rule canonicalize;
    endrule

    // Enq if there's at least one spot open... so, dc is invalid.
    method Action enq(t x) if (!valid || front != rear);
        let next = case (rear)
                       0, 1: return rear + 1;
                       2:    return 0;
                   endcase;

        data[rear] <= x;
        rear <= next;
        valid <= True;
    endmethod

    // Deq if there's a valid d[0]ta at d[0]
    method Action deq() if (valid);
        let next = case (front)
                       0, 1: return front + 1;
                       2:    return 0;
                   endcase;

        front <= next;
        valid <= next == rear ? False : True;
    endmethod

    // First if there's a valid data
    method t first() if (valid);
        return data[front];
    endmethod

    // Check if fifo's empty
    method Bool notEmpty();
        return valid;
    endmethod

    method Bool notFull();
       return !valid || front != rear;
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
