
import FIFO::*;
import FixedPoint::*;
import Vector::*;
import Multiplier::*;

import AudioProcessorTypes::*;
import FilterCoefficients::*;


// The FIR Filter Module Definition
module mkFIRFilter (AudioProcessor);
    FIFO#(Sample) infifo <- mkFIFO();
    FIFO#(Sample) outfifo <- mkFIFO();

    Vector#(8, Reg#(Sample)) r <- replicateM(mkReg(0));
    Vector#(9, Multiplier) mul <- replicateM(mkMultiplier());

    rule shift_and_mul;
        let sample = infifo.first();
        infifo.deq();

        r[0] <= sample;
        for (Integer i = 0; i < 7; i = i + 1) begin
            r[i + 1] <= r[i];
        end

        mul[0].putOperands(c[0], sample);
        for (Integer i = 0; i < 8; i = i + 1) begin
            mul[i + 1].putOperands(c[i + 1], r[i]);
        end
    endrule

    rule do_sum;
        Vector#(9, FixedPoint#(16, 16)) results = ?;
        for (Integer i = 0; i < 9; i = i + 1) begin
            results[i] <- mul[i].getResult();
        end

        // \+ is a function which returns the sum of the elements
        // make sure you leave a space after the \+ and before the ','
        let sum = fold(\+ , results);

        outfifo.enq(fxptGetInt(sum));
    endrule

    method Action putSampleInput(Sample in);
        infifo.enq(in);
    endmethod

    method ActionValue#(Sample) getSampleOutput();
        outfifo.deq();
        return outfifo.first();
    endmethod
endmodule

