
import FIFO::*;
import FixedPoint::*;
import Vector::*;
import Multiplier::*;

import AudioProcessorTypes::*;


module mkFIRFilter (Vector#(tnp1, FixedPoint#(16, 16)) coefficients, AudioProcessor ifc)
  provisos (Add#(1, xxx, tnp1));

    FIFO#(Sample) infifo <- mkFIFO();
    FIFO#(Sample) outfifo <- mkFIFO();

    Vector#(TSub#(tnp1, 1), Reg#(Sample)) r <- replicateM(mkReg(0));
    Vector#(tnp1, Multiplier)           mul <- replicateM(mkMultiplier());

    Integer num_taps = valueOf(tnp1) - 1;

    rule shift_and_mul;
        let sample = infifo.first();
        infifo.deq();

        r[0] <= sample;
        for (Integer i = 1; i < num_taps; i = i + 1) begin
            r[i] <= r[i - 1];
        end

        mul[0].putOperands(coefficients[0], sample);
        for (Integer i = 0; i < num_taps; i = i + 1) begin
            mul[i + 1].putOperands(coefficients[i + 1], r[i]);
        end
    endrule

    rule do_sum;
        Vector#(tnp1, FixedPoint#(16, 16)) results = ?;
        for (Integer i = 0; i < valueOf(tnp1); i = i + 1) begin
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
