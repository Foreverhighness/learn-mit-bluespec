
import ClientServer::*;
import FIFO::*;
import GetPut::*;

import FixedPoint::*;
import Vector::*;

import ComplexMP::*;


typedef Server#(
    Vector#(nbins, ComplexMP#(isize, fsize, psize)),
    Vector#(nbins, ComplexMP#(isize, fsize, psize))
) PitchAdjust#(numeric type nbins, numeric type isize, numeric type fsize, numeric type psize);


// s - the amount each window is shifted from the previous window.
//
// factor - the amount to adjust the pitch.
//  1.0 makes no change. 2.0 goes up an octave, 0.5 goes down an octave, etc...
module mkPitchAdjust(Integer s, FixedPoint#(isize, fsize) factor, PitchAdjust#(nbins, isize, fsize, psize) ifc);
    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) inputFIFO  <- mkFIFO();
    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) outputFIFO <- mkFIFO();

    Reg#(Vector#(nbins, Phase#(psize))) in_phase   <- mkReg(replicate(0));
    Vector#(nbins, Reg#(Phase#(psize))) out_phase  <- replicateM(mkReg(0));

    rule pitch_adjust (True);
        let in = inputFIFO.first();
        inputFIFO.deq();

        Vector#(nbins, Phase#(psize)) new_in_phase = newVector();
        Vector#(nbins, ComplexMP#(isize, fsize, psize)) out = replicate(cmplxmp(0, 0));

        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            new_in_phase[i] = in[i].phase;

            let bin = fxptGetInt(fromInteger(i) * factor);
            let nbin = fxptGetInt(fromInteger(i + 1) * factor);

            if (bin != nbin && 0 <= bin && bin < fromInteger(valueOf(nbins))) begin
                let diff_phase = in[i].phase - in_phase[i];
                FixedPoint#(psize, 0) d_phase = fromInt(diff_phase);

                let shifted = truncate(fxptGetInt(fxptMult(d_phase, factor)));
                // use '*' cause bug or compiler error
                // let shifted = truncate(fxptGetInt(d_phase * factor));

                let new_out_phase = out_phase[bin] + shifted;
                out_phase[bin] <= new_out_phase;

                out[bin] = cmplxmp(in[i].magnitude, new_out_phase);
            end
        end

        in_phase <= new_in_phase;
        outputFIFO.enq(out);
    endrule

    interface Put request  = toPut(inputFIFO);
    interface Get response = toGet(outputFIFO);
endmodule

