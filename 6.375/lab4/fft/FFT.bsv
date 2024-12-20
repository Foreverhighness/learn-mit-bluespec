
import ClientServer::*;
import Complex::*;
import FIFO::*;
import Reg6375::*;
import GetPut::*;
import Real::*;
import Vector::*;

typedef Server#(
    Vector#(fft_points, Complex#(cmplxd)),
    Vector#(fft_points, Complex#(cmplxd))
) FFT#(numeric type fft_points, type cmplxd);

// Get the appropriate twiddle factor for the given stage and index.
// This computes the twiddle factor statically.
function Complex#(cmplxd) getTwiddle(Integer stage, Integer index, Integer points) provisos (RealLiteral#(cmplxd));
    Integer i = ((2*index)/(2 ** (log2(points)-stage))) * (2 ** (log2(points)-stage));
    return cmplx(fromReal(cos(fromInteger(i)*pi/fromInteger(points))),
                 fromReal(-1*sin(fromInteger(i)*pi/fromInteger(points))));
endfunction

// Generate a table of all the needed twiddle factors.
// The table can be used for looking up a twiddle factor dynamically.
typedef Vector#(TLog#(fft_points), Vector#(TDiv#(fft_points, 2), Complex#(cmplxd))) TwiddleTable#(numeric type fft_points, type cmplxd);
function TwiddleTable#(fft_points, cmplxd) genTwiddles() provisos (RealLiteral#(cmplxd));
    TwiddleTable#(fft_points, cmplxd) twids = newVector;
    for (Integer s = 0; s < valueof(TLog#(fft_points)); s = s+1) begin
        for (Integer i = 0; i < valueof(TDiv#(fft_points, 2)); i = i+1) begin
            twids[s][i] = getTwiddle(s, i, valueof(fft_points));
        end
    end
    return twids;
endfunction

// Given the destination location and the number of points in the fft, return
// the source index for the permutation.
function Integer permute(Integer dst, Integer points);
    Integer src = ?;
    if (dst < points/2) begin
        src = dst*2;
    end else begin
        src = (dst - points/2)*2 + 1;
    end
    return src;
endfunction

// Reorder the given vector by swapping words at positions
// corresponding to the bit-reversal of their indices.
// The reordering can be done either as as the
// first or last phase of the FFT transformation.
function Vector#(fft_points, Complex#(cmplxd)) bitReverse(Vector#(fft_points,Complex#(cmplxd)) inVector);
    Vector#(fft_points, Complex#(cmplxd)) outVector = newVector();
    for(Integer i = 0; i < valueof(fft_points); i = i+1) begin
        Bit#(TLog#(fft_points)) reversal = reverseBits(fromInteger(i));
        outVector[reversal] = inVector[i];
    end
    return outVector;
endfunction

// 2-way Butterfly
function Vector#(2, Complex#(cmplxd)) bfly2(Vector#(2, Complex#(cmplxd)) t, Complex#(cmplxd) k) provisos (Arith#(cmplxd));
    Complex#(cmplxd) m = t[1] * k;

    Vector#(2, Complex#(cmplxd)) z = newVector();
    z[0] = t[0] + m;
    z[1] = t[0] - m;

    return z;
endfunction

// Perform a single stage of the FFT, consisting of butterflys and a single
// permutation.
// We pass the table of twiddles as an argument so we can look those up
// dynamically if need be.
function Vector#(fft_points, Complex#(cmplxd))
stage_ft(
    TwiddleTable#(fft_points, cmplxd)          twiddles,
    Bit#(TLog#(TLog#(fft_points)))     stage,
    Vector#(fft_points, Complex#(cmplxd)) stage_in
) provisos (Add#(2, a__, fft_points), Arith#(cmplxd));
    Vector#(fft_points, Complex#(cmplxd)) stage_temp = newVector();
    for(Integer i = 0; i < (valueof(fft_points)/2); i = i+1) begin
        Integer idx = i * 2;
        let twid = twiddles[stage][i];
        let y = bfly2(takeAt(idx, stage_in), twid);

        stage_temp[idx]   = y[0];
        stage_temp[idx+1] = y[1];
    end

    Vector#(fft_points, Complex#(cmplxd)) stage_out = newVector();
    for (Integer i = 0; i < valueof(fft_points); i = i+1) begin
        stage_out[i] = stage_temp[permute(i, valueof(fft_points))];
    end
    return stage_out;
endfunction

module mkCombinationalFFT (FFT#(fft_points, cmplxd)) provisos (Add#(2, a__, fft_points), RealLiteral#(cmplxd), Arith#(cmplxd), Bits#(cmplxd, b__));

  // Statically generate the twiddle factors table.
  TwiddleTable#(fft_points, cmplxd) twiddles = genTwiddles();

  // Define the stage_f function which uses the generated twiddles.
  function Vector#(fft_points, Complex#(cmplxd)) stage_f(Bit#(TLog#(TLog#(fft_points))) stage, Vector#(fft_points, Complex#(cmplxd)) stage_in);
      return stage_ft(twiddles, stage, stage_in);
  endfunction

  FIFO#(Vector#(fft_points, Complex#(cmplxd))) inputFIFO  <- mkFIFO();
  FIFO#(Vector#(fft_points, Complex#(cmplxd))) outputFIFO <- mkFIFO();

  // This rule performs fft using a big mass of combinational logic.
  rule comb_fft;

    Vector#(TAdd#(1, TLog#(fft_points)), Vector#(fft_points, Complex#(cmplxd))) stage_data = newVector();
    stage_data[0] = inputFIFO.first();
    inputFIFO.deq();

    for(Integer stage = 0; stage < valueof(TLog#(fft_points)); stage=stage+1) begin
        stage_data[stage+1] = stage_f(fromInteger(stage), stage_data[stage]);
    end

    outputFIFO.enq(stage_data[valueof(TLog#(fft_points))]);
  endrule

  interface Put request;
    method Action put(Vector#(fft_points, Complex#(cmplxd)) x);
        inputFIFO.enq(bitReverse(x));
    endmethod
  endinterface

  interface Get response = toGet(outputFIFO);

endmodule

module mkLinearFFT (FFT#(fft_points, cmplxd)) provisos (Add#(2, a__, fft_points), RealLiteral#(cmplxd), Arith#(cmplxd), Bits#(cmplxd, b__));
    // Statically generate the twiddle factors table.
    TwiddleTable#(fft_points, cmplxd) twiddles = genTwiddles();

    // Define the stage_f function which uses the generated twiddles.
    function Vector#(fft_points, Complex#(cmplxd)) stage_f(Bit#(TLog#(TLog#(fft_points))) stage, Vector#(fft_points, Complex#(cmplxd)) stage_in);
        return stage_ft(twiddles, stage, stage_in);
    endfunction

    FIFO#(Vector#(fft_points, Complex#(cmplxd))) inputFIFO  <- mkFIFO();
    FIFO#(Vector#(fft_points, Complex#(cmplxd))) outputFIFO <- mkFIFO();

    Vector#(TSub#(TLog#(fft_points), 1), FIFO#(Vector#(fft_points, Complex#(cmplxd)))) stage_data <- replicateM(mkFIFO());

    Bit#(TLog#(TLog#(fft_points))) final_stage = fromInteger(valueOf(TLog#(fft_points)) - 2);

    // This rule performs fft using multi-stage.
    rule stage0 (True);
        let data = inputFIFO.first();
        inputFIFO.deq();

        let stage_out = stage_f(0, data);
        stage_data[0].enq(stage_out);
    endrule

    for (Integer stage = 1; stage < valueOf(TLog#(fft_points)) - 1; stage = stage + 1) begin
        rule stage_i (True);
            let data = stage_data[stage - 1].first();
            stage_data[stage - 1].deq();

            let stage_out = stage_f(fromInteger(stage), data);
            stage_data[stage].enq(stage_out);
        endrule
    end

    rule stageFinal (True);
        let data = stage_data[final_stage].first();
        stage_data[final_stage].deq();

        let stage_out = stage_f(fromInteger(valueOf(TLog#(fft_points)) - 1), data);
        outputFIFO.enq(stage_out);
    endrule

    interface Put request;
        method Action put(Vector#(fft_points, Complex#(cmplxd)) x);
            inputFIFO.enq(bitReverse(x));
        endmethod
    endinterface

    interface Get response = toGet(outputFIFO);
endmodule

module mkCircularFFT (FFT#(fft_points, cmplxd)) provisos (Add#(2, a__, fft_points), RealLiteral#(cmplxd), Arith#(cmplxd), Bits#(cmplxd, b__));
    // Statically generate the twiddle factors table.
    TwiddleTable#(fft_points, cmplxd) twiddles = genTwiddles();

    // Define the stage_f function which uses the generated twiddles.
    function Vector#(fft_points, Complex#(cmplxd)) stage_f(Bit#(TLog#(TLog#(fft_points))) stage, Vector#(fft_points, Complex#(cmplxd)) stage_in);
        return stage_ft(twiddles, stage, stage_in);
    endfunction

    FIFO#(Vector#(fft_points, Complex#(cmplxd))) inputFIFO  <- mkFIFO();
    FIFO#(Vector#(fft_points, Complex#(cmplxd))) outputFIFO <- mkFIFO();

    Reg#(Vector#(fft_points, Complex#(cmplxd)))  stage_data <- mkRegU();
    Reg#(Bit#(TLog#(TLog#(fft_points))))         stage      <- mkReg(0);

    Bit#(TLog#(TLog#(fft_points)))               stage_final = fromInteger(valueOf(TLog#(fft_points)) - 1);

    // This rule performs fft using multi-stage.
    rule stage0 (stage == 0);
        let data = inputFIFO.first();
        inputFIFO.deq();

        let stage_out = stage_f(stage, data);
        stage_data <= stage_out;

        stage <= stage + 1;
    endrule

    rule stage_i (0 != stage && stage != stage_final);
        let data = stage_data;

        let stage_out = stage_f(stage, data);
        stage_data <= stage_out;

        stage <= stage + 1;
    endrule

    rule stageFinal (stage == stage_final);
        let data = stage_data;

        let stage_out = stage_f(stage, data);
        outputFIFO.enq(stage_out);

        stage <= 0;
    endrule

    interface Put request;
        method Action put(Vector#(fft_points, Complex#(cmplxd)) x);
            inputFIFO.enq(bitReverse(x));
        endmethod
    endinterface

    interface Get response = toGet(outputFIFO);
endmodule

// Wrapper around The FFT module we actually want to use
module mkFFT (FFT#(fft_points, cmplxd)) provisos (Add#(2, a__, fft_points), RealLiteral#(cmplxd), Arith#(cmplxd), Bits#(cmplxd, b__));
    FFT#(fft_points, cmplxd) fft <- mkCircularFFT();

    interface Put request = fft.request;
    interface Get response = fft.response;
endmodule

// Inverse FFT, based on the mkFFT module.
// ifft[k] = fft[N-k]/N
module mkIFFT (FFT#(fft_points, cmplxd)) provisos (Add#(2, a__, fft_points), RealLiteral#(cmplxd), Arith#(cmplxd), Bits#(cmplxd, b__), Bitwise#(cmplxd));

    FFT#(fft_points, cmplxd) fft <- mkFFT();
    FIFO#(Vector#(fft_points, Complex#(cmplxd))) outfifo <- mkFIFO();

    Integer n = valueof(fft_points);
    Integer lgn = valueof(TLog#(fft_points));

    function Complex#(cmplxd) scaledown(Complex#(cmplxd) x);
        return cmplx(x.rel >> lgn, x.img >> lgn);
    endfunction

    rule inversify (True);
        let x <- fft.response.get();
        Vector#(fft_points, Complex#(cmplxd)) rx = newVector;

        for (Integer i = 0; i < n; i = i+1) begin
            rx[i] = x[(n - i)%n];
        end
        outfifo.enq(map(scaledown, rx));
    endrule

    interface Put request = fft.request;
    interface Get response = toGet(outfifo);

endmodule

