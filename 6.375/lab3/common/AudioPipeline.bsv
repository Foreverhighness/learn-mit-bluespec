import FixedPoint::*;
import ClientServer::*;
import GetPut::*;

import AudioProcessorTypes::*;
import Chunker::*;
import FFT::*;
import FIRFilter::*;
import Splitter::*;
import Cordic::*;
import PitchAdjust::*;

import FilterCoefficients::*;

module mkAudioPipeline(AudioProcessor);

    AudioProcessor fir <- mkFIRFilter(c);
    Chunker#(FFT_POINTS, ComplexSample) chunker <- mkChunker();
    FFT#(FFT_POINTS, FixedPoint#(16, 16)) fft <- mkFFT();
    FFT#(FFT_POINTS, FixedPoint#(16, 16)) ifft <- mkIFFT();
    Splitter#(FFT_POINTS, ComplexSample) splitter <- mkSplitter();

    ToMP#(8, 16, 16, 16)                  to_mp        <- mkToMP();
    PitchAdjust#(8, 16, 16, 16)           pitch_adjust <- mkPitchAdjust(2, 2);
    FromMP#(8, 16, 16, 16)                from_mp      <- mkFromMP();

    rule fir_to_chunker (True);
        let x <- fir.getSampleOutput();
        chunker.request.put(tocmplx(x));
        $display("fir_to_chunker");
    endrule

    rule chunker_to_fft (True);
        $display("chunker_to_fft");
        let x <- chunker.response.get();
        fft.request.put(x);
    endrule

    rule fft_to_to_mp (True);
        let x <- fft.response.get();
        to_mp.request.put(x);
    endrule

    rule to_mp_to_pitch_adjust (True);
        let x <- to_mp.response.get();
        pitch_adjust.request.put(x);
    endrule

    rule pitch_adjust_to_from_mp (True);
        let x <- pitch_adjust.response.get();
        from_mp.request.put(x);
    endrule

    rule from_mp_to_ifft (True);
        let x <- from_mp.response.get();
        ifft.request.put(x);
    endrule

    rule ifft_to_splitter (True);
        let x <- ifft.response.get();
        splitter.request.put(x);
    endrule

    method Action putSampleInput(Sample x);
        $display("putSampleInput");
        fir.putSampleInput(x);
    endmethod

    method ActionValue#(Sample) getSampleOutput();
        let x <- splitter.response.get();
        return frcmplx(x);
    endmethod

endmodule

