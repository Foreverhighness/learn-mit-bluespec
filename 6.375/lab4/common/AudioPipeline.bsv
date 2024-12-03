import FixedPoint::*;
import ClientServer::*;
import GetPut::*;
import Vector::*;

import AudioProcessorTypes::*;
import Chunker::*;
import FFT::*;
import FIRFilter::*;
import Splitter::*;
import Cordic::*;
import PitchAdjust::*;
import OverSampler::*;
import Overlayer::*;

import FilterCoefficients::*;

module mkAudioPipeline(AudioProcessor);
    AudioProcessor                          fir           <- mkFIRFilter(c);
    Chunker#(2, Sample)                     chunker       <- mkChunker();
    OverSampler#(2, FFT_POINTS, Sample)     over_sampler  <- mkOverSampler(replicate(0));
    FFT#(FFT_POINTS, FixedPoint#(16, 16))   fft           <- mkFFT();
    ToMP#(FFT_POINTS, 16, 16, 16)           to_mp         <- mkToMP();
    PitchAdjust#(FFT_POINTS, 16, 16, 16)    pitch_adjust  <- mkPitchAdjust(2, 2);
    FromMP#(FFT_POINTS, 16, 16, 16)         from_mp       <- mkFromMP();
    FFT#(FFT_POINTS, FixedPoint#(16, 16))   ifft          <- mkIFFT();
    Overlayer#(FFT_POINTS, 2, Sample)       over_layer    <- mkOverlayer(replicate(0));
    Splitter#(2, Sample)                    splitter      <- mkSplitter();

    rule fir_to_chunker (True);
        let x <- fir.getSampleOutput();
        chunker.request.put(x);
    endrule

    rule chunker_to_over_sampler (True);
        let x <- chunker.response.get();
        over_sampler.request.put(x);
    endrule

    rule over_sampler_to_fft (True);
        let x <- over_sampler.response.get();
        fft.request.put(map(tocmplx, x));
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

    rule ifft_to_over_layer (True);
        let x <- ifft.response.get();
        over_layer.request.put(map(frcmplx, x));
    endrule

    rule over_layer_to_splitter (True);
        let x <- over_layer.response.get();
        splitter.request.put(x);
    endrule

    method Action putSampleInput(Sample x);
        $display("putSampleInput");
        fir.putSampleInput(x);
    endmethod

    method ActionValue#(Sample) getSampleOutput();
        let x <- splitter.response.get();
        return x;
    endmethod
endmodule

