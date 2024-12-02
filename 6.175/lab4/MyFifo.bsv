import Ehr::*;
import Vector::*;

//////////////////
// Fifo interface

interface Fifo#(numeric type n, type t);
    method Bool notFull;
    method Action enq(t x);
    method Bool notEmpty;
    method Action deq;
    method t first;
    method Action clear;
endinterface

/////////////////
// Conflict FIFO

module mkMyConflictFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     queue    <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(n)))    front    <- mkReg(0);
    Reg#(Bit#(TLog#(n)))    rear     <- mkReg(0);
    Reg#(Bool)              empty    <- mkReg(True);
    Reg#(Bool)              full     <- mkReg(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    // TODO: Implement all the methods for this module
    method Bool notFull;
        return !full;
    endmethod

    method Action enq(t x) if (!full);
        let new_rear = rear == max_index ? 0 : rear + 1;

        queue[rear] <= x;
        rear <= new_rear;
        full <= new_rear == front;
        empty <= False;
    endmethod

    method Bool notEmpty;
        return !empty;
    endmethod

    method Action deq if (!empty);
        let new_front = front == max_index ? 0 : front + 1;

        front <= new_front;
        full <= False;
        empty <= new_front == rear;
    endmethod

    method t first if (!empty);
        return queue[front];
    endmethod

    method Action clear;
        front <= 0;
        rear <= 0;
        empty <= True;
        full <= False;
    endmethod
endmodule

/////////////////
// Pipeline FIFO

// Intended schedule:
//      {notEmpty, first, deq} < {notFull, enq} < clear
module mkMyPipelineFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))        queue    <- replicateM(mkRegU());
    Ehr#(3, Bit#(TLog#(n)))    front    <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n)))    rear     <- mkEhr(0);
    Ehr#(3, Bool)              empty    <- mkEhr(True);
    Ehr#(3, Bool)              full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    // TODO: Implement all the methods for this module
    method Bool notFull;
        return !full[1];
    endmethod

    method Action enq(t x) if (!full[1]);
        let new_rear = rear[1] == max_index ? 0 : rear[1] + 1;

        queue[rear[1]] <= x;
        rear[1] <= new_rear;
        full[1] <= new_rear == front[1];
        empty[1] <= False;
    endmethod

    method Bool notEmpty;
        return !empty[0];
    endmethod

    method Action deq if (!empty[0]);
        let new_front = front[0] == max_index ? 0 : front[0] + 1;

        front[0] <= new_front;
        full[0] <= False;
        empty[0] <= new_front == rear[0];
    endmethod

    method t first if (!empty[0]);
        return queue[front[0]];
    endmethod

    method Action clear;
        front[2] <= 0;
        rear[2] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod
endmodule

/////////////////////////////
// Bypass FIFO without clear

// Intended schedule:
//      {notFull, enq} < {notEmpty, first, deq} < clear
module mkMyBypassFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Ehr#(2, t))      queue    <- replicateM(mkEhr(?));
    Ehr#(3, Bit#(TLog#(n)))     front    <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n)))     rear     <- mkEhr(0);
    Ehr#(3, Bool)               empty    <- mkEhr(True);
    Ehr#(3, Bool)               full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    // TODO: Implement all the methods for this module
    method Bool notFull;
        return !full[0];
    endmethod

    method Action enq(t x) if (!full[0]);
        let new_rear = rear[0] == max_index ? 0 : rear[0] + 1;

        queue[rear[0]][0] <= x;
        rear[0] <= new_rear;
        full[0] <= new_rear == front[0];
        empty[0] <= False;
    endmethod

    method Bool notEmpty;
        return !empty[1];
    endmethod

    method Action deq if (!empty[1]);
        let new_front = front[1] == max_index ? 0 : front[1] + 1;

        front[1] <= new_front;
        full[1] <= False;
        empty[1] <= new_front == rear[1];
    endmethod

    method t first if (!empty[1]);
        return queue[front[1]][1];
    endmethod

    method Action clear;
        front[2] <= 0;
        rear[2] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod
endmodule

//////////////////////
// Conflict free fifo

// Intended schedule:
//      {notFull, enq} CF {notEmpty, first, deq}
//      {notFull, enq, notEmpty, first, deq} < clear
module mkMyCFNCFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     queue    <- replicateM(mkRegU);
    Reg#(Bit#(TLog#(n)))    front    <- mkReg(0);
    Reg#(Bit#(TLog#(n)))    rear     <- mkReg(0);
    Reg#(Bool)              empty    <- mkReg(True);
    Reg#(Bool)              full     <- mkReg(False);

    Ehr#(2, Maybe#(t))      enq_request  <- mkEhr(Invalid);
    Ehr#(2, Bool)           deq_request  <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    (* no_implicit_conditions, fire_when_enabled *)
    rule canonicalize;
        let new_front = front == max_index ? 0 : front + 1;
        let new_rear = rear == max_index ? 0 : rear + 1;

        case (enq_request[1]) matches
            tagged Valid .data: begin
                queue[rear] <= data;
                rear <= new_rear;
            end
            default: noAction;
        endcase

        if (deq_request[1]) begin
            front <= new_front;
        end

        case (tuple2( isValid(enq_request[1]), deq_request[1] )) matches
            // Only enqueue
            {True, False}: begin
                full <= new_rear == front;
                empty <= False;
            end
            // Only dequeue
            {False, True}: begin
                full <= False;
                empty <= new_front == rear;
            end
            default: noAction;
        endcase

        enq_request[1] <= Invalid;
        deq_request[1] <= False;
    endrule

    method Bool notFull;
        return !full;
    endmethod

    method Action enq(t x) if (!full);
        enq_request[0] <= tagged Valid x;
    endmethod

    method Bool notEmpty;
        return !empty;
    endmethod

    method Action deq if (!empty);
        deq_request[0] <= True;
    endmethod

    method t first if (!empty);
        return queue[front];
    endmethod

    method Action clear;
        rear <= 0;
        front <= 0;

        full <= False;
        empty <= True;
    endmethod
endmodule


//////////////////////
// Conflict free fifo

// Intended schedule:
//      {notFull, enq} CF {notEmpty, first, deq}
//      {notFull, enq, notEmpty, first, deq} < clear
module mkMyCFFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))        queue    <- replicateM(mkRegU);
    Ehr#(2, Bit#(TLog#(n)))    front    <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n)))    rear     <- mkEhr(0);
    Ehr#(2, Bool)              empty    <- mkEhr(True);
    Ehr#(2, Bool)              full     <- mkEhr(False);

    Ehr#(2, Maybe#(t))      enq_request  <- mkEhr(Invalid);
    Ehr#(2, Bool)           deq_request  <- mkEhr(False);
    Ehr#(2, Bool)           clr_request  <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    (* no_implicit_conditions, fire_when_enabled *)
    rule canonicalize;
        let new_front = front[0] == max_index ? 0 : front[0] + 1;
        let new_rear = rear[0] == max_index ? 0 : rear[0] + 1;

        case (enq_request[1]) matches
            tagged Valid .data: begin
                queue[rear[0]] <= data;
                rear[0] <= new_rear;
            end
            default: noAction;
        endcase

        if (deq_request[1]) begin
            front[0] <= new_front;
        end

        case (tuple2( isValid(enq_request[1]), deq_request[1] )) matches
            // Only enqueue
            {True, False}: begin
                full[0] <= new_rear == front[0];
                empty[0] <= False;
            end
            // Only dequeue
            {False, True}: begin
                full[0] <= False;
                empty[0] <= new_front == rear[0];
            end
            default: noAction;
        endcase

        if (clr_request[1]) begin
            rear[1] <= 0;
            front[1] <= 0;

            full[1] <= False;
            empty[1] <= True;
        end

        enq_request[1] <= Invalid;
        deq_request[1] <= False;
        clr_request[1] <= False;
    endrule

    method Bool notFull;
        return !full[0];
    endmethod

    method Action enq(t x) if (!full[0]);
        enq_request[0] <= tagged Valid x;
    endmethod

    method Bool notEmpty;
        return !empty[0];
    endmethod

    method Action deq if (!empty[0]);
        deq_request[0] <= True;
    endmethod

    method t first if (!empty[0]);
        return queue[front[0]];
    endmethod

    method Action clear;
        clr_request[0] <= True;
    endmethod
endmodule
