`ifndef N
`define N              16
`endif
`define W               8
`define lgN     ($clog2(`N))
`define dbLgN (2*$clog2(`N))

typedef struct packed { logic [`W-1:0] data; } data_t;

module add_(//Delay: 1
    input   logic   clock,
    input   data_t  a,
    input   data_t  b,
    output  data_t  out
);
    always_ff @(posedge clock) begin
        out.data <= a.data + b.data;
    end
endmodule

module minus_(//Delay: 1
    input   logic   clock,
    input   data_t  a,
    input   data_t  b,
    output  data_t  out
);
    always_ff @(posedge clock) begin
        out.data <= a.data - b.data;
    end
endmodule

module mul_(//Delay: 1
    input   logic   clock,
    input   data_t  a,
    input   data_t  b,
    output  data_t out
);
    always_ff @(posedge clock) begin
        out.data <= a.data * b.data;
    end
endmodule

module RedUnit(
    input   logic               clock,
                                reset,
    input   data_t              data[`N-1:0],
    input   logic               split[`N-1:0],
    input   logic [`lgN:0]      out_idx[`N-1:0],
    input   logic [`lgN-1:0]    end_row_index,//输出的算到了第几行，在RU里传递，用于pipeline
    input   logic               valid[`N-1:0],//哪些数据位有效，在RU里传递，用于pipeline
    input   logic               first_flag,//是不是第一个输入，如果是，没有halo
    output  data_t              out_data[`N-1:0],
    output  data_t              halo_data_out,//用于输出halo
    output  logic [`lgN-1:0]    end_row_index_out,
    output  logic               valid_out[`N-1:0],
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = `lgN+1;//需要+1

    always@(negedge clock) begin//store
        if(reset) begin
            for(int i=0;i<`N;i=i+1) begin
               for(int j=0;j<`lgN;j=j+1) begin
                   store[i][j] = `W'b0;
               end
            end
        end
    end

    reg split_store[`lgN:0][`N-1:0];
    reg valid_store[`lgN:0][`N-1:0];
    reg first_flag_store[`lgN:0];
    logic [`lgN:0] out_idx_store[`lgN:0][`N-1:0];
    logic [`lgN-1:0] end_row_index_store[`lgN:0];
    always@(posedge clock) begin
        split_store[0] <= split;
        out_idx_store[0] <= out_idx;        
        end_row_index_store[0] <= end_row_index;
        valid_store[0] <= valid;
        first_flag_store[0] <= first_flag;
        for(int i=0;i<`lgN;i=i+1) begin           
            split_store[i+1] <= split_store[i];
            out_idx_store[i+1] <= out_idx_store[i];
            end_row_index_store[i+1] <= end_row_index_store[i];
            valid_store[i+1] <= valid_store[i];
            first_flag_store[i+1] <= first_flag_store[i];
        end
    end
    assign end_row_index_out = end_row_index_store[`lgN];
    assign valid_out = valid_store[`lgN-1];
    
    data_t store[`lgN:0][`N-1:0];//用于存储数据，否则无法pipeline
    generate
        for(genvar i0 = 0; i0 < `N; i0++) begin
            assign store[0][i0] = data[i0];
        end
    endgenerate

    generate
        for(genvar i = 0; i < `N-1; i++) begin: ADDER   
            add_ adder_inst  (
                .clock(clock),
                .a(store[0][i]),
                .b(store[0][i+1]),
                .out(store[1][i+1])
            );           
        end
    endgenerate
    add_ adder_inst  (
        .clock(clock),
        .a(store[0][0]),
        .b(`W'b0),
        .out(store[1][0])
    );
    generate
        for(genvar i1 = 1; i1 < `lgN; i1++) begin: TREE
            for(genvar i2 = 0; i2 < `N; i2++) begin: ADDER   
                if(i2<(1<<i1)) begin
                    add_ adder_inst  (
                        .clock(clock),
                        .a(store[i1][i2]),
                        .b(`W'b0),
                        .out(store[i1+1][i2])
                    );
                end
                else begin
                    add_ adder_inst  (
                        .clock(clock),
                        .a(store[i1][i2-(1<<i1)]),
                        .b(store[i1][i2]),
                        .out(store[i1+1][i2])
                    );
                end      
            end         
        end
    endgenerate

    //求部分和
    always_ff @(posedge clock) begin
        for(int i3 = 0; i3 < `N; i3++) begin: PART_SUM
            if(out_idx_store[`lgN-1][i3] == `N) begin//无效数据
                out_data[i3] = `W'b0;
            end
            else if(out_idx_store[`lgN-1][i3] == 0) begin
                out_data[i3] = store[`lgN][0];
            end
            else begin
            int j;
            for(j=out_idx_store[`lgN-1][i3]-1;j>0;j=j-1) begin
                if(split_store[`lgN-1][j]) begin
                    out_data[i3] = store[`lgN][out_idx_store[`lgN-1][i3]]-store[`lgN][j];
                    break;
                end
            end
            if(j==0)begin
                if(split_store[`lgN-1][0]) begin
                    out_data[i3] = store[`lgN][out_idx_store[`lgN-1][i3]]-store[`lgN][0];
                end
                else begin
                    out_data[i3] = store[`lgN][out_idx_store[`lgN-1][i3]];
                end
            end
        end
        end
    end

    //求halo
    always @(posedge clock) begin
        if(split_store[`lgN-1][`N-1]||first_flag_store[`lgN-1]==1) begin
            halo_data_out = `W'b0;//如果是第一组，也是0
        end
        else begin
            for(int j=`N-2;j>=0;j=j-1) begin
                if(split_store[`lgN-1][j]) begin
                    halo_data_out = store[`lgN][`N-1] - store[`lgN][j];
                    break;
                end
            end        
        end
    end
endmodule


module PE(
    input   logic               clock,
                                reset,
    input   logic               en_b,
    input   logic               lhs_start,
    input   logic [`dbLgN-1:0]  lhs_ptr [`N-1:0],
    input   logic               lhs_os,//用于传递，表示是不是output-stationary
    input   logic [`lgN-1:0]    lhs_col [`N-1:0],
    input   data_t              lhs_data[`N-1:0],
    input   data_t              rhs[`N-1:0],
    output  data_t              out[`N-1:0],
    output  logic [`lgN-1:0]    pe_out_end_idx,//记录算到了哪一行，适用于一次输不完的情况
    output  logic               lhs_os_out,
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = RedUnit_delay+2;

    data_t mul_out[`N-1:0];
    data_t add_out[`N-1:0];

    int RedUnit_delay;
    logic split[`N-1:0];
    logic split_buffer[`N-1:0];
    logic [`lgN:0] out_idx[`N-1:0];
    logic [`lgN:0] out_idx_buffer[`N-1:0];
    logic valid[`N-1:0];
    logic valid_buffer[`N-1:0];
    logic valid_out[`N-1:0];

    logic lhs_os_buffer[`N-1:0];
    always @(posedge clock) begin
        lhs_os_buffer[0] <= lhs_os;
        for(int i1=0;i1<`N-1;i1=i1+1) begin
            lhs_os_buffer[i1+1] <= lhs_os_buffer[i1];
        end
        lhs_os_out = lhs_os_buffer[RedUnit_delay-1];
    end

    int end_row_index;//用于记录行号
    reg [`N-1:0] data_input_index;//用于记录第几组data

    data_t halo_data;

    //计算split, out_idx, valid
    always @(posedge clock) begin
        if(reset||en_b) begin
            data_input_index <= 0;   //输入了几播数据
        end
        else begin
            data_input_index <= data_input_index + 1;
        end
        for (int k = 0 ;k < `N; k++) begin
            split[k] = 0;
            out_idx[k] = `N;
        end
        for (int k = 0 ; k < `N ; k++) begin
            if(lhs_ptr[k] / `N ==data_input_index)begin//-1？
                split[lhs_ptr[k] % `N] = 1;
                out_idx[k] = lhs_ptr[k] % `N;
                end_row_index = k;
            end
        end
        if((data_input_index+1)*`N-1>lhs_ptr[`N-1])begin//最后一组数据
            split[`N-1]=1;
        end

        valid[0] = 1;
        for(int k=1;k<`N;k=k+1) begin
            if(lhs_ptr[k] ==lhs_ptr[k-1]&&lhs_ptr[k] / `N ==data_input_index-1)begin
                valid[k] = 0;
            end
            else begin
                valid[k] = 1;
            end
        end
        for(int i2=0;i2<`N;i2=i2+1) begin
            out_idx_buffer[i2] <= out_idx[i2];
            split_buffer[i2] <= split[i2];   
            valid_buffer[i2] <= valid[i2];
        end
    end

    logic first_flag;//是不是第一次输入
    always @(posedge clock) begin
        if(data_input_index==0) begin
            first_flag = 1; 
        end
        else begin
            first_flag = 0;
        end
    end

    generate
        for(genvar i3 = 0; i3 < `N; i3++) begin: MULTIPLUER
            mul_ mul_inst(
                .clock(clock),
                .a(lhs_data[i3]),
                .b(rhs[lhs_col[i3]]),
                .out(mul_out[i3])
            );
        end
    endgenerate

    RedUnit red_inst(
        .clock(clock),
        .reset(reset),
        .data(mul_out),
        .split(split_buffer),//目前感觉不用buffer
        .out_idx(out_idx_buffer),
        .end_row_index(end_row_index),
        .valid(valid_buffer),
        .first_flag(first_flag),
        .out_data(add_out),
        .halo_data_out(halo_data),
        .end_row_index_out(end_row_index_out),
        .valid_out(valid_out),
        .delay(RedUnit_delay),
        .num_el(num_el)
    );

    reg [`lgN-1:0] end_row_index_out;
    reg [`lgN-1:0] end_row_index_out_buffer;
    data_t halo_data_buffer;
    data_t halo_data_in[`N-1:0];
    always_ff @(posedge clock) begin
        halo_data_buffer <= halo_data;
        end_row_index_out_buffer <= end_row_index_out;
        for(int i=0;i<`N;i++)begin
            halo_data_in[i] <= halo_data_buffer;
        end

    end

    reg [2:0] PE_state;
    reg [`lgN+1:0] counter;
    int  row_done_index;

    //PE状态机
    always @(posedge clock) begin
        if(reset) begin
            PE_state <= 0;
            counter <= 0;
        end else begin
            case(PE_state)
                0: begin//等待lhs_start
                    if(lhs_start) begin
                        PE_state <= 1;
                    end
                end
                1: begin//等待RedUnit输出
                    if(counter == RedUnit_delay-1) begin
                        PE_state <= 2;
                        counter <= 0;
                    end
                    else begin
                        counter <= counter + 1;
                    end
                end
                2: begin//输出            
                    if(end_row_index_out == `N) begin
                        PE_state <= 0;
                    end
                end
                default: begin
                    PE_state <= 0;
                end
            endcase
        end
    end

    always@(negedge clock)begin
        pe_out_end_idx = end_row_index_out;
    end

    //处理halo
    always @(negedge clock) begin
        if(PE_state==2) begin
            for(int i4=0;i4<`N;i4=i4+1) begin
                if(valid_out[i4]) begin
                    if(i4==end_row_index_out_buffer+1) begin
                        out[i4] <= add_out[i4] + halo_data_in[i4];
                    end
                    else begin
                        out[i4] <= add_out[i4];
                    end
                end
                else begin
                    out[i4] <= `W'b0;
                end
            end
        end
    end

endmodule


module SpMM(
    input   logic               clock,
                                reset,
    //输入在各种情况下是否 ready 
    output  logic               lhs_ready_ns,
                                lhs_ready_ws,
                                lhs_ready_os,
                                lhs_ready_wos,
    input   logic               lhs_start,
    // 如果是 weight-stationary, 这次使用的 rhs 将保留到下一次 
                                lhs_ws,
    // 如果是 output-stationary, 将这次的结果加到上次的 output 里 
                                lhs_os,
    input   logic [`dbLgN-1:0]  lhs_ptr [`N-1:0],
    input   logic [`lgN-1:0]    lhs_col [`N-1:0],
    input   data_t              lhs_data[`N-1:0],
    output  logic               rhs_ready,
    input   logic               rhs_start,
    input   data_t              rhs_data [3:0][`N-1:0],//无论N是多少，都是4个PE,4 row all column
    output  logic               out_ready,
    input   logic               out_start,
    output  data_t              out_data [3:0][`N-1:0],
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    
    assign rhs_ready =(rhs_buffer_state[0]==RHS_BUFFER_IDLE||rhs_buffer_state[1]==RHS_BUFFER_IDLE)&&(rhs_input_state==RHS_INPUT_IDLE);
    assign lhs_ready_ns =(lhs_state==LHS_IDLE)&&(rhs_buffer_state[rhs_buffer_toPE_idx]==RHS_BUFFER_OK);
    assign lhs_ready_ws =(lhs_state==LHS_IDLE)&&(rhs_buffer_state[rhs_buffer_toPE_idx]==RHS_BUFFER_OK);
    assign lhs_ready_os =(lhs_state==LHS_IDLE)&&(rhs_buffer_state[rhs_buffer_toPE_idx]==RHS_BUFFER_OK);
    assign lhs_ready_wos =(lhs_state==LHS_IDLE)&&(rhs_buffer_state[rhs_buffer_toPE_idx]==RHS_BUFFER_OK);

    //rhs_input
    logic rhs_input_state;
    reg [`lgN:0] counter_rhs;
    localparam RHS_INPUT_IDLE=0;
    localparam RHS_INPUT_WRITE=1;
    data_t rhs_data_PE[`N-1:0][`N-1:0];
    always@(posedge clock) begin
        if(reset) begin
            rhs_input_state <= RHS_INPUT_IDLE;
        end
        else begin
            case(rhs_input_state)
                RHS_INPUT_IDLE: begin
                    if(rhs_start) begin
                        rhs_input_state <= RHS_INPUT_WRITE;
                        counter_rhs <= counter_rhs + 1;
                    end
                end
                RHS_INPUT_WRITE: begin
                    if(counter_rhs == `N/4) begin//完成一次输入
                        rhs_input_state <= RHS_INPUT_IDLE;
                        counter_rhs <= 0;
                    end
                    else begin
                        counter_rhs <= counter_rhs + 1;
                    end
                end
                default: begin
                    rhs_input_state <= RHS_INPUT_IDLE;
                end
            endcase
        end
    end


    //rhs_buffer
    data_t rhs_buffer[1:0][`N-1:0][`N-1:0];//2个buffer
    logic rhs_buffer_write_idx;//下次写入哪个buffer
    logic rhs_buffer_toPE_idx;//当前读取哪个buffer
    logic [2:0]rhs_buffer_state[1:0];//buffer状态
    localparam RHS_BUFFER_IDLE=0;//空闲,可以写入
    localparam RHS_BUFFER_WRITE=1;//正在写入
    localparam RHS_BUFFER_OK=2;//写入完成

    logic toPE_idx_keep;//ws

    always@(posedge clock)begin
        if(lhs_start)begin//只看输入有效的lhs_ws
            toPE_idx_keep<=lhs_ws;
        end
    end
    
    logic toPE_idx_buffer;
    always@(posedge clock)begin
        rhs_buffer_toPE_idx<=toPE_idx_buffer;
    end

    //rhs_buffer状态机
    always@(posedge clock) begin
        if(reset) begin
            rhs_buffer_write_idx <= 0;
            toPE_idx_buffer<=0;
            rhs_buffer_toPE_idx <= 0;

            rhs_buffer_state[0] <= RHS_BUFFER_IDLE;
            rhs_buffer_state[1] <= RHS_BUFFER_IDLE;
            for(int i=0;i<`N;i=i+1) begin
                for(int j=0;j<`N;j=j+1) begin
                    rhs_buffer[0][i][j] <= `W'b0;
                    rhs_buffer[1][i][j] <= `W'b0;
                end
            end
        end
        else begin
            case(rhs_buffer_state[0])
                RHS_BUFFER_IDLE: begin
                    if(rhs_start&&rhs_buffer_write_idx==0) begin
                        rhs_buffer_state[0] <= RHS_BUFFER_WRITE;
                        rhs_buffer_write_idx<=0;
                        for(int i=0;i<4;i=i+1) begin
                            rhs_buffer[0][i] <= rhs_data[i];
                        end
                    end
                end
                RHS_BUFFER_WRITE: begin
                   if(rhs_input_state==RHS_INPUT_WRITE && counter_rhs<`N/4) begin
                        for(int i=0;i<4;i=i+1) begin
                            rhs_buffer[0][counter_rhs*4+i] <= rhs_data[i];
                        end
                    end
                    if(counter_rhs == `N/4) begin
                        rhs_buffer_state[0] <= RHS_BUFFER_OK;
                        rhs_buffer_write_idx<=1;
                    end
                end
                RHS_BUFFER_OK: begin
                    if(PE_over&&rhs_buffer_toPE_idx==0) begin//RHS已经输入PE，此时RHS_BUFFER可以接受新的值           
                        if(toPE_idx_keep==0)begin
                            toPE_idx_buffer<=1;
                            rhs_buffer_state[0] <= RHS_BUFFER_IDLE;
                        end
                        else begin
                            toPE_idx_buffer<=0;
                        end
                        PE_over<=0;
                    end
                end
                default: begin
                    rhs_buffer_state[0] <= RHS_BUFFER_IDLE;
                end
            endcase
            case(rhs_buffer_state[1])
                RHS_BUFFER_IDLE: begin
                    if(rhs_start&&rhs_buffer_write_idx==1) begin
                        rhs_buffer_state[1] <= RHS_BUFFER_WRITE;
                        for(int i=0;i<4;i=i+1) begin
                            rhs_buffer[1][i] <= rhs_data[i];
                        end
                    end
                end
                RHS_BUFFER_WRITE: begin
                    if(rhs_input_state==RHS_INPUT_WRITE && counter_rhs<`N/4) begin
                        for(int i=0;i<4;i=i+1) begin
                            rhs_buffer[1][counter_rhs*4+i] <= rhs_data[i];
                        end
                    end
                    if(counter_rhs == `N/4) begin
                        rhs_buffer_state[1] <= RHS_BUFFER_OK;
                        rhs_buffer_write_idx<=0;
                    end
                end
                RHS_BUFFER_OK: begin
                    if(PE_over&&rhs_buffer_toPE_idx==1) begin//RHS已经输入PE，此时RHS_BUFFER可以接受新的值
                        if(toPE_idx_keep==0)begin
                            toPE_idx_buffer<=0;//下一个使用的还是这个rhs_buffer里的数据
                            rhs_buffer_state[1] <= RHS_BUFFER_IDLE;
                        end
                        else begin
                            toPE_idx_buffer<=1;
                        end
                        PE_over<=0;
                    end
                end
                default: begin
                    rhs_buffer_state[1] <= RHS_BUFFER_IDLE;
                end
            endcase
        end
    end


    //从buffer里面读取数据的同时，将数据转置
    always@(negedge clock) begin
        if(rhs_buffer_toPE_idx==0)begin
            for(int i=0;i<`N;i=i+1) begin
                for(int j=0;j<`N;j=j+1) begin
                    rhs_data_PE[j][i] <= rhs_buffer[0][i][j];
                end
            end
        end
        else begin
            for(int i=0;i<`N;i=i+1) begin
                for(int j=0;j<`N;j=j+1) begin
                    rhs_data_PE[j][i] <= rhs_buffer[1][i][j];
                end
            end
        end
    end

    //lhs_input
    localparam LHS_IDLE=0;
    localparam LHS_WRITE=1;
    logic lhs_state;
    always@(posedge clock) begin
        if(reset) begin
            lhs_state <= LHS_IDLE;
        end
        else begin
            case(lhs_state)
                LHS_IDLE: begin
                    if(lhs_start) begin
                        lhs_state <= LHS_WRITE;
                    end
                end
                LHS_WRITE: begin
                    if(PE_over) begin
                        lhs_state <= LHS_IDLE;
                    end
                end
                default: begin
                    lhs_state <= LHS_IDLE;
                end
            endcase
        end
    end

    //output_buffer
    data_t out_data_PE[`N-1:0][`N-1:0];
    data_t out_buffer[1:0][`N-1:0][`N-1:0];//2个buffer
    logic out_buffer_write_idx;//下次写入哪个buffer
    logic out_buffer_out_idx;//当前读取哪个buffer

    //output状态机(从PE到buffer)
    logic [2:0] out_state;
    reg [`lgN:0] counter_out;
    localparam OUT_IDLE=0;
    localparam OUT_WRITE=1;
    always@(posedge clock) begin//从out_store到out_buffer
        if(reset) begin
            out_state <= OUT_IDLE;
        end
        else begin
            case(out_state)
                OUT_IDLE: begin
                    if(out_buffer_start) begin
                        out_state <= OUT_WRITE;
                        counter_out <= counter_out + 1;
                    end
                end
                OUT_WRITE: begin
                    if(counter_out==`N) begin
                        out_state <= OUT_IDLE;
                        counter_out <= 0;
                    end
                    else begin
                        counter_out <= counter_out + 1;
                    end
                end
                default: begin
                    out_state <= OUT_IDLE;
                end
            endcase
        end
    end

    //output状态机（从buffer到out）
    logic out_over;
    logic [2:0] counter_out_all;
    logic [2:0] out_all_state;
    always@(negedge clock) begin
        if(reset) begin
            out_all_state <= OUT_IDLE;
            counter_out_all <= 0;
        end
        else begin
            case(out_all_state)
                OUT_IDLE: begin
                    if(out_start) begin
                        out_all_state <= OUT_WRITE;
                        counter_out_all <= 1;
                        out_ready<=0;
                    end
                end
                OUT_WRITE: begin
                    if(counter_out_all==`N/4) begin
                        out_all_state <= OUT_IDLE;
                        out_over<=1;
                    end
                    else begin
                        counter_out_all <= counter_out_all + 1;
                end
                end
                default: begin
                    out_all_state <= OUT_IDLE;
                end
            endcase
        end
    end
    //输出数据
    always@(posedge clock) begin
        if(out_buffer_out_idx==0)begin
            for(int i=0;i<4;i=i+1) begin
                out_data[i] <= out_buffer[0][counter_out_all*4+i];
                end
            end
        else begin
            for(int i=0;i<4;i=i+1) begin
                out_data[i] <= out_buffer[1][counter_out_all*4+i];
                end
        end
    end

    //output_buffer状态机
    logic out_buffer_start;
    int done;
    logic [2:0]out_buffer_state[1:0];//buffer状态
    localparam OUT_BUFFER_IDLE=0;//空闲,可以写入
    localparam OUT_BUFFER_WRITE=1;//正在写入
    localparam OUT_BUFFER_OK=2;//写入完成
    always@(posedge clock) begin
        if(reset) begin
            out_buffer_out_idx = 0;
            out_buffer_write_idx <= 0;
            out_buffer_state[0] <= OUT_BUFFER_IDLE;
            out_buffer_state[1] <= OUT_BUFFER_IDLE;
            for(int i=0;i<`N;i=i+1) begin
                for(int j=0;j<`N;j=j+1) begin
                    out_buffer[0][i][j] <= `W'b0;
                    out_buffer[1][i][j] <= `W'b0;
                end
            end
        end
        else begin
            case(out_buffer_state[0])
                OUT_BUFFER_IDLE: begin
                    if(out_buffer_start&&out_buffer_write_idx==0) begin
                        out_buffer_state[0] <= OUT_BUFFER_WRITE;
                        out_buffer_write_idx<=0;
                        for(;done<=pe_out_end_idx;done=done+1) begin//一个周期可以处理不止一行的数据
                        for (int k = 0 ; k < `N ; k = k + 1 ) begin//转置
                            if(out_sta_flag) begin//output-stationary
                                out_buffer[0][done][k] <= out_data_PE[k][done]+out_buffer[0][done][k];
                            end
                            else begin
                                out_buffer[0][done][k] <= out_data_PE[k][done];
                            end
                        end
                        end
                    end
                end
                OUT_BUFFER_WRITE: begin
                      if(out_buffer_start && done < `N)begin
                        for(;done <= pe_out_end_idx;done=done+1) begin
                            for (int k = 0 ; k < `N ; k = k + 1 ) begin//转置
                                if(out_sta_flag) begin
                                    out_buffer[0][done][k] <= out_data_PE[k][done]+out_buffer[0][done][k];
                                end
                                else begin
                                    out_buffer[0][done][k] <= out_data_PE[k][done];
                                end
                                
                            end
                        end
                    end
                    if(done == `N) begin
                        out_buffer_state[0] <= OUT_BUFFER_OK;
                        if(out_sta_flag) begin
                            out_buffer_write_idx<=0;
                        end
                        else begin
                            out_buffer_write_idx<=1;
                        end
                        out_buffer_start<=0;
                        done=0;
                    end
                end
                OUT_BUFFER_OK: begin
                    if(out_all_state==OUT_IDLE&&out_buffer_out_idx==0) begin
                        if(out_over)begin
                            out_over<=0;
                        end
                    end
                end
                default: begin
                    out_buffer_state[0] <= OUT_BUFFER_IDLE;
                end
            endcase
            case(out_buffer_state[1])
                OUT_BUFFER_IDLE: begin
                    if(out_buffer_start&&out_buffer_write_idx==1) begin
                        out_buffer_state[1] <= OUT_BUFFER_WRITE;
                        for(;done<=pe_out_end_idx;done=done+1) begin
                        for (int k = 0 ; k < `N ; k = k + 1 ) begin//转置
                            if(out_sta_flag) begin
                                out_buffer[1][done][k] <= out_data_PE[k][done]+out_buffer[1][done][k];
                            end
                            else begin
                            out_buffer[1][done][k] <= out_data_PE[k][done];
                            end
                        end
                        end
                    end
                end
                OUT_BUFFER_WRITE: begin
                    if(out_buffer_start && done<`N) begin
                        for(;done<=pe_out_end_idx;done=done+1) begin
                            for(int k = 0 ; k < `N ; k = k + 1 ) begin//转置
                                if(out_sta_flag) begin
                                    out_buffer[1][done][k] <= out_data_PE[k][done]+out_buffer[1][done][k];
                                end
                                else begin
                                    out_buffer[1][done][k] <= out_data_PE[k][done];
                                end
                            end
                        end
                    end
                    if(done == `N) begin
                        out_buffer_state[1] <= OUT_BUFFER_OK;
                        done=0;
                        if(out_sta_flag) begin
                            out_buffer_write_idx<=1;
                        end
                        else begin
                            out_buffer_write_idx<=0;
                        end
                        out_buffer_start<=0;
                    end
                end
                OUT_BUFFER_OK: begin
                    if(out_all_state==OUT_IDLE&&out_buffer_out_idx==1) begin//RHS已经输入PE，此时RHS_BUFFER可以接受新的值
                        if(out_over)begin
                            out_over<=0;        
                        end
                    end
                end
                default: begin
                    out_buffer_state[1] <= OUT_BUFFER_IDLE;
                end
            endcase
        end
    end

    //输出结束后，切换buffer
    always@(posedge out_over) begin
        if(out_buffer_out_idx==0&&out_all_state==OUT_IDLE
        &&out_buffer_state[0]==OUT_BUFFER_OK)begin
            out_buffer_out_idx=1;
            out_buffer_state[0]=OUT_BUFFER_IDLE;
        end
        else if(out_buffer_out_idx==1&&out_all_state==OUT_IDLE
        &&out_buffer_state[1]==OUT_BUFFER_OK)begin
            out_buffer_out_idx=0;
            out_buffer_state[1]=OUT_BUFFER_IDLE;
        end            
    end 
    //如果是output-stationary，不切换buffer
    always @(posedge out_sta_flag)begin 
            if(out_buffer_write_idx==0)begin
                out_buffer_write_idx<=1;
                out_buffer_state[1]<=OUT_BUFFER_IDLE;
            end
            else begin
                out_buffer_write_idx<=0;
                out_buffer_state[0]<=OUT_BUFFER_IDLE;
            end
    end
    //不再是output-stationary，切换buffer
    always @(negedge out_sta_flag)begin
        if(out_buffer_write_idx==0)begin
            out_buffer_state[1]<=OUT_BUFFER_IDLE;
            out_buffer_write_idx<=1;
        end
        else begin
            out_buffer_state[0]<=OUT_BUFFER_IDLE;
            out_buffer_write_idx<=0;
        end
    end

    //由于是output-stationary，还没ready
    always@(posedge lhs_os)begin
        out_ready<=0;
    end

    //总状态机
    reg [2:0] state;
    reg [`lgN:0] counter_delay;
    logic out_sta_flag;
    logic [`lgN:0] counter_out_1;
    logic PE_over;
    always @(posedge clock) begin
        if(reset) begin
            state <= 0;
            counter_rhs <= 0;
        end else begin
            case(state)
                0: begin//wait to read rhs
                en_b = 1;
                    if(rhs_start) begin
                        state <= 1;
                    end
                end
                1: begin//read rhs
                    if(lhs_start) begin
                        state <= 2;
                        PE_over <= 0;
                    end
                end
                2: begin//calculate
                    if(lhs_os)begin
                        out_ready<=0;
                        out_buffer_state[out_buffer_write_idx]<=OUT_BUFFER_IDLE;
                    end
                    if(counter_delay == PE_delay-3) begin
                        state <= 3;
                        out_buffer_start<=1;
                        counter_out_1 <= 0;
                        counter_delay <= 0;
                    end
                    else begin
                        counter_delay <= counter_delay + 1;
                    end
                end
                3: begin//PE到buffer
                    if(counter_out_1 < `N)begin
                        counter_out_1 <= counter_out_1 + 1;
                    end
                    else begin
                        counter_out_1 <= 0;
                        state <= 4;
                        PE_over <= 1;
                        out_ready <= 1;
                        en_b=1;
                    end
                end
                4: begin//write out           
                    if(lhs_start) begin
                        state <= 2;
                    end
                end
                default: begin
                    state <= 0;
                end
            endcase
        end
    end

    logic en_b;
    always@(posedge lhs_start) begin
        en_b = 0;
    end

    logic reset_pe;
    assign reset_pe = reset||(lhs_ready_ns&&!lhs_start);

    logic [`lgN-1:0] pe_out_end_idx;
    int PE_delay;
    generate
        for(genvar i4 = 0; i4 < `N; i4++) begin: pe_inst//N个PE
            PE pe_inst(
                .clock(clock),
                .reset(reset_pe),
                .en_b(en_b),
                .lhs_start(lhs_start),
                .lhs_os(lhs_os),
                .lhs_ptr(lhs_ptr),
                .lhs_col(lhs_col),
                .lhs_data(lhs_data),
                .rhs(rhs_data_PE[i4]),
                .out(out_data_PE[i4]),
                .pe_out_end_idx(pe_out_end_idx),
                .lhs_os_out(out_sta_flag),
                .delay(PE_delay),
                .num_el(num_el)
            );
        end
    endgenerate  
endmodule