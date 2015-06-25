----------------------------------------------------------------------------------
-- __/\\\\\\\\\\\\\\\_        ____/\\\\\\\\\_____        __/\\\\\\\\\\\\\\\_        
--  _\///////\\\/////__        __/\\\///////\\\___        _\///////\\\/////__       
--   _______\/\\\_______        _\/\\\_____\/\\\___        _______\/\\\_______      
--    _______\/\\\_______        _\/\\\\\\\\\\\/____        _______\/\\\_______     
--     _______\/\\\_______        _\/\\\//////\\\____        _______\/\\\_______    
--      _______\/\\\_______        _\/\\\____\//\\\___        _______\/\\\_______   
--       _______\/\\\_______        _\/\\\_____\//\\\__        _______\/\\\_______  
--        _______\/\\\_______        _\/\\\______\//\\\_        _______\/\\\_______ 
--         _______\///________        _\///________\///__        _______\///________
-- 
-- Create Date: 15.05.2014 08:52:23
-- Author:  arnaud.samama@thalesgroup.com
-- Design Name: 
-- Module Name: top - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------
-- 
-- Copyright (C) 2014 Authors 
-- 
-- This source file may be used and distributed without 
-- restriction provided that this copyright statement is not 
-- removed from the file and that any derivative work contains 
-- the original copyright notice and the associated disclaimer. 
-- 
-- This source file is free software; you can redistribute it 
-- and/or modify it under the terms of the GNU Lesser General 
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any 
-- later version. 
-- 
-- This source is distributed in the hope that it will be 
-- useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
-- PURPOSE. See the GNU Lesser General Public License for more 
-- details. 
-- 
-- You should have received a copy of the GNU Lesser General 
-- Public License along with this source; if not, download it 
-- from http://www.opencores.org/lgpl.shtml 
-- 
-------------------------------------------------------------------------------
----------------------------------------------------------------------------------



library ieee;
use ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE ieee.math_real.ALL; 
use work.txt_util.all;


package axi4bfm_pkg is

    
    constant C_M_AXI_THREAD_ID_WIDTH : integer := 1;
    constant C_M_AXI_ADDR_WIDTH      : integer := 32;
    constant C_M_AXI_DATA_WIDTH      : integer := 32;
    constant C_M_AXI_AWUSER_WIDTH    : integer := 1;
    constant C_M_AXI_ARUSER_WIDTH    : integer := 1;
    constant C_M_AXI_WUSER_WIDTH     : integer := 1;
    constant C_M_AXI_RUSER_WIDTH     : integer := 1;
    constant C_M_AXI_BUSER_WIDTH     : integer := 1;
    
    constant C_S_AXI_ADDR_WIDTH      : integer := 32;
    constant C_S_AXI_DATA_WIDTH      : integer := 32;
    
    constant C_AXI_LOCK_WIDTH        : integer := 1;    

    
    type axi4read_data_type is (
        PATTERN_INCREASING,
        PATTERN_DECREASING,
        PATTERN_STATIC,
        PATTERN_RANDOM,
        PATTERN_MIRROR,
        PATTERN_FILE
    );
    
    type AxiLITEslave2master_bus_t is record
        -- Slave Interface Write Address Ports
        S_AXI_AWREADY  :std_logic;

        -- Slave Interface Write Data Ports
        S_AXI_WREADY : std_logic;

        -- Slave Interface Write Response Ports
        S_AXI_BRESP  : std_logic_vector(2-1 downto 0);
        S_AXI_BVALID : std_logic;

        -- Slave Interface Read Address Ports
        S_AXI_ARREADY  : std_logic;

        -- Slave Interface Read Data Ports
        S_AXI_RDATA  : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_RRESP  : std_logic_vector(2-1 downto 0);
        S_AXI_RVALID : std_logic;
    
    end record AxiLITEslave2master_bus_t;
    
    type AxiLITEmaster2slave_bus_t is record
        S_AXI_AWADDR   : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_AWPROT   : std_logic_vector(3-1 downto 0);
        S_AXI_AWVALID  : std_logic;

        -- Slave Interface Write Data Ports
        S_AXI_WDATA  :std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_WSTRB  :std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0);
        S_AXI_WVALID :std_logic;


        -- Slave Interface Write Response Ports
        S_AXI_BREADY : std_logic;

        -- Slave Interface Read Address Ports
        S_AXI_ARADDR   : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_ARPROT   : std_logic_vector(3-1 downto 0);
        S_AXI_ARVALID  : std_logic;


        -- Slave Interface Read Data Ports
        S_AXI_RREADY : std_logic;		        
    
    end record AxiLITEmaster2slave_bus_t;   
    
    type axi4slave2master_bus_t is record
    
        -- Master Interface Write Address
        M_AXI_AWREADY   :  std_logic;
        
        -- Master Interface Write Data
        M_AXI_WREADY    :  std_logic;
        
        -- Master Interface Write Response
        M_AXI_BID    : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
        M_AXI_BRESP  : std_logic_vector(2-1 downto 0);
        M_AXI_BUSER  : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
        M_AXI_BVALID : std_logic; 
        
         -- Master Interface Read Address
        M_AXI_ARREADY : std_logic;  

        -- Master Interface Read Data 
        M_AXI_RID    : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
        M_AXI_RDATA  : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        M_AXI_RRESP  : std_logic_vector(2-1 downto 0);
        M_AXI_RLAST  : std_logic;
        M_AXI_RUSER  : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
        M_AXI_RVALID : std_logic;
       
    end record axi4slave2master_bus_t;
    
    type axi4master2slave_bus_t is record
        -- ======================================================================
        -- AXI Master Interface
        -- ======================================================================
          -- Master Interface Write Address
        M_AXI_AWID    :std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
        M_AXI_AWADDR  :std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        M_AXI_AWLEN   :std_logic_vector(8-1 downto 0);
        M_AXI_AWSIZE  :std_logic_vector(3-1 downto 0);
        M_AXI_AWBURST :std_logic_vector(2-1 downto 0);
        M_AXI_AWLOCK  :std_logic;
        M_AXI_AWCACHE :std_logic_vector(4-1 downto 0);
        M_AXI_AWPROT  :std_logic_vector(3-1 downto 0);
        -- AXI3    M_AXI_AWREGION:out std_logic_vector(4-1 downto 0);
        M_AXI_AWQOS   : std_logic_vector(4-1 downto 0);
        M_AXI_AWUSER  : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
        M_AXI_AWVALID : std_logic;


        -- Master Interface Write Data
        -- AXI3   M_AXI_WID(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
        M_AXI_WDATA  :  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        M_AXI_WSTRB  :  std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
        M_AXI_WLAST  :  std_logic;
        M_AXI_WUSER  :  std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
        M_AXI_WVALID :  std_logic;


        -- Master Interface Write Response
        M_AXI_BREADY : std_logic;

        -- Master Interface Read Address
        M_AXI_ARID    : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
        M_AXI_ARADDR  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        M_AXI_ARLEN   : std_logic_vector(8-1 downto 0);
        M_AXI_ARSIZE  : std_logic_vector(3-1 downto 0);
        M_AXI_ARBURST : std_logic_vector(2-1 downto 0);
        M_AXI_ARLOCK  : std_logic;
        M_AXI_ARCACHE : std_logic_vector(4-1 downto 0);
        M_AXI_ARPROT  : std_logic_vector(3-1 downto 0);
        -- AXI3   M_AXI_ARREGION:out std_logic_vector(4-1 downto 0);
        M_AXI_ARQOS   : std_logic_vector(4-1 downto 0);
        M_AXI_ARUSER  : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
        M_AXI_ARVALID : std_logic;


        -- Master Interface Read Data 
        M_AXI_RREADY : std_logic; 

    end record axi4master2slave_bus_t;

    type memory_element_t is record
        addr   : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        data   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        strobe : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    end record memory_element_t;

    type memory_t is array (integer range <>) of memory_element_t;
    
    procedure AxiLITEMasterInit( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t);
            
    procedure Axi4SlaveWriteInit( 
            signal clk : std_logic; 
            signal  slave2master: out axi4slave2master_bus_t
            ) ; 
            
    procedure Axi4SlaveReadInit( 
            signal clk : std_logic; 
            signal  slave2master: out axi4slave2master_bus_t
            ) ;
            
    procedure Axi4SlaveAcceptAnyRead( 
            signal clk : std_logic; 
            signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t;
            data_type : axi4read_data_type );
            
     --procedure Axi4SlaveAcceptAnyWrite( 
            --signal clk : std_logic; 
            --signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t);   

     procedure Axi4SlaveAcceptAnyWrite( 
            signal clk : std_logic; 
            signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: inout axi4slave2master_bus_t--;
            --variable memory :  out memory_t;
            --variable memory_idx : inout integer;
            --signal clear_memory : in std_logic 
        );   
            
    procedure Axi4SlaveAcceptReadFrom( 
            signal clk : std_logic; 
            signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t; 
            data_type : axi4read_data_type;
            start_address : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0); size  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0) 
            );
    function AxSize2Byte( AxSIZE : std_logic_vector(2 downto 0) ) return unsigned;  

    impure function Axi4SlaveClearMemory return boolean;  

    impure function Axi4SlaveGetMemory return memory_t;  

    impure function Axi4SlaveRewindMemory return boolean;

    impure function Axi4LoadSlaveFromFile( filename : in string )  return boolean;
    
    impure function GetPatternFromAddr( base_addr : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0); 
                                offset_addr : unsigned ) return std_logic_vector;

    procedure GetMemoryPattern( prev_data : in unsigned; next_data : out unsigned; addr : std_logic_vector;  
                    data_type : axi4read_data_type; pattern : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0) );


    procedure AxiLITEMasterWrite( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data : in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0)
            );    
                
    procedure AxiLITEMasterWrite( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data : in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
                    strobe : std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0)   
            );
            
    procedure AxiLITEMasterRead( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address     : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data        : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0)
            );
            
end package;


package body axi4bfm_pkg is

shared variable  memory : memory_t(0 to 1023 );
shared variable memory_idx : integer := 0;

shared variable memory_tb : memory_tb_t(0 to 1023);
shared variable memory_tb_size : integer := 0;

-- Track the last value we have returned
shared variable sent_data  : unsigned(C_M_AXI_DATA_WIDTH-1 downto 0) := (others => '0' );

-- Variable defining
shared variable TicToc: boolean := true;

impure function Axi4SlaveRewindMemory return boolean  is
begin
    sent_data := (others => '0');
    return true;
end function;

impure function Axi4SlaveClearMemory return boolean  is
    variable cell : integer;
begin

    for cell in memory'range loop
        memory(cell).addr     := ( others => 'U'); 
        memory(cell).data     := ( others => 'U'); 
        memory(cell).strobe   := ( others => 'U'); 
    end loop;

    memory_idx := 0;
    return true;

end function;

impure function Axi4SlaveGetMemory return memory_t  is
begin
    return memory;
end function;

impure function Axi4LoadSlaveFromFile( filename : in string )  return boolean  is
begin
    hstr_file_read(filename, memory_tb,memory_tb_size); 
    if memory_tb_size /= 0 then
        return true;
    else
        return false;
    end if;
end function;

function AxSize2Byte( AxSIZE : std_logic_vector(2 downto 0) ) return unsigned is 
begin

    return x"1" sll to_integer(unsigned(AxSIZE));
  
end function;

impure function GetPatternFromAddr( base_addr : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0); offset_addr : unsigned ) return std_logic_vector is

    variable current_addr  : std_logic_vector(base_addr'range);
    variable value_addr     : std_logic_vector(base_addr'range);
    variable pattern_seed : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0)  := x"ACCE55ED";
    
begin
    current_addr := std_logic_vector(unsigned(base_addr) + offset_addr);

    value_addr :=   (pattern_seed(7 ) xor  current_addr(10)) & 
                    (pattern_seed(10) xor  current_addr(8 )) &
                    (pattern_seed(9 ) xor  current_addr(21)) &
                    (pattern_seed(4 ) xor  current_addr(20)) &
                    (pattern_seed(23) xor  current_addr(11)) &
                    (pattern_seed(1 ) xor  current_addr(3 )) &
                    (pattern_seed(2 ) xor  current_addr(7 )) &
                    (pattern_seed(5 ) xor  current_addr(26)) &
                    (pattern_seed(22) xor  current_addr(6 )) &
                    (pattern_seed(6 ) xor  current_addr(14)) &
                    (pattern_seed(27) xor  current_addr(15)) &
                    (pattern_seed(8 ) xor  current_addr(2 )) &
                    (pattern_seed(16) xor  current_addr(25)) &
                    (pattern_seed(15) xor  current_addr(27)) &
                    (pattern_seed(28) xor  current_addr(9 )) &
                    (pattern_seed(3 ) xor  current_addr(19)) &
                    (pattern_seed(11) xor  current_addr(13)) &
                    (pattern_seed(31) xor  current_addr(16)) &
                    (pattern_seed(17) xor  current_addr(18)) &
                    (pattern_seed(10) xor  current_addr(23)) &
                    (pattern_seed(14) xor  current_addr(5 )) &
                    (pattern_seed(20) xor  current_addr(28)) &
                    (pattern_seed(12) xor  current_addr(31)) &
                    (pattern_seed(19) xor  current_addr(17)) &
                    (pattern_seed(24) xor  current_addr(12)) &
                    (pattern_seed(29) xor  current_addr(24 )) &
                    (pattern_seed(18) xor  current_addr(22)) &
                    (pattern_seed(25) xor  current_addr(29)) &
                    (pattern_seed(13) xor  current_addr(20)) &
                    (current_addr(1 ) xor  current_addr(1 )) &
                    (current_addr(2 ) xor  current_addr(4 )) &
                    (current_addr(3 ) xor  current_addr(0 ));
                    
    return value_addr;
                                                
end;
 impure function GetHexFromStrobe( index: integer; data: std_logic_vector; strobe: std_logic_vector ) 
    return string is
    
    variable hex_str : string(1 to 2);
begin

        -- Use strobe information
        if strobe(index-1) = '1' then
            hex_str := hstr(data((index)*8 -1 downto (index-1) * 8 ));
        else
            hex_str := "--";
        end if;
        --print("hex_str" & hex_str);
        return hex_str;
end function;

procedure WaitOnWithTimeout( 
        signal clk : std_logic; 
        signal data : std_logic; 
        constant value : std_logic;
        constant timeout : time; 
        constant message : string) is
        
    variable stop_waiting : time ;
begin
    -- Keep track of the time we entered the function
    stop_waiting := NOW + timeout;
    
    -- Waiting for the master being able to accept our response
    loop
        wait until rising_edge(clk);
        exit when data = value;
        if NOW > stop_waiting then
            report "Timeout occured while waiting " & message severity FAILURE;
        end if;
    end loop;  
    
end procedure;

procedure WaitOnWithTimeout( 
        signal clk : std_logic; 
        signal data : std_logic; 
        constant value : std_logic;
        constant message : string) is   
begin
    WaitOnWithTimeout(clk, data, value, 500 ns, message);    
end procedure;

procedure Axi4SlaveAcceptAnyWrite( 
        signal clk : std_logic; 
        signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: inout axi4slave2master_bus_t
        --variable memory :  out memory_t;
        --variable memory_idx : inout integer;
        --signal clear_memory : in std_logic 
    ) is
        
    variable write_addr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    variable write_len   : unsigned(master2slave . M_AXI_ARLEN'range);
    variable write_size  : unsigned(master2slave . M_AXI_ARLEN'range);
    variable beat_size  : unsigned(3 downto 0);
    variable xfer_size  : integer;
    variable xfered_size  : integer;
    
    variable data_content : string(1 to 8);
    variable burst_started : std_logic := '0';
    -- Timeout after x cycle of inactivity during a transaction
    constant timeout : integer := 32; 
    variable timeout_counter : integer := 0;
    variable stop_waiting : time ;
    variable pause_done : boolean := false;
    --variable memory_idx : integer := memory'right;
    --variable index : integer;
begin
    --print("***************************Entering Write slave");
    -- Not ready to read any address
    slave2master . M_AXI_AWREADY <= '1';  
    
    -- Still not ready to get data to be picked from the bus
    slave2master . M_AXI_WREADY <= '0';
       
    xfered_size := 0; 
    timeout_counter := 0;
    
    -- Waiting for the master presenting the address it wants to write to
    wait until rising_edge(clk) and master2slave . M_AXI_AWVALID = '1';
    write_addr := master2slave . M_AXI_AWADDR;

    -- Store how many beat the master intends to write
    write_len := unsigned(master2slave . M_AXI_AWLEN) + 1;
    write_size := (others => '0');
    assert write_len /= 0 report "Invalid data size during reading " severity failure;  
    
    -- store the size of a beat
    beat_size := AxSize2Byte(master2slave . M_AXI_AWSIZE);
    xfer_size := to_integer( write_len * beat_size);
    print("AXI4 BFM: Write " & str(xfer_size) & " bytes to 0x" & hstr(master2slave . M_AXI_AWADDR) );
    
    -- Let's say we have read the address
    slave2master . M_AXI_AWREADY <= '0'; 
    
    loop
        -- Wait until the master has presented the data
        --wait until rising_edge(clk) and master2slave . M_AXI_WVALID = '1';
        WaitOnWithTimeout(clk, master2slave . M_AXI_WVALID, '1', "M_AXI_WVALID");
        slave2master . M_AXI_AWREADY <= '0'; 

        -- Wait one cycle as the WVALID has to be asserted one cycle after M_AXI_WVALID (cf A3-39)
        if burst_started = '1' then 
            for index in 4 downto 1 loop
                data_content( 9-2*index to 9-2*index+1 ) := GetHexFromStrobe(index, master2slave . M_AXI_WDATA, master2slave . M_AXI_WSTRB);
            end loop;
            
            -- Transfer occurs when both M_AXI_WREADY and M_AXI_WVALID are asserted (cf A3.2.1 )
            if slave2master . M_AXI_WREADY = '1' and master2slave . M_AXI_WVALID = '1' then
                print("AXI4 BFM: 0x" & hstr(write_addr) & "=>" & data_content );

            -- Store the written data if there is still room in the memory area
                if memory_idx <  memory'right then
                    memory(memory_idx).addr := write_addr;
                    memory(memory_idx).data := master2slave . M_AXI_WDATA;
                    memory(memory_idx).strobe := master2slave . M_AXI_WSTRB;
                    memory_idx := memory_idx + 1;
                end if;

                write_addr := std_logic_vector(unsigned(write_addr) + beat_size); -- FIXME this is the word size not beat size
                xfered_size := xfered_size + 4;
                assert xfered_size <= xfer_size report  "Too many byte have been transferred" severity FAILURE;
            end if;

            -- let's do a pause after 20 bytes
            if xfered_size = 20   and not pause_done then
                slave2master . M_AXI_WREADY <= '0'; 
                pause_done := true;
            else
                -- Let's say we have read the data
                slave2master . M_AXI_WREADY <= '1'; 
            end if;
            
            if master2slave . M_AXI_WLAST = '1' then
                print("AXI4 BFM: End of write burst" );
                assert xfer_size = xfered_size report  "Premature ending of write burst" severity FAILURE;
                -- Inform we have accepted the data
                slave2master . M_AXI_BVALID <= '1';
                slave2master . M_AXI_BRESP <= "00";     
                
                WaitOnWithTimeout(clk, master2slave . M_AXI_BREADY, '1', "M_AXI_BREADY");


                exit;
            else
                assert xfered_size < xfer_size report  "Expecting M_AXI_WLAST at the end of the transfert " severity FAILURE;

            end if;
        else
            burst_started := '1';
            slave2master . M_AXI_WREADY <= '1'; 
        end if;
        
        
    end loop;   
    slave2master . M_AXI_BVALID <= '0';    
    --print("***************************Leaving Write slave");    
end procedure;

--procedure Axi4SlaveAcceptAnyWrite( 
        --signal clk : std_logic; 
        --signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t) is
--
    --variable memory : memory_t(0 downto 0);
--
--
--begin
       ----Axi4SlaveAcceptAnyWrite(clk, master2slave, slave2master, memory, null);
--end procedure;

procedure GetMemoryPattern( 
    prev_data : in unsigned; 
    next_data : out unsigned; 
    addr : std_logic_vector;  
    data_type : axi4read_data_type;
    pattern : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0)) is

begin
        case (data_type) is
            when PATTERN_DECREASING => 
                -- Mark with 'F' the upper half-byte, then the decreasing value,
                -- The lower half word, has no the 'F' to be able to distinguish both half
                pattern := "1111" & std_logic_vector(prev_data(11 downto 0)) & "0000" & std_logic_vector(prev_data(11 downto 0));
                next_data := prev_data -1;
                
            when PATTERN_INCREASING =>
                pattern := "1111" & std_logic_vector(prev_data(11 downto 0)) & "0000" & std_logic_vector(prev_data(11 downto 0));
                next_data := prev_data +1;
           
            when PATTERN_STATIC =>
                pattern := x"ACCE55ED";
                
            when PATTERN_RANDOM =>
                pattern :=   GetPatternFromAddr( addr,  prev_data );
                next_data := prev_data + 4;
                
            when PATTERN_MIRROR =>
                pattern :=  std_logic_vector(unsigned(addr) +  prev_data);                                                
                next_data := prev_data + 4;
            when others =>
                null;
                
        end case;

    end procedure;
        
procedure Axi4SlaveAcceptAnyRead( signal clk : std_logic; 
    signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t;
    data_type : axi4read_data_type ) is   

    variable read_len   : unsigned(master2slave . M_AXI_ARLEN'range);
    variable read_size  : unsigned(master2slave . M_AXI_ARLEN'range);
    variable beat_size  : unsigned(master2slave . M_AXI_ARBURST'range);
    variable read_addr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    variable current_addr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    variable xfer_size  : integer;
    
    variable read_addr_current  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    variable read_addr_align_4KB  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);

begin    
    --print("***************************Entering Read slave");
    -- By default ready to read any address
    slave2master . M_AXI_ARREADY <= '1';  
    --slave2master . M_AXI_ARREADY <= '0';  
    
    -- Still no valid data to be read on the bus
    slave2master . M_AXI_RVALID <= '0';
    
    -- And obviously not the last to be read
    --slave2master . M_AXI_RLAST <= '0';     

    assert master2slave . M_AXI_ARCACHE = "0011" report "Memory cache others than Normal Non-cacheable Bufferable, is not supported by DRAM controller. Description in A4-65";
    assert master2slave . M_AXI_ARPROT = "000" report "Memory access permissions others than unprivileged/secure/data, is not supported by DRAM controller. Description in A4-71";
    
    -- Waiting for the master presenting the address it wants to read from
    wait until rising_edge(clk) and master2slave . M_AXI_ARVALID = '1';
    read_addr := master2slave . M_AXI_ARADDR;
    
    -- Store how many beat the master intends to read
    read_len := unsigned(master2slave . M_AXI_ARLEN) + 1;
    read_size := (others => '0');

    -- store the size of a beat
    beat_size := unsigned(master2slave . M_AXI_ARBURST);
    xfer_size := to_integer( read_len * AxSize2Byte(master2slave . M_AXI_ARSIZE));
    print("AXI4 BFM: Read " & str(xfer_size) & " bytes from 0x" & hstr(master2slave . M_AXI_ARADDR) );
    
    -- Let's say we have read the address
    -- Let's say we are not ready to read one new address during one cycle
    slave2master . M_AXI_ARREADY <= '0';  
    slave2master . M_AXI_RLAST <= '0';     
    
   -- And we present the requested data
    loop
        
        slave2master . M_AXI_RVALID <= '1';
        slave2master . M_AXI_RRESP <= "00";  
        case (data_type) is
            when PATTERN_DECREASING => 
                -- Mark with 'F' the upper half-byte, then the decreasing value,
                -- The lower half word, has no the 'F' to be able to distinguish both half
                slave2master . M_AXI_RDATA <= "1111" & std_logic_vector(sent_data(11 downto 0)) & "0000" & std_logic_vector(sent_data(11 downto 0));
                sent_data := sent_data -1;
                
            when PATTERN_INCREASING =>
                slave2master . M_AXI_RDATA <= "1111" & std_logic_vector(sent_data(11 downto 0)) & "0000" & std_logic_vector(sent_data(11 downto 0));
                sent_data := sent_data +1;
           
            when PATTERN_STATIC =>
                slave2master . M_AXI_RDATA <= x"ACCE55ED";
                
            when PATTERN_RANDOM =>
                slave2master . M_AXI_RDATA <=   GetPatternFromAddr( read_addr,  read_size );
                
            when PATTERN_MIRROR =>
                slave2master . M_AXI_RDATA <=  std_logic_vector(unsigned(read_addr) +  read_size);                                                

            when PATTERN_FILE =>
                slave2master . M_AXI_RDATA <=  memory_tb(to_integer(sent_data));
                sent_data := sent_data +1;

            when others =>
                null;
                
        end case;
        
        -- Are we sending the last word ?
        if read_len = 1 then
            slave2master . M_AXI_RLAST <= '1';
        end if;
        
        -- Wait until the master has accepted the data
        wait until rising_edge(clk) and master2slave . M_AXI_RREADY = '1';
        
        -- During the data transfer we don't accept new address
        -- During the data transfer We are able to accept new address one
        --cycle after
        slave2master . M_AXI_ARREADY <= '1';

        -- One word less
        read_len := read_len -1;
        read_size := read_size + 4;

        -- Checking if the master is not crossing a 4KB page during reading
        assert (read_addr + std_logic_vector(read_size) ) <=  (read_addr(read_addr'left downto 12)) + '1' & x"000" report  "Master request has crossed a 4KB boundary page" severity failure;
        
        -- Complete when all the requested data has been sent
        exit when read_len = 0;
    end loop;   
    
    slave2master . M_AXI_RVALID <= '0';
    slave2master . M_AXI_RRESP <= "UU";  
    --slave2master . M_AXI_RLAST  <= '0';
    
    --print("***************************Leaving Read slave");
    
end ;

procedure Axi4SlaveAcceptReadFrom( 
                signal clk : std_logic; 
                signal  master2slave: in axi4master2slave_bus_t; signal  slave2master: out axi4slave2master_bus_t; 
                data_type : axi4read_data_type;
                start_address : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0); size  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0) 
                                ) is   
begin
    null;
end ;
procedure AxiLITEMasterWrite( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data : in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0)
            ) is
    variable strobe : std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0) := (others => '1');
begin
    
    AxiLITEMasterWrite(clk, master2slave, slave2master, address, data, strobe  );
end;
procedure AxiLITEMasterInit( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t
            ) is
begin
    -- Invalid values on the address bus not supposed to be read from
    master2slave . S_AXI_AWADDR     <= (others => 'U');    
    master2slave . S_AXI_AWPROT     <= (others => 'U');  
    -- No address
    master2slave . S_AXI_AWVALID    <= '0';   
    -- Invalid values on the address bus not supposed to be used for writing
    master2slave . S_AXI_WDATA      <= (others => 'U');   
    master2slave . S_AXI_WSTRB      <= (others => 'U') ;
    -- No valid write
    master2slave . S_AXI_WVALID     <= '0';   
    master2slave . S_AXI_BREADY     <= '0';
    -- Invalid values on the address bus not supposed to be read from
    master2slave . S_AXI_ARADDR     <= (others => 'U');   
    master2slave . S_AXI_ARPROT     <= (others => 'U');  
    -- No valid address
    master2slave . S_AXI_ARVALID    <= '0'; 
    master2slave . S_AXI_RREADY      <= '0';
    end;
    
    
procedure Axi4SlaveReadInit( 
            signal clk : std_logic; 
            signal  slave2master: out axi4slave2master_bus_t
            ) is
begin
    
    slave2master . M_AXI_AWREADY    <= 'Z';    
    slave2master . M_AXI_WREADY     <= 'Z';  

    slave2master . M_AXI_BID        <= (others => 'Z');   
    
    slave2master . M_AXI_BRESP      <= (others => 'Z');   
    slave2master . M_AXI_BUSER      <= (others => 'Z') ;
   
    slave2master . M_AXI_BVALID      <= 'Z';    

    -- Signal to be driver by Reader slave
    --  to workaround limitation about record, the Writer slave
    --  has to drive it 'Z'
    slave2master . M_AXI_ARREADY     <= '0';
    
    slave2master . M_AXI_RID     <= (others => '0');   
    slave2master . M_AXI_RDATA     <= (others => 'U');  
    
    slave2master . M_AXI_RRESP      <= (others => 'U'); 
    slave2master . M_AXI_RLAST      <= '0';
    slave2master . M_AXI_RUSER      <= (others => '0');
    slave2master . M_AXI_RVALID     <= '0';
end;
    
procedure Axi4SlaveWriteInit( 
            signal clk : std_logic; 
            signal  slave2master: out axi4slave2master_bus_t
            ) is
begin
    
    slave2master . M_AXI_AWREADY    <= 'U';    
    slave2master . M_AXI_WREADY     <= 'U';  

    slave2master . M_AXI_BID        <= (others => 'U');   
    
    slave2master . M_AXI_BRESP      <= (others => 'U');   
    slave2master . M_AXI_BUSER      <= (others => 'U') ;
   
    slave2master . M_AXI_BVALID      <= '0';    

    -- Signal to be driver by Reader slave
    --  to workaround limitation about record, the Writer slave
    --  has to drive it 'Z'
    slave2master . M_AXI_ARREADY     <= 'Z';
    
    slave2master . M_AXI_RID     <= (others => 'Z');   
    slave2master . M_AXI_RDATA     <= (others => 'Z');  
    
    slave2master . M_AXI_RRESP      <= (others => 'Z'); 
    slave2master . M_AXI_RLAST      <= 'Z';
    slave2master . M_AXI_RUSER      <= (others => 'Z');
    slave2master . M_AXI_RVALID     <= 'Z';
end;


procedure AxiLITEMasterWrite( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data : in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
                    strobe : in std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0)
            ) is
begin
    -- no valid write address on the bus
    master2slave . S_AXI_AWVALID<='0';
    -- no valid data on the bus
    master2slave . S_AXI_WVALID<='0';
    -- no able to accept the write response
    master2slave . S_AXI_BREADY<='0';
    
    -- Present the address to be written on the bus
    master2slave . S_AXI_AWADDR     <= address;
    master2slave . S_AXI_AWVALID    <= '1';
    
    -- waiting for the address being accepted by the slave
    wait until rising_edge(clk) and slave2master . S_AXI_AWREADY = '1';
    
    -- Address not valid anymore
    master2slave . S_AXI_AWVALID <= '0';
    
    -- Present the data on the bus
    master2slave . S_AXI_WDATA  <= data;
    master2slave . S_AXI_WVALID <= '1';
    master2slave . S_AXI_WSTRB  <= strobe;
    
    -- Waiting for the data being accepted
    wait until rising_edge(clk) and slave2master . S_AXI_WREADY = '1';

    -- Data not valid anymore
    master2slave . S_AXI_WVALID <= '0';
    
    -- Waiting for the slave saying if the write was ok
    wait until rising_edge(clk) and slave2master . S_AXI_BVALID = '1';
    
    -- Accept the response from the slave
    master2slave . S_AXI_BREADY <= '1';
    if  slave2master . S_AXI_BRESP /= "00" then
        print("AXI4 BFM: ERROR: unable to write data 0x" & hstr(data) & "@0x" & hstr(address) );
    else
        print("AXI4 BFM: Wrote data 0x" & hstr(data) & " @ 0x" & hstr(address) );
    end if;
    -- wait one more cycle to allow the slave to get our accept
    wait until rising_edge(clk);

    
    master2slave . S_AXI_WVALID <= '0';
    master2slave . S_AXI_WDATA  <= (others => 'U');
    master2slave . S_AXI_AWADDR <= (others => 'U');
    master2slave . S_AXI_BREADY <= '0';
    
    wait for 1 ns;

    --BValid should be lowered (bug already found in client)
    assert slave2master . S_AXI_BVALID = '0' report  "BVALID is still asserted, which is not expected one cycle after deassert of BREADY " severity FAILURE;

end;

procedure AxiLITEMasterRead( 
            signal clk : std_logic; 
            signal  master2slave: out AxiLITEmaster2slave_bus_t;
            signal  slave2master: in  AxiLITEslave2master_bus_t;
                    address     : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
                    data        : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0)
            ) is
                variable bus_data :  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
begin
    -- no valid read address on the bus
    master2slave . S_AXI_ARVALID<='0';
    -- no able to accept the read response
    master2slave . S_AXI_RREADY<='0';
    
    -- Present the address to be written on the bus
    master2slave . S_AXI_ARADDR     <= address;
    master2slave . S_AXI_ARVALID    <= '1';

    -- One call out of 2, we say we are ready to get the data immediately, without waiting for its presentation, to test
    --  correct behaviour of the slave
    if TicToc= true then

        -- Accept the data and the response from the slave
        master2slave . S_AXI_RREADY <= '1';  

    end if;
    
    -- waiting for the address being accepted by the slave
    wait until rising_edge(clk) and slave2master . S_AXI_ARREADY = '1';
    
    -- Address not valid anymore
    master2slave . S_AXI_ARVALID <= '0';

    -- If we have already say we are ready to accept the addres, maybe the slave has presented it
    if TicToc= true and  slave2master . S_AXI_RVALID = '1' then

    else
        -- Accept the data and the response from the slave
        master2slave . S_AXI_RREADY <= '1';  
      
        -- Waiting for the data being present on the bus
        wait until rising_edge(clk) and slave2master . S_AXI_RVALID = '1'; 

    end if;
   

    -- When both S_AXI_RREADY and S_AXI_ARVALID are asserted, we have to sample the data
    bus_data := slave2master . S_AXI_RDATA;
    print("AXI4 BFM: Read data  0x" & hstr(bus_data) & " @ 0x" & hstr(address) );

    if  slave2master . S_AXI_RRESP /= "00" then
        print("AXI4 BFM: ERROR: Slave has reported an error while reading data from 0x" & hstr(bus_data) & " @ 0x" & hstr(address) );
        data := (others => 'U');
    else
        data := bus_data;
    end if;

    -- wait one more cycle to allow the slave to get our accept
    wait until rising_edge(clk);

    -- Maintain RREADY until the slave deassert S_AXI_RVALID, 
    --  it is the observed hardware behaviour
    --  The slave is suppose to do it immediately
    assert slave2master . S_AXI_RVALID = '0' report "############ Slave tries to transfer more than one word during a single request" severity FAILURE;
    
    master2slave . S_AXI_ARVALID <= '0';
    master2slave . S_AXI_ARADDR <= (others => 'U');
    master2slave . S_AXI_RREADY <= '0';

    TicToc := not TicToc;

end;            
end axi4bfm_pkg;
