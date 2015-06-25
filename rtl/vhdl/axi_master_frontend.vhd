---------------------------------------------------------------------------------
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
-- Create Date: 08.10.2014 
-- Author:  arnaud.samama@thalesgroup.com
-- Design Name: 
-- Module Name: axi_master_frontend.vhd - Behavioral
-- Project Name: SANDRA
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
-------------------------------------------------------------------------------
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


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
--use ieee.std_logic_arith.all;

entity axi_master_frontend is 
    generic(
               C_M_AXI_THREAD_ID_WIDTH : integer := 1;
               C_M_AXI_ADDR_WIDTH      : integer := 32;
               C_M_AXI_DATA_WIDTH      : integer := 32;
               C_M_AXI_AWUSER_WIDTH    : integer := 1;
               C_M_AXI_ARUSER_WIDTH    : integer := 1;
               C_M_AXI_WUSER_WIDTH     : integer := 1;
               C_M_AXI_RUSER_WIDTH     : integer := 1;
               C_M_AXI_BUSER_WIDTH     : integer := 1;

               C_S_AXI_ADDR_WIDTH   : integer := 32;
               C_S_AXI_DATA_WIDTH   : integer := 32;

               C_AXI_LOCK_WIDTH        : integer := 1

           );
    port ( 
             clk : in STD_LOGIC;
             rst_n : in STD_LOGIC;

             -- ======================================================================
             -- User interface
             -- ======================================================================


             -- Address target of the transfer
             xfer_addr_i : in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
             -- Size of the data to be transferred expressed in C_S_AXI_ADDR_WIDTH bits
             xfer_size_i : in unsigned(8-1 downto 0);
             -- Must be asserted during one-cycle when the address and size is valid
             xfer_addr_valid_i : in std_logic;
             -- Do we need to signal the end of the transfer
             xfer_completion_request_i : in std_logic;


             -- Data to be transfered
             xfer_data_i  : in std_logic_vector(C_S_AXI_DATA_WIDTH -1 downto 0);
             -- Must be asserted one cycle, each time xfer_data_i is valid
             xfer_data_valid_i : in std_logic;

             full_o : out std_logic;
             almost_full_o : out std_logic;
             completed_o : out std_logic;

             -- ======================================================================
             -- AXI Master Interface
             -- ======================================================================
             -- Master Interface Write Address
             M_AXI_AWID    : out std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
             M_AXI_AWADDR  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
             M_AXI_AWLEN   : out std_logic_vector(8-1 downto 0);
             M_AXI_AWSIZE  : out std_logic_vector(3-1 downto 0);
             M_AXI_AWBURST : out std_logic_vector(2-1 downto 0);
             M_AXI_AWLOCK  : out std_logic;
             M_AXI_AWCACHE : out std_logic_vector(4-1 downto 0);
             M_AXI_AWPROT  : out std_logic_vector(3-1 downto 0);
             -- AXI3    M_AXI_AWREGION:out std_logic_vector(4-1 downto 0);
             M_AXI_AWQOS   : out std_logic_vector(4-1 downto 0);
             M_AXI_AWUSER  : out std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
             M_AXI_AWVALID : out std_logic;
             M_AXI_AWREADY : in  std_logic;

             -- Master Interface Write Data
             -- AXI3   M_AXI_WID(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
             M_AXI_WDATA  : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
             M_AXI_WSTRB  : out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
             M_AXI_WLAST  : out std_logic;
             M_AXI_WUSER  : out std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
             M_AXI_WVALID : out std_logic;
             M_AXI_WREADY : in  std_logic;

             -- Master Interface Write Response
             M_AXI_BID    : in  std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
             M_AXI_BRESP  : in  std_logic_vector(2-1 downto 0);
             M_AXI_BUSER  : in  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
             M_AXI_BVALID : in  std_logic;
             M_AXI_BREADY : out std_logic;

             -- Master Interface Read Address
             M_AXI_ARID    : out std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
             M_AXI_ARADDR  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
             M_AXI_ARLEN   : out std_logic_vector(8-1 downto 0);
             M_AXI_ARSIZE  : out std_logic_vector(3-1 downto 0);
             M_AXI_ARBURST : out std_logic_vector(2-1 downto 0);
             M_AXI_ARLOCK  : out std_logic;
             M_AXI_ARCACHE : out std_logic_vector(4-1 downto 0);
             M_AXI_ARPROT  : out std_logic_vector(3-1 downto 0);
             -- AXI3   M_AXI_ARREGION:out std_logic_vector(4-1 downto 0);
             M_AXI_ARQOS   : out std_logic_vector(4-1 downto 0);
             M_AXI_ARUSER  : out std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
             M_AXI_ARVALID : out std_logic;
             M_AXI_ARREADY : in  std_logic;

             -- Master Interface Read Data 
             M_AXI_RID    : in  std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
             M_AXI_RDATA  : in  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
             M_AXI_RRESP  : in  std_logic_vector(2-1 downto 0);
             M_AXI_RLAST  : in  std_logic;
             M_AXI_RUSER  : in  std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
             M_AXI_RVALID : in  std_logic;
             M_AXI_RREADY : out std_logic


         );

end entity;

architecture Behavioral of axi_master_frontend is

constant XFER_SIDEBAND_BITSIZE : integer := 8;
	
-- AXI Writing state machine
type writer_state_t is (

    -- Waiting for data in the FIFO
	WRITER_XFER_IDLE,

    -- Preparint the transaction by presenting addr/size on the bus
	WRITER_XFER_INIT,

    -- Waiting for the slave having accepted the address
	WRITER_WAIT_ACK_ADDR,

    -- Transferring data to the slave
	WRITER_XFER_DATA,

    -- Waiting for the result of the transaction
	WRITER_XFER_END
);
signal writer_state : writer_state_t;



-- Address of the next transaction
signal user_to_axi_xfer_addr : unsigned(C_M_AXI_ADDR_WIDTH-1 downto 0);
-- Size of the next transaction
signal user_to_axi_xfer_size : unsigned(xfer_size_i'range);
-- Do we need to signale the end of the transfer
signal user_to_axi_completion_request : std_logic;

-- Address of the next transaction
signal xfer_addr : unsigned(C_M_AXI_ADDR_WIDTH-1 downto 0);
-- Size of the next transaction
signal xfer_size : unsigned(xfer_size_i'range);

signal xfer_completion_request: std_logic;




signal    rst : std_logic;

-- Interface to the data FIFO
signal    user_to_axi_din 			:  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal    user_to_axi_wr_en 		:  std_logic;
signal    user_to_axi_rd_en 		:  std_logic;
signal    user_to_axi_dout 			:  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal    user_to_axi_full 			:  std_logic;
signal    user_to_axi_almost_full 	:  std_logic;
signal    user_to_axi_empty 		:  std_logic;
signal    user_to_axi_valid         :  std_logic;
signal    user_to_axi_almost_empty :  std_logic;

signal    user_to_axi_dout_last 	:  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal    user_to_axi_valid_last    :  std_logic;

-- Interface to the addr FIFO
signal    user_to_axi_addr_din 			:  std_logic_vector(C_M_AXI_DATA_WIDTH-1 + 8 + XFER_SIDEBAND_BITSIZE  downto 0);
signal    user_to_axi_addr_wr_en 		:  std_logic;
signal    user_to_axi_addr_rd_en 		:  std_logic;
signal    user_to_axi_addr_dout 		:  std_logic_vector(C_M_AXI_DATA_WIDTH-1 + 8 + XFER_SIDEBAND_BITSIZE downto 0);
signal    user_to_axi_addr_full 		:  std_logic;
signal    user_to_axi_addr_almost_full	:  std_logic;
signal    user_to_axi_addr_empty 		:  std_logic;
signal    user_to_axi_addr_valid        :  std_logic;


-- Mirror AXI bus signals when used in internal logic
signal    axi_awlen     :  unsigned(M_AXI_AWLEN'range);
signal    axi_wvalid    :  std_logic;
signal    axi_awsize    :  std_logic_vector(M_AXI_AWSIZE'range);
signal    axi_bready    :  std_logic;
signal    axi_wlast     :  std_logic;
signal    axi_wready_q0 :  std_logic;


-- Contains the status of the error of the last transaction
signal  axi_last_write_error : std_logic_vector(M_AXI_BRESP'range);

-- Number of beat poped out from the FIFO
signal fifo_pop_data_count : unsigned(M_AXI_AWLEN'range);
-- Request to pop data from the FIFO
signal fifo_pop_data : std_logic;
-- Reset the FIFO data counter
signal fifo_pop_reset : std_logic;

-- Define we are doing a single burst to transfer data to the master
signal processing_burst : std_logic := '0';

-- Define if a transfer to a slave is being processed, eventually made of several burst
signal processing_xfer : std_logic := '0';

-- Count how many beat we have already pusblish on the AXI bus for the current transaction
signal written_beat : unsigned(xfer_size'range);

signal dbg_in_axi_xfer : std_logic;

attribute mark_debug : string;
attribute mark_debug of dbg_in_axi_xfer : signal is "true";


-- Data FIFO between the user and the logic
COMPONENT axi_data_fifo
  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    full : OUT STD_LOGIC;
    almost_full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    almost_empty : OUT STD_LOGIC;
    valid : OUT STD_LOGIC
  );
end component;

COMPONENT axi_addr_fifo
  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(47 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(47 DOWNTO 0);
    full : OUT STD_LOGIC;
    almost_full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    valid : OUT STD_LOGIC
  );
END COMPONENT;

-- function called clogb2 that returns an integer which has the
--value of the ceiling of the log base 2

function clogb2 (bit_depth : integer) return integer is            
    variable depth  : integer := bit_depth;                               
    variable count  : integer := 1;                                       
begin                                                                   
    for clogb2 in 1 to bit_depth loop  -- Works for up to 32 bit integers
        if (bit_depth <= 2) then                                           
            count := 1;                                                      
        else                                                               
            if(depth <= 1) then                                              
                        count := count;                                                
        else                                                             
            depth := depth / 2;                                            
            count := count + 1;                                            
        end if;                                                          
    end if;                                                            
end loop;                                                             
return(count);        	                                              
end;                                                                    


begin

    user_to_axi_data : axi_data_fifo
    PORT MAP (
                 clk             => clk,
                 rst             => rst,
                 din             => user_to_axi_din,
                 wr_en           => user_to_axi_wr_en,
                 rd_en           => user_to_axi_rd_en,
                 dout            => user_to_axi_dout,
                 full            => user_to_axi_full,
                 almost_full     => user_to_axi_almost_full,
                 empty           => user_to_axi_empty,
                 almost_empty    => user_to_axi_almost_empty,
                 valid           => user_to_axi_valid
             );

    user_to_axi_addr : axi_addr_fifo
    PORT MAP (
                 clk            => clk,
                 rst            => rst,
                 din            => user_to_axi_addr_din,
                 wr_en          => user_to_axi_addr_wr_en,
                 rd_en          => user_to_axi_addr_rd_en,
                 dout           => user_to_axi_addr_dout,
                 full           => user_to_axi_addr_full,
                 almost_full    => user_to_axi_addr_almost_full,
                 empty          => user_to_axi_addr_empty,
                 valid          => user_to_axi_addr_valid
             );

-- Positive reset for the FIFO
rst <= not rst_n;

-- We are always ready, except if one of the two FIFO is full
full_o          <=  user_to_axi_full        or user_to_axi_addr_full;

-- Make easier the anticipation on the client side
almost_full_o   <=  user_to_axi_almost_full or user_to_axi_addr_almost_full;


M_AXI_AWID      <= (others => '0');
M_AXI_WSTRB     <= (others => '1');
M_AXI_AWBURST   <= "01";
M_AXI_AWLOCK    <= '0';
M_AXI_AWCACHE   <= "0011";
M_AXI_AWPROT    <= "000";
M_AXI_AWQOS     <= x"0";
M_AXI_AWUSER    <= (others => '1');
M_AXI_WSTRB     <= (others => '1');
M_AXI_WUSER     <= (others => '0');
M_AXI_ARID      <= (others => '0');
M_AXI_ARBURST   <= "01";
M_AXI_ARLOCK    <= '0';
M_AXI_ARCACHE   <= "0011";
M_AXI_ARPROT    <= "000";
M_AXI_ARQOS     <= x"0";
M_AXI_ARUSER    <= (others => '1');


-- Data are directly pushed into the FIFO according to the user validity 
user_to_axi_wr_en   <= '1' when xfer_data_valid_i = '1' else '0';
user_to_axi_din     <= xfer_data_i;

-- We store the address and the size to be transfered within the same FIFO line
user_to_axi_addr_din    <= std_logic_vector(xfer_size_i) & xfer_addr_i & xfer_completion_request_i & "0000000"; -- 7 spare bits
user_to_axi_addr_wr_en  <= xfer_addr_valid_i;

-- Separate address from size for a specific transfer request
user_to_axi_xfer_addr <= unsigned(user_to_axi_addr_dout(C_M_AXI_ADDR_WIDTH-1 + XFER_SIDEBAND_BITSIZE     downto XFER_SIDEBAND_BITSIZE));
user_to_axi_xfer_size <= unsigned(user_to_axi_addr_dout(C_M_AXI_ADDR_WIDTH-1 + 8 + XFER_SIDEBAND_BITSIZE downto C_M_AXI_ADDR_WIDTH + XFER_SIDEBAND_BITSIZE));
user_to_axi_completion_request <= user_to_axi_addr_dout(7);

-- Mirror out signal also used for internal logic
M_AXI_WVALID    <= axi_wvalid;
M_AXI_BREADY    <= axi_bready;
M_AXI_AWLEN     <= std_logic_vector(axi_awlen);
M_AXI_AWSIZE	<= axi_awsize;
M_AXI_WLAST     <= axi_wlast;


-- Size of one beat is determined from the size of the bus
axi_awsize <=  std_logic_vector(to_unsigned(clogb2((C_M_AXI_DATA_WIDTH/8)-1),3 ) );



--=================================================================
--
-- Handle the publication of the written data on the AXI bus
--
--=================================================================
axi_wdata_mngr: process(clk) 
begin
    if rising_edge(clk) then
        
        -- Are we transfering data ?
        if processing_burst='1' then

            -- Keep track of the slave historic
            axi_wready_q0 <= M_AXI_WREADY;

            -- If this is the first time we present the data on the bus
            if axi_wvalid = '0' then 

                -- Data is presented on the bus according to the validity
                --  reported by the FIFO (when the data is valid, we publish it)
                axi_wvalid     <= user_to_axi_valid;
                M_AXI_WDATA    <= user_to_axi_dout;


            -- The slave is ready but wasn't, if we have stored a data beacuse the
            --  slave did a pause, we restore it from the backup
            elsif M_AXI_WREADY = '1' and axi_wready_q0 = '0' then 

                M_AXI_WDATA    <= user_to_axi_dout_last;
                axi_wvalid     <= user_to_axi_valid_last;

            -- Otherwise, when the slave is ready for a while, we just present the data
            --  according to its validity
            elsif M_AXI_WREADY = '1' then 

                -- When the slave is ready we publish the data to the pace of the
                --  FIFO
                axi_wvalid     <= user_to_axi_valid;
                M_AXI_WDATA    <= user_to_axi_dout;

            -- The slave is not ready, but it was, so we may have already extracted a data from the FIFO
            --  we have to save it, in order to restore it when it will become ready
            elsif axi_wready_q0 = '1' and M_AXI_WREADY = '0' then

                user_to_axi_dout_last   <= user_to_axi_dout;
                user_to_axi_valid_last  <= user_to_axi_valid;

            end if;

           -- We maintain the counter of written data, each time we _publish_ it
           -- (i.e. even if the slave has not already accepted it)
           if user_to_axi_valid = '1' then
                written_beat <= written_beat + 1;
            end if;
            
       -- When not publishing we maintain data on reset
       else 
           axi_wvalid               <= '0';
           M_AXI_WDATA              <= (others => 'U');
           written_beat             <= (others => '0');
           user_to_axi_valid_last   <= '0';
           user_to_axi_dout_last    <= (others => 'U');
       end if;
        
    end if;
end process;


--=================================================================
--
-- Handle the WLAST generation
--
--=================================================================
axi_wlast_mngr: process(clk) 
begin
    if rising_edge(clk) then
        

        -- Maintain WLAST while the target has not accepted the transfer
        if axi_wlast = '1' and M_AXI_WREADY = '0' then
            axi_wlast   <= '1';
        else
            -- By default, this is not the last word
            axi_wlast   <= '0';
        end if ;

        -- If we are transferring data ...
        if processing_burst='1' then

            -- ... and the next beat will be the last and there is a data ready to be published
            --  then we inform the slave it will be the last word.
            -- NB: axi_awlen is the size to be transferred minus 1, it's why comparing
            --  written_beat = axi_awlen means we are on the penultimate beat
            if written_beat = axi_awlen  and user_to_axi_valid = '1' then 
                axi_wlast     <= '1';
            end if;
        end if;
    end if;
    
end process;

--=================================================================
--
-- Handle the BREADY generation
--
--=================================================================
axi_bready_mngr: process(clk, rst_n)
begin

    if rst_n = '0' then

        axi_bready <= '0';

    elsif rising_edge(clk) then

        -- We confirm the reception of the status...
        if M_AXI_BVALID = '1' and axi_bready='0' then

            axi_bready <= '1';

            -- Keep track if the last status
            -- TODO: report it to the user
            axi_last_write_error <= M_AXI_BRESP;

        -- ... during one cycle
        elsif axi_bready='1' then

            axi_bready <= '0';

        end if;

    end if;

end process;

-- When no data has been generated on the bus, we have to pop out the first one, else
--  but during transfer we pop the data if we are not at the end, and the slave is ready else we stop popping out.
user_to_axi_rd_en <= fifo_pop_data when fifo_pop_data_count = (fifo_pop_data_count'range => '0')  else 
                     fifo_pop_data when fifo_pop_data_count -1 /= axi_awlen and M_AXI_WREADY = '1' else '0';

--=================================================================
--
-- Handle FIFO extracted words counter
--
--=================================================================
process(clk, rst_n)
begin

    if rst_n = '0' then

        fifo_pop_data_count <= (others => '0');

    elsif rising_edge(clk) then

        -- Are we started a new transaction ?
        if fifo_pop_reset = '1' then

            -- Reset the counter of pop words
            fifo_pop_data_count <= (others => '0');

            -- If while reseting we are asked to pop a word
            --  we immediately count it
            if fifo_pop_data = '1' then 

                fifo_pop_data_count(0) <= '1';

            end if;

        -- We count the word, each time we extract one
        elsif user_to_axi_rd_en = '1' then

            fifo_pop_data_count <= fifo_pop_data_count + 1;

        end if;
        
    end if;

end process;


--=================================================================
--
-- Pop out the address from the FIFO when the beginning of a message
--  is available
--
--=================================================================
process(clk, rst_n)
begin

    if rst_n = '0' then

        user_to_axi_addr_rd_en <= '0';

    elsif rising_edge(clk) then

        -- We need to have data in both FIFO to start the transfer if no transfer is running
        --  and we have not just started a new transfer and we have not already a valid address on the bus
        if user_to_axi_addr_empty = '0' and user_to_axi_empty = '0' and processing_xfer='0' and user_to_axi_addr_rd_en = '0' and user_to_axi_addr_valid = '0' then
            user_to_axi_addr_rd_en <= '1';
        else
            user_to_axi_addr_rd_en <= '0';

        end if;
    end if;

end process;


--=================================================================
--
-- Main WRITE Channel state machine
--
--=================================================================
axi_writer: process(clk, rst_n)	

    variable xfer_addr_end      : unsigned( xfer_addr'range);
    variable xfer_size_4KB_end_byte  : unsigned( xfer_addr'range);
    variable xfer_size_4KB_end_beat  : unsigned( xfer_addr'range);
    variable xfer_size_transaction  : unsigned( xfer_size'range);

begin
    if rst_n='0' then

        processing_burst    <= '0';
        processing_xfer     <= '0';
        writer_state        <= WRITER_XFER_IDLE;
        fifo_pop_data       <= '0';
        M_AXI_AWVALID       <= '0';
        completed_o <= '0';
        dbg_in_axi_xfer     <= '0';

    elsif rising_edge(clk) then

        -- Data counter reset  and completion signaling is only a pulse
        if fifo_pop_reset = '1' then
            fifo_pop_reset <= '0';
            completed_o <= '0';
        end if;

        case (writer_state) is

            when WRITER_XFER_IDLE =>

                M_AXI_AWVALID <= '0';

                -- Latch the user address when valid
                if user_to_axi_addr_valid = '1' then

                    xfer_addr               <= unsigned(user_to_axi_xfer_addr);
                    xfer_size               <= user_to_axi_xfer_size;
                    xfer_completion_request <= user_to_axi_completion_request;

                    -- Having the address means we are ready to tranfer data
                    processing_xfer    <= '1';
                    dbg_in_axi_xfer     <= '1';
                    writer_state <= WRITER_XFER_INIT;
                else

                    -- Invalid data for debug
                    xfer_addr <= (others => 'U');
                    xfer_size <= (others => 'U');
                    processing_xfer    <= '0';
                    fifo_pop_data <= '0';
                    fifo_pop_reset <= '1';
                    dbg_in_axi_xfer     <= '0';

                    writer_state <= WRITER_XFER_IDLE;

                end if;


            when WRITER_XFER_INIT =>

                    -- Reset the data counter
                    fifo_pop_reset <= '1';

                    -- What is the end address of this transfer ?
                    xfer_addr_end := xfer_addr + xfer_size * (C_S_AXI_ADDR_WIDTH/8);

                    -- Check if the transfer will cross a 4KB page
                    if xfer_addr(xfer_addr'left  downto 12) /= xfer_addr_end(xfer_addr_end'left  downto 12) then

                        -- Compute the byte size up to the 4KB boundary
                        xfer_size_4KB_end_byte := (xfer_addr(xfer_addr'left downto 12) + 1) & x"000" - xfer_addr; 

                        -- Convert the byte size into beat size
                        xfer_size_4KB_end_beat := xfer_size_4KB_end_byte srl to_integer(unsigned(axi_awsize));

                        -- All the size are minus 1 (i.e. 1 Beat = 00, 2 beat = 01, etc.)
                        xfer_size_transaction := xfer_size_4KB_end_beat(xfer_size_transaction'range) - 1;

                    -- No 4KB page crossing
                    else
                        -- Don't care about the end address, it won't never been used
                        xfer_size_4KB_end_byte := xfer_addr;

                        -- Transfer the complete requested size
                        xfer_size_transaction := xfer_size ;
                    end if;


                    -- Update the tranfer size with the size of the content still to be transfered (maybe 0 if no cross)
                    xfer_size <= xfer_size - (xfer_size_transaction +1 );

                    -- Update the next address where to start the next transfer from
                    xfer_addr <= xfer_addr + ( (xfer_size_transaction +1) sll to_integer(unsigned(axi_awsize)) );

                     -- Present the address on the bus
                    M_AXI_AWADDR    <= std_logic_vector(xfer_addr);

                    -- Present the size of the current transaction on the bus
                    axi_awlen       <= xfer_size_transaction;

                    -- Inform the slave the address cycle is ready
                    M_AXI_AWVALID <= '1';

                     -- If the slave is already ready to get the address
                     -- Let's move immediately to the transmission phase
                    if M_AXI_AWREADY = '1' then

                        writer_state <= WRITER_XFER_DATA;

                         -- Pop the first data from the FIFO
                        fifo_pop_data <= '1';

                         -- We can start to push data on the AXI bus
                        processing_burst <= '1';

                    -- If the slave is not ready, wait for it
                    else
                        writer_state <= WRITER_WAIT_ACK_ADDR;
                    end if;


            when WRITER_WAIT_ACK_ADDR =>

                -- Has the slave accepted the address cycle ?
                if M_AXI_AWREADY = '1' then

                    writer_state <= WRITER_XFER_DATA;

                     -- Start to pop the first data from the FIFO
                    fifo_pop_data <= '1';

                     -- We can start to push data on the AXI bus
                    processing_burst <= '1';

                -- Continuing to wait for the slave
                else
                    writer_state <= WRITER_WAIT_ACK_ADDR;
                    processing_burst <= '0';
                end if; 

            when WRITER_XFER_DATA =>

                -- Address not valid anymore
                M_AXI_AWVALID <= '0';
                processing_burst <= '1';

                -- If the slave has accepted the data, we continue to pop the data
                if  M_AXI_WREADY='1' and  axi_wlast = '0' then
                    fifo_pop_data <= '1';

                -- Let's do a pause, if there is data, but the slave is not ready
                elsif axi_wlast = '0' then
                    fifo_pop_data <= '0';


                -- This was the last data, let's stop popping out the data
                else 

                    fifo_pop_data <= '0';
                    processing_burst <= '0';
                    writer_state <= WRITER_XFER_END;

                end if;


            when WRITER_XFER_END =>

                -- Wait for the completion of the transaction
                if axi_bready = '1' then

                    -- Transfer has been completed, let's reset the FIFO extraction counter
                    fifo_pop_reset <= '1';

                    -- Do we have still data to be transferred ?
                    if xfer_size /= (xfer_size'range => '1') then
                        writer_state <= WRITER_XFER_INIT;
                    else
                        writer_state <= WRITER_XFER_IDLE;

                        -- Do we need to signal the completion of the transfer ?
                        if xfer_completion_request = '1' then
                            completed_o <= '1';
                        end if;
                    end if;

                -- No status, still wait
                else
                    writer_state <= WRITER_XFER_END;
                end if;

        end case;


    end if;

end process;

end Behavioral ;



