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
-- Create Date: 12.02.2014 10:32:02 
-- Author:  arnaud.samama@thalesgroup.com
-- Design Name: 
-- Module Name: RioSerial2Axi4Bridge - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity RioSerial2Axi4Bridge is

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
        
        C_AXI_LOCK_WIDTH        : integer := 1;
        C_USE_WINDOW_BUFFER     : boolean := true;
        
        -- Registers 32 bits long
        C_S_AXI_REGS_BITSIZE : integer := 4;
        C_S_AXI_REGS_LOW_ORDER : integer := 4 + 4; -- C_S_AXI_REGS_BITSIZE + 4;
        -- C_S_AXI_BASE_ADDR : std_logic_vector(C_S_AXI_ADDR_WIDTH - 1 downto C_S_AXI_REGS_LOW_ORDER)
        C_S_AXI_BASE_ADDR : std_logic_vector(32 - 1 downto 8)
    );
    port ( 
	
		clk : in STD_LOGIC;
        rst_n : in STD_LOGIC;
        
        -- ======================================================================
        -- AXI Lite Slave Interface
        -- ======================================================================
        
        -- Slave Interface Write Address Ports
        S_AXI_AWADDR   : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_AWPROT   : in  std_logic_vector(3-1 downto 0);
        S_AXI_AWVALID  : in  std_logic;
        S_AXI_AWREADY  : out std_logic;

        -- Slave Interface Write Data Ports
        S_AXI_WDATA  : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_WSTRB  : in  std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0);
        S_AXI_WVALID : in  std_logic;
        S_AXI_WREADY : out std_logic;

        -- Slave Interface Write Response Ports
        S_AXI_BRESP  : out std_logic_vector(2-1 downto 0);
        S_AXI_BVALID : out std_logic;
        S_AXI_BREADY : in  std_logic;

        -- Slave Interface Read Address Ports
        S_AXI_ARADDR   : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_ARPROT   : in  std_logic_vector(3-1 downto 0);
        S_AXI_ARVALID  : in  std_logic;
        S_AXI_ARREADY  : out std_logic;

        -- Slave Interface Read Data Ports
        S_AXI_RDATA  : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_RRESP  : out std_logic_vector(2-1 downto 0);
        S_AXI_RVALID : out std_logic;
        S_AXI_RREADY : in  std_logic;		
        
        
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
        M_AXI_RREADY : out std_logic;

        -- Example Design only
        error : out std_logic;
        
        -- Interrupt
        intr : out std_logic;
        
        -- ======================================================================
        -- Serial RapidIO interface
        -- ======================================================================
		-- Support for portLocalAckIdCSR.
		localAckIdWrite_o : out std_logic;
		clrOutstandingAckId_o : out std_logic;
		inboundAckId_o : out std_logic_vector(4 downto 0);
		outstandingAckId_o : out std_logic_vector(4 downto 0);
		outboundAckId_o : out std_logic_vector(4 downto 0);
		inboundAckId_i : in std_logic_vector(4 downto 0);
		outstandingAckId_i : in std_logic_vector(4 downto 0);
		outboundAckId_i : in std_logic_vector(4 downto 0);
		
		-- Outbound frame interface.
		readFrameEmpty_o : out STD_LOGIC;
		readFrame_i : in STD_LOGIC;
		readFrameRestart_i : in STD_LOGIC;
		readFrameAborted_o : out STD_LOGIC;
		readWindowEmpty_o : out STD_LOGIC;
		readWindowReset_i : in STD_LOGIC;
		readWindowNext_i : in STD_LOGIC;
		readContentEmpty_o : out STD_LOGIC;
		readContent_i : in STD_LOGIC;
		readContentEnd_o : out STD_LOGIC;
		readContentData_o : out STD_LOGIC_VECTOR (31 downto 0);
		
		-- Inbound frame interface.
		writeFrameFull_o : out STD_LOGIC;
		writeFrame_i : in STD_LOGIC;
		writeFrameAbort_i : in STD_LOGIC;
		writeContent_i : in STD_LOGIC;
		writeContentData_i : in STD_LOGIC_VECTOR (31 downto 0);

        -- ======================================================================
        -- SRIO physical layer status
        -- ======================================================================
        port_initialized_i : in std_logic_vector(3 downto 0);
        Nx_mode_active_i   : in std_logic;
        mgt_pll_locked_i     : in std_logic;
        rxelecidle_i     : in std_logic_vector(3 downto 0);
        linkInitialized_i : in std_logic_vector(3 downto 0)
    
		   );
end entity;

architecture Behavioral of RioSerial2Axi4Bridge is

  attribute mark_debug : string;

  -- ======================================================================
  -- AXI Lite Slave Interface
  -- ======================================================================
  attribute mark_debug of S_AXI_AWADDR: signal is "true";
  attribute mark_debug of S_AXI_AWPROT: signal is "true";
  attribute mark_debug of S_AXI_AWVALID: signal is "true";
  attribute mark_debug of S_AXI_AWREADY: signal is "true";
  attribute mark_debug of S_AXI_WDATA: signal is "true";
  attribute mark_debug of S_AXI_WSTRB: signal is "true";
  attribute mark_debug of S_AXI_WVALID: signal is "true";
  attribute mark_debug of S_AXI_WREADY: signal is "true";
  attribute mark_debug of S_AXI_BRESP: signal is "true";
  attribute mark_debug of S_AXI_BVALID: signal is "true";
  attribute mark_debug of S_AXI_BREADY: signal is "true";
  attribute mark_debug of S_AXI_ARADDR: signal is "true";
  attribute mark_debug of S_AXI_ARPROT: signal is "true";
  attribute mark_debug of S_AXI_ARVALID: signal is "true";
  attribute mark_debug of S_AXI_ARREADY: signal is "true";
  attribute mark_debug of S_AXI_RDATA: signal is "true";
  attribute mark_debug of S_AXI_RRESP: signal is "true";
  attribute mark_debug of S_AXI_RVALID: signal is "true";
  attribute mark_debug of S_AXI_RREADY: signal is "true";


  -- ======================================================================
  -- AXI Master Interface
  -- ======================================================================
  attribute mark_debug of M_AXI_AWID    : signal is "true";
  attribute mark_debug of M_AXI_AWADDR  : signal is "true";
  attribute mark_debug of M_AXI_AWLEN   : signal is "true";
  attribute mark_debug of M_AXI_AWSIZE  : signal is "true";
  attribute mark_debug of M_AXI_AWBURST : signal is "true";
  attribute mark_debug of M_AXI_AWLOCK  : signal is "true";
  attribute mark_debug of M_AXI_AWCACHE : signal is "true";
  attribute mark_debug of M_AXI_AWPROT  : signal is "true";
        -- AXI3    M_AXI_AWREGION:out std_logic_vector(4-1 downto 0);
  attribute mark_debug of M_AXI_AWQOS   : signal is "true";
  attribute mark_debug of M_AXI_AWUSER  : signal is "true";
  attribute mark_debug of M_AXI_AWVALID : signal is "true";
  attribute mark_debug of M_AXI_AWREADY : signal is "true";

        -- Master Interface Write Data
        -- AXI3   M_AXI_WID(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
  attribute mark_debug of M_AXI_WDATA  : signal is "true";
  attribute mark_debug of M_AXI_WSTRB  : signal is "true";
  attribute mark_debug of M_AXI_WLAST  : signal is "true";
  attribute mark_debug of M_AXI_WUSER  : signal is "true";
  attribute mark_debug of M_AXI_WVALID : signal is "true";
  attribute mark_debug of M_AXI_WREADY : signal is "true";

        -- Master Interface Write Response
  attribute mark_debug of M_AXI_BID    : signal is "true";
  attribute mark_debug of M_AXI_BRESP  : signal is "true";
  attribute mark_debug of M_AXI_BUSER  : signal is "true";
  attribute mark_debug of M_AXI_BVALID : signal is "true";
  attribute mark_debug of M_AXI_BREADY : signal is "true";

        -- Master Interface Read Address
  attribute mark_debug of M_AXI_ARID    : signal is "true";
  attribute mark_debug of M_AXI_ARADDR  : signal is "true";
  attribute mark_debug of M_AXI_ARLEN   : signal is "true";
  attribute mark_debug of M_AXI_ARSIZE  : signal is "true";
  attribute mark_debug of M_AXI_ARBURST : signal is "true";
  attribute mark_debug of M_AXI_ARLOCK  : signal is "true";
  attribute mark_debug of M_AXI_ARCACHE : signal is "true";
  attribute mark_debug of M_AXI_ARPROT  : signal is "true";
        -- AXI3   M_AXI_ARREGION:out std_logic_vector(4-1 downto 0);
  attribute mark_debug of M_AXI_ARQOS   : signal is "true";
  attribute mark_debug of M_AXI_ARUSER  : signal is "true";
  attribute mark_debug of M_AXI_ARVALID : signal is "true";
  attribute mark_debug of M_AXI_ARREADY : signal is "true";

        -- Master Interface Read Data 
  attribute mark_debug of M_AXI_RID    : signal is "true";
  attribute mark_debug of M_AXI_RDATA  : signal is "true";
  attribute mark_debug of M_AXI_RRESP  : signal is "true";
  attribute mark_debug of M_AXI_RLAST  : signal is "true";
  attribute mark_debug of M_AXI_RUSER  : signal is "true";
  attribute mark_debug of M_AXI_RVALID : signal is "true";
  attribute mark_debug of M_AXI_RREADY : signal is "true";

    component Crc16CITT is
        port(
        d_i : in std_logic_vector(15 downto 0);
        crc_i : in std_logic_vector(15 downto 0);
        crc_o : out std_logic_vector(15 downto 0)
        );
    end component;
    
    component dual_input_fifo is
        port (  
            rst_n       : in std_logic;
            clk         : in std_logic;
            halfword_1  : in std_logic_vector(15 downto 0);
            halfword_2  : in std_logic_vector(15 downto 0);

            wr_type     : in std_logic_vector(1 downto 0);
            end_frame   : in std_logic;

            fullwordout : out std_logic_vector(31 downto 0);
            valid       : out std_logic;
            last_frame  : out std_logic;
            
            readword    : in std_logic

        );
    end component;
    
    COMPONENT srio_lookup_table
      PORT (
        clka : IN STD_LOGIC;
        rsta : IN STD_LOGIC;
        ena : IN STD_LOGIC;
        wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
        addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
        dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
      );
    END COMPONENT;
    ATTRIBUTE SYN_BLACK_BOX : BOOLEAN;
    ATTRIBUTE SYN_BLACK_BOX OF srio_lookup_table : COMPONENT IS TRUE;
    ATTRIBUTE BLACK_BOX_PAD_PIN : STRING;
    ATTRIBUTE BLACK_BOX_PAD_PIN OF srio_lookup_table : COMPONENT IS "clka,ena,wea[0:0],addra[15:0],dina[7:0],douta[7:0]";

constant    SIZE_ADDRESS_WIDTH 		: natural := 6;
constant    CONTENT_ADDRESS_WIDTH 	: natural := 8;
constant    CONTENT_WIDTH 			: natural := 32;
constant    MAX_PACKET_SIZE 		: natural := 69;
constant    XFER_BEAT_SIZE_BIT_HI 	: natural := 11;

--
-- Response code on the AXI bus
--
type RespCode_t is (OKAY, EXOKAY, SLVERR, DECERR);
type RespCode_array is array(RespCode_t) of std_logic_vector(1 downto 0);
constant EncodeRespCode : RespCode_array :=
(
    OKAY    => "00",
    EXOKAY  => "01",
    SLVERR  => "10",
    DECERR  => "11"
);
    
signal rst 	: STD_LOGIC;

signal readFrameEmpty 	: STD_LOGIC;
signal readFrame 			: STD_LOGIC;
signal readFrameRestart 	: STD_LOGIC;
signal readFrameAborted 	: STD_LOGIC;
signal readWindowEmpty   	: STD_LOGIC;
signal readWindowReset  	: STD_LOGIC;
signal readWindowNext   	: STD_LOGIC;
signal readContentEmptyn 	: STD_LOGIC;
signal readContent  		: STD_LOGIC;
signal readContentEmpty  		: STD_LOGIC;
signal readContentEnd 	: STD_LOGIC;
signal readContentData  	: STD_LOGIC_VECTOR (31 downto 0);
signal writeFrameFull 	 : STD_LOGIC;
signal writeFrame  		 : STD_LOGIC;
signal writeFrameAbort  	: STD_LOGIC;
signal writeContent  		: STD_LOGIC;
signal writeContentData  	: STD_LOGIC_VECTOR (31 downto 0);


signal  inboundReadFrameEmpty_o 	: std_logic;
signal  inboundReadFrame_i 			: std_logic;
signal  inboundReadFrameRestart_i 	: std_logic;
signal  inboundReadFrameAborted_o 	: std_logic;
signal  inboundReadFrameSize_o 		: std_logic_vector(CONTENT_ADDRESS_WIDTH-1 downto 0);
signal  inboundReadContentEmpty_o 	: std_logic;
signal  inboundReadContent_request 		: std_logic;
signal  inboundReadContent 		: std_logic; -- BEFORE
signal  inboundReadContentValid 		: std_logic;
signal  inboundReadContentEnd_o 	: std_logic;
signal  inboundReadContentData_o 	: std_logic_vector(CONTENT_WIDTH-1 downto 0);

signal  inboundHeaderFirst 	    : std_logic_vector(CONTENT_WIDTH-1 downto 0);
signal  inboundHeaderSecond 	: std_logic_vector(CONTENT_WIDTH-1 downto 0);
signal  inboundHeaderHalfWordSize 	: unsigned(3 downto 0);
signal  inboundSkipedHeaderWordSize 	: unsigned(2 downto 0);

type PktSpecificHeader is array (0 to 3) of std_logic_vector(CONTENT_WIDTH-1 downto 0);
signal  inboundPktSepcificHeaders 	: PktSpecificHeader;

signal  outboundWriteFrameFull_o    : std_logic;
signal  outboundWriteFrame          : std_logic;
signal  outboundWriteFrameAbort_i   : std_logic;
signal  outboundWriteContent      : std_logic;   
signal  outboundWriteContentNext_q      : std_logic;		  
signal  axi_target_address : std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0); 
signal  mbox_frame_base_address : std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0); 
----------------------------------------------------------
-- Used to create the header for any SRIO frame
----------------------------------------------------------
signal ackid_o      : std_logic_vector(4 downto 0);
signal vc_o         : std_logic;
signal crf_o        : std_logic;
signal prior        : std_logic_vector(1 downto 0);
signal tt           : std_logic_vector(1 downto 0);
signal ftype        : std_logic_vector(3 downto 0);
signal targetId_o   : std_logic_vector(15 downto 0);
signal sourceId_o   : std_logic_vector(15 downto 0);
signal address_o    : std_logic_vector(28 downto 0);

----------------------------------------------------------
-- Used to decode the header for any SRIO frame
----------------------------------------------------------
signal ackid_i      : std_logic_vector(4 downto 0);
signal vc_i         : std_logic;
signal crf_i        : std_logic;
signal prior_i      : std_logic_vector(1 downto 0);
signal tt_i         : std_logic_vector(1 downto 0);
signal ftype_i      : std_logic_vector(3 downto 0);
signal targetId_i   : std_logic_vector(15 downto 0);
signal sourceId_i   : std_logic_vector(15 downto 0);
signal address_i    : std_logic_vector(28 downto 0);

type SrioFrameType is array (natural range <>) of std_logic_vector(31 downto 0);
signal srio_frame_content : SrioFrameType(0 to 69) := (others=>(others=>'1'));


----------------------------------------------------------
-- Used to parse incoming  MESSAGE header  
----------------------------------------------------------
signal msg_length_i       : std_logic_vector(3 downto 0);
signal msg_size_i           : std_logic_vector(3 downto 0);
signal letter_i           : std_logic_vector(1 downto 0);
signal mbox_i             : std_logic_vector(1 downto 0);
signal msg_seg_i          : std_logic_vector(3 downto 0);


----------------------------------------------------------
-- Used to create the header for MESSAGE
----------------------------------------------------------
signal msg_length       : std_logic_vector(3 downto 0);
signal msg_size      : std_logic_vector(3 downto 0);
signal letter           : std_logic_vector(1 downto 0);
signal mbox             : std_logic_vector(1 downto 0);
signal msg_seg          : std_logic_vector(3 downto 0);
signal packetHeader     : std_logic_vector(31 downto 0);
signal packetHeaderExt  : std_logic_vector(15 downto 0);
signal messageAddress_o : std_logic_vector(15 downto 0);

----------------------------------------------------------
-- Used to create the header for SWRITE
----------------------------------------------------------
signal swriteHeader_o   : std_logic_vector(31 downto 0);
signal swriteAddress_o   : std_logic_vector(31 downto 0);


----------------------------------------------------------
-- Computation of the SRIO frame CRC
----------------------------------------------------------
signal crc16Data : std_logic_vector(31 downto 0);
signal crc16Current : std_logic_vector(15 downto 0);
signal crc16Temp : std_logic_vector(15 downto 0);
signal crc16Next : std_logic_vector(15 downto 0);

signal crc16OutboundData : std_logic_vector(31 downto 0);
signal crc16OutboundCurrent : std_logic_vector(15 downto 0);
signal crc16OutboundTemp : std_logic_vector(15 downto 0);
signal crc16OutboundNext : std_logic_vector(15 downto 0);

signal outboundWordCounter : std_logic_vector(15 downto 0);
signal last_frame_q0 : std_logic;

signal processing_axi_transaction : std_logic;

attribute mark_debug of crc16Current : signal is "true";
attribute mark_debug of crc16Temp : signal is "true";
attribute mark_debug of crc16Next : signal is "true";
attribute mark_debug of processing_axi_transaction : signal is "true";
--
-- Length of a message computed after the complete retrieval of a message
--
signal msg_total_length : std_logic_vector(12 downto 0);


signal startingSrioFrame : std_logic;

-- Interface between the FIFO and the SRIO component
signal  outboundWriteContentData  : std_logic_vector(CONTENT_WIDTH-1 downto 0);

signal  axi_target_data_save  : std_logic_vector(CONTENT_WIDTH/2-1 downto 0);

-- Contains 2 half word of the AXI side concatenated, ready to be passed to SRIO
signal  outboundAxiData  : std_logic_vector(CONTENT_WIDTH-1 downto 0);

------------------------------------------------------------------------
-- Defines the various write feature supported by the DualInputFIFO
------------------------------------------------------------------------
constant    OUTBOUND_WRITE_NONE 	: STD_LOGIC_VECTOR (1 downto 0) := "00";
constant    OUTBOUND_WRITE_SINGLE 	: STD_LOGIC_VECTOR (1 downto 0) := "01";
constant    OUTBOUND_WRITE_DUAL 	: STD_LOGIC_VECTOR (1 downto 0) := "11";

-- Used to specify the writing mode to the DualInputFIFO
signal outboundWriteType    : STD_LOGIC_VECTOR (1 downto 0);

------------------------------------------------------------------------
-- Allow to pass half-word to the DualInputFIFO
------------------------------------------------------------------------
signal   outboundHalfContentData_1 : std_logic_vector(CONTENT_WIDTH/2-1 downto 0);
signal   outboundHalfContentData_2 : std_logic_vector(CONTENT_WIDTH/2-1 downto 0);

signal  axi_data_valid, axi_data_read, axi_deferred_write : std_logic;

-- Indicate the the DualInputFIFO, if a complete frame has been written
signal  end_frame : std_logic;

-- Defines if the word just retrieved is the last one of a frame.
signal  last_frame : std_logic;
signal  srio_frame_completed : std_logic;

-----------------------------------------------------------------------
-- Supported configuration registers
------------------------------------------------------------------------
signal reg_srio_ip_vid : std_logic_vector( C_S_AXI_DATA_WIDTH - 1 downto 0) := x"600D5410";
signal reg_srio_id_len : std_logic_vector( 1 downto 0);
signal reg_srio_target_id : std_logic_vector( 15 downto 0);
signal reg_srio_target_addr : std_logic_vector( 31 downto 0);
signal reg_srio_source_id : std_logic_vector( 15 downto 0);
signal reg_srio_source_addr : std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0);
signal reg_srio_target_mbox : std_logic_vector( 7 downto 0);
signal reg_srio_source_size : std_logic_vector( 11 downto 0);
signal reg_srio_maint_timeout : std_logic_vector( 31 downto 0);

signal reg_srio_maint_config_addr : std_logic_vector( 31 downto 0);
signal reg_srio_maint_config_data : std_logic_vector( 31 downto 0);
signal reg_srio_maint_config_data_ans : std_logic_vector( 63 downto 0);

signal reg_axi_mbox_addr : std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0);
signal reg_axi_mbox_slot_mask: std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0);
signal reg_axi_mbox_current_slot : std_logic_vector( C_M_AXI_ADDR_WIDTH -1 downto 0);
signal reg_axi_burst_size : std_logic_vector( 2 downto 0);
signal reg_srio_last_target_id : std_logic_vector( 15 downto 0);
signal reg_srio_last_source_id : std_logic_vector( 15 downto 0);
signal reg_srio_last_ftype  : std_logic_vector( 3 downto 0);
signal reg_err_axi_write  : std_logic_vector( 31 downto 0);
signal reg_err_dropped_frame  : std_logic_vector( 31 downto 0);
signal reg_err_dropped_maint  : std_logic_vector( 31 downto 0);
signal reg_err_received_frame  : std_logic_vector( 31 downto 0);
signal reg_mbox_received_msg  : std_logic_vector( 31 downto 0);
signal reg_mbox_xmited_msg  : std_logic_vector( 31 downto 0);


-- Max size of a burst length is 256 see A3-44 
signal reg_axi_burst_length_max : std_logic_vector( 7 downto 0) := x"08"; 

-- trigger a data transfer over SRIO from an AXI address
signal complete_xfer: std_logic;



signal master_xfer_start : std_logic;
signal master_maint_start_wr : std_logic;
signal master_maint_start_rd : std_logic;
signal master_maint_response : std_logic;

signal pending_request_MAINTENANCE,  pending_request_MESSAGE : std_logic;
signal pending_request_MAINTENANCE_WR,  pending_request_MAINTENANCE_RD : std_logic;

-- Contains the next transaction ID to be used for maintenance packet
signal master_maint_id : std_logic_vector(7 downto 0);

signal maint_ttype : std_logic_vector(3 downto 0);
signal maint_status : std_logic_vector(3 downto 0);
signal maint_ttid : std_logic_vector(7 downto 0);
signal maint_hopcount : std_logic_vector(7 downto 0);

signal maint_wr_ttype : std_logic_vector(3 downto 0);

signal master_xfer_source_addr : unsigned( C_M_AXI_ADDR_WIDTH -1 downto 0);



signal master_axi_araddr    : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
signal master_axi_arvalid   : std_logic;
signal master_axi_arlen     : std_logic_vector(8-1 downto 0);
signal master_axi_arsize    : std_logic_vector(3-1 downto 0);
signal master_axi_arburst   : std_logic_vector(2-1 downto 0);
signal master_axi_arready   : std_logic;
signal master_axi_rvalid    : std_logic;
signal master_axi_rready    : std_logic;
signal master_axi_rdata     : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal master_axi_rlast     : std_logic;
signal master_axi_rlast_deffered     : std_logic;
signal master_axi_rdata_delayed     : std_logic_vector(C_M_AXI_DATA_WIDTH/2-1 downto 0);
signal master_axi_has_rdata_delayed     : std_logic;

signal master_axi_awid    : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
signal master_axi_awaddr  : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
signal master_axi_awlen   : std_logic_vector(8-1 downto 0);
signal master_axi_awsize  : std_logic_vector(3-1 downto 0);
signal master_axi_awburst : std_logic_vector(2-1 downto 0);
signal master_axi_awlock  : std_logic;
signal master_axi_awcache : std_logic_vector(4-1 downto 0);
signal master_axi_awprot  : std_logic_vector(3-1 downto 0);
signal master_axi_awqos   : std_logic_vector(4-1 downto 0);
signal master_axi_awuser  : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
signal master_axi_awvalid : std_logic;
signal master_axi_awready : std_logic;

signal master_axi_wdata  : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal master_axi_wstrb  : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
signal master_axi_wlast  : std_logic;
signal master_axi_wuser  : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
signal master_axi_wvalid :  std_logic;
signal master_axi_wready :  std_logic;
signal master_axi_bid    :  std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
signal master_axi_bresp  :  std_logic_vector(2-1 downto 0);
signal master_axi_buser  :  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
signal master_axi_bvalid :  std_logic;
signal master_axi_bready :  std_logic;

signal xfer_remaining_beat_count  : unsigned( XFER_BEAT_SIZE_BIT_HI downto 0); -- std_logic_vector( XFER_BEAT_SIZE_BIT_HI downto 0);
signal xfer_remaining_byte_count  : unsigned( XFER_BEAT_SIZE_BIT_HI downto 0); -- std_logic_vector( XFER_BEAT_SIZE_BIT_HI downto 0);

signal xfer_next_beat_count       : unsigned( XFER_BEAT_SIZE_BIT_HI downto 0); -- std_logic_vector( XFER_BEAT_SIZE_BIT_HI downto 0);
signal xfer_remaining_byte        : unsigned( reg_srio_source_size'range ); -- std_logic_vector( reg_srio_source_size'range );
signal xfer_current_beat_count    : unsigned( XFER_BEAT_SIZE_BIT_HI downto 0); -- std_logic_vector(master_axi_arlen'range);
signal xfer_max_beat              : unsigned( XFER_BEAT_SIZE_BIT_HI downto 0); -- std_logic_vector(master_axi_arlen'range);

-- Contains the size of the SRIO header frame being built
signal xfer_srio_header_size      : unsigned( 3 downto 0); 

-- How many byte is already inside the current SRIO frame ?
signal xfer_srio_byte_count       : unsigned( xfer_remaining_byte'range); -- std_logic_vector(master_axi_arlen'range);
signal max_srio_payload_byte_count  : unsigned( xfer_remaining_byte'range); -- std_logic_vector(master_axi_arlen'range);

signal recv_srio_byte_count       : unsigned( xfer_remaining_byte'range); -- 
signal axi_xfrd_len               : unsigned( xfer_remaining_byte'range);
signal axi_xfrd_len_next          : unsigned( xfer_remaining_byte'range);
signal axi_max_current_reached    : std_logic;
signal axi_beat_len_total             : unsigned(master_axi_awlen'range);
signal reg_beat_len_max         : unsigned(master_axi_awlen'range);
signal axi_beat_len_current         : unsigned(master_axi_awlen'range);
signal axi_byte_len_current      : unsigned(15 downto 0);
signal axi_srio_payload_size   : unsigned( 9 downto 0);

signal srio_inbound_data_count : unsigned(15 downto 0);
signal reset_srio_inbound_counter : std_logic;
 
signal recv_hdr_32bits_aligned : std_logic;
signal recv_data_32bits_aligned : std_logic;
signal reg_axi_mbox_addr_update : std_logic;
signal reg_axi_mbox_addr_update_done : std_logic;
signal intr_message_complete : std_logic;
signal intr_message_complete_done : std_logic;
signal intr_message_xmited : std_logic;
signal intr_message_xmited_done : std_logic;

signal tied_to_ground : std_logic;
signal loopkup_enable : std_logic;
signal loopkup_source_id : std_logic_vector(15 downto 0);
signal loopkup_source_id_wr : std_logic;
signal loopkup_mbox_slot_in : std_logic_vector(7 downto 0);
signal loopkup_mbox_slot_out : std_logic_vector(7 downto 0);

type XferStateType is (
    STATE_XFER_IDLE,
    STATE_XFER_CREATE_SRIO_HEADER,
    STATE_XFER_CREATE_MAINT_HEADER,
    STATE_XFER_CREATE_MAINT_HEADER_EXT,
    STATE_XFER_CREATE_MAINT_DEST,
    STATE_XFER_CREATE_MAINT_DATA,
    STATE_XFER_CREATE_MAINT_DATA2,
    STATE_XFER_CREATE_MAINT_DATA_PAD,
    STATE_XFER_ENDING_MAINT,
    STATE_XFER_START,    
    STATE_XFER_WAITING_SLAVE_ACK_ADDR,
    STATE_XFER_WAITING_SLAVE_DATA,
    STATE_XFER_ENDING_BURST,
    STATE_XFER_ENDING_SRIO_FRAME
);
signal xfer_state : XferStateType; 
signal xfer_state_next : XferStateType;

type RecvStateType is (
    STATE_RECV_IDLE,
    STATE_RECV_PARSE_SRIO_HEADER,
    STATE_RECV_WAIT_SRIO_HEADER,
    STATE_RECV_PARSE_SRIO_ADDRESS,
    STATE_RECV_PARSE_REQUEST_FIELDS,
    STATE_RECV_WAIT_LOOKUP_MBOX_ADDR,
    STATE_RECV_START,    
    STATE_RECV_WAITING_SLAVE_ACK_ADDR,
    STATE_RECV_PUSH_DESTINATION,
    STATE_RECV_PUSH_SOURCE,
    STATE_RECV_ALIGN_START_FRAME,
    STATE_RECV_PUSH_FRAME_ADDR,
    STATE_RECV_PUSH_FRAME_DATA,
    STATE_RECV_ENDING_BURST,
    STATE_RECV_DONE,
    STATE_RECV_NOTIFY,
    STATE_RECV_ENDING_SRIO_FRAME,
    STATE_RECV_DISCARDING_SRIO_FRAME,
    STATE_RECV_MAINT_RESPONSE,
    STATE_RECV_MAINT_ENDING,
    STATE_RECV_MAINT_DONE,
    STATE_RECV_MAINT_DATA2,
    STATE_RECV_MAINT_DATA
);

signal recv_state : RecvStateType; 


-- ************************************************************
-- AXI4 Lite read support

type AXI4LiteReadState is (
    STATE_AXIL_RD_IDLE,
    STATE_AXIL_RD_WAITING_DATA,
    STATE_AXIL_RD_WAITING_ACK

);
signal axil_read_state, axil_read_state_next : AXI4LiteReadState;

signal slave_axi_araddr_latched, slave_axi_araddr_latched_next   : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
signal slave_axi_arready, slave_axi_arready_next : std_logic;
signal slave_axi_rvalid, slave_axi_rvalid_next : std_logic;
signal slave_axi_rdata, slave_axi_rdata_next : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
signal slave_axi_rresp, slave_axi_rresp_next  : std_logic_vector(2-1 downto 0);

signal accessed_register : std_logic_vector(7 downto 0);
signal register_access, register_access_rd, register_access_rd_next : std_logic;
signal register_addr_rd,register_addr_rd_next : std_logic;
signal register_access_rd_done  : std_logic;
signal register_get_value : std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);

signal register_processing_deferred_rd : std_logic;


type RegAccessType is (
    STATE_REG_ACCESS_IDLE,
    STATE_REG_ACCESS_RD,
    STATE_REG_ACCESS_WR
);

signal reg_access_state : RegAccessType;
signal register_access_remote_rd : std_logic;
signal register_access_remote_rd_error : std_logic;
signal register_access_remote_wr : std_logic;
signal register_access_remote_wr_error : std_logic;
signal register_access_remote_rd_done : std_logic;
signal register_access_remote_wr_done : std_logic;
signal register_access_remote_timeout_counter : std_logic_vector(15 downto 0);

signal register_remote_get_value : std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);
signal register_remote_addr : std_logic_vector(7 downto 0);

-- ************************************************************

-- ************************************************************
-- AXI4 Lite Write support

type AXI4LiteWrState is (
    STATE_AXIL_WR_IDLE,
    STATE_AXIL_WR_WAITING_DATA,
    STATE_AXIL_WR_WAITING_ACK,
    STATE_AXIL_WR_WAITING_BREADY

);
signal axil_wr_state, axil_wr_state_next : AXI4LiteWrState;

-- defines if the slave is ready to receive a write data
signal slave_axi_awaddr   : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
signal slave_axi_wvalid : std_logic;
signal slave_axi_bresp, slave_axi_bresp_next  : std_logic_vector(2-1 downto 0);
signal slave_axi_bvalid, slave_axi_bvalid_next : std_logic;
signal slave_axi_bready : std_logic;

signal slave_axi_awaddr_latched, slave_axi_awaddr_latched_next   : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
signal slave_axi_wready, slave_axi_wready_next : std_logic;
signal slave_axi_awready, slave_axi_awready_next : std_logic;
signal register_set_value_next : std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);

signal register_addr_wr,register_addr_wr_next : std_logic;
signal register_access_wr,register_access_wr_next : std_logic;
signal register_access_wr_done : std_logic;
signal register_access_wr_error : std_logic;
signal register_set_value : std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);

signal register_processing_deferred_wr : std_logic;


-- ************************************************************

signal srio_next_word_position : unsigned(7 downto 0);

type SrioHalfWord is (
    SRIO_WORD_HI,
    SRIO_WORD_LO,
    SRIO_WORD_HILO
);

type SrioWriteHalfWord is (
    SRIO_WRITE_HALF_WORD,
    SRIO_WRITE_FULL_WORD,
    SRIO_WRITE_NONE
);
signal srio_half_word : SrioHalfWord; 


  
type BurstSize_t is (
    BURST_001, BURST_002, BURST_004, BURST_008, 
    BURST_016, BURST_032, BURST_064, BURST_128
);
type BurstSize_array is array(BurstSize_t) of std_logic_vector(2 downto 0);
constant EncodeBurstSize : BurstSize_array :=
(
    BURST_001    => "000",
    BURST_002    => "001",
    BURST_004    => "010",
    BURST_008    => "011",
    BURST_016    => "100",
    BURST_032    => "101",
    BURST_064    => "110",
    BURST_128    => "111"
);

type DecodeBurstSize_array is array(BurstSize_t) of unsigned(7 downto 0);
constant DecodeBurstSize : DecodeBurstSize_array :=
(
    BURST_001    => "00000001",
    BURST_002    => "00000010",
    BURST_004    => "00000100",
    BURST_008    => "00001000",
    BURST_016    => "00010000",
    BURST_032    => "00100000",
    BURST_064    => "01000000",
    BURST_128    => "10000000"
);

type BurstType_t is (
    BURST_FIXED, BURST_INCR, BURST_WRAP, BURST_RSVD
);
type BurstType_array is array(BurstType_t) of std_logic_vector(1 downto 0);
constant EncodeBurstType : BurstType_array :=
(
    BURST_FIXED   => "00",
    BURST_INCR    => "01",
    BURST_WRAP    => "10",
    BURST_RSVD    => "11"
);
  

signal xfer_to_axi_addr : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
signal xfer_to_axi_addr_valid : std_logic;
signal xfer_completion_request : std_logic;
signal xfer_to_axi_completed : std_logic;
signal xfer_to_axi_size : unsigned(8-1 downto 0);

signal xfer_to_axi_data  : std_logic_vector(C_S_AXI_DATA_WIDTH -1 downto 0);
signal xfer_to_axi_data_valid : std_logic;

signal xfer_to_axi_complete : std_logic;

signal xfer_to_axi_full : std_logic;

--===============================================================================================
--
-- Function declarations
--
--===============================================================================================
function BurstSizeToByte( burst_size: std_logic_vector(2 downto 0) ) return integer is
        variable result : integer;
begin
    case (burst_size) is
        when "000"    => 
            result := 1; -- BURST_001
        when "001"    => 
            result := 2; -- BURST_002
        when "010"    => 
            result := 4; -- BURST_004
        when "011"    => 
            result := 8; -- BURST_008
        when "100"    => 
            result := 16; -- BURST_016
        when "101"    => 
            result := 32; -- BURST_032
        when "110"    => 
            result := 64; -- BURST_064
        when "111"    => 
            result := 128; -- BURST_128
        when others =>
            result := 238;
            
     end case;

    return result;
end function;

-- function BurstSizeToByte( burst_size: std_logic_vector(2 downto 0) ) return integer is
        -- variable result : integer;
    -- begin
        -- case (burst_size) is
            -- when "000"    => 
                -- result := "00000001"; -- BURST_001
            -- when "001"    => 
                -- result := "00000010"; -- BURST_002
            -- when "010"    => 
                -- result := "00000100"; -- BURST_004
            -- when "011"    => 
                -- result := "00001000"; -- BURST_008
            -- when "100"    => 
                -- result := "00010000"; -- BURST_016
            -- when "101"    => 
                -- result := "00100000"; -- BURST_032
            -- when "110"    => 
                -- result := "01000000"; -- BURST_064
            -- when "111"    => 
                -- result := "10000000"; -- BURST_128
         -- end case;

        -- return result;
    -- end function;    
    
    function GetSRioPacketOrder( message_size: unsigned ) return integer is
        variable result : integer;
    begin
        if message_size > 255 then
            result := 8; -- 256
        -- Above 127 but below 256, wtill sent 256 packet size
        elsif message_size > 127 then
            result := 8; 
        elsif message_size > 63 then
            result := 7; 
        elsif message_size > 31 then
            result := 6; -- 
        elsif message_size > 15 then
            result := 5;
        else 
            result := 4;
        end if;
       
        return result;

    end function;
    
   function EncodeSRioPacketSize( srio_packet_order: integer ) return std_logic_vector is
        variable result : std_logic_vector(3 downto 0);
    begin
     
        if srio_packet_order = 8 then
            result := "1110";
        elsif srio_packet_order = 7 then
            result := "1101";
        elsif srio_packet_order = 6 then
            result := "1100";
        elsif srio_packet_order = 5 then
            result := "1011";
        elsif srio_packet_order = 4 then
            result := "1010";
        elsif srio_packet_order = 3 then 
            result := "1001";
        else
            report "Invalid SRIO packet order" severity FAILURE;
            result := "1110";
        end if;
       
        return result;

    end function;   
    
    function DecodeSRioPacketSize( srio_msg_length: std_logic_vector(3 downto 0) ) return std_logic_vector is
        -- Eight bit to allow to compute the full size upon data return
        variable result : std_logic_vector(8 downto 0);
    begin
        -- on AXI the effective length is the returned value + 1 (cf A3-44)
        case srio_msg_length is
            when "1110"   =>
                result := "011111111";  -- 255 + 1 => 256 
                
            when "1101"   => 
                result := "001111111";  -- 127 + 1 => 128 
                
            when "1100"   => 
                result := "000111111";  -- 63 + 1 => 64 
                
            when "1011"   => 
                result := "000011111";  -- 31 + 1 => 32 
                
            when "1010"   => 
                result := "000001111";  -- 15 + 1 => 16 
                
            when "1001"   => 
                result := "000000111";  -- 7 + 1 => 8 
                
            when    others => 
                result := "000000000";  -- 0 + 1 => 1 
        end case;
        return result;

    end function;    
    
begin

    rst <= not rst_n;

    tied_to_ground <= '0';

    --M_AXI_AWID          <= (others => '0'); 
    
    --M_AXI_AWLOCK        <= tied_to_ground;
    --M_AXI_AWQOS         <= (others => '0');
    --M_AXI_AWUSER        <= (others => '0');


    M_AXI_ARID          <= (others => '0'); 
    M_AXI_ARLOCK        <= tied_to_ground; 
    M_AXI_ARCACHE       <= "0011"; 
    M_AXI_ARPROT        <= "000"; 
    M_AXI_ARQOS         <= (others => '0'); 
    M_AXI_ARUSER        <= (others => '0'); 
    
    error               <= '0';
    

    intr <= intr_message_complete or intr_message_xmited;

 axi_master_frontend_INST: entity work.axi_master_frontend 
    generic map (
                    C_M_AXI_THREAD_ID_WIDTH => C_M_AXI_THREAD_ID_WIDTH,
                    C_M_AXI_ADDR_WIDTH      => C_M_AXI_ADDR_WIDTH,
                    C_M_AXI_DATA_WIDTH      => C_M_AXI_DATA_WIDTH,
                    C_M_AXI_AWUSER_WIDTH    => C_M_AXI_AWUSER_WIDTH,
                    C_M_AXI_ARUSER_WIDTH    => C_M_AXI_ARUSER_WIDTH,
                    C_M_AXI_WUSER_WIDTH     => C_M_AXI_WUSER_WIDTH,
                    C_M_AXI_RUSER_WIDTH     => C_M_AXI_RUSER_WIDTH,
                    C_M_AXI_BUSER_WIDTH     => C_M_AXI_BUSER_WIDTH,
                    C_S_AXI_ADDR_WIDTH      => C_S_AXI_ADDR_WIDTH,
                    C_S_AXI_DATA_WIDTH      => C_S_AXI_DATA_WIDTH,
                    C_AXI_LOCK_WIDTH        => C_AXI_LOCK_WIDTH

                )

    port map(
                clk     => clk,
                rst_n => rst_n,


                xfer_addr_i => xfer_to_axi_addr,
                xfer_addr_valid_i => xfer_to_axi_addr_valid,
                xfer_size_i => xfer_to_axi_size,
                xfer_completion_request_i => xfer_completion_request,


                xfer_data_i  => xfer_to_axi_data,
                xfer_data_valid_i => xfer_to_axi_data_valid,

                full_o =>  xfer_to_axi_full,
                almost_full_o => open,
                completed_o => xfer_to_axi_completed,

                M_AXI_AWID              => M_AXI_AWID,
                M_AXI_AWADDR            => M_AXI_AWADDR,
                M_AXI_AWLEN             => M_AXI_AWLEN,
                M_AXI_AWSIZE            => M_AXI_AWSIZE,
                M_AXI_AWBURST           => M_AXI_AWBURST,
                M_AXI_AWLOCK            => M_AXI_AWLOCK,
                M_AXI_AWCACHE           => M_AXI_AWCACHE,
                M_AXI_AWPROT            => M_AXI_AWPROT,
                M_AXI_AWQOS             => M_AXI_AWQOS,
                M_AXI_AWUSER            => M_AXI_AWUSER,
                M_AXI_AWVALID           => M_AXI_AWVALID,
                M_AXI_AWREADY           => M_AXI_AWREADY,
                M_AXI_WDATA             => M_AXI_WDATA,
                M_AXI_WSTRB             => M_AXI_WSTRB,
                M_AXI_WLAST             => M_AXI_WLAST,
                M_AXI_WUSER             => M_AXI_WUSER,
                M_AXI_WVALID            => M_AXI_WVALID,
                M_AXI_WREADY            => M_AXI_WREADY,
                M_AXI_BID               => M_AXI_BID,
                M_AXI_BRESP             => M_AXI_BRESP,
                M_AXI_BUSER             => M_AXI_BUSER,
                M_AXI_BVALID            => M_AXI_BVALID,
                M_AXI_BREADY            => M_AXI_BREADY,

                --M_AXI_ARID              => M_AXI_ARID,
                --M_AXI_ARADDR            => M_AXI_ARADDR,
                --M_AXI_ARLEN             => M_AXI_ARLEN,
                --M_AXI_ARSIZE            => M_AXI_ARSIZE,
                --M_AXI_ARBURST           => M_AXI_ARBURST,
                --M_AXI_ARLOCK            => M_AXI_ARLOCK,
                --M_AXI_ARCACHE           => M_AXI_ARCACHE,
                --M_AXI_ARPROT            => M_AXI_ARPROT,
                --M_AXI_ARQOS             => M_AXI_ARQOS,
                --M_AXI_ARUSER            => M_AXI_ARUSER,
                --M_AXI_ARVALID           => M_AXI_ARVALID,
                --M_AXI_ARREADY           => M_AXI_ARREADY,
                --M_AXI_RID               => M_AXI_RID,
                --M_AXI_RDATA             => M_AXI_RDATA,
                --M_AXI_RRESP             => M_AXI_RRESP,
                --M_AXI_RLAST             => M_AXI_RLAST,
                --M_AXI_RUSER             => M_AXI_RUSER,
                --M_AXI_RVALID            => M_AXI_RVALID,
                --M_AXI_RREADY            => M_AXI_RREADY

                M_AXI_ARID              => open,
                M_AXI_ARADDR            => open,
                M_AXI_ARLEN             => open,
                M_AXI_ARSIZE            => open,
                M_AXI_ARBURST           => open,
                M_AXI_ARLOCK            => open,
                M_AXI_ARCACHE           => open,
                M_AXI_ARPROT            => open,
                M_AXI_ARQOS             => open,
                M_AXI_ARUSER            => open,
                M_AXI_ARVALID           => open,
                M_AXI_ARREADY           => '0',
                M_AXI_RID               => (others => '0'),
                M_AXI_RDATA             => (others => '0'),
                M_AXI_RRESP             => (others => '0'),
                M_AXI_RLAST             => '0',
                M_AXI_RUSER             => (others => '0'),
                M_AXI_RVALID            => '0',
                M_AXI_RREADY            => open

            );

    
    
    
    
BufferWindowsSelect: if C_USE_WINDOW_BUFFER = true generate
begin

   RioPacketBufferWindow_INST: entity work.RioPacketBufferWindow
   generic map (
      SIZE_ADDRESS_WIDTH    => SIZE_ADDRESS_WIDTH,
      CONTENT_ADDRESS_WIDTH => CONTENT_ADDRESS_WIDTH,
      CONTENT_WIDTH         => CONTENT_WIDTH,
      MAX_PACKET_SIZE       => MAX_PACKET_SIZE)
   port map (
      clk                        => clk,
      areset_n                   => rst_n,
	  
	  ------------------------------------------------------------
	  -- Interface the RIO core for inbound data
	  ------------------------------------------------------------
      inboundWriteFrameFull_o    => writeFrameFull_o, --
      inboundWriteFrame_i        => writeFrame_i,--
      inboundWriteFrameAbort_i   => writeFrameAbort_i, --
      inboundWriteContent_i      => writeContent_i, --
      inboundWriteContentData_i  => writeContentData_i, --

	  ------------------------------------------------------------
	  -- Interface to pull  inbound data
	  ------------------------------------------------------------	  
      inboundReadFrameEmpty_o    => inboundReadFrameEmpty_o,
	  
	  --
	  -- Start reading a frame ? ...
	  --
      inboundReadFrame_i         => inboundReadFrame_i,
      inboundReadFrameRestart_i  => inboundReadFrameRestart_i,
      inboundReadFrameAborted_o  => inboundReadFrameAborted_o,
      inboundReadContentEmpty_o  => inboundReadContentEmpty_o,
      inboundReadFrameSize_o     => inboundReadFrameSize_o,
	  
	  --
	  -- ... start reading each word of the frame
	  --
      inboundReadContent_i       => inboundReadContent,
      -- BEFORE inboundReadContent_i       => inboundReadContent_i,
	  
	  -- No more data to be read
      inboundReadContentEnd_o    => inboundReadContentEnd_o,
      inboundReadContentData_o   => inboundReadContentData_o,
	  
	  ------------------------------------------------------------
	  -- Interface to push outbound data
	  ------------------------------------------------------------
      outboundWriteFrameFull_o   => outboundWriteFrameFull_o,
      outboundWriteFrame_i       => outboundWriteFrame,
      outboundWriteFrameAbort_i  => outboundWriteFrameAbort_i,
	  
	  --
	  -- Put a word on outboundWriteContentData_i, and assert outboundWriteContent
	  -- 	once all word have been loaded, then assert outboundWriteFrame_i
	  -- 	to indicate the complete frame has been loaded
	  --
      outboundWriteContent_i     => outboundWriteContent,
      outboundWriteContentData_i => outboundWriteContentData,

	  -----------------------------------------------------------
	  -- Interface the RIO core for outbound data
	  ------------------------------------------------------------	  
      outboundReadFrameEmpty_o   => readFrameEmpty_o, --
      outboundReadFrame_i        => readFrame_i,--
      outboundReadFrameRestart_i => readFrameRestart_i,--
      outboundReadFrameAborted_o => readFrameAborted_o,--
      outboundReadWindowEmpty_o  => readWindowEmpty_o,--
      outboundReadWindowReset_i  => readWindowReset_i,--
      outboundReadWindowNext_i   => readWindowNext_i,--
      outboundReadContentEmpty_o => readContentEmpty_o,--
      outboundReadContent_i      => readContent_i,--
      outboundReadContentEnd_o   => readContentEnd_o,--
      outboundReadContentData_o  => readContentData_o --
      ); 

end generate;

BufferSelect: if C_USE_WINDOW_BUFFER = false generate

   RioPacketBufferWindow_INST: entity work.RioPacketBuffer
   generic map (
      SIZE_ADDRESS_WIDTH    => SIZE_ADDRESS_WIDTH,
      CONTENT_ADDRESS_WIDTH => CONTENT_ADDRESS_WIDTH,
      CONTENT_WIDTH         => CONTENT_WIDTH,
      MAX_PACKET_SIZE       => MAX_PACKET_SIZE)
   port map (
      clk                        => clk,
      areset_n                   => rst_n,
	  
	  ------------------------------------------------------------
	  -- Interface the RIO core for inbound data
	  ------------------------------------------------------------
      inboundWriteFrameFull_o    => writeFrameFull_o, --
      inboundWriteFrame_i        => writeFrame_i,--
      inboundWriteFrameAbort_i   => writeFrameAbort_i, --
      inboundWriteContent_i      => writeContent_i, --
      inboundWriteContentData_i  => writeContentData_i, --

	  ------------------------------------------------------------
	  -- Interface to pull  inbound data
	  ------------------------------------------------------------	  
      inboundReadFrameEmpty_o    => inboundReadFrameEmpty_o,
	  
	  --
	  -- Start reading a frame ? ...
	  --
      inboundReadFrame_i         => inboundReadFrame_i,
      inboundReadFrameRestart_i  => inboundReadFrameRestart_i,
      inboundReadFrameAborted_o  => inboundReadFrameAborted_o,
      inboundReadContentEmpty_o  => inboundReadContentEmpty_o,
      inboundReadFrameSize_o     => inboundReadFrameSize_o,
	  
	  --
	  -- ... start reading each word of the frame
	  --
      inboundReadContent_i       => inboundReadContent,
      -- BEFORE inboundReadContent_i       => inboundReadContent_i,
	  
	  -- No more data to be read
      inboundReadContentEnd_o    => inboundReadContentEnd_o,
      inboundReadContentData_o   => inboundReadContentData_o,
	  
	  ------------------------------------------------------------
	  -- Interface to push outbound data
	  ------------------------------------------------------------
      outboundWriteFrameFull_o   => outboundWriteFrameFull_o,
      outboundWriteFrame_i       => outboundWriteFrame,
      outboundWriteFrameAbort_i  => outboundWriteFrameAbort_i,
	  
	  --
	  -- Put a word on outboundWriteContentData_i, and assert outboundWriteContent
	  -- 	once all word have been loaded, then assert outboundWriteFrame_i
	  -- 	to indicate the complete frame has been loaded
	  --
      outboundWriteContent_i     => outboundWriteContent,
      outboundWriteContentData_i => outboundWriteContentData,

	  -----------------------------------------------------------
	  -- Interface the RIO core for outbound data
	  ------------------------------------------------------------	  
      outboundReadFrameEmpty_o   => readFrameEmpty_o, --
      outboundReadFrame_i        => readFrame_i,--
      outboundReadFrameRestart_i => readFrameRestart_i,--
      outboundReadFrameAborted_o => readFrameAborted_o,--
      --outboundReadWindowEmpty_o  => readWindowEmpty_o,--
      --outboundReadWindowReset_i  => readWindowReset_i,--
      --outboundReadWindowNext_i   => readWindowNext_i,--
      outboundReadContentEmpty_o => readContentEmpty_o,--
      outboundReadContent_i      => readContent_i,--
      outboundReadContentEnd_o   => readContentEnd_o,--
      outboundReadContentData_o  => readContentData_o --
      ); 

   end generate;

    -----------------------------------------------------------------------------
    -- CRC generation for outbound frames.
    -----------------------------------------------------------------------------

    
    packetHeader <= ackid_o & "0" & vc_o & crf_o & prior & tt & ftype & targetId_o(7 downto 0) & sourceId_o(7 downto 0) when tt = "00" else
                    ackid_o & "0" & vc_o & crf_o & prior & tt & ftype & targetId_o ;
    packetHeaderExt <= sourceId_o;

	messageAddress_o <= msg_length & msg_size & letter & mbox & msg_seg;   
   
    lookup_table : srio_lookup_table
      PORT MAP (
        clka    => clk,
        rsta    => rst,
        ena     => loopkup_enable,
        wea(0)  => loopkup_source_id_wr,
        addra   => loopkup_source_id,
        dina    => loopkup_mbox_slot_in,
        douta   => loopkup_mbox_slot_out
      );
  
    
      crc16Data <= outboundHalfContentData_1 & outboundHalfContentData_2;

    Crc16High: Crc16CITT
        port map(
            d_i=>crc16OutboundCurrent,
            crc_i=>crc16OutboundCurrent, 
            crc_o=>crc16Temp);
            
    Crc16Low: Crc16CITT
        port map(
            d_i=>outboundAxiData(15 downto 0),
            crc_i=>crc16Temp, 
            crc_o=>crc16Next); 
   -- N.B. The ackID must _not_ be part of the CRC count, so we are supposing
   --   here the caller has set the AckID to '0' (as it is added by lower
   --- layer)
   Crc16HighOutbound: Crc16CITT
        port map(
            d_i=>outboundAxiData(31 downto 16), 
            crc_i=>crc16OutboundCurrent, 
            crc_o=>crc16OutboundTemp);
            
    Crc16LowOutbound: Crc16CITT
        port map(
            d_i=>outboundAxiData(15 downto 0), 
            crc_i=>crc16OutboundTemp, 
            crc_o=>crc16OutboundNext); 
 
    
    M_AXI_ARADDR    <= master_axi_araddr;
    M_AXI_ARVALID   <= master_axi_arvalid;
    M_AXI_ARLEN     <= master_axi_arlen;    
    M_AXI_ARSIZE    <= master_axi_arsize;   
    M_AXI_ARBURST   <= master_axi_arburst;
    M_AXI_RREADY    <= master_axi_rready;
   
    
    master_axi_arready  <= M_AXI_ARREADY;
    master_axi_rvalid   <= M_AXI_RVALID;
    master_axi_rdata    <= M_AXI_RDATA;
    master_axi_rlast    <= M_AXI_RLAST;
   
      
    axi_to_srio_fifo : dual_input_fifo
        PORT MAP (
            rst_n          => rst_n,
            clk            => clk,
            halfword_1     => outboundHalfContentData_1,
            halfword_2     => outboundHalfContentData_2,
            wr_type        => outboundWriteType,
            end_frame      => end_frame,
            fullwordout    => outboundAxiData,
            valid          => axi_data_valid,
            last_frame     => last_frame,
            readword       => axi_data_read
        );

     
    SrioWrite: process(clk, rst_n)
    begin
        if rst_n = '0' then
        
               outboundWriteContent <= '0';   
               axi_data_read <= '0';  
               srio_frame_completed <= '0';
               outboundWriteFrame <= '0';
               crc16OutboundCurrent <= x"ffff";
               outboundWordCounter <= (others => '0');
               axi_deferred_write <= '0';
            
        elsif rising_edge(clk) then

            -- Delay of one cycle to report the completion of the
            --  frame
            last_frame_q0 <= last_frame;
        
            outboundWriteFrame <= '0';
            
            -- If we don't have pushed a complete frame, then we try
            --  to fetch more data from the FIFO
            if srio_frame_completed = '0' then

                axi_deferred_write <= '0';

                -- No write by default
                outboundWriteContent <= '0';

                -- Do we have a pending write ?
                if axi_deferred_write = '1' then
                        outboundWriteContent <= '1';
                end if;

                -- If there is some data valid, we push it on the SRIO side
                if axi_data_valid = '1' then
                    axi_data_read <= '1'; 
                end if;

                if axi_data_read = '1' and axi_data_valid = '1' then
                    crc16OutboundCurrent <= crc16OutboundNext;
                    outboundWordCounter <= outboundWordCounter + 1;
                end if;
                
                -- When data available and we said we are ready, we can sample the data
                if axi_data_valid = '1' and axi_data_read = '1'then

                    -- Is it the time to insert the middle-checksum
                    --  The writer has already save the place to insert the
                    --  checksum inside
                    if outboundWordCounter = x"14" then
                        outboundWriteContentData <= crc16OutboundCurrent & outboundAxiData(15 downto 0);
                        crc16OutboundCurrent <= crc16Next;
                        outboundWriteContent <= '1';
                    else
                        outboundWriteContentData <= outboundAxiData;
                        outboundWriteContent <= '1';
                    end if;
                    
                end if;

                    -- If it was the last frame, on the next cycle
                    --  we need to complete the frame on the SRIO side
                --if last_frame_q0 = '1' then
                if last_frame = '1' then
                    srio_frame_completed   <= '1'; 
                    -- Reset the CRC counter
                    crc16OutboundCurrent <= x"ffff";
                    outboundWordCounter <= (others => '0');
                else 
                    srio_frame_completed <= '0';    
                end if;

                -- on the last word, we have to insert the final checksum
                if last_frame = '1' then

                    -- If the last word is set to 0, it means, it's just
                    --  padding so we insert checksum in the higer part
                    if outboundAxiData(15 downto 0) = x"0000" then
                        outboundWriteContentData(31 downto 16) <= crc16OutboundCurrent;
                        outboundWriteContentData(15 downto 0) <= outboundAxiData(15 downto 0);
                    -- If not, then the checkum is the last part. We have to
                    --  provide the checksum include the upper half part of
                    -- the word. This CRC is located in the temporary
                    -- location
                    else
                        outboundWriteContentData(31 downto 16) <= outboundAxiData(31 downto 16);
                        outboundWriteContentData(15 downto 0) <= crc16OutboundTemp;
                    end if;
                end if;


            -- If a frame is complete, we have to push it
            --  and while completing a frame we can not push new data
            else
                outboundWriteContent <= '0';   
                axi_data_read <= '0'; 
                
                outboundWriteFrame <= '1';
                srio_frame_completed <= '0';

                -- If there is already a data valid, it means, the next frame is already ready
                -- As the SRIO is not ready, we need to hold up for the next sequence
                if axi_data_valid = '1' then
                    outboundWordCounter <= outboundWordCounter + '1';
                    outboundWriteContentData <= outboundAxiData;
                    crc16OutboundCurrent <= crc16OutboundNext;
                    axi_deferred_write <= '1';
                end if;


            end if;
        end if;
    
    end process;
    --
    -- Put the address on the bus when we say it's valid. Assign it to 'U' for debug
    --
    --master_axi_araddr <= std_logic_vector(master_xfer_source_addr) when  master_axi_arvalid = '1' else (others => 'U');
    master_axi_araddr <= std_logic_vector(master_xfer_source_addr) ;
    
  
    
    XferStateMachine: process(clk, rst_n) 
    
        -- Store how many beat we are able to transfer in the next cycle
        -- We create from byte size, as it's the maximum size if ever one beat == one byte
        variable xfer_size_beat         : unsigned( xfer_max_beat'range ); 
        variable xfer_size_beat_large   : unsigned( xfer_remaining_byte'range );
        variable xfer_size_byte         : unsigned( xfer_remaining_byte'range );
        
        variable srio_msg_order         : integer;
        variable msg_count         : std_logic_vector(reg_srio_source_size'range);
        variable srio_frame_size     : unsigned( xfer_remaining_byte'range  );
        variable master_xfer_source_end_addr:  unsigned( master_xfer_source_addr'range);
        variable master_xfer_source_4KB: unsigned( master_xfer_source_addr'range);
        
        variable srio_frame_increment : unsigned( 3 downto 0);
               
    begin
        if rst_n = '0' then
        
            xfer_state          <= STATE_XFER_IDLE; 
            
            outboundWriteType   <= OUTBOUND_WRITE_NONE; 
            
            startingSrioFrame   <= '0'; 
            end_frame           <= '0';  
            master_axi_has_rdata_delayed   <= '0';   
            
            xfer_srio_header_size  <= (others => '0' );
            
            master_axi_arburst      <= (others => '0');
            master_axi_arsize       <= (others => '0');
            intr_message_xmited <= '0';
            master_maint_id <= (others => '0');

            reg_mbox_xmited_msg <= (others => '0' );
           
            localAckIdWrite_o <= '0';

            pending_request_MAINTENANCE_WR <= '0';
            pending_request_MAINTENANCE_RD <= '0';
            pending_request_MAINTENANCE <= '0';

	    -- Inform the SRIO interface of the packet ID,
	    -- we are going to send
	    -- TODO: when do we need to populate the AckID ???
	    -- localAckIdWrite_o <= '1';            

        elsif rising_edge(clk) then


            -- Clear the interrupt when acked
            if intr_message_xmited_done = '1' then
                intr_message_xmited <= '0';
            end if;

            -- Store the pending request
            if master_xfer_start = '1' then
                pending_request_MESSAGE <= '1';
            end if;
                
            if  master_maint_start_wr = '1' or  master_maint_start_rd = '1' then

                pending_request_MAINTENANCE_WR <= master_maint_start_wr;
                pending_request_MAINTENANCE_RD <= master_maint_start_rd;
                pending_request_MAINTENANCE <= '1';

            end if;

            case (xfer_state) is 
            
                ----------------------------------------------------------------------------
                --
                -- Waiting for being asked to transfer some data
                --
                ----------------------------------------------------------------------------
                when STATE_XFER_IDLE =>                
                    
                    -- We are not accepting any data
                    master_axi_rready       <= '0';
                    master_axi_arlen        <= (others => '0');
                    master_axi_arsize       <= (others => '0');
                    
                    -- Ensure we don't say any address is valid on the bus 
                    master_axi_arvalid   <= '0';
                    
                    -- Burst is for now hard-coded to the native width of the bus
                    -- FIXME adapt to C_M_AXI_DATA_WIDTH
                    master_axi_arburst      <= EncodeBurstType(BURST_INCR);
                    master_axi_arsize       <= EncodeBurstSize(BURST_004);                   
                    
                    -- Nothing to be push on the SRIO side
                    outboundWriteType    <= OUTBOUND_WRITE_NONE;
                                      
                    -- We are not creating any new SRIO frame
                    startingSrioFrame      <= '0';
                    end_frame              <= '0';

                    master_axi_rlast_deffered     <= '0';
                    
                    -- Prepare the message header to be inserted at the beginning of the 
                    --  frame                       
                    ackid_o         <= "00000"; -- The AckID is populated by the PCS layer, so always zero
                    vc_o            <= '0';
                    crf_o           <= '0';
                    prior           <= (others => '0');
                    tt              <= reg_srio_id_len;
                    targetId_o      <= reg_srio_target_id;
                    sourceId_o      <= reg_srio_source_id;       
                    ftype           <= "UUUU";

                    maint_wr_ttype <= "0000";

                    -- Are we requested to start DMA transfer to SRIO ?
                    if master_xfer_start = '1' then

                        ftype <= "1011";
                    
                        -- How many byte do we have to transfer ?
                        xfer_remaining_byte     <= unsigned(reg_srio_source_size);
                        
                        -- Where do we need to transfer the data from ?
                        master_xfer_source_addr <= unsigned(reg_srio_source_addr);
                        
                        -- What is the size of the beat to be used
                        xfer_max_beat                                   <= (others => '0');
                        xfer_max_beat(reg_axi_burst_length_max'range)   <= unsigned(reg_axi_burst_length_max);
                        
                        -- Sending the first message segment
                        msg_seg                 <= (others => '0');     
                        
                        -- Let's move to the creation of the transfer.
                        xfer_state               <= STATE_XFER_CREATE_SRIO_HEADER;                      

                    -- Do we have to generate a maintenance transaction ?
                    elsif pending_request_MAINTENANCE='1' then

                        xfer_state               <= STATE_XFER_CREATE_MAINT_HEADER;                      

                        if pending_request_MAINTENANCE_WR = '1' then
                            maint_wr_ttype <= "0001";
                        else
                            maint_wr_ttype <= "0000";
                        end if;

                        ftype <= "1000";

                        -- No more request pending
                        pending_request_MAINTENANCE <= '0';

                    end if;                        

                ----------------------------------------------------------------------------
                --
                -- Initiate a transfer by creating the SRIO packet header
                --
                ----------------------------------------------------------------------------               
                when STATE_XFER_CREATE_MAINT_HEADER =>              
           
                    outboundHalfContentData_1           <= packetHeader(31 downto 16); 
                    outboundHalfContentData_2           <= packetHeader(15 downto 0); 
                    outboundWriteType                   <= OUTBOUND_WRITE_DUAL;

                    -- Do we need to send a long id message ?
                    if tt = "01" then
                        xfer_state               <= STATE_XFER_CREATE_MAINT_HEADER_EXT;                      
                    else
                        xfer_state               <= STATE_XFER_CREATE_MAINT_DEST;                      
                    end if;

                when STATE_XFER_CREATE_MAINT_HEADER_EXT =>              
           
                    outboundHalfContentData_1           <= packetHeaderExt(15 downto 0); 
                    outboundWriteType                   <= OUTBOUND_WRITE_SINGLE;

                    xfer_state               <= STATE_XFER_CREATE_MAINT_DEST;                      

                when STATE_XFER_CREATE_MAINT_DEST =>              
           
                    outboundHalfContentData_1           <= maint_wr_ttype & "0001" & master_maint_id;
                    outboundHalfContentData_2           <= reg_srio_maint_config_addr(31 downto 16);
                    outboundWriteType                   <= OUTBOUND_WRITE_DUAL;

                    master_maint_id <= master_maint_id + '1';

                    xfer_state               <= STATE_XFER_CREATE_MAINT_DATA;                      

                when STATE_XFER_CREATE_MAINT_DATA =>              

                    outboundHalfContentData_1           <= reg_srio_maint_config_addr(15 downto 0);
                    -- For read there is no data
                    if maint_wr_ttype = "0000"  then
                        outboundWriteType                   <= OUTBOUND_WRITE_SINGLE;
                        xfer_state               <= STATE_XFER_ENDING_MAINT;                      
                    else
                        -- If the access is 64 bits aligned
                        if reg_srio_maint_config_addr(2) = '0' then
                            outboundHalfContentData_2           <= reg_srio_maint_config_data(31 downto 16);
                        else
                            outboundHalfContentData_2           <= (others => '0');
                        end if;
                        outboundWriteType                   <= OUTBOUND_WRITE_DUAL;
                        xfer_state               <= STATE_XFER_CREATE_MAINT_DATA2;                      
                    end if;


                when STATE_XFER_CREATE_MAINT_DATA2 =>              
                    -- If the access is 64 bits aligned
                    if reg_srio_maint_config_addr(2) = '0' then
                        outboundHalfContentData_1           <= reg_srio_maint_config_data(15 downto 0);
                    else
                        outboundHalfContentData_1           <= (others => '0');
                    end if;
                    outboundWriteType                   <= OUTBOUND_WRITE_SINGLE;

                    xfer_state               <= STATE_XFER_CREATE_MAINT_DATA_PAD;                      

                -- Data of maintenance packet must be 64 bits long
                when STATE_XFER_CREATE_MAINT_DATA_PAD =>              

                    -- If the access is 64 bits unaligned
                    if reg_srio_maint_config_addr(2) = '1' then
                        outboundHalfContentData_1           <= reg_srio_maint_config_data(31 downto 16);
                        outboundHalfContentData_2           <= reg_srio_maint_config_data(15 downto 0);
                    else
                        outboundHalfContentData_1           <= (others => '0');
                        outboundHalfContentData_2           <= (others => '0');
                    end if;
                    outboundWriteType                   <= OUTBOUND_WRITE_DUAL;

                    xfer_state               <= STATE_XFER_ENDING_MAINT;   
                    
                when STATE_XFER_ENDING_MAINT =>              

                    -- If we are using long id, we are not aligned so we need to pad
                    if tt = "01" then
                        outboundHalfContentData_1           <= x"FFFF";
                        outboundHalfContentData_2           <= x"0000";
                        outboundWriteType                   <= OUTBOUND_WRITE_DUAL;
                    else
                        outboundHalfContentData_1           <= x"FFFF";
                        outboundWriteType                   <= OUTBOUND_WRITE_SINGLE;
                    end if;

                    -- The SRIO frame is now complete
                    end_frame          <= '1';

                    xfer_state  <= STATE_XFER_IDLE;


                ----------------------------------------------------------------------------
                --
                -- Initiate a transfer by creating the SRIO packet header
                --
                ----------------------------------------------------------------------------               
                when STATE_XFER_CREATE_SRIO_HEADER =>

                    end_frame             <= '0';
 
                    -- Compute the size of each separate SRIO message to send the complete message
                    srio_msg_order := GetSRioPacketOrder(unsigned(reg_srio_source_size));

                    -- How many SRIO message to send the complete message ?
                    -- SRIO Ch. 4.2.5 a value of 0 indicate single packet, 0xf 16-packet message
                    msg_count := std_logic_vector( (unsigned(reg_srio_source_size) -1) srl srio_msg_order);
                    msg_length  <= msg_count(msg_length'range);

                    -- Size of each packet making the complete message
                    msg_size    <= EncodeSRioPacketSize(srio_msg_order);
                    letter      <= reg_srio_target_mbox(7 downto 6);           
                    mbox        <= reg_srio_target_mbox(5 downto 4);
                    
                    -- We prepare the SRIO frame by pushing the header of the frame
                    outboundHalfContentData_1           <= packetHeader(31 downto 16); 
                    outboundHalfContentData_2           <= packetHeader(15 downto 0); 
                    outboundWriteType                   <= OUTBOUND_WRITE_DUAL;
                     
                    -- No bytes for now in the payload
                    xfer_srio_byte_count                 <= (others => '0');
 
                    -- We don't accept any data on the AXI bus
                    master_axi_rready <= '0';

                    
                    -- Inform we are creating a new SRIO frame 
                    startingSrioFrame    <= '1';

                    xfer_state <= STATE_XFER_START;                    

                ----------------------------------------------------------------------------
                --
                -- Start to push the SRIO header, and presenting the address on the AXI Bus
                --
                ----------------------------------------------------------------------------               
                 when STATE_XFER_START =>
                
                    -- No half-word to be written
                    outboundWriteType              <= OUTBOUND_WRITE_NONE;
                    
                    -- If we are starting a new SRIO frame, then we have the header
                    --  of the frame to complete, and we can push it...
                    if startingSrioFrame = '1' then 
                    
                        -- Complete the SRIO Header if we are using long identifier
                        if tt = "01" then
                            outboundHalfContentData_1    <= packetHeaderExt;
                            outboundWriteType             <= OUTBOUND_WRITE_SINGLE;
                            xfer_srio_header_size <= x"6";
                        else
                            -- Nothing to write for short identifier
                            xfer_srio_header_size <= x"4";
                        end if;
                        
                    end if;
                    

                    -- We don't accept any data on the AXI bus
                    master_axi_rready <= '0';                

                    
                    -- How many beat (i.e. number of master_axi_arsize we are going to transfer)
                    xfer_size_beat_large  :=  xfer_remaining_byte srl  to_integer(unsigned(master_axi_arsize));
                    xfer_size_beat := xfer_size_beat_large(xfer_size_beat'range);
                    if xfer_size_beat > xfer_max_beat then
                       xfer_size_beat := (others => '0'); 
                       xfer_size_beat := xfer_max_beat;
                    end if;
                    
                    
                    -- How many bytes are we going to transfer in this cycle ?
                    xfer_size_byte := (others => '0' );
                    xfer_size_byte(xfer_size_beat'range) := xfer_size_beat;
                    xfer_size_byte :=  xfer_size_byte sll to_integer(unsigned(master_axi_arsize));

                    -- What is the end address of this transfer ?
                    master_xfer_source_end_addr := master_xfer_source_addr + xfer_size_byte;

                    -- Are we crossing a 4 KB page ?
                    if master_xfer_source_end_addr(master_xfer_source_end_addr'left  downto 12) /= master_xfer_source_addr(master_xfer_source_addr'left  downto 12) then

                        -- If we are crossing a page, we limit the xfer to just reach the page boundary
                        master_xfer_source_4KB :=  (master_xfer_source_addr(master_xfer_source_addr'left downto 12) + 1) & x"000" - master_xfer_source_addr;
                        xfer_size_byte := master_xfer_source_4KB(xfer_size_byte'range);
                        xfer_size_beat_large := xfer_size_byte srl to_integer(unsigned(master_axi_arsize));
                        xfer_size_beat := xfer_size_beat_large(xfer_size_beat'range);
                    end if;


                    -- ARLEN has to be the size minus 1 beat, i.e. 0 means 1 beat to be transfered
                    master_axi_arlen        <=  std_logic_vector(xfer_size_beat(master_axi_arlen'range) -1 );
                    
                    -- How many bytes there will be still to transfer after this cycle ?
                    xfer_remaining_byte           <= xfer_remaining_byte - xfer_size_byte; 
                    
                    -- Mark the address bus as valid
                    master_axi_arvalid   <= '1';                   
                    xfer_state   <= STATE_XFER_WAITING_SLAVE_ACK_ADDR;
    
                ----------------------------------------------------------------------------
                --
                -- Waiting for the slave to accept the address we have presented
                --
                ----------------------------------------------------------------------------               
                when STATE_XFER_WAITING_SLAVE_ACK_ADDR =>
                
                    outboundWriteType             <= OUTBOUND_WRITE_NONE; 
                    
                    -- Has the slave accepted the address ?
                    -- FIXME: timeout here ?
                    if master_axi_arready = '1' then 
                        master_axi_arvalid <= '0';
                        
                        if startingSrioFrame = '1' then 
                        
                            -- at the beginning of the next word, we push the destination address 
                            --  of the message; Low part will be complete by the data
                            outboundHalfContentData_1    <= messageAddress_o;
                            outboundWriteType             <= OUTBOUND_WRITE_SINGLE; 
                            
                            xfer_srio_header_size <= xfer_srio_header_size + 2;
                            -- Max payload is 256 bytes + 2 bytes intermediary checksum
                            max_srio_payload_byte_count <= x"102";
                            startingSrioFrame  <= '0';
                        end if;
                       
                        -- We now wait for the data. As we have to push the SRIO address
                        --  it's useless to try to get the data even if it's already present
                        xfer_state  <= STATE_XFER_WAITING_SLAVE_DATA;
                        
                    end if;
                    
                    
                ----------------------------------------------------------------------------
                --
                -- Copy the data from the AXI bus to the SRIO frame and insert CRC
                -- FIXME: abort the frame creation in case of error on the AXI side
                --
                ----------------------------------------------------------------------------                        
                when STATE_XFER_WAITING_SLAVE_DATA => 
                
                    -- Consider no data word will be available
                    outboundWriteType             <= OUTBOUND_WRITE_NONE;
                    master_axi_rready             <= '0';
                    master_axi_rlast_deffered     <= '0';
                    
                    -- No data is being delayed for now
                    master_axi_has_rdata_delayed  <= '0';
                    
                    -- By default no data is pushed into the SRIO frame
                    srio_frame_increment := x"0";
                    
                    -- By default if there is a valid data, we accept it. We override it later in the process
                    --  if needed
                    if master_axi_rvalid = '1' then
                        master_axi_rready <= '1';
                    end if;
                                       
                    -- We sample the data when both sender and receiver have agreed, or if a data have been 
                    --  delayed we need to push it into the SRIO frame
                    if (master_axi_rvalid = '1' and master_axi_rready = '1') or master_axi_has_rdata_delayed = '1' then                       
                        
                        case (xfer_srio_byte_count +  xfer_srio_header_size) is
                        
                            -- We are going to reach 80, so first word is fine but the second word
                            --  will be replaced by the CRC. 
                            -- As we need to wait for the data being written to be part of the CRC computation
                            --  we have to delay the CRC writing to the next cycle
                            when x"04E" => 
                                outboundHalfContentData_1    <= master_axi_rdata(C_M_AXI_DATA_WIDTH -1 downto  C_M_AXI_DATA_WIDTH/2); 
                                master_axi_rdata_delayed     <= master_axi_rdata(C_M_AXI_DATA_WIDTH/2-1 downto 0);                       
            
                                -- An half-word has been delayed to be able to insert the CRC
                                master_axi_has_rdata_delayed  <= '1';
                                
                                -- We need to little pause to allow the CRC insertion
                                master_axi_rready             <= '0';

                                srio_frame_increment    := x"2";         
                                outboundWriteType       <= OUTBOUND_WRITE_SINGLE;
                                
                            -- If we have reach the 81th byte, it's time to insert the CRC then a half-word 
                            when x"050" =>
                            
                                -- If a data has been previously delayed, we need to insert it just after
                                -- the computed CRC
                                if master_axi_has_rdata_delayed = '1' then
                                
                                    -- Insert the CRC and the delayed data
                                    outboundHalfContentData_2    <= master_axi_rdata_delayed;

                                    -- No more delayed data
                                    master_axi_has_rdata_delayed  <= '0';
                                    
                                    srio_frame_increment := x"4";
                                    outboundWriteType             <= OUTBOUND_WRITE_DUAL;                                   
                                
                                else 
                                    --There was no delayed data, so we have to insert or CRC, then half of then
                                    --  data present on the AXI bus
                                    outboundHalfContentData_2    <= master_axi_rdata(C_M_AXI_DATA_WIDTH -1 downto  C_M_AXI_DATA_WIDTH/2);
                                    
                                    -- We have to store the next part of the word to insert it in the next cycle
                                    master_axi_rdata_delayed        <= master_axi_rdata(C_M_AXI_DATA_WIDTH/2-1 downto 0);
                                    master_axi_has_rdata_delayed    <= '1';
                                    
                                    -- We need to have an empty cycle to have the cycle to insert the delayed data 
                                    --  in the next cycle
                                    master_axi_rready <= '0';
                                    
                                    srio_frame_increment := x"4";
                                    outboundWriteType             <= OUTBOUND_WRITE_DUAL;
                                    
                                end if;                          

                            -- When there is no CRC, we have just to copy the AXI data
                            when others =>
                            
                                -- Just after having written the word containing the checksum, a data may have been
                                --  delayed, we need to flush it
                                if master_axi_has_rdata_delayed = '1' then
                                    outboundHalfContentData_1     <=  master_axi_rdata_delayed;
                                    
                                    -- No more delayed data
                                    master_axi_has_rdata_delayed  <= '0';
                                    
                                    srio_frame_increment := x"2";
                                    outboundWriteType             <= OUTBOUND_WRITE_SINGLE;   

                                    
                                -- In cruising mode, we have just to pass AXI words down to the SRIO side
                                else
                                    -- Transfer the data to SRIO
                                    outboundHalfContentData_1       <= master_axi_rdata(C_M_AXI_DATA_WIDTH -1 downto  C_M_AXI_DATA_WIDTH/2);                     
                                    outboundHalfContentData_2       <= master_axi_rdata(C_M_AXI_DATA_WIDTH/2-1 downto 0); 
 
                                    srio_frame_increment := x"4"; 
                                    outboundWriteType              <= OUTBOUND_WRITE_DUAL;   
                                end if;

                        
                        end case;
                        
                        -- Prepare the next frame size
                        xfer_srio_byte_count  <= xfer_srio_byte_count + srio_frame_increment;
                        
                        -- If we have reach the maximum SRIO message size, 
                        --      we have to move on the next SRIO frame
                        if (xfer_srio_byte_count + srio_frame_increment) = max_srio_payload_byte_count then
                        
                            xfer_state  <= STATE_XFER_ENDING_BURST;
                            
                        else 
 
                            -- Is it the last word of the current burst ?
                            if master_axi_rlast = '1'  or master_axi_rlast_deffered = '1' then

                                -- Do we have received all the data weSTATE_XFER_ENDING_MAINT were interested in ?
                                if xfer_remaining_byte = (xfer_remaining_byte'range => '0') then
                                
                                -- We are on the last word, but there is half of a word stuck
                                -- It may happend if the end of the transaction was exactly on
                                --  the middle CRC. In this case, we have to add it.
                                    if (xfer_srio_byte_count +  xfer_srio_header_size) = x"04E" then

                                        master_axi_rlast_deffered <= '1';
                                    else

                                        -- In this case we prepare to complete the SRIO frame
                                        xfer_state  <= STATE_XFER_ENDING_BURST;
                                    end if;
                                
                                -- If not, we have to restart another burst
                                else 
                                    master_xfer_source_addr <= master_xfer_source_addr + xfer_size_byte;
                                    xfer_state  <= STATE_XFER_START;
                                end if;
                            end if;                            
                        end if;

                    else 
                        outboundWriteType  <= OUTBOUND_WRITE_NONE;
                    end if;
                    
                    
                ----------------------------------------------------------------------------
                --
                -- Terminate an SRIO frame with the final CRC
                --
                ----------------------------------------------------------------------------                          
                when STATE_XFER_ENDING_BURST =>
                
                    -- we inform the sender we have accepted the last data
                    master_axi_rready <= '1';
                    
                    -- Get the last CRC including the last word, it's the final CRC  
                    -- The CRC by itself is not part of the CRC computation
                    
                    -- if we are already on a 32 bits boundary, once the CRC added
                    --  we won't be any more, so we have to pad with zero
                    srio_frame_size := xfer_srio_byte_count + xfer_srio_header_size;
                    if srio_frame_size(1 downto 0) = "00" then

                        -- If we have an half word stuck, it's now time to write it
                        if master_axi_has_rdata_delayed = '1' then
                            -- As we are aligned on a new word, We need to add CRC fist then padding
                            outboundHalfContentData_1       <= master_axi_rdata_delayed;                    
                            outboundHalfContentData_2       <= x"FFFF" ;
                            master_axi_has_rdata_delayed <= '0';
                        else
                            -- As we are aligned on a new word, We need to add CRC fist then padding
                            outboundHalfContentData_1       <= x"FFFF" ;                    
                            outboundHalfContentData_2       <= x"0000" ;
                        end if;
                    
                        -- Pushing the CRC and the padding
                        outboundWriteType             <= OUTBOUND_WRITE_DUAL;
                                              
                    else
                        -- As we we are not aligned on a new word (the CRC ends and completes a word)
                        outboundHalfContentData_1       <= x"FFFF" ; 
                        
                        -- Pushing the CRC
                        outboundWriteType                <= OUTBOUND_WRITE_SINGLE; 
                        
                    end if;
                    
                    -- The SRIO frame is now complete
                    end_frame          <= '1';
                    
                    xfer_state  <= STATE_XFER_ENDING_SRIO_FRAME;
                    
                when STATE_XFER_ENDING_SRIO_FRAME => 
                
                    -- we don't accept data any more
                    master_axi_rready <= '0';

                    -- No more data for the SRIO frame
                    outboundWriteType                <= OUTBOUND_WRITE_NONE; 
                    
                    -- Do we have send all the data we have to  ?
                    if xfer_remaining_byte = (xfer_remaining_byte'range => '0') then
                        xfer_state  <= STATE_XFER_IDLE;

                        -- Raise completion interrupt
                        intr_message_xmited <= '1';
                        reg_mbox_xmited_msg <= reg_mbox_xmited_msg + '1';
                        
                    -- elsewhere we have to create a new SRIO frame for the following data
                    else
                        msg_seg  <= msg_seg + 1;
                        
                        -- Prepare the next address we want to fetch data from
                        master_xfer_source_addr  <= master_xfer_source_addr + xfer_size_byte;
                        xfer_state  <= STATE_XFER_CREATE_SRIO_HEADER;
                    end if;
                    
                when others =>
                        ---------------------------------------------------------------
                        -- Ouch.. bad shape.. try to recover by moving to IDLE
                        ---------------------------------------------------------------
                        xfer_state  <= STATE_XFER_IDLE;
  
                        
            end case;  
        end if;

    end process;
    
    --
    -- Decoding when the incoming word is an SRIO header
    --
    ackid_i         <= inboundHeaderFirst(31 downto 27);
    vc_i            <= inboundHeaderFirst(25);         
    crf_i           <= inboundHeaderFirst(24);        
    prior_i         <= inboundHeaderFirst(23 downto 22);      
    ftype_i         <= inboundHeaderFirst(19 downto 16);    
    targetId_i      <= x"00" & inboundHeaderFirst(15 downto 8 ) when tt_i = "00" else inboundHeaderFirst(15 downto 0 );   
    sourceId_i      <= x"00" & inboundHeaderFirst(7  downto 0 ) when tt_i = "00" else inboundHeaderSecond(31 downto 16 ); 

    -- The format specific header is either aligned or not depending on the size of the IDs
    recv_hdr_32bits_aligned <= '1' when inboundHeaderHalfWordSize(0) = '0' else '0';
    
    inboundPktSepcificHeaders(0) <= inboundHeaderSecond( 31 downto 0) when recv_hdr_32bits_aligned = '0' else inboundHeaderSecond( 15 downto 0) & x"0000";

    --
    -- Decoding when the incoming word is an SRIO address message
    --  
    msg_length_i    <= inboundPktSepcificHeaders(0)(31 downto 28);
    msg_size_i      <= inboundPktSepcificHeaders(0)(27 downto 24);         
    letter_i        <= inboundPktSepcificHeaders(0)(23 downto 22);        
    mbox_i          <= inboundPktSepcificHeaders(0)(21 downto 20);      
    msg_seg_i       <= inboundPktSepcificHeaders(0)(19 downto 16);  
    
    axi_xfrd_len_next <= axi_xfrd_len + 4;
    
    -- We are on the last word, when counter is going to reach the maximum expected value
    --  (i.e. Word we are writing is the last one), and when the data is marked as valid
    master_axi_wlast        <= '1' when axi_byte_len_current = axi_xfrd_len_next and xfer_to_axi_data_valid = '1' else '0';

    -- If we have declared a number of bytes to be transmissted (i.e. /= 0),
    --- We report if we have extracted from the FIFO the maximum number of byte we have said we are 
    --  going to transmit on the AXI bus.
    axi_max_current_reached  <= '1' when  
                                        (axi_byte_len_current /= (axi_byte_len_current'range => '0')) and 
                                        ( srio_inbound_data_count  >= axi_byte_len_current )
                                        --( (srio_inbound_data_count  >= axi_byte_len_current and recv_data_32bits_aligned = '1' ) or ( srio_inbound_data_count  >= axi_byte_len_current  and recv_data_32bits_aligned = '0' ))
                                    else '0';

    -- We accept to extract byte from the FIFO only if the maximum number in the current transaction
    --  has not been reached
    inboundReadContent <= inboundReadContent_request when axi_max_current_reached = '0' else '0'; -- BEFORE



   --
   -- A SRIO word is valide one cycle after we request it to the FIFO
   --
   ReadDataValid: process(clk, rst_n)
   begin
       if rst_n = '0' then

           inboundReadContentValid <= '0';    

        -- Data is valid one cycle after having assert the read query
       elsif rising_edge(clk) then
           inboundReadContentValid <= inboundReadContent_request;
       end if;

   end process;


    -- 
    -- Count how many bytes we have extracted from the SRIO FIFO in the
    --  current transaction
    --
    InboundDataCounter: process(clk, rst_n)
    begin
        if rst_n = '0' then

            srio_inbound_data_count <= (others => '0' );    

        elsif rising_edge(clk) then

            -- If the rest has been asked, we just clear the counter to initiate
            --  a new transaction
            if reset_srio_inbound_counter = '1' then

                srio_inbound_data_count <= (others => '0' );    

                -- if during reset counter, we are popping data from the FIFO
                --  we count the on-going popping
                if inboundReadContent = '1' then
                    srio_inbound_data_count(2 downto 0) <= "100";
                end if;

            -- We increment the number of word popped when the command is issue
            elsif inboundReadContent = '1' then 

                -- if we are on the checksum location, the word won't be used, so don't count it
                -- ASA if recv_srio_byte_count /= x"50" then 
                    -- ASA srio_inbound_data_count <= srio_inbound_data_count + 4 ;
                -- ASA end if; 
                case(recv_srio_byte_count) is
                    -- Just after we have the middle checksum, so it's just half a word going out
                    when  x"04C" =>
                         srio_inbound_data_count <= srio_inbound_data_count + 2 ;
                     when others =>
                         srio_inbound_data_count <= srio_inbound_data_count + 4 ;
                 end case;

            end if;
        end if;

    end process;
       
    
   
    --
    -- Main process handling all SRIO data receiving
    --
    -- All message written in the mailbox are formatted "like" ethernet frame (they are prefixed by OUI)
    -- The Ethernet header is simulated with an OUI made of 0x0000 then 16 bits RIO Id, then 0x0000
    -- Header must be made of:
    -- Dest: 0x0000SRIO 0x0000  and Sender: 0x0000SRIO 0x0000 
    --
    RecvStateMachine: process(clk, rst_n) 
    
        -- Allowed to compute the footprint of the header in the incomint message
        variable headerHalfWordSize : unsigned (inboundHeaderHalfWordSize'range);
        -- Contains eventually the size of the data prefix in host memory, the size of the payload coming from SRIO (e.g. Ethernet prefix)
        variable recv_data_prefix_size : std_logic_vector(master_axi_awlen'range);
        variable next_mbox_slot : std_logic_vector(loopkup_mbox_slot_in'range);
        variable mbox_msg_offset : std_logic_vector(master_axi_awaddr'range);
        variable mbox_msg_segment_offset : std_logic_vector(master_axi_awaddr'range);
        
        variable axi_target_data : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        variable axi_target_data_valid : std_logic;
        variable mbox_slot  : std_logic_vector(7 downto 0);
        variable axi_beat_len_total_tmp             : unsigned(master_axi_awlen'range);
       
        
    begin
        if rst_n = '0' then

            recv_state <= STATE_RECV_IDLE;
            reg_axi_mbox_current_slot <= (others => '0');
            intr_message_complete <= '0';
            loopkup_source_id_wr <= '0';
            loopkup_source_id <= (others => '0');
            loopkup_mbox_slot_in <= (others => '1');

            axi_byte_len_current <= (others => '0' );

            reg_err_dropped_frame   <= (others => '0' );
            reg_err_received_frame  <= (others => '0' );
            reg_mbox_received_msg   <= (others => '0' );
            reg_err_axi_write       <= (others => '0' );
            reg_err_dropped_maint   <= (others => '0' );

            reset_srio_inbound_counter <= '0';

            -- Write interface to the AXI bus
            xfer_to_axi_addr        <= (others => 'U');
            xfer_to_axi_addr_valid  <= '0';
            xfer_to_axi_size        <= (others => 'U');

            xfer_to_axi_data        <= (others => 'U');
            xfer_to_axi_data_valid  <= '0';

            master_maint_response   <= '0';

        elsif rising_edge(clk) then
        
            -- Enable the lookup table
            loopkup_enable <= '1';

            -- Clear the interrupt when acked
            if intr_message_complete_done = '1' then
                intr_message_complete <= '0';
            end if;

            -- When a transfer is signaled has completed we raise the interrupt
            --   We define it after the clear, has this is a new interrupt, so it's not
            --  the one just clear by the software
            if xfer_to_axi_completed = '1' then
                intr_message_complete <= '1';
            end if;


            -- Asserting address valid is always a single clock operation
            xfer_to_axi_addr_valid  <= '0';
            xfer_to_axi_data_valid  <=  '0';    
            xfer_completion_request <= '0';
            
            case (recv_state) is 
                when STATE_RECV_IDLE                    =>
                
                    -- We take into account any change in the mailbox base addr
                    --  only if there is no ongoing transaction
                    -- FIXME what about a partial message ??
                    if reg_axi_mbox_addr_update = '1' then
                        reg_axi_mbox_current_slot <= (others => '0');
                        reg_axi_mbox_addr_update_done <= '1';
                    end if;
                    
                    -- The updated flag is just for one cycle
                    if reg_axi_mbox_addr_update_done = '1' then
                        reg_axi_mbox_addr_update_done <= '0';
                    end if;
                    
                    loopkup_mbox_slot_in        <= (others => '1');
                    inboundReadFrameRestart_i   <= '0';
                    inboundReadContent_request        <= '0';
                    inboundReadFrame_i          <= '0';
                    master_axi_awvalid          <= '0';
                    master_axi_wvalid           <= '0';
                    master_axi_bready           <= '0';
                    master_axi_wstrb            <= (others => 'U');
                    inboundHeaderHalfWordSize   <= (others => '0');
                    inboundSkipedHeaderWordSize <= (others => '0');
                    mbox_frame_base_address     <= (others => '0');
                    
                    recv_srio_byte_count                <= (others => '0');
                    
                    axi_byte_len_current    <= (others => '0' );
                    axi_xfrd_len            <= (others => '0');

                    axi_srio_payload_size   <=  (others => '0');

                    master_maint_response   <= '0';
                    reg_srio_maint_config_data_ans  <= (others => 'U' );
                    
                    
                    recv_state <=  STATE_RECV_IDLE;
                    
                    -- do we have a frame ready in the incoming buffer ?
                    if inboundReadFrameEmpty_o /= '1' then
                    
                        -- Ask to get the first word...
                        inboundReadContent_request <= '1';

                        -- Byte count from the SRIO don't start from 0, but 1,
                        --  so we start from the first word, to have the same value in the code and in the specification.
                        --recv_srio_byte_count(3 downto 0)    <= x"4";
                        
                        -- ... and becomes ready to parse the first word
                        recv_state <=  STATE_RECV_WAIT_SRIO_HEADER;

                    end if;
                    
                    
                when STATE_RECV_WAIT_SRIO_HEADER        =>

                    
                    -- ask to get the second word
                    inboundReadContent_request <= '1';
                    recv_srio_byte_count <= recv_srio_byte_count + 4;
                    
                    -- First word should be ready to be parsed on next cycle
                    recv_state <=  STATE_RECV_PARSE_SRIO_HEADER;

                when STATE_RECV_PARSE_SRIO_HEADER       =>

                    -- Latch the first header content
                    inboundHeaderFirst <= inboundReadContentData_o;

                    tt_i            <= inboundReadContentData_o(21 downto 20);  

                    -- stop poping from the FIFO,
                    inboundReadContent_request <= '0';

                    -- Becomes ready to parse the address part of the header
                    recv_state <=  STATE_RECV_PARSE_SRIO_ADDRESS;
                    reg_err_received_frame <= reg_err_received_frame + 1;
                    
                when STATE_RECV_PARSE_SRIO_ADDRESS      =>   

                    inboundReadContent_request <= '0';

                    -- Do we have short or long address IDs ?
                    if tt_i = "00" then
                        headerHalfWordSize := to_unsigned(2, headerHalfWordSize'length);
                    else 
                        headerHalfWordSize := to_unsigned(3, headerHalfWordSize'length);
                    end if;
                    
                     -- Latch the second header content
                    inboundHeaderSecond <= inboundReadContentData_o;   
                    
                    -- As we have the first Header word retrieved, we are sure
                    --  we are able to latch the target ID for processing
                    --  and debug
                    reg_srio_last_target_id     <= targetId_i;

                    -- we are only interested for the ftype of the request for now
                    reg_srio_last_ftype         <= ftype_i;    

                    -- We need to compute as soon as possible the size of the header
                    --  to correctly know where are located the fields of interest
                    case ftype_i is 

                        when "1011" =>

                            -- MESSAGE specific header is made of 2 bytes
                            inboundHeaderHalfWordSize <= headerHalfWordSize + 1;    

                        when "1000" =>

                            -- MAINTENANCE specific header is made of 4 bytes
                            inboundHeaderHalfWordSize <= headerHalfWordSize + 2;    

                        when others =>
                            null; -- FIXME

                    end case;

                    -- Waiting for the request specific header to be loaded
                    recv_state <=  STATE_RECV_PARSE_REQUEST_FIELDS;

                    
                when STATE_RECV_PARSE_REQUEST_FIELDS =>
                
                    case ftype_i is 
                        when "1011" =>
                            
                            -- If the mailbox has not been set, we need to silently discard the data
                            --
                            if reg_axi_mbox_addr = (reg_axi_mbox_addr'range => '1')  then 

                                inboundReadFrame_i <= '1';
                                recv_state <=  STATE_RECV_DISCARDING_SRIO_FRAME;
                            else

                                -- search if this source has an on-going transmission
                                -- We have to wait 2 cycles before having the data available
                                loopkup_source_id       <= sourceId_i;
                                loopkup_source_id_wr    <= '0';

                                -- if the frame is smaller than 80 bytes and the header is short
                                --  then header + csum = 2x32bits words
                                if inboundReadFrameSize_o < x"15" and tt_i = "00" then

                                    axi_beat_len_total_tmp    := unsigned(inboundReadFrameSize_o)  - 2;

                                -- in all other cases: header + one or two csum = 3x32bits words
                                else
                                    axi_beat_len_total_tmp    := unsigned(inboundReadFrameSize_o)  - 3;
                                end if;

                                -- Save how many bytes we are going to transfer either in one
                                --  or several AXI burst
                                axi_srio_payload_size <= ("00" & axi_beat_len_total_tmp) sll 2;

                                if axi_beat_len_total_tmp > reg_beat_len_max then
                                    axi_beat_len_current <= reg_beat_len_max;
                                else
                                    axi_beat_len_current <= axi_beat_len_total_tmp;
                                end if;

                                axi_beat_len_total    <= axi_beat_len_total_tmp;

                                -- We have to search where this buffer has to be written
                                --  inside the receiving mailbox.
                                recv_state <=  STATE_RECV_WAIT_LOOKUP_MBOX_ADDR;

                            end if;
                            
                        when "1000" =>
                                recv_state <=  STATE_RECV_MAINT_RESPONSE;
                                inboundReadContent_request        <= '1';

                        when others =>
                            -- Discard the unknown frame
                            inboundReadFrame_i <= '1';
                            recv_state <=  STATE_RECV_DISCARDING_SRIO_FRAME;
                            
                    end case;
                    
               
                when STATE_RECV_MAINT_RESPONSE    =>   

                    -- Short ID
                    if tt_i = "00" then

                        maint_ttype     <=  inboundReadContentData_o(31 downto 28);
                        maint_status    <=  inboundReadContentData_o(27 downto 24);
                        maint_ttid      <=  inboundReadContentData_o(23 downto 16);
                        maint_hopcount  <=  inboundReadContentData_o(15 downto 8);
                        -- inboundReadContentData_o(15 downto 8) is made of x"000000"

                    -- Long Id
                    else

                        maint_ttype     <=  inboundReadContentData_o(15 downto 12);
                        maint_status    <=  inboundReadContentData_o(11 downto 8);
                        maint_ttid      <=  inboundReadContentData_o(7 downto 0);

                    end if;
                    inboundReadContent_request <= '1';
                    recv_state <=  STATE_RECV_MAINT_DATA;

                when STATE_RECV_MAINT_DATA    =>   

                    case (maint_ttype ) is

                        -- Maintenance READ Request
                        when "0000" =>
                            reg_err_dropped_maint <= reg_err_dropped_maint + '1';
                            recv_state <=  STATE_RECV_MAINT_DONE;

                        -- Maintenance WRITE Request
                        when "0001" =>
                            reg_err_dropped_maint <= reg_err_dropped_maint + '1';
                            recv_state <=  STATE_RECV_MAINT_DONE;

                        -- Maintenance PORT WRITE Request
                        when "0100" =>
                            reg_err_dropped_maint <= reg_err_dropped_maint + '1';
                            recv_state <=  STATE_RECV_MAINT_DONE;

                        -- Maintenance READ or WRITE response 
                        when "0010" | "0011"=>

                            -- Check if this is the expected answer
                            if maint_ttid + 1 = master_maint_id then 

                                -- Short ID
                                if tt_i = "00" then

                                    report "Non implemented" severity failure;

                                -- Long Id
                                else
                                    maint_hopcount  <=  inboundReadContentData_o(31 downto 24);
                                    -- Rest of inboundReadContentData_o is made of x"000000"
                                end if;

                                inboundReadContent_request <= '1';

                            else

                                reg_err_dropped_maint <= reg_err_dropped_maint + '1';
                                recv_state <=  STATE_RECV_MAINT_DONE;

                            end if;

                            recv_state <=  STATE_RECV_MAINT_DATA2;

                        when others =>
                            null;

                    end case;


                when STATE_RECV_MAINT_DATA2    =>   

                        -- Short ID
                    if tt_i = "00" then
                        report "Non implemented" severity failure;

                        -- Long Id
                    else
                        reg_srio_maint_config_data_ans(63 downto 48) <= inboundReadContentData_o(31 downto 16);
                        reg_srio_maint_config_data_ans(47 downto 32) <= inboundReadContentData_o(15 downto 0);
                    end if;

                    master_maint_response <= '0';

                    inboundReadContent_request <= '1';
                    recv_state          <=  STATE_RECV_MAINT_ENDING;


                when STATE_RECV_MAINT_ENDING    =>   

                    -- Short ID
                    if tt_i = "00" then

                        report "Non implemented" severity failure;

                    -- Long Id
                    else
                        reg_srio_maint_config_data_ans(31 downto 16) <= inboundReadContentData_o(31 downto 16);
                        reg_srio_maint_config_data_ans(15 downto 0)  <= inboundReadContentData_o(15 downto 0);
                    end if;

                    master_maint_response <= '1';
                    recv_state          <=  STATE_RECV_MAINT_DONE;


                when STATE_RECV_MAINT_DONE    =>   

                    master_maint_response <= '0';
                    inboundReadContent_request <= '0';
                    inboundReadFrame_i <= '1';
                    recv_state          <=  STATE_RECV_DONE;

                when STATE_RECV_WAIT_LOOKUP_MBOX_ADDR    =>   

                    -- During the discarding of the header, we wait at least one 
                    --  cycle to allow the lookup table to give the answer
                    recv_state <=  STATE_RECV_WAITING_SLAVE_ACK_ADDR;                        
                    
                
                when STATE_RECV_WAITING_SLAVE_ACK_ADDR  =>
                
                    -- If we are restarting the last frame, let's complete the cycle
                    --  by asking the a content read to resume on the first word
                    if inboundReadFrameRestart_i = '1' then 
                        inboundReadContent_request        <= '1';
                        inboundReadFrameRestart_i   <= '0';
                        recv_srio_byte_count <= recv_srio_byte_count + 4;
                    else
                        inboundReadContent_request        <= '0';
                        inboundReadFrameRestart_i   <= '0';                        
                    end if;
                
                    -- If there is space to transfer data on AXI bus
                    if xfer_to_axi_full = '0' then
                    
                        -- The word containing the Source Id has been retrieved whatever its position
                        --  we can store it.
                        reg_srio_last_source_id     <= sourceId_i;  
                                              
                       
                        -- ask to write into the next mailbox location
                        -- TODO: for now, we don't manage more than one mailbox
                        
                        -- If the address is not valid, there is no pending transmission
                        if loopkup_mbox_slot_out = x"FF" then
                        
                            -- What is the next slot available (wrapped according to
                            --  mask specified by the user) ?
                            mbox_slot := (reg_axi_mbox_current_slot(7 downto 0) and reg_axi_mbox_slot_mask(7 downto 0));
                            
                            -- Compute the next slot available inside the mailbox to insert this message
                            -- Each message is 0x1000 byte long multiply by t
                            mbox_msg_offset := x"001000" * mbox_slot ;
                            
                            -- Starting from the mbox message base to be used, compute where this particular message segment
                            --  has to be written
                            mbox_msg_segment_offset   := mbox_msg_offset + msg_seg_i * ( DecodeSRioPacketSize(msg_size_i) +1 );
                            
                            -- Save the position inside the mailbox to be used for other segments coming from the same source.
                            loopkup_source_id           <= sourceId_i;
                            
                            -- Keep track of the used slot for this particular sourceId and move on to the next one
                            loopkup_mbox_slot_in        <= mbox_slot;
                            reg_axi_mbox_current_slot   <= reg_axi_mbox_current_slot + 1;
                            
                            -- Save the information inside the lookup table
                            loopkup_source_id_wr        <= '1';
                            
                        -- Otherwise we have to compute the position of the current message
                        --  inside the allocated slot
                        else
                            mbox_msg_offset             := x"001000" * loopkup_mbox_slot_out;
                            mbox_msg_segment_offset     := mbox_msg_offset + msg_seg_i * ( DecodeSRioPacketSize(msg_size_i) +1 );
                            
                            -- Nothing to save
                            loopkup_source_id_wr    <= '0';
                        
                        end if;
                        

                        
                        
                        --For a MESSAGE, we need to add the Ethernet-like header in front of the payload
                        if (ftype_i = "1011")   then
                        
                            -- The data segment will be written just after the header + the size of the message
                            axi_target_address  <= reg_axi_mbox_addr + mbox_msg_segment_offset +12 + 4;

                            -- Keep track of the message base address
                            mbox_frame_base_address <= reg_axi_mbox_addr + mbox_msg_offset;
                            
                            -- Initially the read data are either aligned or not depending on the header
                            -- (after it may change depending if we encounter the intermediary checksum)
                            recv_data_32bits_aligned <= recv_hdr_32bits_aligned; 
       
                            -- If this is the first part of the message, then we have to send the
                            --  Ethernet-like header
                            if  msg_seg_i = (msg_seg_i'range => '0') then
                            
                                -- For the first segment we have to prefix it with the Ethernet-like header
                                --  we keep 4 bytes at the beginning to write the written message size
                                xfer_to_axi_addr <= reg_axi_mbox_addr + mbox_msg_segment_offset + 4;
                                xfer_to_axi_size <= x"02";  -- Xfered size is (awlen +1)
                                xfer_to_axi_addr_valid <= '1';
                                
                                -- Create a MAC-like destination address
                                -- we write 4 first byte of the 6 bytes OUI
                                xfer_to_axi_data  <=  x"0000" & reg_srio_last_target_id;
                                xfer_to_axi_data_valid  <=  '1';

                                recv_state <=  STATE_RECV_PUSH_DESTINATION;
                                
                            -- For others segments we have just to copy data from the FIFO
                            else
                                    recv_state <=  STATE_RECV_ALIGN_START_FRAME;
                            end if;                                                        
                            
                        -- For others type we have just to copy data from the FIFO
                        else
                                recv_state <=  STATE_RECV_ALIGN_START_FRAME;
                        end if;    
                    end if;
                    
                -- Only use for FTYPE MESSAGE
                when STATE_RECV_PUSH_DESTINATION     =>
                
                    -- No need to pop a word, we know already the addresses
                    inboundReadContent_request        <= '0';

                    -- Complete the write of the allocated mailbox slot, if ever
                    loopkup_source_id_wr        <= '0';

                        -- write the last 2 bytes of the destination OUI
                        --  and the 4 starting bytes of the Source OUI 
                        --  i.e. all zeros
                    xfer_to_axi_data <= x"0000_0000";
                    xfer_to_axi_data_valid <= '1';

                        -- In the previous cycle, the slave accepted 4 more bytes
                    axi_xfrd_len    <= axi_xfrd_len + 4;

                        -- If the header is not aligned on a 32 bits boundary, then we have some
                        --  data to be kept from the second part of the incoming data
                    if recv_hdr_32bits_aligned = '0' then
                        axi_target_data_save <= inboundReadContentData_o(15 downto 0);

                        -- If it is aligned we have just to discard the whole word as all data
                        --  as been used as an header.
                    else
                        null;
                    end if;

                    recv_state <=  STATE_RECV_PUSH_SOURCE;
                
                -- Only use for FTYPE MESSAGE
                when STATE_RECV_PUSH_SOURCE            =>
                
                    inboundReadContent_request    <= '0';

                    -- we complete the source OUI, by pushing the last 4 bytes made of then
                    --  source id and 0 padding
                    xfer_to_axi_data    <= reg_srio_last_source_id & x"0000" ; 
                    master_axi_wstrb    <= (others => '1');
                    xfer_to_axi_data_valid   <= '1';

                    -- In the previous cycle, the slave accepted 4 more bytes
                    axi_xfrd_len        <= axi_xfrd_len + 4;      

                    -- Reset the counting of the number of word outputed before
                    --  starting to retrieve an SRIO frame
                    reset_srio_inbound_counter <= '1';


                    -- Ready to accept completion status from the slave                      
                    recv_state <=  STATE_RECV_PUSH_FRAME_ADDR;

                    
                when STATE_RECV_ALIGN_START_FRAME =>

                    -- Reset the counting of the number of word outputed before
                    --  starting to retrieve an SRIO frame
                    reset_srio_inbound_counter <= '1';
                
                    -- If the header is not aligned on a 32 bits boundary, then we have some
                    --  data to be kept from the second part of the incoming data
                    -- Otherwise, the data contained inside inboundReadContentData_o has just to
                    --  be copied on the AXI bus on the next cycle
                    if recv_hdr_32bits_aligned = '0' then

                        axi_target_data_save <= inboundReadContentData_o(15 downto 0);

                    end if;

                    recv_state <=  STATE_RECV_PUSH_FRAME_ADDR;    
                
                when STATE_RECV_PUSH_FRAME_ADDR       => 


                    -- Stop reseting the SRIO word counter.
                    reset_srio_inbound_counter <= '0';

                    -- We anticipate the reading of the next word, we want a single
                    --  word.
                    inboundReadContent_request    <= '1';

                    -- Use the address previously computed depending on the type the SRIO payload
                    xfer_to_axi_addr        <= axi_target_address;
                    xfer_to_axi_addr_valid  <= '1';
                    xfer_to_axi_size        <= axi_beat_len_current - 1; 

                    -- How many bytes do we intend to transfer to the slave ?
                    axi_byte_len_current <= axi_beat_len_current * BurstSizeToByte(reg_axi_burst_size);


                    -- No transferred byte for now
                    axi_xfrd_len                <= (others => '0');                 

                    -- ... move to send the data
                    recv_state <= STATE_RECV_PUSH_FRAME_DATA;  


                when STATE_RECV_PUSH_FRAME_DATA       =>    

                    -- By default, we are supposed to write a data on each cycle
                    axi_target_data_valid := '1';
                    
                    -- If we are aligned, we have just to copy the data from the FIFO
                    -- onto the AXI bus
                    if recv_data_32bits_aligned = '1' then
                        
                        -- If we are not at the checksum location ...
                        if  recv_srio_byte_count /= x"050" then
                        
                            -- ... just do a copy
                            axi_target_data := inboundReadContentData_o;
                            
                        -- This is the location of the checksum, we have to skip the checksum
                        --  located in inboundReadContentData_o(31 downto 16)
                        else
                                axi_target_data_save <= inboundReadContentData_o(15 downto 0); 
                            
                                -- No valid word (missing a part located in the next word)
                                axi_target_data_valid := '0';   
                            
                                -- Now we are no more aligned (i.e. one word of the FIFO doesn't equal = one word
                                --  on the AXI bus), we need to pop 2 word from the FIFO to get a complete one
                                recv_data_32bits_aligned <= '0';                            

                                -- The end is exactly at the checksum, we have to complete the transactionn then
                                --  we don't about the values they won't be used, and we need to proceed 
                                if master_axi_wlast = '1' then 
                                    axi_target_data_valid := '1';   
                                end if;
                        end if;
    
                    -- If we are not aligned, it means the data, is made of a previously
                    --  read half-block, and the current half data
                    else
                        -- If we are not at the checksum location ...
                        if  recv_srio_byte_count /= x"050" then
                        
                            -- ... create the data, from the saved half-word, and the current half part
                            axi_target_data     := axi_target_data_save & inboundReadContentData_o(31 downto 16); 
                            
                        -- This is the location of the checksum, we have to skip the checksum
                        --  located in inboundReadContentData_o(31 downto 16)
                        else
                            -- it means, the first part of the current word has been stored, and the 
                            --  second part is the one just retrieved, thus we are ready to push the complete
                            --  word on the AXI side    
                            axi_target_data     := axi_target_data_save & inboundReadContentData_o(15 downto  0);
                            
                            -- we are now aligned
                            recv_data_32bits_aligned <= '1'; 
                        end if;
                    end if;

                    -- Depending on the alignment have we been able to made a complete word, and put it on the bus ?              
                    if axi_target_data_valid = '1' then

                        xfer_to_axi_data        <=  axi_target_data;
                        xfer_to_axi_data_valid  <=  '1';    


                        -- Stop poping data from the FIFO when all the SRIO word necessary for satisfying the current
                        --	transaction has been retrieved
                        if axi_max_current_reached = '1' then
                            inboundReadContent_request    <= '0';

                        -- But if there is still data needed to complete the transaction
                        --	and there is still place in the AXI FIFO
                        elsif  xfer_to_axi_full= '0' then
                            inboundReadContent_request    <= '1';  

                        -- .. but if the slave is not ready, we make a break
                        else 
                            inboundReadContent_request    <= '0';  
                        end if;


                        -- Do we have reach the end of the frame we intended to transmit ?
                        if master_axi_wlast = '1'  then

                            -- Next data is not valid any more (it is the final checksum of the SRIO frame)
                            xfer_to_axi_data_valid  <=  '0';    

                            -- Complete the transaction
                            recv_state              <=  STATE_RECV_ENDING_BURST;

                        -- Or if we are not at the end, has the slave been able to read this data ?
                        elsif xfer_to_axi_full = '0' then

                            -- However if the slave has accepted the data (meaning it has read the first word at least)
                            -- If we have not a valid data from the FIFO, it means we have an old data on the bus
                            -- So we need to wait for the updated version (1 cycle delay)
                            if inboundReadContentValid = '0' then
                                xfer_to_axi_data_valid       <=  '0';   
                            end if;

                            -- If data is not aligned, we save the part not used in the word we put on the bus
                            --  for the next word to be written during the next cycle
                            if recv_data_32bits_aligned = '0' then
                                axi_target_data_save <= inboundReadContentData_o(15 downto 0);
                            end if;

                        -- Slave is not ready, let's do a pause
                        else 
                            inboundReadContent_request <= '0';
                        end if;

                    -- If no data is valid, update the bus accordingly    
                    else

                        xfer_to_axi_data_valid      <=  '0';    
                        xfer_to_axi_data            <=  (others => 'U');

                    end if;

                    -- Count how many bytes we are transferring on the AXI side
                    if   xfer_to_axi_data_valid = '1' then
                        axi_xfrd_len            <= axi_xfrd_len + 4;

                    end if;
                    
                    -- Increment the total number of byte popped from the FIFO in order to detect the intermediary 
                    --  checksum.
                    -- NB: here we are really using 'inboundReadContent' and not 'inboundReadContent_request' as we
                    --  really want to know how many bytes have been popped and not how many we requested
                    if inboundReadContent = '1' then
                       recv_srio_byte_count    <= recv_srio_byte_count + 4;
                    end if;
                    
                when STATE_RECV_ENDING_BURST       =>  
                
                    -- No more valid transferred to the slave
                    inboundReadContent_request  <= '0';
                    xfer_to_axi_data_valid      <=  '0';    
                    xfer_to_axi_data            <=  (others => 'U');

                    -- Reset the number of retrieve SRIO word before initiating a new AXI
                    --  transfer
                    reset_srio_inbound_counter <= '1';

                    -- Do we have transferred all the data we wanted to ?
                    if axi_xfrd_len = axi_beat_len_total * 4 then

                        -- Ack the frame to the SRIO side
                        inboundReadFrame_i <= '1';

                        -- If it was the last segment, it's time to raise
                        --  an interrupt
                        if msg_seg_i = msg_length_i then

                            -- Compute the size of the message we have just completed
                            -- This is the size of the previous segment + the size of the last packet
                            -- msg_length_i is 0 when there is a single message
                            msg_total_length <= msg_length_i * ( DecodeSRioPacketSize(msg_size_i) +1 ) + std_logic_vector(axi_srio_payload_size);

                            -- Write the size of the message
                            xfer_to_axi_addr <= mbox_frame_base_address;
                            xfer_to_axi_size <= x"00";  
                            xfer_to_axi_addr_valid <= '1';
                            -- Interrupt has to be raised at the end of this writing
                            xfer_completion_request <= '1';

                            xfer_to_axi_data                            <=  (others => '0');
                            xfer_to_axi_data(msg_total_length'range)    <=  msg_length_i * ( DecodeSRioPacketSize(msg_size_i) +1 ) + std_logic_vector(axi_srio_payload_size);
                            xfer_to_axi_data_valid  <=  '1';

                            -- We erase the location reserved for this particular message
                            -- TODO: we don't manage out-of-order message.
                            --  it could be done using an indirection table indexed by each slot available
                            --  a counter is incremented each time a segment is received, when count equals
                            --  expected count, then we pass the message
                            loopkup_source_id           <= sourceId_i;
                            loopkup_mbox_slot_in        <= x"FF";
                            loopkup_source_id_wr        <= '1'; 

                            -- One more message has been received
                            reg_mbox_received_msg <= reg_mbox_received_msg + '1';

                            recv_state <=  STATE_RECV_NOTIFY;

                        else
                            recv_state <=  STATE_RECV_DONE;
                        end if;

                    -- More data have to be transfered
                    else 


                        -- How many beat still to be transferred ?
                        axi_beat_len_total_tmp    := axi_beat_len_total - axi_beat_len_current;

                        -- If we are above the max configured we have to limit the next transfer
                        --   to this maximum
                        if axi_beat_len_total_tmp > reg_beat_len_max then
                            axi_beat_len_current <= reg_beat_len_max;
                        else
                            axi_beat_len_current <= axi_beat_len_total_tmp;
                        end if;

                        -- Copy the eventual part of the word for restoring it at the next cycle
                        axi_target_data_save <= inboundReadContentData_o(15 downto 0);

                        -- Update the destination address
                        axi_target_address <= std_logic_vector(unsigned(axi_target_address) + axi_byte_len_current );

                        -- Save how many beat we have still to transfer
                        axi_beat_len_total    <= axi_beat_len_total_tmp;

                        -- Let's restart a new AXI transaction
                        recv_state <=  STATE_RECV_PUSH_FRAME_ADDR;

                    end if;

                when STATE_RECV_NOTIFY =>

                    xfer_to_axi_addr_valid <= '0';
                    xfer_to_axi_data_valid  <=  '0';

                    inboundReadFrame_i  <= '0';    

                    -- Stop deleting the slot which was reserved for this message
                    loopkup_source_id_wr        <= '0'; 
                    recv_state          <=  STATE_RECV_IDLE;


                when STATE_RECV_DONE =>
                    inboundReadFrame_i  <= '0';    
                    
                    recv_state          <=  STATE_RECV_IDLE;

                -- Waiting for the discarding to be taken into account
                when STATE_RECV_DISCARDING_SRIO_FRAME =>
                    reg_err_dropped_frame <= reg_err_dropped_frame + 1;
                    inboundReadFrame_i  <= '0';    
                    recv_state          <=  STATE_RECV_IDLE;
                    
                when others => 
                    recv_state <=  STATE_RECV_IDLE;
                    
            
            end case;
        end if;
    end process;
    
      
    
    --
    -- Combinatorial path 
    --
    S_AXI_AWREADY   <= slave_axi_awready;
    S_AXI_WREADY    <= slave_axi_wready;
    S_AXI_BRESP     <= slave_axi_bresp;
    S_AXI_BVALID    <= slave_axi_bvalid;
    S_AXI_ARREADY   <= slave_axi_arready;
    S_AXI_RVALID    <= slave_axi_rvalid;
    S_AXI_RRESP     <= slave_axi_rresp;
    S_AXI_RDATA     <= slave_axi_rdata;
    
    slave_axi_wvalid <= S_AXI_WVALID;
    slave_axi_bready <= S_AXI_BREADY;
    

    -- Detect any write or read access ready to be processed, only if we are not completing an access
    register_access <= (register_access_wr or register_access_rd);
    accessed_register <= slave_axi_araddr_latched(7 downto 0) when register_access_rd = '1' else slave_axi_awaddr_latched(7 downto 0) ;



    --process(clk)
    --begin
--
        --if rising_edge(clk) then
--
            ---- As soon we have accept the addres, we latch the register address
            --if  register_addr_rd = '1' then 
                --accessed_register <= slave_axi_araddr_latched(7 downto 0);
            --elsif register_addr_wr ='1' then 
                --accessed_register <= slave_axi_awaddr_latched(7 downto 0);
            --end if ;
--
--
        --end if;
--
    --end process;
--

    -- ******************************************************************************
    --
    -- Provide configuration register access
    --
    -- ******************************************************************************
    ConfigProvider: process (clk, rst_n)
    begin
        if rst_n = '0' then

            --
            -- Default register values
            --
            reg_srio_target_id          <= (others => '0'); 
            reg_srio_source_id          <= (others => '0');
            reg_srio_target_addr        <= (others => '0'); 
            reg_srio_source_addr        <= (others => '0'); 
            reg_srio_target_mbox        <= (others => '0'); 
            reg_axi_mbox_addr           <= (others => '1');
            reg_axi_mbox_slot_mask      <= (others => '0');
            reg_axi_mbox_addr_update    <= '0';
            reg_axi_burst_size          <= EncodeBurstSize(BURST_004);
            reg_beat_len_max            <= x"0F";
            reg_srio_maint_timeout      <= x"000000FF";

            -- Defaulting to 16 bits identifier
            reg_srio_id_len          <= "01";

            -- Use to start a message transfert
            master_xfer_start           <= '0'; 

            -- Use to report interrupt acknowledge
            intr_message_complete_done <= '0';

            register_access_wr_done     <= '0';
            register_access_rd_done     <= '0';

            register_processing_deferred_wr <= '0';
            register_processing_deferred_rd <= '0';

            register_access_wr_error        <= '0';


        elsif rising_edge(clk) then

            -- We supposed the data will be returned by default
            register_access_rd_done <= register_access and register_access_rd;
            register_access_wr_done <= register_access and register_access_wr;

            -- Asserted only once
            master_xfer_start <= '0';

            -- FIXME !!
            if reg_axi_mbox_addr_update_done = '1' then
                reg_axi_mbox_addr_update <= '0';
            end if;

            -- Clear request asserted just one cycle
            intr_message_complete_done  <= '0';
            intr_message_xmited_done  <= '0';
            register_access_remote_rd <= '0';
            register_access_remote_wr <= '0';



            -- Any access ?
            if register_access = '1' then 

                -- Default value for unknown register
                register_get_value <= x"BADACCE5";

                register_access_wr_error  <= '0';

                register_access_remote_wr <= '0';

                case (accessed_register) is


                    ---------------------------------------------------------------------------    
                    -- Device Magic ID: Good SRIO
                    --
                    ---------------------------------------------------------------------------    
                    when x"00" =>
                        if ( register_access_rd = '1' ) then
                            register_get_value <= x"600D0510";
                        end if;
                      

                    ---------------------------------------------------------------------------    
                    -- Version identification
                    -- 00 - Major - Minor - BugFix
                    ---------------------------------------------------------------------------    
                    when x"04" =>
                        register_get_value <= x"00000401";


                    ---------------------------------------------------------------------------    
                    -- Remote address where the next DMA transfer must be written
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"08" =>
                        if ( register_access_wr = '1' ) then
                            reg_srio_target_addr <= register_set_value;
                        else
                            register_get_value <= reg_srio_target_addr;
                        end if;
                        
                    ---------------------------------------------------------------------------    
                    -- Local address where the next memory transfer must be read from 
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"0C" =>
                        if ( register_access_wr = '1' ) then
                            reg_srio_source_addr <= register_set_value;
                        else
                            register_get_value <= reg_srio_source_addr;
                        end if;
                        
                    ---------------------------------------------------------------------------    
                    -- Size of the message to be read from the local memory.
                    -- Any write triggers the data transfer
                    ---------------------------------------------------------------------------    
                    when x"10" =>
                        if ( register_access_wr = '1' ) then

                            -- The size has always to be multiple of 32 bits
                            reg_srio_source_size <= register_set_value(reg_srio_source_size'left downto 2) & "00";
                            master_xfer_start <= '1';
                        else
                            register_get_value <= (others => '0');
                        end if;

                        
                    ---------------------------------------------------------------------------    
                    -- Returns the source and target ID of the next RIO packet
                    -- 
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"14" =>
                        if ( register_access_wr = '1' ) then
                            reg_srio_target_id <= register_set_value(15 downto 0);
                            reg_srio_source_id <= register_set_value(31 downto 16);
                        else
                            register_get_value <= reg_srio_source_id & reg_srio_target_id;
                        end if;

                    ---------------------------------------------------------------------------    
                    --  Length of the RapidIO identifiers to be used: 8 or 16bits
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"18"          => 
                        if ( register_access_wr = '1' ) then
                            reg_srio_id_len <= '0' & register_set_value(0);
                        else
                            register_get_value(1 downto 0)  <= reg_srio_id_len;
                        end if;


                    ---------------------------------------------------------------------------    
                    -- Remote mailbox number where the next message must be written into
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"1C" => 
                        if ( register_access_wr = '1' ) then
                            reg_srio_target_mbox <=  register_set_value(7 downto 0);
                        else
                            register_get_value( 7 downto 0) <= reg_srio_target_mbox;
                        end if;


                    ---------------------------------------------------------------------------    
                    -- Local memory address where the incoming mailbox message must be written to 
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"20" =>
                        if ( register_access_wr = '1' ) then
                            reg_axi_mbox_addr           <= register_set_value;
                            reg_axi_mbox_addr_update    <= '1';
                        else
                            register_get_value <= reg_axi_mbox_addr;
                        end if;


                    ---------------------------------------------------------------------------    
                    -- Size of the local mailbox memory
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"24" =>
                        if ( register_access_wr = '1' ) then
                            reg_axi_mbox_slot_mask           <= register_set_value;
                            reg_axi_mbox_addr_update    <= '1';     
                        else
                            register_get_value <= reg_axi_mbox_slot_mask;
                        end if;


                    ---------------------------------------------------------------------------    
                    -- AXI transfer configuration 
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"28" =>
                        if ( register_access_wr = '1' ) then
                            reg_beat_len_max           <= unsigned(register_set_value(7 downto 0)); 
                            reg_axi_burst_size         <= register_set_value(10 downto 8); 


                        else
                            register_get_value              <= (others => '0');
                            register_get_value(10 downto 0) <= reg_axi_burst_size & std_logic_vector(reg_beat_len_max);
                        end if;


                    ---------------------------------------------------------------------------    
                     -- Free register
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"2C" =>
                         null;

                    ---------------------------------------------------------------------------    
                    -- SRIO link status report 
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"30" =>
                         if ( register_access_rd = '1' ) then
                             register_get_value(13 downto 0 ) <=     port_initialized_i & 
                                                                            Nx_mode_active_i & 
                                                                            mgt_pll_locked_i &                                              
                                                                            rxelecidle_i &
                                                                            linkInitialized_i;
                         end if;
                    ---------------------------------------------------------------------------    
                    -- Interrupt status register 
                    -- 
                    ---------------------------------------------------------------------------    
                     when x"34" =>
                         if ( register_access_wr = '1' ) then
                             intr_message_complete_done  <= register_set_value(0);
                             intr_message_xmited_done    <= register_set_value(1);
                         else
                             register_get_value(31 downto 1)    <= (others => '0');
                             register_get_value(0)              <= intr_message_complete;
                             register_get_value(1)              <= intr_message_xmited;
                         end if;

                        
                     
                    ---------------------------------------------------------------------------    
                    -- Number of errors on AXI 
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"40" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_err_axi_write;
                         end if;
                        
                    ---------------------------------------------------------------------------    
                    -- Number of dropped frame
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"44" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_err_dropped_frame;
                         end if;

                    ---------------------------------------------------------------------------    
                    -- Number of received frame
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"48" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_err_received_frame;
                         end if;

                    ---------------------------------------------------------------------------    
                    -- Number of received MESSAGE
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"4C" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_mbox_received_msg;
                         end if;

                    ---------------------------------------------------------------------------    
                    -- Number of transmitted MESSAGE
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"50" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_mbox_xmited_msg;
                         end if;

                    ---------------------------------------------------------------------------    
                    -- Number of dropped maintenance response
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"54" =>                     
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_err_dropped_maint;
                         end if;

                    ---------------------------------------------------------------------------    
                    -- Address of next maintenance register accessed when a write or read will
                    --  be done at register 0xA4
                    -- 
                    ---------------------------------------------------------------------------    
                    when x"A0" =>
                         if ( register_access_rd = '1' ) then
                             register_get_value <= reg_srio_maint_config_addr;
                         else
                             reg_srio_maint_config_addr <=  register_set_value(reg_srio_maint_config_addr'left downto 2) & "00";
                         end if;


                    ---------------------------------------------------------------------------    
                    -- Any write or read access triggers a remote maintenance resgiter access
                    --
                    ---------------------------------------------------------------------------    
                    when x"A4" =>

                        if ( register_access_rd = '1' and register_processing_deferred_rd = '0' ) then
                            
                            -- Trigger a remote access
                            register_access_remote_rd <= '1';

                            -- On going processing started
                            register_processing_deferred_rd <= '1';


                        elsif  ( register_access_wr = '1' and register_processing_deferred_wr = '0') then

                            -- Trigger a remote access
                            register_access_remote_wr <= '1';

                            -- On going processing started
                            register_processing_deferred_wr <= '1';


                        end if;

                        -- We are unable to provide the answer for now
                        register_access_wr_done <= '0';
                        -- We are unable to provide the answer for now
                        register_access_rd_done <= '0';


                    when others =>
                        if ( register_access_rd = '1' ) then
                             register_get_value <= x"BADACCE5";
                        end if;

                end case; 


                --==============================================================
                --
                -- Common processing for all deferred write and read
                --
                --==============================================================


            else -- if register_access = '1' then 

                -- Do we have a pending read waiting an answer ?
                if register_processing_deferred_rd = '1' then 

                    if register_access_remote_rd_done = '1' then
                        register_access_rd_done             <= '1';
                        register_processing_deferred_rd     <= '0';
                        register_get_value                  <= register_remote_get_value;
                    else
                        register_access_rd_done             <= '0';
                        register_processing_deferred_rd     <= '1';
                    end if;

                -- Do we have a pending write waiting for acknowledge ?
                elsif register_processing_deferred_wr = '1' then

                    if register_access_remote_wr_done = '1' then

                        register_access_wr_done         <= '1';
                        register_access_wr_error        <= register_access_remote_wr_error;
                        register_processing_deferred_wr <= '0';

                    else
                        register_access_wr_done <= '0';
                    end if;
                end if;
            end if;

        end if;
    end process;

    -- ******************************************************************************
    --
    -- Manage AXI Read channel protocol
    --
    -- ******************************************************************************
    SlaveReadStateTransition: process( clk, rst_n )
    begin

		if rst_n = '0' then

            axil_read_state <= STATE_AXIL_RD_IDLE;
            slave_axi_araddr_latched <= (others => 'U');
            register_addr_rd        <= '0';
            axil_read_state         <= STATE_AXIL_RD_IDLE;
            slave_axi_arready       <= '0';
            register_access_rd      <= '0';
            slave_axi_rvalid        <= '0';
            slave_axi_rdata         <= (others => 'U');
            slave_axi_rresp         <= (others => 'U');

        elsif rising_edge(clk) then

            axil_read_state         <= axil_read_state_next;
            slave_axi_araddr_latched <= slave_axi_araddr_latched_next;
            register_addr_rd        <= register_addr_rd_next;
            slave_axi_arready       <= slave_axi_arready_next;
            register_access_rd      <= register_access_rd_next;
            slave_axi_rvalid        <= slave_axi_rvalid_next;
            slave_axi_rdata         <= slave_axi_rdata_next;
            slave_axi_rresp         <= slave_axi_rresp_next;

        end if;


    end process;

    SlaveReadState: process( rst_n,axil_read_state, S_AXI_ARVALID, S_AXI_RREADY, register_access_rd_done  )
    begin

        if rst_n = '0' then

            slave_axi_araddr_latched_next   <= (others => 'U');
            slave_axi_arready_next          <= '0';
            register_access_rd_next         <= '0';
            register_addr_rd_next           <= '0';
            axil_read_state_next            <= STATE_AXIL_RD_IDLE;
            slave_axi_rdata_next            <= (others => 'U');
            slave_axi_rresp_next            <= EncodeRespCode(OKAY);
            slave_axi_rvalid_next           <= '0';

        else

            -- Prevent latches
            axil_read_state_next            <= STATE_AXIL_RD_IDLE;
            slave_axi_arready_next          <= '0';
            register_access_rd_next         <= '0';
            register_addr_rd_next           <= '0';
            slave_axi_rvalid_next           <= '0';
            slave_axi_rdata_next            <= (others => 'U');
            slave_axi_rresp_next            <= EncodeRespCode(OKAY);


            case (axil_read_state ) is

                -- Waiting for an incoming request
                when STATE_AXIL_RD_IDLE =>

                    -- No valid data on the bus while idling
                    slave_axi_rvalid_next <= '0';                    

                    -- Address is ready, latch the address
                    if S_AXI_ARVALID = '1' then 

                        slave_axi_araddr_latched_next <= S_AXI_ARADDR;

                        -- Address is ready to be processed
                        register_addr_rd_next <= '1';

                        -- Inform the master we have read the address
                        slave_axi_arready_next <= '1';

                        -- Start to process a read transaction
                        register_access_rd_next <= '1';

                        -- Start to wait for the data being available
                        axil_read_state_next <= STATE_AXIL_RD_WAITING_DATA; 

                    -- No address to be processed
                    else 

                        -- Invalidate any previous value
                        slave_axi_araddr_latched_next <= (others => 'U');
                        register_addr_rd_next <= '0';

                        -- No read address
                        slave_axi_arready_next <= '0';

                        -- No read being processed
                        register_access_rd_next <= '0';

                        -- Staying idling
                        axil_read_state_next <= STATE_AXIL_RD_IDLE; 

                    end if;

                -- Read request being processed by ConfigProvider
                when STATE_AXIL_RD_WAITING_DATA =>

                    -- Deassert the confirmation we have read the address
                    slave_axi_arready_next <= '0';

                    -- Still processing a reading
                    -- Single shot
                    register_access_rd_next <= '0'; -- ASA

                    -- Has ConfigProvided reported, it has returned the data ?
                    if register_access_rd_done = '1' then

                        -- Put the returned value on the bus
                        slave_axi_rdata_next <= register_get_value;

                        -- By default everything is supposed to be ok.
                        slave_axi_rresp_next <= EncodeRespCode(OKAY);

                        -- By default data is considered as valid
                        slave_axi_rvalid_next <= '1';                    

                        -- Start to wait for the Master having confirmed our status reception
                        axil_read_state_next <= STATE_AXIL_RD_WAITING_ACK;

                    -- Still no data...
                    else
                        slave_axi_rdata_next <= (others => 'U');
                        slave_axi_rresp_next <= EncodeRespCode(OKAY);
                        slave_axi_rvalid_next <= '0';                    
                        axil_read_state_next <= STATE_AXIL_RD_WAITING_DATA;
                    end if;

                -- Wait for the master having confirmed the end of the transaction
                when STATE_AXIL_RD_WAITING_ACK =>

                    -- No more transaction requested
                    register_access_rd_next <= '0';


                    -- Master has accepted the data
                    if S_AXI_RREADY = '1' then

                        -- Data is not valid anymore
                        slave_axi_rvalid_next <= '0';                    

                        -- Go back to wait new request
                        axil_read_state_next <= STATE_AXIL_RD_IDLE;
                    else
                        -- maintain the data on the bus
                        slave_axi_rdata_next <= register_get_value;

                        slave_axi_rvalid_next <= '1';                    
                        axil_read_state_next <= STATE_AXIL_RD_WAITING_ACK;
                    end if;

            end case;

        end if;


    end process;


    -- ******************************************************************************
    --
    -- Manage AXI Write channel protocol
    --
    -- ******************************************************************************
    SlaveWrStateTransition: process( clk, rst_n )
    begin

		if rst_n = '0' then

            axil_wr_state               <= STATE_AXIL_WR_IDLE;
            slave_axi_awaddr_latched    <= (others => 'U');
            register_addr_wr            <= '0';
            register_set_value          <= (others => 'U');
            register_access_wr          <= '0';
            slave_axi_wready            <= '0';
            slave_axi_awready           <= '0';
            slave_axi_bvalid            <= '0';
            slave_axi_bresp             <= EncodeRespCode(OKAY); 

        elsif rising_edge(clk) then

            axil_wr_state               <= axil_wr_state_next;
            slave_axi_awaddr_latched    <= slave_axi_awaddr_latched_next;
            register_addr_wr            <= register_addr_wr_next;
            register_access_wr          <= register_access_wr_next;
            slave_axi_wready            <= slave_axi_wready_next;
            slave_axi_awready           <= slave_axi_awready_next;
            slave_axi_bvalid            <= slave_axi_bvalid_next;
            slave_axi_bresp             <= slave_axi_bresp_next; 
            register_set_value          <= register_set_value_next;

        end if;

    end process;


    SlaveWrState: process( rst_n, axil_wr_state, S_AXI_AWVALID, 
        register_access_wr_done, register_access_wr_error, 
        slave_axi_wvalid, slave_axi_bready  )
    begin
        
        if rst_n = '0' then

            slave_axi_wready_next       <= '0';                    
            register_access_wr_next     <= '0';
            register_addr_wr_next       <= '0';
            slave_axi_awready_next      <= '0';
            slave_axi_awaddr_latched_next <= (others => 'U');
            register_addr_wr_next       <= '0';
            slave_axi_bvalid_next       <= '0';
            slave_axi_bresp_next        <= EncodeRespCode(OKAY);

            axil_wr_state_next          <= STATE_AXIL_WR_IDLE; 

            register_set_value_next     <= (others => 'U');


        else

            -- Prevent latches
            axil_wr_state_next              <= STATE_AXIL_WR_IDLE;
            register_addr_wr_next           <= '0';
            register_access_wr_next         <= '0';
            slave_axi_wready_next           <= '0';                    
            slave_axi_awready_next          <= '0';
            slave_axi_bvalid_next           <= '0';
            slave_axi_bresp_next            <= EncodeRespCode(OKAY);

            case (axil_wr_state ) is

                -- Waiting for an incoming request
                when STATE_AXIL_WR_IDLE =>

                    -- Address is ready, latch the address
                    if S_AXI_AWVALID = '1' then 

                        slave_axi_awaddr_latched_next <= S_AXI_AWADDR;
                        register_addr_wr_next       <= '1';

                        -- And inform we have read the address
                        slave_axi_awready_next <= '1';


                        -- Waiting for the data being written
                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_DATA; 

                    else 

                        slave_axi_awaddr_latched_next   <= (others => 'U');

                        slave_axi_awready_next <= '0';
                        register_access_wr_next <= '0';

                        axil_wr_state_next <= STATE_AXIL_WR_IDLE; 

                    end if;

                when STATE_AXIL_WR_WAITING_DATA =>

                    -- Master has presented the data to be written
                    if slave_axi_wvalid = '1' then
                        register_set_value_next <= S_AXI_WDATA;

                        -- Move to Write Register Access
                        register_access_wr_next <= '1';

                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_ACK; 

                    else

                        register_set_value_next <= (others => 'U');

                        register_access_wr_next <= '0';

                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_DATA; 

                    end if;


                when STATE_AXIL_WR_WAITING_ACK =>

                    register_access_wr_next <= '0';
                    if register_access_wr_done = '1' then

                        -- If we have an error, we report it
                        slave_axi_bvalid_next <= '1';
                        if register_access_wr_error = '1' then
                            slave_axi_bresp_next<= EncodeRespCode(SLVERR);
                        else
                            slave_axi_bresp_next <= EncodeRespCode(OKAY);

                        end if;

                        slave_axi_wready_next <= '1';

                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_BREADY;

                    else
                        slave_axi_wready_next <= '0';                    

                        slave_axi_bvalid_next <= '0';
                        slave_axi_bresp_next <= EncodeRespCode(OKAY);

                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_ACK;
                    end if;

                when STATE_AXIL_WR_WAITING_BREADY =>

                    -- No more transaction requested
                    register_access_wr_next <= '0';

                    if slave_axi_bready = '1' then
                        axil_wr_state_next <= STATE_AXIL_WR_IDLE;
                        slave_axi_bvalid_next <= '0';
                    else
                        axil_wr_state_next <= STATE_AXIL_WR_WAITING_BREADY;
                        slave_axi_bvalid_next <= '1';
                    end if;

            end case;
        end if;


    end process;


    RegisterAccessRemote: process(rst_n, clk)
    begin
        if rst_n = '0' then

            reg_access_state               <= STATE_REG_ACCESS_IDLE;
            master_maint_start_wr       <= '0';
            master_maint_start_rd       <= '0';


        elsif rising_edge(clk) then

            case (reg_access_state ) is

                when STATE_REG_ACCESS_IDLE =>

                    -- Hold on reset timeout counter when there is no ongoing transaction
                    register_access_remote_timeout_counter <= (others => '0' );
                    register_access_remote_wr_error        <= '0';
                    register_access_remote_rd_done         <= '0';
                    register_access_remote_wr_done         <= '0';

                    if register_access_remote_rd = '1' then

                        master_maint_start_rd  <= '1';
                        reg_access_state <= STATE_REG_ACCESS_RD;

                    elsif register_access_remote_wr = '1' then

                        -- Start writing to the remote the specified value
                        master_maint_start_wr  <= '1';
                        reg_srio_maint_config_data <= register_set_value;
                        reg_access_state <= STATE_REG_ACCESS_WR;

                    else

                        reg_access_state <= STATE_REG_ACCESS_IDLE;
                    end if;


                when STATE_REG_ACCESS_RD =>

                    -- Always a single pulse
                    master_maint_start_rd  <= '0';

                    register_access_remote_rd_done <= '1';
                    reg_access_state <= STATE_REG_ACCESS_IDLE;

                    -- if we have received the answer we can return
                    if master_maint_response = '1' then

                        if reg_srio_maint_config_addr(2) = '0' then
                            register_remote_get_value <= reg_srio_maint_config_data_ans(63 downto 32);
                        else
                            register_remote_get_value <= reg_srio_maint_config_data_ans(31 downto 0);
                        end if;

                    -- If the timeout has expired we have to return
                    elsif register_access_remote_timeout_counter = reg_srio_maint_timeout then

                        -- Let's return an error, and we complete the transaction
                        register_remote_get_value <= x"BADACCE5";

                    -- Wait for more time
                    -- We don't change the register_access_rd_done as we may have just received the result
                    else
                        register_access_remote_timeout_counter <= register_access_remote_timeout_counter + '1';
                        register_access_remote_rd_done <= '0';
                        reg_access_state <= STATE_REG_ACCESS_RD;
                    end if;

                when STATE_REG_ACCESS_WR =>

                    -- Always a single pulse
                    master_maint_start_wr  <= '0';

                    -- By default, we suppose we are going to complete the transaction and
                    --  resume to idle
                    reg_access_state <= STATE_REG_ACCESS_IDLE;
                    register_access_remote_wr_done <= '1';

                    -- if we have received the ack we can return
                    if master_maint_response = '1' then

                        register_access_remote_wr_error <= '0';

                    -- If the timeout has expired we have to return
                    elsif register_access_remote_timeout_counter = reg_srio_maint_timeout then

                        -- Let's return an error, and we complete the transaction
                        register_access_remote_wr_error <= '1';

                    -- Wait for more time
                    else
                        register_access_remote_wr_error <= '0';
                        register_access_remote_timeout_counter <= register_access_remote_timeout_counter + '1';
                        register_access_remote_wr_done <= '0';
                        reg_access_state <= STATE_REG_ACCESS_WR;
                    end if;

                when others =>
                        reg_access_state <= STATE_REG_ACCESS_IDLE;


            end case;

        end if;

    end process;

end Behavioral;



