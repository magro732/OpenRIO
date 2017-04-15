-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains commonly used types, functions, procedures and entities used in
-- the RapidIO IP library project.
-- 
-- To Do:
-- -
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2013 Authors and OPENCORES.ORG 
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


-------------------------------------------------------------------------------
-- RioCommon library.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;


-------------------------------------------------------------------------------
-- RioCommon package description.
-------------------------------------------------------------------------------
package rio_common is

  -----------------------------------------------------------------------------
  -- Primitive memory component declarations.
  -----------------------------------------------------------------------------
  
  component MemorySimpleDualPort
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1);
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

      clkB_i : in std_logic;
      enableB_i : in std_logic;
      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;

  component MemoryDualPort is
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1);
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      writeEnableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
      dataA_o : out std_logic_vector(DATA_WIDTH-1 downto 0);

      clkB_i : in std_logic;
      enableB_i : in std_logic;
      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;
  
  component Crc16CITT is
    port(
      d_i : in  std_logic_vector(15 downto 0);
      crc_i : in  std_logic_vector(15 downto 0);
      crc_o : out std_logic_vector(15 downto 0));
  end component;

  -----------------------------------------------------------------------------
  -- Logical layer component declarations.
  -----------------------------------------------------------------------------

  component RioLogicalCommon is
    generic(
      PORTS : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;
      
      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);

      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0);

      inboundStb_o : out std_logic;
      inboundAdr_o : out std_logic_vector(3 downto 0);
      inboundDat_o : out std_logic_vector(31 downto 0);
      inboundStall_i : in std_logic;
      
      outboundStb_i : in std_logic_vector(PORTS-1 downto 0);
      outboundAdr_i : in std_logic_vector(PORTS-1 downto 0);
      outboundDat_i : in std_logic_vector(32*PORTS-1 downto 0);
      outboundStall_o : out std_logic_vector(PORTS-1 downto 0));
  end component;

  component RioLogicalMaintenance is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      readRequestReady_i : in std_logic;
      writeRequestReady_i : in std_logic;
      size_i : in std_logic_vector(3 downto 0);
      offset_i : in std_logic_vector(20 downto 0);
      wdptr_i : in std_logic;
      payloadLength_i : in std_logic_vector(2 downto 0);
      payloadIndex_o : out std_logic_vector(2 downto 0);
      payload_i : in std_logic_vector(63 downto 0);
      done_o : out std_logic;
      
      readResponseReady_o : out std_logic;
      writeResponseReady_o : out std_logic;
      status_o : out std_logic_vector(3 downto 0);
      payloadLength_o : out std_logic_vector(2 downto 0);
      payloadIndex_i : in std_logic_vector(2 downto 0);
      payload_o : out std_logic_vector(63 downto 0);
      done_i : in std_logic;
      
      configStb_o : out std_logic;
      configWe_o : out std_logic;
      configAdr_o : out std_logic_vector(21 downto 0);
      configDat_o : out std_logic_vector(31 downto 0);
      configDat_i : in std_logic_vector(31 downto 0);
      configAck_i : in std_logic);
  end component;
  
  component MaintenanceInbound is
    generic(
      ENABLE_READ_REQUEST : boolean := true;
      ENABLE_WRITE_REQUEST : boolean := true;
      ENABLE_READ_RESPONSE : boolean := true;
      ENABLE_WRITE_RESPONSE : boolean := true;
      ENABLE_PORT_WRITE : boolean := true);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      readRequestReady_o : out std_logic;
      writeRequestReady_o : out std_logic;
      readResponseReady_o : out std_logic;
      writeResponseReady_o : out std_logic;
      portWriteReady_o : out std_logic;
      
      vc_o : out std_logic;
      crf_o : out std_logic;
      prio_o : out std_logic_vector(1 downto 0);
      tt_o : out std_logic_vector(1 downto 0);
      dstid_o : out std_logic_vector(31 downto 0);
      srcid_o : out std_logic_vector(31 downto 0);
      size_o : out std_logic_vector(3 downto 0);
      status_o : out std_logic_vector(3 downto 0);
      tid_o : out std_logic_vector(7 downto 0);
      hop_o : out std_logic_vector(7 downto 0);
      offset_o : out std_logic_vector(20 downto 0);
      wdptr_o : out std_logic;
      payloadLength_o : out std_logic_vector(2 downto 0);
      payloadIndex_i : in std_logic_vector(2 downto 0);
      payload_o : out std_logic_vector(63 downto 0);
      done_i : in std_logic;
      
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(3 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundStall_o : out std_logic);
  end component;
  
  component RequestClassInbound is
    generic(
      EXTENDED_ADDRESS : natural range 0 to 2 := 0);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      nreadReady_o : out std_logic;
      
      vc_o : out std_logic;
      crf_o : out std_logic;
      prio_o : out std_logic_vector(1 downto 0);
      tt_o : out std_logic_vector(1 downto 0);
      dstId_o : out std_logic_vector(31 downto 0);
      srcId_o : out std_logic_vector(31 downto 0);
      tid_o : out std_logic_vector(7 downto 0);
      address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      length_o : out std_logic_vector(4 downto 0);
      select_o : out std_logic_vector(7 downto 0);
      done_i : in std_logic;
      
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(3 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundStall_o : out std_logic);
  end component;

  component WriteClassInbound is
    generic(
      ENABLE_NWRITE : boolean := true;
      ENABLE_NWRITER : boolean := true;
      EXTENDED_ADDRESS : natural range 0 to 2 := 0);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      nwriteReady_o : out std_logic;
      nwriterReady_o : out std_logic;
      
      vc_o : out std_logic;
      crf_o : out std_logic;
      prio_o : out std_logic_vector(1 downto 0);
      tt_o : out std_logic_vector(1 downto 0);
      dstId_o : out std_logic_vector(31 downto 0);
      srcId_o : out std_logic_vector(31 downto 0);
      tid_o : out std_logic_vector(7 downto 0);
      address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      length_o : out std_logic_vector(4 downto 0);
      select_o : out std_logic_vector(7 downto 0);
      payloadIndex_i : in std_logic_vector(4 downto 0);
      payload_o : out std_logic_vector(63 downto 0);
      done_i : in std_logic;
      
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(3 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundStall_o : out std_logic);
  end component;

  component MaintenanceOutbound is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      readRequestReady_i : in std_logic;
      writeRequestReady_i : in std_logic;
      readResponseReady_i : in std_logic;
      writeResponseReady_i : in std_logic;
      portWriteReady_i : in std_logic;
      
      vc_i : in std_logic;
      crf_i : in std_logic;
      prio_i : in std_logic_vector(1 downto 0);
      tt_i : in std_logic_vector(1 downto 0);
      dstid_i : in std_logic_vector(31 downto 0);
      srcid_i : in std_logic_vector(31 downto 0);
      size_i : in std_logic_vector(3 downto 0);
      status_i : in std_logic_vector(3 downto 0);
      tid_i : in std_logic_vector(7 downto 0);
      hop_i : in std_logic_vector(7 downto 0);
      offset_i : in std_logic_vector(20 downto 0);
      wdptr_i : in std_logic;
      payloadLength_i : in std_logic_vector(2 downto 0);
      payloadIndex_o : out std_logic_vector(2 downto 0);
      payload_i : in std_logic_vector(63 downto 0);
      done_o : out std_logic;
      
      outboundStb_o : out std_logic;
      outboundAdr_o : out std_logic;
      outboundDat_o : out std_logic_vector(31 downto 0);
      outboundStall_i : in std_logic);
  end component;

  component ResponseClassOutbound is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      doneNoPayloadReady_i : in std_logic;
      doneWithPayloadReady_i :  in std_logic;
      errorReady_i : in std_logic;
      
      vc_i : in std_logic;
      crf_i : in std_logic;
      prio_i : in std_logic_vector(1 downto 0);
      tt_i : in std_logic_vector(1 downto 0);
      dstid_i : in std_logic_vector(31 downto 0);
      srcid_i : in std_logic_vector(31 downto 0);
      tid_i : in std_logic_vector(7 downto 0);
      payloadLength_i : in std_logic_vector(4 downto 0);
      payloadIndex_o : out std_logic_vector(4 downto 0);
      payload_i : in std_logic_vector(63 downto 0);
      done_o : out std_logic;
      
      outboundStb_o : out std_logic;
      outboundAdr_o : out std_logic;
      outboundDat_o : out std_logic_vector(31 downto 0);
      outboundStall_i : in std_logic);
  end component;

  component RioFrameBuffer is
    generic(
      SIZE_ADDRESS_WIDTH : natural := 6;
      CONTENT_ADDRESS_WIDTH : natural := 8;
      CONTENT_WIDTH : natural := 32;
      MAX_PACKET_SIZE : natural := 69);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      inboundWriteFrameFull_o : out std_logic;
      inboundWriteFrame_i : in std_logic;
      inboundWriteFrameAbort_i : in std_logic;
      inboundWriteContent_i : in std_logic;
      inboundWriteContentData_i : in std_logic_vector(CONTENT_WIDTH-1 downto 0);
      inboundReadFrameEmpty_o : out std_logic;
      inboundReadFrame_i : in std_logic;
      inboundReadFrameRestart_i : in std_logic;
      inboundReadFrameAborted_o : out std_logic;
      inboundReadFrameSize_o : out std_logic_vector(CONTENT_ADDRESS_WIDTH-1 downto 0);
      inboundReadContentEmpty_o : out std_logic;
      inboundReadContent_i : in std_logic;
      inboundReadContentEnd_o : out std_logic;
      inboundReadContentData_o : out std_logic_vector(CONTENT_WIDTH-1 downto 0);
      
      outboundWriteFrameFull_o : out std_logic;
      outboundWriteFrame_i : in std_logic;
      outboundWriteFrameAbort_i : in std_logic;
      outboundWriteContent_i : in std_logic;
      outboundWriteContentData_i : in std_logic_vector(CONTENT_WIDTH-1 downto 0);
      outboundReadFrameEmpty_o : out std_logic;
      outboundReadFrame_i : in std_logic;
      outboundReadFrameRestart_i : in std_logic;
      outboundReadFrameAborted_o : out std_logic;
      outboundReadFrameSize_o : out std_logic_vector(CONTENT_ADDRESS_WIDTH-1 downto 0);
      outboundReadContentEmpty_o : out std_logic;
      outboundReadContent_i : in std_logic;
      outboundReadContentEnd_o : out std_logic;
      outboundReadContentData_o : out std_logic_vector(CONTENT_WIDTH-1 downto 0));
  end component;

  component Timer is
  generic(timerWidth    : positive := 16;      -- After 'resetValue' number of 'timerTick's, Expired gets high! A brief '1' on wd preload the timer counter.
          repeatedPulse : boolean  := true);   -- if False, a steady '1' is present on Expired after timeout elapsed. if True, a pulse on Expired after each timeout period.
     port(clk_i : in  std_logic;               -- the clk input..
       reset_ni : in  std_logic;               -- apply '0' for reset
   resetValue_i : in  std_logic_vector(timerWidth-1 downto 0); -- on each wd_i pulse, this is the value that load into counter. 
    timerTick_i : in  std_logic;               -- apply Ticker pulse on this signal
           wd_i : in  std_logic;               -- a '1' restart the timer.
      expired_o : out std_logic);              -- '1' when timer has expired
  end component;

  component Ticker is
  generic(clksPerTick : positive := 10);       -- select based on clk rate and desired tick rate.
     port(clk_i : in  std_logic;               -- for example: 25MHz/10 = 2.5 MHz. 
       reset_ni : in  std_logic;               -- reset when '0'
         tick_o : out std_logic);              -- pulsed high one clk each clksPerTick clk's
  end component;


  
  -----------------------------------------------------------------------------
  -- Commonly used types.
  -----------------------------------------------------------------------------
  type Array1 is array (natural range <>) of
    std_logic;
  type Array2 is array (natural range <>) of
    std_logic_vector(1 downto 0);
  type Array3 is array (natural range <>) of
    std_logic_vector(2 downto 0);
  type Array4 is array (natural range <>) of
    std_logic_vector(3 downto 0);
  type Array5 is array (natural range <>) of
    std_logic_vector(4 downto 0);
  type Array8 is array (natural range <>) of
    std_logic_vector(7 downto 0);
  type Array9 is array (natural range <>) of
    std_logic_vector(8 downto 0);
  type Array10 is array (natural range <>) of
    std_logic_vector(9 downto 0);
  type Array16 is array (natural range <>) of
    std_logic_vector(15 downto 0);
  type Array32 is array (natural range <>) of
    std_logic_vector(31 downto 0);
  type Array34 is array (natural range <>) of
    std_logic_vector(33 downto 0);

  -----------------------------------------------------------------------------
  -- Commonly used constants.
  -----------------------------------------------------------------------------
  
  -- Symbol types between the serial and the PCS layer.
  constant SYMBOL_IDLE : std_logic_vector(1 downto 0) := "00";
  constant SYMBOL_CONTROL : std_logic_vector(1 downto 0) := "01";
  constant SYMBOL_ERROR : std_logic_vector(1 downto 0) := "10";
  constant SYMBOL_DATA : std_logic_vector(1 downto 0) := "11";
  
  -- STYPE0 constants.
  constant STYPE0_PACKET_ACCEPTED : std_logic_vector(2 downto 0) := "000";
  constant STYPE0_PACKET_RETRY : std_logic_vector(2 downto 0) := "001";
  constant STYPE0_PACKET_NOT_ACCEPTED : std_logic_vector(2 downto 0) := "010";
  constant STYPE0_RESERVED : std_logic_vector(2 downto 0) := "011";
  constant STYPE0_STATUS : std_logic_vector(2 downto 0) := "100";
  constant STYPE0_VC_STATUS : std_logic_vector(2 downto 0) := "101";
  constant STYPE0_LINK_RESPONSE : std_logic_vector(2 downto 0) := "110";
  constant STYPE0_IMPLEMENTATION_DEFINED : std_logic_vector(2 downto 0) := "111";

  -- STYPE1 constants.
  constant STYPE1_START_OF_PACKET : std_logic_vector(2 downto 0) := "000";
  constant STYPE1_STOMP : std_logic_vector(2 downto 0) := "001";
  constant STYPE1_END_OF_PACKET : std_logic_vector(2 downto 0) := "010";
  constant STYPE1_RESTART_FROM_RETRY : std_logic_vector(2 downto 0) := "011";
  constant STYPE1_LINK_REQUEST : std_logic_vector(2 downto 0) := "100";
  constant STYPE1_MULTICAST_EVENT : std_logic_vector(2 downto 0) := "101";
  constant STYPE1_RESERVED : std_logic_vector(2 downto 0) := "110";
  constant STYPE1_NOP : std_logic_vector(2 downto 0) := "111";

  -- FTYPE constants.
  constant FTYPE_REQUEST_CLASS : std_logic_vector(3 downto 0) := "0010";
  constant FTYPE_WRITE_CLASS : std_logic_vector(3 downto 0) := "0101";
  constant FTYPE_STREAMING_WRITE_CLASS : std_logic_vector(3 downto 0) := "0110";
  constant FTYPE_MAINTENANCE_CLASS : std_logic_vector(3 downto 0) := "1000";
  constant FTYPE_RESPONSE_CLASS : std_logic_vector(3 downto 0) := "1101";
  constant FTYPE_DOORBELL_CLASS : std_logic_vector(3 downto 0) := "1010";
  constant FTYPE_MESSAGE_CLASS : std_logic_vector(3 downto 0) := "0010";

  -- TTYPE Constants
  constant TTYPE_MAINTENANCE_READ_REQUEST : std_logic_vector(3 downto 0) := "0000";
  constant TTYPE_MAINTENANCE_WRITE_REQUEST : std_logic_vector(3 downto 0) := "0001";
  constant TTYPE_MAINTENANCE_READ_RESPONSE : std_logic_vector(3 downto 0) := "0010";
  constant TTYPE_MAINTENANCE_WRITE_RESPONSE : std_logic_vector(3 downto 0) := "0011";
  constant TTYPE_MAINTENANCE_PORT_WRITE : std_logic_vector(3 downto 0) := "0100";
  constant TTYPE_NREAD_TRANSACTION : std_logic_vector(3 downto 0) := "0100";
  constant TTYPE_NWRITE_TRANSACTION : std_logic_vector(3 downto 0) := "0100";
  constant TTYPE_NWRITER_TRANSACTION : std_logic_vector(3 downto 0) := "0101";
  constant TTYPE_RESPONSE_NO_PAYLOAD : std_logic_vector(3 downto 0) := "0000";
  constant TTYPE_RESPONSE_WITH_PAYLOAD : std_logic_vector(3 downto 0) := "1000";

  constant LINK_REQUEST_CMD_RESET_DEVICE : std_logic_vector(2 downto 0) := "011";
  constant LINK_REQUEST_CMD_INPUT_STATUS : std_logic_vector(2 downto 0) := "100";

  constant PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID : std_logic_vector(4 downto 0) := "00001";
  constant PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC : std_logic_vector(4 downto 0) := "00010";
  constant PACKET_NOT_ACCEPTED_CAUSE_NON_MAINTENANCE_STOPPED : std_logic_vector(4 downto 0) := "00011";
  constant PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC : std_logic_vector(4 downto 0) := "00100";
  constant PACKET_NOT_ACCEPTED_CAUSE_INVALID_CHARACTER : std_logic_vector(4 downto 0) := "00101";
  constant PACKET_NOT_ACCEPTED_CAUSE_NO_RESOURCES : std_logic_vector(4 downto 0) := "00110";
  constant PACKET_NOT_ACCEPTED_CAUSE_LOSS_DESCRAMBLER : std_logic_vector(4 downto 0) := "00111";
  constant PACKET_NOT_ACCEPTED_CAUSE_GENERAL_ERROR : std_logic_vector(4 downto 0) := "11111";
  
  -----------------------------------------------------------------------------
  -- Function to or together all bits in a vector.
  -----------------------------------------------------------------------------
  function or_reduce( V: std_logic_vector )
    return std_ulogic;

end package;

-------------------------------------------------------------------------------
-- RioCommon package body description.
-------------------------------------------------------------------------------
package body rio_common is

  -----------------------------------------------------------------------------
  -- Function to or together all bits in a vector.
  -----------------------------------------------------------------------------
  function or_reduce( V: std_logic_vector )
    return std_ulogic is
    variable result: std_ulogic;
  begin
    for i in V'range loop
      if i = V'left then
        result := V(i);
      else
        result := result or V(i);
      end if;
      exit when result = '1';
    end loop;
    return result;
  end function;

end rio_common;



-------------------------------------------------------------------------------
-- Crc16CITT
-- A CRC-16 calculator following the implementation proposed in the 2.2
-- standard.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;


-------------------------------------------------------------------------------
-- Entity for Crc16CITT.
-------------------------------------------------------------------------------
entity Crc16CITT is
  port(
    d_i : in  std_logic_vector(15 downto 0);
    crc_i : in std_logic_vector(15 downto 0);
    crc_o : out std_logic_vector(15 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for Crc16CITT.
-------------------------------------------------------------------------------
architecture Crc16Impl of Crc16CITT is
  signal d : std_logic_vector(0 to 15);
  signal c : std_logic_vector(0 to 15);
  signal e : std_logic_vector(0 to 15);
  signal cc : std_logic_vector(0 to 15);
begin

  -- Reverse the bit vector indexes to make them the same as in the standard.
  d(15) <= d_i(0); d(14) <= d_i(1); d(13) <= d_i(2); d(12) <= d_i(3);
  d(11) <= d_i(4); d(10) <= d_i(5); d(9) <= d_i(6); d(8) <= d_i(7);
  d(7) <= d_i(8); d(6) <= d_i(9); d(5) <= d_i(10); d(4) <= d_i(11);
  d(3) <= d_i(12); d(2) <= d_i(13); d(1) <= d_i(14); d(0) <= d_i(15);
  
  -- Reverse the bit vector indexes to make them the same as in the standard.
  c(15) <= crc_i(0); c(14) <= crc_i(1); c(13) <= crc_i(2); c(12) <= crc_i(3);
  c(11) <= crc_i(4); c(10) <= crc_i(5); c(9) <= crc_i(6); c(8) <= crc_i(7);
  c(7) <= crc_i(8); c(6) <= crc_i(9); c(5) <= crc_i(10); c(4) <= crc_i(11);
  c(3) <= crc_i(12); c(2) <= crc_i(13); c(1) <= crc_i(14); c(0) <= crc_i(15);
  
  -- Calculate the resulting crc.
  e <= c xor d;
  cc(0) <= e(4) xor e(5) xor e(8) xor e(12);
  cc(1) <= e(5) xor e(6) xor e(9) xor e(13);
  cc(2) <= e(6) xor e(7) xor e(10) xor e(14);
  cc(3) <= e(0) xor e(7) xor e(8) xor e(11) xor e(15);
  cc(4) <= e(0) xor e(1) xor e(4) xor e(5) xor e(9);
  cc(5) <= e(1) xor e(2) xor e(5) xor e(6) xor e(10);
  cc(6) <= e(0) xor e(2) xor e(3) xor e(6) xor e(7) xor e(11);
  cc(7) <= e(0) xor e(1) xor e(3) xor e(4) xor e(7) xor e(8) xor e(12);
  cc(8) <= e(0) xor e(1) xor e(2) xor e(4) xor e(5) xor e(8) xor e(9) xor e(13);
  cc(9) <= e(1) xor e(2) xor e(3) xor e(5) xor e(6) xor e(9) xor e(10) xor e(14);
  cc(10) <= e(2) xor e(3) xor e(4) xor e(6) xor e(7) xor e(10) xor e(11) xor e(15);
  cc(11) <= e(0) xor e(3) xor e(7) xor e(11);
  cc(12) <= e(0) xor e(1) xor e(4) xor e(8) xor e(12);
  cc(13) <= e(1) xor e(2) xor e(5) xor e(9) xor e(13);
  cc(14) <= e(2) xor e(3) xor e(6) xor e(10) xor e(14);
  cc(15) <= e(3) xor e(4) xor e(7) xor e(11) xor e(15);

  -- Reverse the bit vector indexes to make them the same as in the standard.
  crc_o(15) <= cc(0); crc_o(14) <= cc(1); crc_o(13) <= cc(2); crc_o(12) <= cc(3);
  crc_o(11) <= cc(4); crc_o(10) <= cc(5); crc_o(9) <= cc(6); crc_o(8) <= cc(7);
  crc_o(7) <= cc(8); crc_o(6) <= cc(9); crc_o(5) <= cc(10); crc_o(4) <= cc(11);
  crc_o(3) <= cc(12); crc_o(2) <= cc(13); crc_o(1) <= cc(14); crc_o(0) <= cc(15);

end architecture;



-------------------------------------------------------------------------------
-- MemoryDualPort
-- Generic synchronous memory with one read/write port and one read port.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 


-------------------------------------------------------------------------------
-- Entity for MemoryDualPort.
-------------------------------------------------------------------------------
entity MemoryDualPort is
  generic(
    ADDRESS_WIDTH : natural := 1;
    DATA_WIDTH : natural := 1);
  port(
    clkA_i : in std_logic;
    enableA_i : in std_logic;
    writeEnableA_i : in std_logic;
    addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
    dataA_o : out std_logic_vector(DATA_WIDTH-1 downto 0);

    clkB_i : in std_logic;
    enableB_i : in std_logic;
    addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for MemoryDualPort.
-------------------------------------------------------------------------------
architecture MemoryDualPortImpl of MemoryDualPort is
  type MemoryType is array (natural range <>) of
    std_logic_vector(DATA_WIDTH-1 downto 0);
  
  signal memory : MemoryType(0 to (2**ADDRESS_WIDTH)-1);
  
begin
  process(clkA_i)
  begin
    if (clkA_i'event and clkA_i = '1') then
      if (enableA_i = '1') then
        if (writeEnableA_i = '1') then
          memory(to_integer(unsigned(addressA_i))) <= dataA_i;
        end if;

        dataA_o <= memory(to_integer(unsigned(addressA_i)));
      end if;
    end if;
  end process;

  process(clkB_i)
  begin
    if (clkB_i'event and clkB_i = '1') then
      if (enableB_i = '1') then
        dataB_o <= memory(to_integer(unsigned(addressB_i)));
      end if;
    end if;
  end process;
  
end architecture;



-------------------------------------------------------------------------------
-- MemorySimpleDualPort
-- Generic synchronous memory with one write port and one read port.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 


-------------------------------------------------------------------------------
-- Entity for MemorySimpleDualPort.
-------------------------------------------------------------------------------
entity MemorySimpleDualPort is
  generic(
    ADDRESS_WIDTH : natural := 1;
    DATA_WIDTH : natural := 1);
  port(
    clkA_i : in std_logic;
    enableA_i : in std_logic;
    addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

    clkB_i : in std_logic;
    enableB_i : in std_logic;
    addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for MemorySimpleDualPort.
-------------------------------------------------------------------------------
architecture MemorySimpleDualPortImpl of MemorySimpleDualPort is
  type MemoryType is array (natural range <>) of
    std_logic_vector(DATA_WIDTH-1 downto 0);
  
  signal memory : MemoryType(0 to (2**ADDRESS_WIDTH)-1);
  
begin
  process(clkA_i)
  begin
    if (clkA_i'event and clkA_i = '1') then
      if (enableA_i = '1') then
        memory(to_integer(unsigned(addressA_i))) <= dataA_i;
      end if;
    end if;
  end process;

  process(clkB_i)
  begin
    if (clkB_i'event and clkB_i = '1') then
      if (enableB_i = '1') then
        dataB_o <= memory(to_integer(unsigned(addressB_i)));
      end if;
    end if;
  end process;
  
end architecture;



-------------------------------------------------------------------------------
-- MemorySinglePort
-- Generic synchronous memory with one read/write port.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 


-------------------------------------------------------------------------------
-- Entity for MemorySinglePort.
-------------------------------------------------------------------------------
entity MemorySinglePort is
  generic(
    ADDRESS_WIDTH : natural := 1;
    DATA_WIDTH : natural := 1);
  port(
    clk_i : in std_logic;
    enable_i : in std_logic;
    writeEnable_i : in std_logic;
    address_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    data_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
    data_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for MemorySinglePort.
-------------------------------------------------------------------------------
architecture MemorySinglePortImpl of MemorySinglePort is
  type MemoryType is array (natural range <>) of
    std_logic_vector(DATA_WIDTH-1 downto 0);
  
  signal memory : MemoryType(0 to (2**ADDRESS_WIDTH)-1);
  
begin
  process(clk_i)
  begin
    if (clk_i'event and clk_i = '1') then
      if (enable_i = '1') then
        if (writeEnable_i = '1') then
          memory(to_integer(unsigned(address_i))) <= data_i;
        end if;

        data_o <= memory(to_integer(unsigned(address_i)));
      end if;
    end if;
  end process;

end architecture;




-------------------------------------------------------------------------------
-- MemorySimpleDualPortAsync
-- Generic memory with one synchronous write port and one asynchronous read port.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 


-------------------------------------------------------------------------------
-- Entity for MemorySimpleDualPortAsync.
-------------------------------------------------------------------------------
entity MemorySimpleDualPortAsync is
  generic(
    ADDRESS_WIDTH : natural := 1;
    DATA_WIDTH : natural := 1;
    INIT_VALUE : std_logic := 'U');
  port(
    clkA_i : in std_logic;
    enableA_i : in std_logic;
    addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

    addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for MemorySimpleDualPortAsync.
-------------------------------------------------------------------------------
architecture MemorySimpleDualPortAsyncImpl of MemorySimpleDualPortAsync is
  type MemoryType is array (natural range <>) of
    std_logic_vector(DATA_WIDTH-1 downto 0);
  
  signal memory : MemoryType(0 to (2**ADDRESS_WIDTH)-1) := (others=>(others=>INIT_VALUE));
  
begin
  process(clkA_i)
  begin
    if (clkA_i'event and clkA_i = '1') then
      if (enableA_i = '1') then
        memory(to_integer(unsigned(addressA_i))) <= dataA_i;
      end if;
    end if;
  end process;

  dataB_o <= memory(to_integer(unsigned(addressB_i)));
  
end architecture;



-------------------------------------------------------------------------------
-- Timer Counter with watch dog input
-------------------------------------------------------------------------------

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all; 

entity Timer is
  generic(timerWidth    : positive := 16;
          repeatedPulse : boolean  := true);
     port(clk_i : in  std_logic;
       reset_ni : in  std_logic;
   resetValue_i : in  std_logic_vector(timerWidth-1 downto 0);
    timerTick_i : in  std_logic;
           wd_i : in  std_logic;
      expired_o : out std_logic);
end entity;

architecture rtl of Timer is
   signal counter : unsigned(timerWidth-1 downto 0) := (others=>'0');
   signal inhibit : std_logic := '1';
begin

   process(reset_ni, clk_i)
   begin
      if reset_ni='0' then
         counter<=(others=>'0');
         expired_o<='0';
         inhibit  <='1';
      elsif rising_edge(clk_i) then
         --
         if resetValue_i=(resetValue_i'range=>'0') then
            inhibit <= '1';
         else
            inhibit <= '0';
         end if;
         --
         if  wd_i='1' or inhibit='1' then
            counter<=unsigned(resetValue_i);
            expired_o<='0';
         elsif timerTick_i='1' then
            if counter=0 then
               counter<=unsigned(resetValue_i);
               expired_o<='1';
            else
               counter<=counter-1;
            end if;
         elsif repeatedPulse then
            expired_o<='0';
         end if;
      end if;
   end process;
end architecture;


-------------------------------------------------------------------------------
-- Ticker generator
-------------------------------------------------------------------------------

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all; 
    use ieee.math_real."log2";
    use ieee.math_real."floor";

entity Ticker is
  generic(clksPerTick : positive := 10);  --select based on clk rate and desired tick rate.
     port(clk_i : in  std_logic;          --for example: 25MHz/10 = 2.5 MHz. 
       reset_ni : in  std_logic;
         tick_o : out std_logic);
end entity;

architecture rtl of Ticker is
   signal counter : unsigned(integer(floor(log2(real(clksPerTick-1)))) downto 0) := (others=>'0');
begin
   process(reset_ni, clk_i)
   begin
      if reset_ni='0' then
         counter<=(others=>'0');
         tick_o<='0';
      elsif rising_edge(clk_i) then
         if counter=0 then
            counter<=to_unsigned(clksPerTick-1,counter'length);
            tick_o<='1';
         else
            counter<=counter-1;
            tick_o<='0';
         end if;
      end if;
   end process;
end architecture;




