-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing a template of how to assemble the IP-block in this project to
-- create a simple endpoint.
-- 
-- To Do:
-- - Update all entities and write a testbench. The file does not compile yet.
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2015 Authors and OPENCORES.ORG 
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
-- RioEndpoint
-- This is a template to create endpoints using the IP-blocks in the RapidIO IP
-- library.
-------------------------------------------------------------------------------
-- REMARK: Make sure that the clocking of all blocks work, even when
-- idle-sequences are not even 4 characters long.

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioEndpoint is
  port(
    -- System interface.
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- Serial 1x I/O pins.
    outputEnable_o : out std_logic;
    inboundSerial_i : in std_logic;
    outboundSerial_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioEndpointImpl of RioEndpoint is
  
  component RioPma is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      realign_i : in std_logic;
      aligned_o : out std_logic;
      
      outboundData_i : in std_logic_vector(0 to 9);
      outboundRead_o : out std_logic;
      inboundWrite_o : out std_logic;
      inboundData_o : out std_logic_vector(0 to 9);

      in_i : in std_logic;
      out_o : out std_logic);
  end component;

  component RioPcs is
    generic(
      TICKS_PER_US : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portInitialized_o : out std_logic;
      outboundControlValid_i : in std_logic;
      outboundControlSymbol_i : in std_logic_vector(23 downto 0);
      outboundDataValid_i : in std_logic;
      outboundDataSymbol_i : in std_logic_vector(31 downto 0);
      inboundControlValid_o : out std_logic;
      inboundControlSymbol_o : out std_logic_vector(23 downto 0);
      inboundDataValid_o : out std_logic;
      inboundDataSymbol_o : out std_logic_vector(31 downto 0);
      
      rxValid_i : in std_logic;
      rxInput_i : in std_logic_vector(0 to 9);
      txValid_o : out std_logic;
      txOutput_o : out std_logic_vector(0 to 9));
  end component;
  
  component RioSerial is
    generic(
      TIMEOUT_WIDTH : natural;
      SYMBOL_COUNTER_WIDTH : natural := 8;
      TICKS_SEND_STATUS_STARTUP : natural := 15;
      TICKS_SEND_STATUS_OPERATIONAL : natural := 255);
    port(
      -- System signals.
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      -- Status signals for maintenance operations.
      portLinkTimeout_i : in std_logic_vector(TIMEOUT_WIDTH-1 downto 0);
      linkInitialized_o : out std_logic;
      inputPortEnable_i : in std_logic;
      outputPortEnable_i : in std_logic;

      -- Support for portLocalAckIdCSR.
      localAckIdWrite_i : in std_logic;
      clrOutstandingAckId_i : in std_logic;
      inboundAckId_i : in std_logic_vector(4 downto 0);
      outstandingAckId_i : in std_logic_vector(4 downto 0);
      outboundAckId_i : in std_logic_vector(4 downto 0);
      inboundAckId_o : out std_logic_vector(4 downto 0);
      outstandingAckId_o : out std_logic_vector(4 downto 0);
      outboundAckId_o : out std_logic_vector(4 downto 0);
      
      -- Outbound frame interface.
      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readWindowEmpty_i : in std_logic;
      readWindowReset_o : out std_logic;
      readWindowNext_o : out std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);

      -- Inbound frame interface.
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0);

      -- PCS layer signals.
      portInitialized_i : in std_logic;
      outboundControlValid_o : out std_logic;
      outboundControlSymbol_o : out std_logic_vector(23 downto 0);
      outboundDataValid_o : out std_logic;
      outboundDataSymbol_o : out std_logic_vector(31 downto 0);
      inboundControlValid_i : in std_logic;
      inboundControlSymbol_i : in std_logic_vector(23 downto 0);
      inboundDataValid_i : in std_logic;
      inboundDataSymbol_i : in std_logic_vector(31 downto 0));
  end component;
  
  component RioPacketBufferWindow is
    generic(
      SIZE_ADDRESS_WIDTH : natural := 6;
      CONTENT_ADDRESS_WIDTH : natural := 8);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      inboundWriteFrameFull_o : out std_logic;
      inboundWriteFrame_i : in std_logic;
      inboundWriteFrameAbort_i : in std_logic;
      inboundWriteContent_i : in std_logic;
      inboundWriteContentData_i : in std_logic_vector(31 downto 0);
      inboundReadFrameEmpty_o : out std_logic;
      inboundReadFrame_i : in std_logic;
      inboundReadFrameRestart_i : in std_logic;
      inboundReadFrameAborted_o : out std_logic;
      inboundReadContentEmpty_o : out std_logic;
      inboundReadContent_i : in std_logic;
      inboundReadContentEnd_o : out std_logic;
      inboundReadContentData_o : out std_logic_vector(31 downto 0);
      
      outboundWriteFrameFull_o : out std_logic;
      outboundWriteFrame_i : in std_logic;
      outboundWriteFrameAbort_i : in std_logic;
      outboundWriteContent_i : in std_logic;
      outboundWriteContentData_i : in std_logic_vector(31 downto 0);
      outboundReadFrameEmpty_o : out std_logic;
      outboundReadFrame_i : in std_logic;
      outboundReadFrameRestart_i : in std_logic;
      outboundReadFrameAborted_o : out std_logic;
      outboundReadWindowEmpty_o : out std_logic;
      outboundReadWindowReset_i : in std_logic;
      outboundReadWindowNext_i : in std_logic;
      outboundReadContentEmpty_o : out std_logic;
      outboundReadContent_i : in std_logic;
      outboundReadContentEnd_o : out std_logic;
      outboundReadContentData_o : out std_logic_vector(31 downto 0));
  end component;

  component RioWbBridge is
    generic(
      EXTENDED_ADDRESS : natural range 0 to 2 := 0;
      DEVICE_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_REV : std_logic_vector(31 downto 0);
      ASSY_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_REV : std_logic_vector(15 downto 0));
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

      cyc_o : out std_logic;
      stb_o : out std_logic;
      we_o : out std_logic;
      adr_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      sel_o : out std_logic_vector(7 downto 0); 
      dat_o : out std_logic_vector(63 downto 0);
      dat_i : in std_logic_vector(63 downto 0);
      err_i : in std_logic;
      ack_i : in std_logic);
  end component;

begin

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  LogicalWbBridge: RioWbBridge
    generic map(
      EXTENDED_ADDRESS=>0,
      DEVICE_IDENTITY=>x"1122",
      DEVICE_VENDOR_IDENTITY=>x"3344",
      DEVICE_REV=>x"55667788",
      ASSY_IDENTITY=>x"99aa",
      ASSY_VENDOR_IDENTITY=>"bbcc",
      ASSY_REV=>x"ddee")
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 

      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 
      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 

      cyc_o=>cyc_o, 
      stb_o=>stb_o, 
      we_o=>we_o, 
      adr_o=>adr_o, 
      sel_o=>sel_o, 
      dat_o=>dat_o, 
      dat_i=>dat_i, 
      err_i=>err_i, 
      ack_i=>ack_i);

  -- REMARK: This could be changed using accesses in config-space.
  forceReinit <= '0';
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  PacketQueue: RioFrameBufferWindow
    generic map(
      SIZE_ADDRESS_WIDTH=>6,
      CONTENT_ADDRESS_WIDTH=>8)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 

      inboundWriteFrameFull_o=>inboundWriteFrameFull_o, 
      inboundWriteFrame_i=>inboundWriteFrame_i, 
      inboundWriteFrameAbort_i=>inboundWriteFrameAbort_i, 
      inboundWriteContent_i=>inboundWriteContent_i, 
      inboundWriteContentData_i=>inboundWriteContentData_i, 
      inboundReadFrameEmpty_o=>inboundReadFrameEmpty_o, 
      inboundReadFrame_i=>inboundReadFrame_i, 
      inboundReadFrameRestart_i=>inboundReadFrameRestart_i, 
      inboundReadFrameAborted_o=>inboundReadFrameAborted_o, 
      inboundReadContentEmpty_o=>inboundReadContentEmpty_o, 
      inboundReadContent_i=>inboundReadContent_i, 
      inboundReadContentEnd_o=>inboundReadContentEnd_o, 
      inboundReadContentData_o=>inboundReadContentData_o, 
    
      outboundWriteFrameFull_o=>outboundWriteFrameFull_o, 
      outboundWriteFrame_i=>outboundWriteFrame_i, 
      outboundWriteFrameAbort_i=>outboundWriteFrameAbort_i, 
      outboundWriteContent_i=>outboundWriteContent_i, 
      outboundWriteContentData_i=>outboundWriteContentData_i, 
      outboundReadFrameEmpty_o=>outboundReadFrameEmpty_o, 
      outboundReadFrame_i=>outboundReadFrame_i, 
      outboundReadFrameRestart_i=>outboundReadFrameRestart_i, 
      outboundReadFrameAborted_o=>outboundReadFrameAborted_o, 
      outboundReadWindowEmpty_o=>outboundReadWindowEmpty_o, 
      outboundReadWindowReset_i=>outboundReadWindowReset_i, 
      outboundReadWindowNext_i=>outboundReadWindowNext_i, 
      outboundReadContentEmpty_o=>outboundReadContentEmpty_o, 
      outboundReadContent_i=>outboundReadContent_i, 
      outboundReadContentEnd_o=>outboundReadContentEnd_o, 
      outboundReadContentData_o=>outboundReadContentData_o);

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  LpSerialLayer: RioSerial
    generic map(
      TIMEOUT_WIDTH=>???,
      SYMBOL_COUNTER_WIDTH=>8,
      TICKS_SEND_STATUS_STARTUP=>15,
      TICKS_SEND_STATUS_OPERATIONAL=>255)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 

      portLinkTimeout_i=>portLinkTimeout_i, 
      linkInitialized_o=>linkInitialized_o, 
      inputPortEnable_i=>inputPortEnable_i, 
      outputPortEnable_i=>outputPortEnable_i, 
      localAckIdWrite_i=>localAckIdWrite_i, 
      clrOutstandingAckId_i=>clrOutstandingAckId_i, 
      inboundAckId_i=>inboundAckId_i, 
      outstandingAckId_i=>outstandingAckId_i, 
      outboundAckId_i=>outboundAckId_i, 
      inboundAckId_o=>inboundAckId_o, 
      outstandingAckId_o=>outstandingAckId_o, 
      outboundAckId_o=>outboundAckId_o, 

      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readFrameRestart_o=>readFrameRestart_o, 
      readFrameAborted_i=>readFrameAborted_i, 
      readWindowEmpty_i=>readWindowEmpty_i, 
      readWindowReset_o=>readWindowReset_o, 
      readWindowNext_o=>readWindowNext_o, 
      readContentEmpty_i=>readContentEmpty_i, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 

      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 

      portInitialized_i=>portInitialized, 
      outboundControlValid_o=>outboundControlValid_o, 
      outboundControlSymbol_o=>outboundControlSymbol_o, 
      outboundDataValid_o=>outboundDataValid_o, 
      outboundDataSymbol_o=>outboundDataSymbol_o, 
      inboundControlValid_i=>inboundControlValid_i, 
      inboundControlSymbol_i=>inboundControlSymbol_i, 
      inboundDataValid_i=>inboundDataValid_i, 
      inboundDataSymbol_i=>inboundDataSymbol_i);

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  -- REMARK: It is deterministic how many clock ticks it takes to complete a
  -- symbol but unknown when a symbol is finished/needed.
  -- REMARK: Instatiate with LANES=>1.
  PhysicalCodingSublayer: RioPcs
    generic map(
      TICKS_PER_US=>TICKS_PER_US)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>'1',
      portInitialized_o=>portInitialized, 
      outboundControlValid_i=>outboundControlValid_i, 
      outboundControlSymbol_i=>outboundControlSymbol_i, 
      outboundDataValid_i=>outboundDataValid_i, 
      outboundDataSymbol_i=>outboundDataSymbol_i, 
      inboundControlValid_o=>inboundControlValid_o, 
      inboundControlSymbol_o=>inboundControlSymbol_o, 
      inboundDataValid_o=>inboundDataValid_o, 
      inboundDataSymbol_o=>inboundDataSymbol_o,
      laneTrained_i=>laneTrained,     
      rxValid_i=>pmaInboundWrite, 
      rxInput_i=>pmaInboundData,
      txOe_o=>lane0DriverEnable,
      txValid_i=>pmaOutboundRead, 
      txOutput_o=>pmaOutboundData);

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  -- REMARK: Add 8b/10b codec here instead of inside PCS?
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  -- REMARK: It is deterministic how many clock ticks it takes to complete a
  -- code-group but unknown when a code-group is finished/needed.
  -- REMARK: A "stall"-mechanism is needed to hold the pipeline until a new
  -- code-group is ready. This mechanism needs to be implemented all the way up
  -- to the packet-queue. The same applies to the outbound direction. It is not
  -- sufficient to match the clock-frequencies between layers.
  -- REMARK: There can be many PMAs, one for each lane, instantiate in a generate-statement.
  PhysicalMediumAccess: RioPma
    port map(
      clk=>clk,
      areset_n=>areset_n, 
      realign_i=>forceReinit,
      aligned_o=>laneTrained,
      outboundRead_o=>pmaOutboundRead, 
      outboundCodegroup_i=>pmaOutboundData, 
      inboundWrite_o=>pmaInboundWrite, 
      inboundCodegroup_o=>pmaInboundData, 
      serial_i=>inboundSerial_i, 
      serial_o=>outboundSerial_o);
  outputEnable_o <= lane0DriverEnable;

end architecture;
