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
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/

-- Author:  arnaud.samama@thalesgroup.com
-- 
----------------------------------------------------------------------------------
-- 
-- Copyright (C) 2014 Authors and OPENCORES.ORG
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


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use work.txt_util.all;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
use std.textio.all;

use work.axi4bfm_pkg.all;
use work.rio_common.all;

architecture SIMULATION of srio_cpu_wrapper is

    signal clk : std_logic;
    
    signal master2slaveLite  : AxiLITEmaster2slave_bus_t;
    signal slave2masterLite  : AxiLITEslave2master_bus_t; 
    signal master2slave      : axi4master2slave_bus_t;
    signal slave2master      : axi4slave2master_bus_t;

impure function CheckResult( 
    test_name   : string;
    expected    : std_logic_vector(31 downto 0);
    data        : std_logic_vector(31 downto 0);
    ok_message  : string) return boolean is
begin
    if expected /= data then 
        print("ERROR: " & test_name & " read 0x" & hstr(data) & " instead of 0x" & hstr(expected) );
        return false;
    else
        print("OK   : " & test_name & ", " & ok_message);
        return true;
    end if;

end function;

begin

    led_4bits_tri_o <= (others => '0');

    FCLK_CLK0 <= clk;


    SRIO_AXI_MASTER_araddr          <= master2slaveLite.S_AXI_ARADDR;
    SRIO_AXI_MASTER_arprot          <= master2slaveLite.S_AXI_ARPROT;
    slave2masterLite.S_AXI_ARREADY  <= SRIO_AXI_MASTER_arready(0);
    SRIO_AXI_MASTER_arvalid(0)      <= master2slaveLite.S_AXI_ARVALID;
    SRIO_AXI_MASTER_awaddr          <= master2slaveLite.S_AXI_AWADDR;
    SRIO_AXI_MASTER_awprot          <= master2slaveLite.S_AXI_AWPROT;
    slave2masterLite.S_AXI_AWREADY  <= SRIO_AXI_MASTER_awready(0);
    SRIO_AXI_MASTER_awvalid(0)         <= master2slaveLite.S_AXI_AWVALID;
    SRIO_AXI_MASTER_bready(0)          <= master2slaveLite.S_AXI_BREADY;
    slave2masterLite.S_AXI_BRESP    <= SRIO_AXI_MASTER_bresp;
    slave2masterLite.S_AXI_BVALID   <= SRIO_AXI_MASTER_bvalid(0);
    slave2masterLite.S_AXI_RDATA    <= SRIO_AXI_MASTER_rdata;
    SRIO_AXI_MASTER_rready(0)          <= master2slaveLite.S_AXI_RREADY;
    slave2masterLite.S_AXI_RRESP    <= SRIO_AXI_MASTER_rresp;
    slave2masterLite.S_AXI_RVALID   <= SRIO_AXI_MASTER_rvalid(0);
    SRIO_AXI_MASTER_wdata           <= master2slaveLite.S_AXI_WDATA;
    slave2masterLite.S_AXI_WREADY   <= SRIO_AXI_MASTER_wready(0);
    SRIO_AXI_MASTER_wstrb           <= master2slaveLite.S_AXI_WSTRB;
    SRIO_AXI_MASTER_wvalid(0)          <= master2slaveLite.S_AXI_WVALID;

    master2slave.M_AXI_ARADDR       <= SRIO_AXI_SLAVE_araddr;
    master2slave.M_AXI_ARBURST      <= SRIO_AXI_SLAVE_arburst;
    master2slave.M_AXI_ARCACHE      <= SRIO_AXI_SLAVE_arcache ;
    master2slave.M_AXI_ARID         <= SRIO_AXI_SLAVE_arid;
    master2slave.M_AXI_ARLEN        <= SRIO_AXI_SLAVE_arlen;
    master2slave.M_AXI_ARLOCK       <= SRIO_AXI_SLAVE_arlock(0);
    master2slave.M_AXI_ARPROT       <= SRIO_AXI_SLAVE_arprot;
    master2slave.M_AXI_ARQOS        <= SRIO_AXI_SLAVE_arqos;
    SRIO_AXI_SLAVE_arready          <= slave2master.M_AXI_ARREADY;
    -- Not used SRIO_AXI_SLAVE_arregion  
    master2slave.M_AXI_ARSIZE       <= SRIO_AXI_SLAVE_arsize;
    master2slave.M_AXI_ARVALID     <= SRIO_AXI_SLAVE_arvalid ;

    master2slave.M_AXI_AWADDR       <= SRIO_AXI_SLAVE_awaddr;
    master2slave.M_AXI_AWBURST      <= SRIO_AXI_SLAVE_awburst;
    master2slave.M_AXI_AWCACHE      <= SRIO_AXI_SLAVE_awcache;
    master2slave.M_AXI_AWID         <= SRIO_AXI_SLAVE_awid;
    master2slave.M_AXI_AWLEN        <= SRIO_AXI_SLAVE_awlen;
    master2slave.M_AXI_AWLOCK       <= SRIO_AXI_SLAVE_awlock(0);
    master2slave.M_AXI_AWPROT       <= SRIO_AXI_SLAVE_awprot;
    master2slave.M_AXI_AWQOS        <= SRIO_AXI_SLAVE_awqos;
    SRIO_AXI_SLAVE_awready          <= slave2master.M_AXI_AWREADY;
     -- Not used SRIO_AXI_SLAVE_awregion;
    master2slave.M_AXI_AWSIZE       <= SRIO_AXI_SLAVE_awsize;
    master2slave.M_AXI_AWVALID      <= SRIO_AXI_SLAVE_awvalid;
    SRIO_AXI_SLAVE_bid              <= slave2master.M_AXI_BID;
    master2slave.M_AXI_BREADY       <= SRIO_AXI_SLAVE_bready;
    SRIO_AXI_SLAVE_bresp            <= slave2master.M_AXI_BRESP;
    SRIO_AXI_SLAVE_bvalid           <= slave2master.M_AXI_BVALID;
    SRIO_AXI_SLAVE_rdata            <= slave2master.M_AXI_RDATA;
    SRIO_AXI_SLAVE_rid              <= slave2master.M_AXI_RID;
    SRIO_AXI_SLAVE_rlast            <= slave2master.M_AXI_RLAST;
    master2slave.M_AXI_RREADY       <= SRIO_AXI_SLAVE_rready;
    SRIO_AXI_SLAVE_rresp            <= slave2master.M_AXI_RRESP;
    SRIO_AXI_SLAVE_rvalid           <= slave2master.M_AXI_RVALID;
    master2slave.M_AXI_WDATA        <= SRIO_AXI_SLAVE_wdata;
    master2slave.M_AXI_WLAST        <= SRIO_AXI_SLAVE_wlast;
    SRIO_AXI_SLAVE_wready           <= slave2master.M_AXI_WREADY;
    master2slave.M_AXI_WSTRB        <= SRIO_AXI_SLAVE_wstrb;
    master2slave.M_AXI_WVALID       <= SRIO_AXI_SLAVE_wvalid ;

	-----------------------------------------------------------------------------
	-- Clock generation.
	-----------------------------------------------------------------------------
	-- 200 MHz
	SysClockGenerator: process
	begin
		clk <= '0';
		wait for 2.5 ns;
		clk <= '1';
		wait for 2.5 ns;
	end process;

	TestDriver: process

        variable data : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        variable result : boolean;
        variable frame_size : integer;
        variable test_result : boolean;
        variable base_addr   : unsigned(31 downto 0);
        variable maint_addr : std_logic_vector(31 downto 0);
   
    begin
        -- Generate a startup reset pulse.
        FCLK_RESET0_N <= '0';
        peripheral_aresetn(0) <= '0';
        wait for 10 ns;
        wait until rising_edge(clk);
        FCLK_RESET0_N <= '1';

        -- Reset peripheral
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        peripheral_aresetn(0) <= '1';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        report "CPU Resetting done";

      -- Waiting for the clocks being stable
        wait for 7 us;

        -- Default initialization of the bus values
        AxiLITEMasterInit(clk, master2slaveLite);
        
        -- Read the identification register
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"FFFF0000", data );
        
        -- Do we have read GOOD_SIO in hexspeak
        result := CheckResult("Identification register", x"600D0510", data, hstr(data)); 
        
        -- Reading invalid address register
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000F0", data );
        result := CheckResult("Non-existent register", x"BADACCE5", data, hstr(data)); 
        
        -- Set source and destination IDs
        -- Source      = 0x0
        -- Destination = 0x1
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"00000014", x"00000001");

        -- Read a maintenance packet targetting the switch
        ---            Hop-Count _ Config-offset 21bits _ w _ 00 
        -- Read Device identity CAR
        maint_addr := "00000000" & "00000" & x"0000"    & "0" & "00";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Checking the Switch Device Identity Register", x"5A11DAAA", data, hstr(data)); 

        -- Read a maintenance packet targetting an invalid address
        ---            Hop-Count _ Config-offset 21bits _ w _ 00 
        -- Read Device identity CAR, we must have a failure
        maint_addr := "00000111" & "00000" & x"0000"    & "0" & "00";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Checking we have an invalid access when hop doesn't reach a valid switch", x"BADACCE5", data, hstr(data)); 

        -- Set the memory address to access the Switch Lock
        maint_addr := "00000000" & "00000000" & x"0068";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        -- Read the content of the lock
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Lock register is not held", x"0000FFFF", data, hstr(data)); 
        -- then try to lock
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00000002" );
        -- Read back the value to check if we got the Lock
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Checking the lock is held by us", x"00000002", data, hstr(data)); 

        -- then release the lock
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00000002" );
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Checking the lock is not held by anyone", x"0000FFFF", data, hstr(data)); 


        -- Enable port 1 to allow loopback mode
        ---            Hop-Count _ Config-offset 21bits _ w _ 00 
        maint_addr := "00000000" & "00000000" & x"017C" ;
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        -- Set bit 21 and 22: enable oputput and input port
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00600000" );

        -- Enable port 2 to allow loopback mode
        ---            Hop-Count _ Config-offset 21bits _ w _ 00 
        maint_addr := "00000000" & "00000000" & x"019C";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        -- Set bit 21 and 22: enable oputput and input port
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00600000" );

        -- Wait for SRIO link being synchronized
        wait for 18 us;

        -- Define the output port for the target-id
        ---            Hop-Count _ Config-offset 21bits _ w _ 00 
        maint_addr := "00000000" & "00000000" & x"0070";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        -- Set bit 21 and 22: enable oputput and input port
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00000001" );
        maint_addr := "00000000" & "00000000" & x"0074";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        -- Set bit 21 and 22: enable oputput and input port
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A4", x"00000001" );

        -- Read other register to be sure we are going to readback the value
        maint_addr := "00000000" & "00000000" & x"0000";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );

        -- Readback the written value
        maint_addr := "00000000" & "00000000" & x"0074";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"000000A0", maint_addr);
        AxiLITEMasterRead( clk, master2slaveLite, slave2masterLite, x"000000A4", data );
        result := CheckResult("Checking we are able to read back the routing table", x"00000001", data, hstr(data)); 



        -- Set the mailbox base adress on the receiver side
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"00000020", x"70000000");


        -- Prepare a transfer from the address 0x600DF00D of 0x00000248 bytes
        base_addr := x"600DF000";
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"0000000C", x"600DF000");
        AxiLITEMasterWrite( clk, master2slaveLite, slave2masterLite, x"00000010", x"00000248");

        wait; -- will wait forever
		

    end process;

 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst read bus simulation 
	-- -----------------------------------------------------------------------------  
   slave_read: process
    begin
        Axi4SlaveReadInit(clk, slave2master );
        loop
            Axi4SlaveAcceptAnyRead( clk, master2slave, slave2master, PATTERN_RANDOM );
        end loop;
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst write bus simulation 
	-- -----------------------------------------------------------------------------  
    slave_write: process
     begin
        Axi4SlaveWriteInit(clk, slave2master );
         loop
             Axi4SlaveAcceptAnyWrite( clk, master2slave, slave2master );
         end loop;
     end process; 

end SIMULATION;
