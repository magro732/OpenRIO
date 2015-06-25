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
use work.txt_util.all;
use std.textio.all;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;

use work.axi4bfm_pkg.all;
use work.rio_common.all;
    
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity axi_master_frontend_tb is
end axi_master_frontend_tb;

architecture Behavioral of axi_master_frontend_tb is

    constant C_M_AXI_THREAD_ID_WIDTH : integer := 2;
    constant C_M_AXI_ADDR_WIDTH      : integer := 32;
    constant C_M_AXI_DATA_WIDTH      : integer := 32;
    constant C_M_AXI_AWUSER_WIDTH    : integer := 1;
    constant C_M_AXI_ARUSER_WIDTH    : integer := 1;
    constant C_M_AXI_WUSER_WIDTH     : integer := 1;
    constant C_M_AXI_RUSER_WIDTH     : integer := 1;
    constant C_M_AXI_BUSER_WIDTH     : integer := 1;
    constant C_S_AXI_ADDR_WIDTH      : integer := 32;
    constant C_S_AXI_DATA_WIDTH      : integer := 32;
    
	signal clk : std_logic;
	
	signal rst_n : std_logic;	
	
    
    signal master2slave      : axi4master2slave_bus_t;
    signal slave2master      : axi4slave2master_bus_t;
    
    signal xfer_addr : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal xfer_addr_valid : std_logic;
    signal xfer_completion_request : std_logic;
    signal xfer_size : unsigned(C_S_AXI_ADDR_WIDTH-1 downto 0);
    
    
    signal xfer_data  : std_logic_vector(C_S_AXI_DATA_WIDTH -1 downto 0);
    signal xfer_data_valid : std_logic;
    
    signal xfer_complete : std_logic;
    
    signal xfer_ready : std_logic;

        
    
   impure function CheckResult( 
        test_name   : string;
        expected    : boolean;
        data        : boolean;
        ok_message  : string) return boolean is
        begin
            if expected /= data then 
                print("ERROR: " & test_name & " read " & str(data) & " instead of " & str(expected) );
                return false;
            else
                print("OK   : " & test_name & ", " & ok_message);
                return true;
            end if;
            
        end function;


  

     procedure WaitAndCheckResultFor( 
        test_name   : string;
        signal expected    : std_logic;
        value       : std_logic;
        ok_message  : string;
        timeout : time ) is
        begin
            if expected /= value then 
                wait until expected = value for timeout;
            end if;
            if expected /= value then 
                print("ERROR: " & test_name & " read 0x" & chr(value) & " instead of 0x" & chr(expected) );
            else
                print("OK   : " & test_name & ", " & ok_message);
            end if;
            
        end procedure;


begin


axi_master_frontend_INST: entity axi_master_frontend 
    port map(
    clk     => clk,
    rst_n => rst_n,
    
    
    xfer_addr_i => xfer_addr,
    xfer_size_i => xfer_size,
    xfer_addr_valid_i => xfer_addr_valid,
    xfer_completion_request_i => xfer_completion_request,
    
    
    xfer_data_i  => xfer_data,
    xfer_data_valid_i => xfer_data_valid,
    
    completed_o => xfer_complete,
    
    xfer_ready_o => xfer_ready,
    
    M_AXI_AWID              => master2slave.M_AXI_AWID,
    M_AXI_AWADDR            => master2slave.M_AXI_AWADDR,
    M_AXI_AWLEN             => master2slave.M_AXI_AWLEN,
    M_AXI_AWSIZE            => master2slave.M_AXI_AWSIZE,
    M_AXI_AWBURST           => master2slave.M_AXI_AWBURST,
    M_AXI_AWLOCK            => master2slave.M_AXI_AWLOCK,
    M_AXI_AWCACHE           => master2slave.M_AXI_AWCACHE,
    M_AXI_AWPROT            => master2slave.M_AXI_AWPROT,
    M_AXI_AWQOS             => master2slave.M_AXI_AWQOS,
    M_AXI_AWUSER            => master2slave.M_AXI_AWUSER,
    M_AXI_AWVALID           => master2slave.M_AXI_AWVALID,
    M_AXI_AWREADY           => slave2master.M_AXI_AWREADY,
    M_AXI_WDATA             => master2slave.M_AXI_WDATA,
    M_AXI_WSTRB             => master2slave.M_AXI_WSTRB,
    M_AXI_WLAST             => master2slave.M_AXI_WLAST,
    M_AXI_WUSER             => master2slave.M_AXI_WUSER,
    M_AXI_WVALID            => master2slave.M_AXI_WVALID,
    M_AXI_WREADY            => slave2master.M_AXI_WREADY,
    M_AXI_BID               => slave2master.M_AXI_BID,
    M_AXI_BRESP             => slave2master.M_AXI_BRESP,
    M_AXI_BUSER             => slave2master.M_AXI_BUSER,
    M_AXI_BVALID            => slave2master.M_AXI_BVALID,
    M_AXI_BREADY            => master2slave.M_AXI_BREADY,
    M_AXI_ARID              => master2slave.M_AXI_ARID,
    M_AXI_ARADDR            => master2slave.M_AXI_ARADDR,
    M_AXI_ARLEN             => master2slave.M_AXI_ARLEN,
    M_AXI_ARSIZE            => master2slave.M_AXI_ARSIZE,
    M_AXI_ARBURST           => master2slave.M_AXI_ARBURST,
    M_AXI_ARLOCK            => master2slave.M_AXI_ARLOCK,
    M_AXI_ARCACHE           => master2slave.M_AXI_ARCACHE,
    M_AXI_ARPROT            => master2slave.M_AXI_ARPROT,
    M_AXI_ARQOS             => master2slave.M_AXI_ARQOS,
    M_AXI_ARUSER            => master2slave.M_AXI_ARUSER,
    M_AXI_ARVALID           => master2slave.M_AXI_ARVALID,
    M_AXI_ARREADY           => slave2master.M_AXI_ARREADY,
    M_AXI_RID               => slave2master.M_AXI_RID,
    M_AXI_RDATA             => slave2master.M_AXI_RDATA,
    M_AXI_RRESP             => slave2master.M_AXI_RRESP,
    M_AXI_RLAST             => slave2master.M_AXI_RLAST,
    M_AXI_RUSER             => slave2master.M_AXI_RUSER,
    M_AXI_RVALID            => slave2master.M_AXI_RVALID,
    M_AXI_RREADY            => master2slave.M_AXI_RREADY
   
    );
      
      
  	FMC_HPC_GBTCLK0_M2C_P <= gt_clk0;
	FMC_HPC_GBTCLK0_M2C_N <= not gt_clk0;	

  	FMC_HPC_GBTCLK1_M2C_P <= gt_clk1;
	FMC_HPC_GBTCLK1_M2C_N <= not gt_clk1;	

	-----------------------------------------------------------------------------
	-- Clock generation.
	-----------------------------------------------------------------------------
    -- 125 MHz => 8 ns
	ClockGenerator: process
	begin
		clk <= '0';
		wait for 4 ns;
		clk <= '1';
		wait for 4 ns;
	end process;
       
    
    Reset: process

	begin
        wait until rising_edge(clk);
        rst_n <= '0';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        rst_n <= '1';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        report "Reseting done";
        xfer_completion_request <= '0';

         -- Default initialization of the bus values
        AxiLITEMasterInit(clk, master2slaveLite);
            
        wait;
        
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst read bus simulation 
	-- -----------------------------------------------------------------------------  
   slave_read: process
    begin
        Axi4SlaveReadInit(clk, slave2master );
        loop
            Axi4SlaveAcceptAnyRead( clk, master2slave, slave2master, PATTERN_INCREASING );
        end loop;
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst write bus simulation 
	-- -----------------------------------------------------------------------------  
    slave_write: process
        -- Simulate memory for up to 1024 transaction
     begin
        Axi4SlaveWriteInit(clk, slave2master );
         loop
             Axi4SlaveAcceptAnyWrite( clk, master2slave, slave2master );
         end loop;
     end process; 
     
 
end Behavioral;
