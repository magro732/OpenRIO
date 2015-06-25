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

entity top_loopback_tb is
end top_loopback_tb;

architecture Behavioral of top_loopback_tb is

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
	signal sys_clk_p : std_logic;
	signal sys_clk_n : std_logic;
    signal sys_clk : std_logic;
	
	signal gt_clk0 : std_logic;
	signal gt_clk0_p : std_logic;
	signal gt_clk0_n : std_logic;

	signal gt_clk1 : std_logic;
	signal gt_clk1_p : std_logic;
	signal gt_clk1_n : std_logic;

	signal rst_n : std_logic;	
	
	signal busRX_01_p : std_logic_vector(0 to 3);	
	signal busRX_01_n : std_logic_vector(0 to 3);
	signal busTX_01_p : std_logic_vector(0 to 3);	
	signal busTX_01_n : std_logic_vector(0 to 3);
                                         
	signal busRX_02_p : std_logic_vector(0 to 3);	
	signal busRX_02_n : std_logic_vector(0 to 3);              
	signal busTX_02_p : std_logic_vector(0 to 3);	
	signal busTX_02_n : std_logic_vector(0 to 3);    
    
 	-- signal busA_p : std_logic;	
	-- signal busA_n : std_logic;
	
	-- signal busB_p : std_logic;	
	-- signal busB_n : std_logic;   
    
    signal master2slaveLite_01  : AxiLITEmaster2slave_bus_t;
    signal slave2masterLite_01  : AxiLITEslave2master_bus_t; 
    signal master2slave_01      : axi4master2slave_bus_t;
    signal slave2master_01      : axi4slave2master_bus_t;
    
    signal master2slaveLite_02  : AxiLITEmaster2slave_bus_t;
    signal slave2masterLite_02  : AxiLITEslave2master_bus_t; 
    signal master2slave_02      : axi4master2slave_bus_t;
    signal slave2master_02      : axi4slave2master_bus_t;

    
    signal   FIXED_IO_ddr_vrn_01        : STD_LOGIC;
    signal   FIXED_IO_ddr_vrp_01        : STD_LOGIC;
    signal   FIXED_IO_mio_01            : STD_LOGIC_VECTOR ( 53 downto 0 );
    signal   FIXED_IO_ps_clk_01         : STD_LOGIC;
    signal   FIXED_IO_ps_porb_01        : STD_LOGIC;
    signal   FIXED_IO_ps_srstb_01       : STD_LOGIC;
    
    signal   DDR_addr_01                : STD_LOGIC_VECTOR ( 14 downto 0 );
    signal   DDR_ba_01                  : STD_LOGIC_VECTOR ( 2 downto 0 );
    signal   DDR_cas_n_01               : STD_LOGIC;
    signal   DDR_ck_n_01                : STD_LOGIC;
    signal   DDR_ck_p_01                : STD_LOGIC;
    signal   DDR_cke_01                 : STD_LOGIC;
    signal   DDR_cs_n_01                : STD_LOGIC;
    signal   DDR_dm_01                  : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_dq_01                  : STD_LOGIC_VECTOR ( 31 downto 0 );
    signal   DDR_dqs_n_01               : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_dqs_p_01               : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_odt_01                 : STD_LOGIC;
    signal   DDR_ras_n_01               : STD_LOGIC;
    signal   DDR_reset_n_01             : STD_LOGIC;
    signal   DDR_we_n_01                : STD_LOGIC;
    signal   GPIO_LED_LEFT_01           : std_logic;
    signal   GPIO_LED_CENTER_01         : std_logic;
    signal   GPIO_LED_RIGHT_01          : std_logic;
    signal   GPIO_LED_0_01              : std_logic;
    signal   GPIO_DIP_SW0_01            : std_logic;
    signal   GPIO_DIP_SW1_01            : std_logic;
    signal   GPIO_DIP_SW2_01            : std_logic;
    signal   GPIO_DIP_SW3_01            : std_logic;
    
    signal   S_AXI_AWADDR_01            : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal   S_AXI_AWPROT_01            : std_logic_vector(3-1 downto 0);
    signal   S_AXI_AWVALID_01           : std_logic;
    signal   S_AXI_AWREADY_01           : std_logic;
    signal   S_AXI_WDATA_01             : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal   S_AXI_WSTRB_01             : std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0);
    signal   S_AXI_WVALID_01            : std_logic;
    signal   S_AXI_WREADY_01            : std_logic;
    signal   S_AXI_BRESP_01             : std_logic_vector(2-1 downto 0);
    signal   S_AXI_BVALID_01            : std_logic;
    signal   S_AXI_BREADY_01            : std_logic;
    signal   S_AXI_ARADDR_01            : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal   S_AXI_ARPROT_01            : std_logic_vector(3-1 downto 0);
    signal   S_AXI_ARVALID_01           : std_logic;
    signal   S_AXI_ARREADY_01           : std_logic;
    signal   S_AXI_RDATA_01             : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal   S_AXI_RRESP_01             : std_logic_vector(2-1 downto 0);
    signal   S_AXI_RVALID_01            : std_logic;
    signal   S_AXI_RREADY_01            : std_logic;
    signal   M_AXI_AWID_01              : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_AWADDR_01            : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal   M_AXI_AWLEN_01             : std_logic_vector(8-1 downto 0);
    signal   M_AXI_AWSIZE_01            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_AWBURST_01           : std_logic_vector(2-1 downto 0);
    signal   M_AXI_AWLOCK_01            : std_logic;
    signal   M_AXI_AWCACHE_01           : std_logic_vector(4-1 downto 0);
    signal   M_AXI_AWPROT_01            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_AWQOS_01             : std_logic_vector(4-1 downto 0);
    signal   M_AXI_AWUSER_01            : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
    signal   M_AXI_AWVALID_01           : std_logic;
    signal   M_AXI_AWREADY_01           : std_logic;
    signal   M_AXI_WDATA_01             : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal   M_AXI_WSTRB_01             : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    signal   M_AXI_WLAST_01             : std_logic;
    signal   M_AXI_WUSER_01             : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
    signal   M_AXI_WVALID_01            : std_logic;
    signal   M_AXI_WREADY_01            : std_logic;
    signal   M_AXI_BID_01               : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_BRESP_01             : std_logic_vector(2-1 downto 0);
    signal   M_AXI_BUSER_01             : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
    signal   M_AXI_BVALID_01            : std_logic;
    signal   M_AXI_BREADY_01            : std_logic;
    signal   M_AXI_ARID_01              : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_ARADDR_01            : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal   M_AXI_ARLEN_01             : std_logic_vector(8-1 downto 0);
    signal   M_AXI_ARSIZE_01            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_ARBURST_01           : std_logic_vector(2-1 downto 0);
    signal   M_AXI_ARLOCK_01            : std_logic;
    signal   M_AXI_ARCACHE_01           : std_logic_vector(4-1 downto 0);
    signal   M_AXI_ARPROT_01            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_ARQOS_01             : std_logic_vector(4-1 downto 0);
    signal   M_AXI_ARUSER_01            : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
    signal   M_AXI_ARVALID_01           : std_logic;
    signal   M_AXI_ARREADY_01           : std_logic;
    signal   M_AXI_RID_01               : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_RDATA_01             : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal   M_AXI_RRESP_01             : std_logic_vector(2-1 downto 0);
    signal   M_AXI_RLAST_01             : std_logic;
    signal   M_AXI_RUSER_01             : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
    signal   M_AXI_RVALID_01            : std_logic;
    signal   M_AXI_RREADY_01            : std_logic;
   
    signal   FIXED_IO_ddr_vrn_02        : STD_LOGIC;
    signal   FIXED_IO_ddr_vrp_02        : STD_LOGIC;
    signal   FIXED_IO_mio_02            : STD_LOGIC_VECTOR ( 53 downto 0 );
    signal   FIXED_IO_ps_clk_02         : STD_LOGIC;
    signal   FIXED_IO_ps_porb_02        : STD_LOGIC;
    signal   FIXED_IO_ps_srstb_02       : STD_LOGIC;
    signal   DDR_addr_02                : STD_LOGIC_VECTOR ( 14 downto 0 );
    signal   DDR_ba_02                  : STD_LOGIC_VECTOR ( 2 downto 0 );
    signal   DDR_cas_n_02               : STD_LOGIC;
    signal   DDR_ck_n_02                : STD_LOGIC;
    signal   DDR_ck_p_02                : STD_LOGIC;
    signal   DDR_cke_02                 : STD_LOGIC;
    signal   DDR_cs_n_02                : STD_LOGIC;
    signal   DDR_dm_02                  : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_dq_02                  : STD_LOGIC_VECTOR ( 31 downto 0 );
    signal   DDR_dqs_n_02               : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_dqs_p_02               : STD_LOGIC_VECTOR ( 3 downto 0 );
    signal   DDR_odt_02                 : STD_LOGIC;
    signal   DDR_ras_n_02               : STD_LOGIC;
    signal   DDR_reset_n_02             : STD_LOGIC;
    signal   DDR_we_n_02                : STD_LOGIC;
    signal   GPIO_LED_LEFT_02           : std_logic;
    signal   GPIO_LED_CENTER_02         : std_logic;
    signal   GPIO_LED_RIGHT_02          : std_logic;
    signal   GPIO_LED_0_02              : std_logic;
    signal   GPIO_DIP_SW0_02            : std_logic;
    signal   GPIO_DIP_SW1_02            : std_logic;
    signal   GPIO_DIP_SW2_02            : std_logic;
    signal   GPIO_DIP_SW3_02            : std_logic;
    
    signal   S_AXI_AWADDR_02            : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal   S_AXI_AWPROT_02            : std_logic_vector(3-1 downto 0);
    signal   S_AXI_AWVALID_02           : std_logic;
    signal   S_AXI_AWREADY_02           : std_logic;
    signal   S_AXI_WDATA_02             : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal   S_AXI_WSTRB_02             : std_logic_vector(C_S_AXI_DATA_WIDTH/8-1 downto 0);
    signal   S_AXI_WVALID_02            : std_logic;
    signal   S_AXI_WREADY_02            : std_logic;
    signal   S_AXI_BRESP_02             : std_logic_vector(2-1 downto 0);
    signal   S_AXI_BVALID_02            : std_logic;
    signal   S_AXI_BREADY_02            : std_logic;
    signal   S_AXI_ARADDR_02            : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal   S_AXI_ARPROT_02            : std_logic_vector(3-1 downto 0);
    signal   S_AXI_ARVALID_02           : std_logic;
    signal   S_AXI_ARREADY_02           : std_logic;
    signal   S_AXI_RDATA_02             : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal   S_AXI_RRESP_02             : std_logic_vector(2-1 downto 0);
    signal   S_AXI_RVALID_02            : std_logic;
    signal   S_AXI_RREADY_02            : std_logic;
    signal   M_AXI_AWID_02              : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_AWADDR_02            : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal   M_AXI_AWLEN_02             : std_logic_vector(8-1 downto 0);
    signal   M_AXI_AWSIZE_02            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_AWBURST_02           : std_logic_vector(2-1 downto 0);
    signal   M_AXI_AWLOCK_02            : std_logic;
    signal   M_AXI_AWCACHE_02           : std_logic_vector(4-1 downto 0);
    signal   M_AXI_AWPROT_02            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_AWQOS_02             : std_logic_vector(4-1 downto 0);
    signal   M_AXI_AWUSER_02            : std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
    signal   M_AXI_AWVALID_02           : std_logic;
    signal   M_AXI_AWREADY_02           : std_logic;
    signal   M_AXI_WDATA_02             : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal   M_AXI_WSTRB_02             : std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
    signal   M_AXI_WLAST_02             : std_logic;
    signal   M_AXI_WUSER_02             : std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
    signal   M_AXI_WVALID_02            : std_logic;
    signal   M_AXI_WREADY_02            : std_logic;
    signal   M_AXI_BID_02               : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_BRESP_02             : std_logic_vector(2-1 downto 0);
    signal   M_AXI_BUSER_02             : std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
    signal   M_AXI_BVALID_02            : std_logic;
    signal   M_AXI_BREADY_02            : std_logic;
    signal   M_AXI_ARID_02              : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_ARADDR_02            : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal   M_AXI_ARLEN_02             : std_logic_vector(8-1 downto 0);
    signal   M_AXI_ARSIZE_02            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_ARBURST_02           : std_logic_vector(2-1 downto 0);
    signal   M_AXI_ARLOCK_02            : std_logic;
    signal   M_AXI_ARCACHE_02           : std_logic_vector(4-1 downto 0);
    signal   M_AXI_ARPROT_02            : std_logic_vector(3-1 downto 0);
    signal   M_AXI_ARQOS_02             : std_logic_vector(4-1 downto 0);
    signal   M_AXI_ARUSER_02            : std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
    signal   M_AXI_ARVALID_02           : std_logic;
    signal   M_AXI_ARREADY_02           : std_logic;
    signal   M_AXI_RID_02               : std_logic_vector(C_M_AXI_THREAD_ID_WIDTH-1 downto 0);
    signal   M_AXI_RDATA_02             : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal   M_AXI_RRESP_02             : std_logic_vector(2-1 downto 0);
    signal   M_AXI_RLAST_02             : std_logic;
    signal   M_AXI_RUSER_02             : std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
    signal   M_AXI_RVALID_02            : std_logic;
    signal   M_AXI_RREADY_02            : std_logic;
    
    signal   USRCLK_P           : std_logic;
    signal   USRCLK_N           : std_logic;
    
    signal   SI5324_OUT_C_P     : std_logic;
    signal   SI5324_OUT_C_N     : std_logic;    

    signal  FMC_HPC_GBTCLK0_M2C_P  : std_logic;
    signal  FMC_HPC_GBTCLK0_M2C_N  : std_logic;
    signal  FMC_HPC_GBTCLK1_M2C_P  : std_logic;
    signal  FMC_HPC_GBTCLK1_M2C_N  : std_logic;

  
    signal   peripheral_aresetn  : STD_LOGIC;
    
    -- Interrupt raised by SRIO bridge
    signal In1 : STD_LOGIC;
    signal intr_srio2axi : STD_LOGIC;

    signal FCLK_CLK0 : STD_LOGIC;


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


    impure function CheckResult( 
        test_name   : string;
        addr    : std_logic_vector;
        memory      : memory_t;
        expected_pattern   : axi4read_data_type;
        ok_message  : string) return boolean is

            variable pattern : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0) ;
            variable prev_data : unsigned(C_M_AXI_DATA_WIDTH-1 downto 0) ;
            variable next_data : unsigned(C_M_AXI_DATA_WIDTH-1 downto 0) ;
            variable error_detected : boolean;

        begin
            prev_data := (others => '0');
            next_data := (others => '0');
            error_detected := false;

            -- We initialize the recursive function from the first retrieved value
            prev_data:= unsigned(memory(memory'left).data);

            for i_memory_elem in memory'left to memory'right loop

                -- Comppute the next pattern
                GetMemoryPattern(prev_data, next_data, addr, expected_pattern, pattern );

                if pattern /= memory(i_memory_elem).data then
                    error_detected := true;
                    print("ERROR: " & test_name & " read " & hstr(memory(i_memory_elem).addr)  & ":0x" & hstr(memory(i_memory_elem).data) & " instead of 0x" & hstr(pattern) );
                else
                    --print("OK   : " & test_name & " read " & hstr(memory(i_memory_elem).addr)  & ":0x" & hstr(memory(i_memory_elem).data) );
                end if;

                prev_data := next_data;
             end loop;

            if error_detected then 
                print("ERROR: " & test_name );
                return false;
            else
                print("OK   : " & test_name & ", " & ok_message);
                return true;
            end if;
                
            
        end function;

   impure function CheckResult( 
        test_name   : string;
        expected    : std_logic;
        data        : std_logic;
        ok_message  : string) return boolean is
        begin
            if expected /= data then 
                print("ERROR: " & test_name & " read " & chr(data) & " instead of " & chr(expected) );
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
	-----------------------------------------------------------------------------
	-- Buses connectivity.
	-----------------------------------------------------------------------------
	-- busRX_01_p <= busTX_02_p;	
	-- busRX_01_n <= busTX_02_n;
	
	-- busRX_02_p <= busTX_01_p;	
	-- busRX_02_n <= busTX_01_n;  
      
	busRX_01_p(2 to 3) <= (others => '0') ;
	busRX_01_n(2 to 3) <= (others => '1');

	busRX_02_p(2 to 3) <= (others => '0');
	busRX_02_n(2 to 3) <= (others => '1');

    --
    -- Loopack using physical cabling
    --
	--busRX_01_p(0) <=  transport busTX_01_p(0) after 1.4 ns;	
	--busRX_01_n(0) <=  transport busTX_01_n(0) after 1.4 ns;

    -- Turn on switch enabling loopback mode
    GPIO_DIP_SW0_01 <= '1';

top_1: entity work.top_simu
   port map (
    sysclk_p          => sys_clk_p,
    sysclk_n          => sys_clk_n,

    USRCLK_P          => USRCLK_P,
    USRCLK_N          => USRCLK_N,
    
    
    FCLK_CLK0         => FCLK_CLK0,

    FMC_HPC_GBTCLK0_M2C_C_P  => FMC_HPC_GBTCLK0_M2C_P,
    FMC_HPC_GBTCLK0_M2C_C_N  => FMC_HPC_GBTCLK0_M2C_N,
    FMC_HPC_GBTCLK1_M2C_C_P  => FMC_HPC_GBTCLK1_M2C_P,
    FMC_HPC_GBTCLK1_M2C_C_N  => FMC_HPC_GBTCLK1_M2C_N,
    
    ext_rst_n         => rst_n,
    
    FIXED_IO_ddr_vrn  => FIXED_IO_ddr_vrn_01,
    FIXED_IO_ddr_vrp  => FIXED_IO_ddr_vrp_01,
    FIXED_IO_mio      => FIXED_IO_mio_01,
    FIXED_IO_ps_clk   => FIXED_IO_ps_clk_01,
    FIXED_IO_ps_porb  => FIXED_IO_ps_porb_01,
    FIXED_IO_ps_srstb => FIXED_IO_ps_srstb_01,
    
    -- TX differential on Channel 0
    FMC_HPC_DP3_C2M_P =>  busTX_01_p(0), --  -----------------| 
    FMC_HPC_DP3_C2M_N =>  busTX_01_n(0), --  -----------------|
                                         --                   |
                                         --                   |
    -- RX differential on Channel 0                           | 
    FMC_HPC_DP3_M2C_P  =>  busRX_01_p(0), --  <---------------| 
    FMC_HPC_DP3_M2C_N  =>  busRX_01_n(0), --  <---------------|
                                          -- 
    -- TX differential on Channel 1       --              
    FMC_HPC_DP2_C2M_P  =>  busTX_01_p(1), -- 
    FMC_HPC_DP2_C2M_N  =>  busTX_01_n(1), -- 
                                          --              
    -- RX differential on Channel 1       --              
    FMC_HPC_DP2_M2C_P  => busRX_01_p(1), --  
    FMC_HPC_DP2_M2C_N  => busRX_01_n(1), -- 

    S_AXI_AWADDR            => master2slaveLite_01.S_AXI_AWADDR, 
    S_AXI_AWPROT            => master2slaveLite_01.S_AXI_AWPROT, 
    S_AXI_AWVALID           => master2slaveLite_01.S_AXI_AWVALID, 
    S_AXI_AWREADY           => slave2masterLite_01.S_AXI_AWREADY, 
    S_AXI_WDATA             => master2slaveLite_01.S_AXI_WDATA, 
    S_AXI_WSTRB             => master2slaveLite_01.S_AXI_WSTRB, 
    S_AXI_WVALID            => master2slaveLite_01.S_AXI_WVALID, 
    S_AXI_WREADY            => slave2masterLite_01.S_AXI_WREADY, 
    S_AXI_BRESP             => slave2masterLite_01.S_AXI_BRESP, 
    S_AXI_BVALID            => slave2masterLite_01.S_AXI_BVALID, 
    S_AXI_BREADY            => master2slaveLite_01.S_AXI_BREADY, 
    S_AXI_ARADDR            => master2slaveLite_01.S_AXI_ARADDR, 
    S_AXI_ARPROT            => master2slaveLite_01.S_AXI_ARPROT, 
    S_AXI_ARVALID           => master2slaveLite_01.S_AXI_ARVALID, 
    S_AXI_ARREADY           => slave2masterLite_01.S_AXI_ARREADY, 
    S_AXI_RDATA             => slave2masterLite_01.S_AXI_RDATA, 
    S_AXI_RRESP             => slave2masterLite_01.S_AXI_RRESP, 
    S_AXI_RVALID            => slave2masterLite_01.S_AXI_RVALID, 
    S_AXI_RREADY            => master2slaveLite_01.S_AXI_RREADY, 
    
    M_AXI_AWID              => master2slave_01.M_AXI_AWID,
    M_AXI_AWADDR            => master2slave_01.M_AXI_AWADDR,
    M_AXI_AWLEN             => master2slave_01.M_AXI_AWLEN,
    M_AXI_AWSIZE            => master2slave_01.M_AXI_AWSIZE,
    M_AXI_AWBURST           => master2slave_01.M_AXI_AWBURST,
    M_AXI_AWLOCK            => master2slave_01.M_AXI_AWLOCK,
    M_AXI_AWCACHE           => master2slave_01.M_AXI_AWCACHE,
    M_AXI_AWPROT            => master2slave_01.M_AXI_AWPROT,
    M_AXI_AWQOS             => master2slave_01.M_AXI_AWQOS,
    M_AXI_AWUSER            => master2slave_01.M_AXI_AWUSER,
    M_AXI_AWVALID           => master2slave_01.M_AXI_AWVALID,
    M_AXI_AWREADY           => slave2master_01.M_AXI_AWREADY,
    M_AXI_WDATA             => master2slave_01.M_AXI_WDATA,
    M_AXI_WSTRB             => master2slave_01.M_AXI_WSTRB,
    M_AXI_WLAST             => master2slave_01.M_AXI_WLAST,
    M_AXI_WUSER             => master2slave_01.M_AXI_WUSER,
    M_AXI_WVALID            => master2slave_01.M_AXI_WVALID,
    M_AXI_WREADY            => slave2master_01.M_AXI_WREADY,
    M_AXI_BID               => slave2master_01.M_AXI_BID,
    M_AXI_BRESP             => slave2master_01.M_AXI_BRESP,
    M_AXI_BUSER             => slave2master_01.M_AXI_BUSER,
    M_AXI_BVALID            => slave2master_01.M_AXI_BVALID,
    M_AXI_BREADY            => master2slave_01.M_AXI_BREADY,
    M_AXI_ARID              => master2slave_01.M_AXI_ARID,
    M_AXI_ARADDR            => master2slave_01.M_AXI_ARADDR,
    M_AXI_ARLEN             => master2slave_01.M_AXI_ARLEN,
    M_AXI_ARSIZE            => master2slave_01.M_AXI_ARSIZE,
    M_AXI_ARBURST           => master2slave_01.M_AXI_ARBURST,
    M_AXI_ARLOCK            => master2slave_01.M_AXI_ARLOCK,
    M_AXI_ARCACHE           => master2slave_01.M_AXI_ARCACHE,
    M_AXI_ARPROT            => master2slave_01.M_AXI_ARPROT,
    M_AXI_ARQOS             => master2slave_01.M_AXI_ARQOS,
    M_AXI_ARUSER            => master2slave_01.M_AXI_ARUSER,
    M_AXI_ARVALID           => master2slave_01.M_AXI_ARVALID,
    M_AXI_ARREADY           => slave2master_01.M_AXI_ARREADY,
    M_AXI_RID               => slave2master_01.M_AXI_RID,
    M_AXI_RDATA             => slave2master_01.M_AXI_RDATA,
    M_AXI_RRESP             => slave2master_01.M_AXI_RRESP,
    M_AXI_RLAST             => slave2master_01.M_AXI_RLAST,
    M_AXI_RUSER             => slave2master_01.M_AXI_RUSER,
    M_AXI_RVALID            => slave2master_01.M_AXI_RVALID,
    M_AXI_RREADY            => master2slave_01.M_AXI_RREADY,
   
	In1                     => In1,
	intr_srio2axi           => intr_srio2axi,
 
    GPIO_LED_LEFT     => GPIO_LED_LEFT_01,
    GPIO_LED_CENTER   => GPIO_LED_CENTER_01,
    GPIO_LED_RIGHT    => GPIO_LED_RIGHT_01,
    GPIO_LED_0        => GPIO_LED_0_01,
    
    GPIO_DIP_SW0      => GPIO_DIP_SW0_01,
    GPIO_DIP_SW1      => GPIO_DIP_SW1_01,
    GPIO_DIP_SW2      => GPIO_DIP_SW2_01,
    GPIO_DIP_SW3      => GPIO_DIP_SW3_01,
    
    peripheral_aresetn(0)  => peripheral_aresetn
    );
      
      
  	FMC_HPC_GBTCLK0_M2C_P <= gt_clk0;
	FMC_HPC_GBTCLK0_M2C_N <= not gt_clk0;	

  	FMC_HPC_GBTCLK1_M2C_P <= gt_clk1;
	FMC_HPC_GBTCLK1_M2C_N <= not gt_clk1;	

	-----------------------------------------------------------------------------
	-- Clock generation.
	-----------------------------------------------------------------------------
	-- 200 MHz => 5 ns
	------SysClockGenerator: process
	--begin
		--sys_clk <= '0';
		--wait for 2.5 ns;
		--sys_clk <= '1';
		--wait for 2.5 ns;
	--end process;
	SysClockGenerator: process
	begin
		sys_clk <= '0';
		wait for 4 ns;
		sys_clk <= '1';
		wait for 4 ns;
	end process;

    -- 125 MHz => 8 ns
	FClkClockGenerator: process
	begin
		FCLK_CLK0 <= '0';
		wait for 4 ns;
		FCLK_CLK0 <= '1';
		wait for 4 ns;
	end process;
    
	sys_clk_p <= sys_clk;
	sys_clk_n <= not sys_clk;
    
    -- Use to drive test bench
    clk <= FCLK_CLK0;
	
	-- 250 MHz => 4 ns
	-- 156.25 MHz => 6.4 ns
    -- 
	GtClock0Generator: process
	begin
		gt_clk0 <= '0';
		wait for 2 ns;
		gt_clk0 <= '1';
		wait for 2 ns;
	end process;
    
	-- 250 MHz => 4 ns
	-- 156.25 MHz => 6.4 ns
    -- 
	GtClock1Generator: process
	begin
		gt_clk1 <= '0';
		wait for 2 ns;
		gt_clk1 <= '1';
		wait for 2 ns;
	end process;
    
    -----------------------------------------------------------------------------
	-- Reset generation.
	-----------------------------------------------------------------------------
    peripheral_aresetn <= rst_n;
    
    Reset: process
        variable data : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        variable result : boolean;
        variable frame_size : integer;
        variable test_result : boolean;
        variable base_addr   : std_logic_vector(31 downto 0);

        variable senderid : std_logic_vector(15 downto 0) := x"CAFE";
        variable destid : std_logic_vector(15 downto 0) := x"F00D";
        variable headerok : boolean := false;
        variable xfer_size : integer;
        variable memory : memory_t(0 to 1023);
        variable success : boolean;
	begin
        wait until rising_edge(clk);
        rst_n <= '0';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        rst_n <= '1';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        report "Reseting done";

         -- Default initialization of the bus values
        AxiLITEMasterInit(clk, master2slaveLite_01);
        AxiLITEMasterInit(clk, master2slaveLite_02);
        
        --result := Axi4LoadSlaveFromFile( "/home/asamama/Documents/test_data.txt");
        result := Axi4LoadSlaveFromFile( "/home/asamama/devel/distributedpf/hw/fpga/srio/data-netperf");
        -- Wait for physical connection to be detected
        WaitAndCheckResultFor("Port link ",  GPIO_LED_LEFT_01, '1',  "is up", 30 us);
        
        -- Read the identification register
        AxiLITEMasterRead( clk, master2slaveLite_01, slave2masterLite_01, x"FFFF0000", data );
        
        -- Do we have read GOOD_SIO in hexspeak
        result := CheckResult("Identification register", x"600D0510", data, hstr(data)); 
        
        -- Waiting for the connext being established, the led is turned on when it's ok
        WaitAndCheckResultFor("Logical link ",  GPIO_LED_CENTER_01, '1',  "is up", 30 us);

        -- Configure destination and reception ids
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000014", senderid & destid );

        -- Prepare a transfer without have been set the mailbox. This transfer should be
        --  ignored
        --base_addr := x"600DF000";
        -- ASA AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", x"600DFFF4");
        -- ASA AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", x"00000020");

        -- a Frame must be dropped at the receiving side. We wait for it
        -- ASA data :=  x"0000_0000";
        -- ASA while ( data = x"0000_0000" ) loop
            -- ASA AxiLITEMasterRead( clk, master2slaveLite_01, slave2masterLite_01, x"00000044", data);
            -- ASA wait for 1 us;
        -- ASA end loop;
        -- ASA result := CheckResult("Number of dropped frames", x"0000_0001", data, hstr(data)); 
        -- ASA result := CheckResult("Raised interrupt", '1', intr_srio2axi, "One interrupt raised");

        -- Reading the interrupt statsus
        -- ASA AxiLITEMasterRead( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", data );
        -- ASA result := CheckResult(" checking no RX (only TX)interrupt has been raised", x"0000_0010", data, hstr(data)); 
        -- Clear interrupts
        -- ASA AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"FFFF_FFFF" );

        -- Set the mailbox base adress on the receiver side
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000020", x"70000000");
        -- Set the mask specifying the size of the mailbox. here 256 entries of 4KB. 
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000024", x"000000FF");



        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DF000 of 0x00000020 bytes
        base_addr := x"600DF000";
        xfer_size := 76;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been received
        WaitAndCheckResultFor("Xfer 01 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterRead( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", data );
        result := CheckResult("Xfer 01 -  checking no RX (only TX)interrupt has been raised", x"0000_0002", data, hstr(data)); 
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"FFFF_FFFF" );

        WaitAndCheckResultFor("Xfer 01 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterRead( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", data );
        result := CheckResult("Xfer 01 -  checking RX interrupt has been raised", x"0000_0001", data, hstr(data)); 
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"FFFF_FFFF" );

        -- Checking if the memory content is as expected
        -- The Ethernet header is simulated with an OUI made of 0x0000 then 16 bits RIO Id, then 0x0000
        -- Header must be made of:
        -- Dest: 0x0000SRIO 0x0000  and Sender: 0x0000SRIO 0x0000 
        -- thus in 32 bits word
        -- 0x0000SRIO 0x0000000 0xSRIO0000 
        memory := Axi4SlaveGetMemory;


        headerok := memory(0).data = x"0000" & destid  and memory(1).data= x"0000_0000" and  memory(2).data = senderid & x"0000";
        result := CheckResult("Xfer 01 - Header from mailbox message", true, headerok, " Content ok"); 
        result := CheckResult("Xfer 01 - Payload from mailbox message", base_addr,  memory(3 to (3 + xfer_size/4 -1) ), PATTERN_INCREASING, "Content ok");

        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DF200 of 76 bytes
        -- one word after the checksum
        base_addr := x"26993000";
        xfer_size := 76;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 02 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been recevied
        WaitAndCheckResultFor("Xfer 02 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );

        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DF200 of 72 bytes, i.e.
        -- exactly stopping on the checksum
        base_addr := x"26993000";
        xfer_size := 72;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 02b - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been recevied
        WaitAndCheckResultFor("Xfer 02b - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );

        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DF480 of 0x000000100 bytes
        base_addr := x"600DF480";
        xfer_size := 256;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 03 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been recevied
        WaitAndCheckResultFor("Xfer 03 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );

        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DFw59000 of 100 bytes
        base_addr := x"600DF590";
        xfer_size := 100;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 04 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been recevied
        WaitAndCheckResultFor("Xfer 04 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );

        ----------------------------------------------------------------------------------------------------------------
        -- Prepare a transfer from the address 0x600DF790 of 20 bytes
        base_addr := x"600DF790";
        xfer_size := 100;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 05 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been received
        WaitAndCheckResultFor("Xfer 05 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        -- Acknowledge the interruption
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );
        WaitAndCheckResultFor("Xfer 05 - Waiting for interrupt cleared ",  intr_srio2axi, '0',  "done !", 30 us);

        --reset memory content
        result := Axi4SlaveClearMemory;

        -- Prepare a transfer from the address 0x80000000 of 256 bytes
        base_addr := x"80000000";
        xfer_size := 256;
        result := Axi4SlaveRewindMemory;
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"0000000C", base_addr);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000010", std_logic_vector(to_unsigned(xfer_size, 32)) );

        -- Waiting for interruption saying the a message has been transmitted
        WaitAndCheckResultFor("Xfer 06 - Waiting for message being transmitted ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0002" );
        -- Waiting for interruption saying the a message has been received
        WaitAndCheckResultFor("Xfer 06 - Waiting for message being received ",  intr_srio2axi, '1',  "is up", 30 us);
        AxiLITEMasterWrite( clk, master2slaveLite_01, slave2masterLite_01, x"00000034", x"0000_0001" );

        -- Retrieve the new memory content
        memory := Axi4SlaveGetMemory;

        headerok := memory(0).data = x"0000" & destid  and memory(1).data= x"0000_0000" and  memory(2).data = senderid & x"0000";
        result := CheckResult("Xfer 06 - Header from mailbox message", true, headerok, " Content ok"); 
        result := CheckResult("Xfer 06 - Payload from mailbox message", base_addr,  memory(3 to (3 + xfer_size/4 -1) ), PATTERN_INCREASING, "Content ok");
        wait;
        
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst read bus simulation 
	-- -----------------------------------------------------------------------------  
   slave_read_01: process
    begin
        Axi4SlaveReadInit(clk, slave2master_01 );
        loop
            --Axi4SlaveAcceptAnyRead( clk, master2slave_01, slave2master_01, PATTERN_FILE );
            Axi4SlaveAcceptAnyRead( clk, master2slave_01, slave2master_01, PATTERN_INCREASING );
        end loop;
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst write bus simulation 
	-- -----------------------------------------------------------------------------  
    slave_write_01: process
        -- Simulate memory for up to 1024 transaction
     begin
        Axi4SlaveWriteInit(clk, slave2master_01 );
         loop
             Axi4SlaveAcceptAnyWrite( clk, master2slave_01, slave2master_01 );
         end loop;
     end process; 
     
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst read bus simulation 
	-- -----------------------------------------------------------------------------  
   slave_read_02: process
    begin
        Axi4SlaveReadInit(clk, slave2master_02 );
        loop
            --Axi4SlaveAcceptAnyRead( clk, master2slave_02, slave2master_02, PATTERN_FILE );
            Axi4SlaveAcceptAnyRead( clk, master2slave_02, slave2master_02, PATTERN_INCREASING );
        end loop;
    end process;
    
 	-- -----------------------------------------------------------------------------
	-- -- AXI Slave burst write bus simulation 
	-- -----------------------------------------------------------------------------  
    slave_write_02: process
     begin
        Axi4SlaveWriteInit(clk, slave2master_02 );
         loop
             Axi4SlaveAcceptAnyWrite( clk, master2slave_02, slave2master_02 );
         end loop;
     end process; 	

end Behavioral;
