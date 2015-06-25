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

entity top_switch_tb is
end top_switch_tb;

architecture Behavioral of top_switch_tb is

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
    
   
      
    signal   USRCLK_P           : std_logic;
    signal   USRCLK_N           : std_logic;
    

    signal  FMC_HPC_GBTCLK0_M2C_P  : std_logic;
    signal  FMC_HPC_GBTCLK0_M2C_N  : std_logic;
    signal  FMC_HPC_GBTCLK1_M2C_P  : std_logic;
    signal  FMC_HPC_GBTCLK1_M2C_N  : std_logic;

    signal PMOD1_0_LS : STD_LOGIC;  -- UART RX
    signal PMOD1_1_LS : STD_LOGIC; -- UART TX
  
   
begin
	-----------------------------------------------------------------------------
	-- Buses connectivity.
	-----------------------------------------------------------------------------
	busRX_01_p(2 to 3) <= (others => '0');
	busRX_01_n(2 to 3) <= (others => '1');

	busRX_01_p(0) <=  busTX_01_p(1) ;	
	busRX_01_n(0) <=  busTX_01_n(1) ;
	
	busRX_01_p(1) <= busTX_01_p(0) ;	
	busRX_01_n(1) <= busTX_01_n(0) ;        

top_1: entity work.top_switch
   port map (
    sysclk_p          => sys_clk_p,
    sysclk_n          => sys_clk_n,

    USRCLK_P          => USRCLK_P,
    USRCLK_N          => USRCLK_N,
 
    -- UART0 connected to PMOD_1_x
    PMOD1_0_LS        => PMOD1_0_LS,
    PMOD1_1_LS        => PMOD1_1_LS,
   
    ext_rst_n         => rst_n,

    FIXED_IO_ddr_vrn  => FIXED_IO_ddr_vrn_01,
    FIXED_IO_ddr_vrp  => FIXED_IO_ddr_vrp_01,
    FIXED_IO_mio      => FIXED_IO_mio_01,
    FIXED_IO_ps_clk   => FIXED_IO_ps_clk_01,
    FIXED_IO_ps_porb  => FIXED_IO_ps_porb_01,
    FIXED_IO_ps_srstb => FIXED_IO_ps_srstb_01,

    -- TX differential on Channel 0
    FMC_HPC_DP3_C2M_P =>  busTX_01_p(0),
    FMC_HPC_DP3_C2M_N =>  busTX_01_n(0),

    -- RX differential on Channel 0
    FMC_HPC_DP3_M2C_P  =>  busRX_01_p(0),
    FMC_HPC_DP3_M2C_N  =>  busRX_01_n(0),

    -- TX differential on Channel 1
    FMC_HPC_DP2_C2M_P  =>  busTX_01_p(1),
    FMC_HPC_DP2_C2M_N  =>  busTX_01_n(1),

    -- RX differential on Channel 1
    FMC_HPC_DP2_M2C_P  => busRX_01_p(1),
    FMC_HPC_DP2_M2C_N  => busRX_01_n(1),

    -- TX differential on Channel 2
    FMC_HPC_DP1_C2M_P  =>  busTX_01_p(2),
    FMC_HPC_DP1_C2M_N  =>  busTX_01_n(2),

    -- RX differential on Channel 2
    FMC_HPC_DP1_M2C_P  => busRX_01_p(2),
    FMC_HPC_DP1_M2C_N  => busRX_01_n(2),

    -- TX differential on Channel 3
    FMC_HPC_DP0_C2M_P  =>  busTX_01_p(3),
    FMC_HPC_DP0_C2M_N  =>  busTX_01_n(3),

    -- RX differential on Channel 3
    FMC_HPC_DP0_M2C_P  => busRX_01_p(3),
    FMC_HPC_DP0_M2C_N  => busRX_01_n(3),



    FMC_HPC_GBTCLK0_M2C_C_P  => FMC_HPC_GBTCLK0_M2C_P,
    FMC_HPC_GBTCLK0_M2C_C_N  => FMC_HPC_GBTCLK0_M2C_N,

    FMC_HPC_GBTCLK1_M2C_C_P  => FMC_HPC_GBTCLK1_M2C_P,
    FMC_HPC_GBTCLK1_M2C_C_N  => FMC_HPC_GBTCLK1_M2C_N,

    -- ====================================================================
    -- DDR-SDRAM Signals
    -- ====================================================================
    DDR_addr        => DDR_addr_01,
    DDR_ba          => DDR_ba_01   ,
    DDR_cas_n       => DDR_cas_n_01,
    DDR_ck_n        => DDR_ck_n_01 ,
    DDR_ck_p        => DDR_ck_p_01 ,
    DDR_cke         => DDR_cke_01  ,
    DDR_cs_n        => DDR_cs_n_01 ,
    DDR_dm          => DDR_dm_01   ,
    DDR_dq          => DDR_dq_01   ,
    DDR_dqs_n       => DDR_dqs_n_01,
    DDR_dqs_p       => DDR_dqs_p_01,
    DDR_odt         => DDR_odt_01  ,
    DDR_ras_n       => DDR_ras_n_01,
    DDR_reset_n     => DDR_reset_n_01 ,
    DDR_we_n        => DDR_we_n_01   ,

    -- ====================================================================
    -- Soldered board leds
    -- ====================================================================
    GPIO_LED_LEFT       => open,
    GPIO_LED_CENTER     => open,
    GPIO_LED_RIGHT      => open,
    GPIO_LED_0          => open,

    -- ====================================================================
    -- Soldered board switches
    -- ====================================================================
    GPIO_DIP_SW0        => '0',
    GPIO_DIP_SW1        => '0',
    GPIO_DIP_SW2        => '0',
    GPIO_DIP_SW3        => '0'

);

    
  	FMC_HPC_GBTCLK0_M2C_P <= gt_clk0;
	FMC_HPC_GBTCLK0_M2C_N <= not gt_clk0;	

  	FMC_HPC_GBTCLK1_M2C_P <= gt_clk1;
	FMC_HPC_GBTCLK1_M2C_N <= not gt_clk1;	

	-----------------------------------------------------------------------------
	-- Clock generation.
	-----------------------------------------------------------------------------
	-- 125 MHz => 8 ns
	-- 200 MHz => 5 ns
	-- 250 MHz => 4 ns
	SysClockGenerator: process
	begin
		sys_clk <= '0';
		wait for 4 ns;
		sys_clk <= '1';
		wait for 4 ns;
	end process;

	sys_clk_p <= sys_clk;
	sys_clk_n <= not sys_clk;
    
	
    --
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
    
    --
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
    
    Reset: process
	begin
        wait for 5 ns;
        rst_n <= '0';
        wait for 5 ns;
        rst_n <= '1';
        report "External resetting done";

        wait;
        
    end process;
    
    
end Behavioral;
