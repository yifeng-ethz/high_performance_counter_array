-- File name: counter_avmm.vhd 
-- Author: Yifeng Wang (yifenwan@phys.ethz.ch)
-- =======================================
-- Revision: 1.0 (file created)
--		Date: Feb 23, 2024
-- =========
-- Description:	[Counter with Avalon Memory-Mapped Inerface] 
-- 		this is a general counter using Altera LPM IP core. It has been equiped with an 
--		avalon interface for system integration.
--		The counting speed is set by the input clock. 
--		The overflow happens at 2^31, where bit 32 is used to indicate of an overflow. 
--		Once the overflow happened, the bit 32 will rise and not lower down until the reset is given
--		But, the counter will be able to continous count with the lower bits from 0. 
-- 		
-- ================ synthsizer configuration =================== 		
-- altera vhdl_input_version vhdl_2008
-- ============================================================= 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.math_real.log2;
use IEEE.math_real.ceil;
use ieee.std_logic_arith.conv_std_logic_vector;
--use ieee.std_logic_arith.conv_integer;

LIBRARY lpm; 
USE lpm.lpm_components.all;


entity counter_avmm is 
	generic (
		N_COUNTER					: natural;
		COUNTER_BITS				: natural := 31;
		ADDR_W						: natural;
		READ_ON_THE_FLY				: boolean; -- true: read always the current value of counters (errcnt). false: read the value before last sclr (chcnt)
		USE_TOP_BIT_AS_OF_FLAG		: natural := 1
	);
	port (
		-- avalon slave read-only interface 
		-- the readdata will be last point data before the last sclr (i.e. the counts of the last monitor window)
		avs_cntval_readdata				: out std_logic_vector(COUNTER_BITS+USE_TOP_BIT_AS_OF_FLAG-1 downto 0);
		avs_cntval_read					: in  std_logic;
		avs_cntval_address				: in  std_logic_vector(ADDR_W-1 downto 0);
		avs_cntval_writedata			: in  std_logic_vector(31 downto 0);
		avs_cntval_write				: in  std_logic;
		avs_cntval_waitrequest			: out std_logic;
		
		-- conduit input
		counter_en						: in std_logic_vector(N_COUNTER-1 downto 0);
		counter_sclr					: in std_logic;
		
		--
		i_clk							: in std_logic;
		i_rst							: in std_logic
	);
end entity counter_avmm;

architecture rtl of counter_avmm is
	--constant W_ADDR								: natural	 := integer(ceil(log2(real(N_COUNTER))));
	signal address_in											: std_logic_vector(ADDR_W-1 downto 0);
	signal avs_cntval_address_reg								: std_logic_vector(ADDR_W-1 downto 0);
	
	signal counter_cout_d1,counter_cout,counter_set_ov			: std_logic_vector(N_COUNTER-1 downto 0);
	signal overflow_flag,overflow_flag_last						: std_logic_vector(N_COUNTER-1 downto 0);
	
	type counter_value_t		is array(N_COUNTER-1 downto 0) of std_logic_vector(COUNTER_BITS-1 downto 0);
	signal counter_value,counter_value_last						: counter_value_t;
	signal counter_value_selected								: std_logic_vector(COUNTER_BITS+USE_TOP_BIT_AS_OF_FLAG-1 downto 0);
	
	type AVS_STATE_t				is (IDLE, RD, WR);
	signal AVS_STATE				: AVS_STATE_t;

	component LPM_COUNTER
	generic ( 
		LPM_WIDTH : natural; 
		LPM_MODULUS : natural := 0;
		LPM_DIRECTION : string := "UNUSED";
		LPM_AVALUE : string := "UNUSED";
		LPM_SVALUE : string := "UNUSED";
		LPM_PORT_UPDOWN : string := "PORT_CONNECTIVITY";
		LPM_PVALUE : string := "UNUSED";
		LPM_TYPE : string := L_COUNTER;
		LPM_HINT : string := "UNUSED");
	port (
		DATA : in std_logic_vector(LPM_WIDTH-1 downto 0):= (OTHERS => '0');
		CLOCK : in std_logic ;
		CLK_EN : in std_logic := '1';
		CNT_EN : in std_logic := '1';
		UPDOWN : in std_logic := '1';
		SLOAD : in std_logic := '0';
		SSET : in std_logic := '0';
		SCLR : in std_logic := '0';
		ALOAD : in std_logic := '0';
		ASET : in std_logic := '0';
		ACLR : in std_logic := '0';
		CIN : in std_logic := '1';
		COUT : out std_logic := '0';
		Q : out std_logic_vector(LPM_WIDTH-1 downto 0);
		EQ : out std_logic_vector(15 downto 0));
	end component;
	
	signal master_reset				: std_logic;
		

begin
	
	proc_offload_value : process (i_clk,i_rst)
	begin
		if (i_rst = '1') then
			
		elsif (rising_edge(i_clk)) then
			if (counter_sclr = '1' or READ_ON_THE_FLY) then -- reg the cnt value within last interval or it is set to continuously record the most up-to-date value
				counter_value_last		<= counter_value; -- this comb value is reg'd 
				overflow_flag_last		<= overflow_flag;
			else 
				counter_value_last		<= counter_value_last;
				overflow_flag_last		<= overflow_flag_last;
			end if;
			
		end if;
		
	-- once the sclr is coming, offload all register value (takes N cycles, where N is number of counters to the RAM of 32 bit width)
	-- when sclr will reset all counters simultaneously (or can be sequentially?) 
	-- This action will preserve the last value from the monitor window of the counters. 
	
	
	end process proc_offload_value;


	gen_counters : for i in 0 to N_COUNTER-1 generate 
		alt_lpm_counter : LPM_COUNTER 
		generic map (
			LPM_WIDTH		=> 31,
			--LPM_MODULUS		=> 2**31-1, -- The maximum count, plus one. 
			-- Number of unique states in the counter's cycle. 
			-- If the load value is larger than the LPM_MODULUS parameter, the behavior of the counter is not specified. 
			LPM_DIRECTION	=> "UP",
			LPM_AVALUE		=> "0",
			LPM_SVALUE		=> "0",
			LPM_PORT_UPDOWN	=> "PORT_CONNECTIVITY",
			LPM_PVALUE		=> "0"
		)
		port map (
			CLOCK			=> i_clk,
			CLK_EN			=> '1',
			CNT_EN			=> counter_en(i),
			SCLR			=> counter_sclr or i_rst or master_reset,
			COUT			=> counter_cout(i),
			Q				=> counter_value(i)
		);
	end generate gen_counters;
	
	gen_overflow_det: for i in 0 to N_COUNTER-1 generate 
	-- generate the overflow flags for all counters individually
		proc_counter_overflow_det	: process (i_clk,i_rst)
		begin
			if (i_rst = '1') then
				overflow_flag(i)		<= '0';
			elsif (rising_edge(i_clk)) then
				counter_cout_d1(i)		<= counter_cout(i);
				counter_set_ov(i)		<= counter_cout_d1(i) xor counter_cout(i);
				if (master_reset = '1') then -- reset from avalon write
					overflow_flag(i)		<= '0';
				elsif (counter_set_ov(i) = '1') then
					overflow_flag(i)		<= '1';
				end if;
			end if;
		end process proc_counter_overflow_det;
	end generate gen_overflow_det;
		
	proc_avs_interface : process (i_clk,i_rst) 
	begin
		if (i_rst = '1') then
			master_reset		<= '0';
			AVS_STATE			<= IDLE;
		elsif (rising_edge(i_clk)) then
			-- this intrinsically supports burst read (which do not happen by agent, TODO: implement burst port), and the read latency is 1. 
			case (AVS_STATE) is 
				when IDLE =>
					if (avs_cntval_read = '1') then
						AVS_STATE					<= RD;
						avs_cntval_address_reg		<= avs_cntval_address;
						 -- latch data already here! timing budget is 1-read_setup+1
					elsif (avs_cntval_write = '1') then
						AVS_STATE					<= WR;
					else
						avs_cntval_address_reg		<= (others=>'0');
					end if;
					master_reset		<= '0'; -- unset the reset
				when RD	=>
					if (avs_cntval_read = '1' and avs_cntval_address = avs_cntval_address_reg) then
					-- no change in addr (1st cycle): halt for master to capture
						
					elsif (avs_cntval_read = '0') then
					-- master completed transaction (2nd+ cycle): go back to IDLE
						AVS_STATE				<= IDLE;
					elsif (avs_cntval_read = '1' and avs_cntval_address /= avs_cntval_address_reg) then
					-- master started a new transaction (2nd+ cycle): new data will be available in the next cycle as addr=addr_reg
						avs_cntval_address_reg		<= avs_cntval_address;
					else
						-- release bus
					end if;
				when WR =>
					if (avs_cntval_writedata(0) = '0') then
						master_reset		<= '1'; -- set the reset
					end if;
					AVS_STATE			<= IDLE;
				when others=>
			end case;
		end if;
	end process proc_avs_interface;
	
	proc_avs_interface_comb	: process (all) 
	begin
		-- default
		
		address_in			<= (others => '1');
		case (AVS_STATE) is 
			when IDLE => 
				avs_cntval_waitrequest		<= '1';
				if (avs_cntval_read = '1') then
					address_in			<= avs_cntval_address;
				else
					address_in			<= (others => '1');
				end if;
			when RD => 
				address_in			<= avs_cntval_address; -- keep track of the address
				if (avs_cntval_read = '1' and avs_cntval_address = avs_cntval_address_reg) then
				-- no change (1st cycle): data is ready
					avs_cntval_waitrequest		<= '0';
				elsif (avs_cntval_read = '0') then
				-- master completed transaction (2nd+ cycle): go back to IDLE
					avs_cntval_waitrequest		<= '1'; -- TODO: check if this signal can be 1? yes trying now
				elsif (avs_cntval_read = '1' and avs_cntval_address /= avs_cntval_address_reg) then
				-- master started a new transaction
					avs_cntval_waitrequest		<= '1';
				else
					avs_cntval_waitrequest		<= '0'; -- release from exc case, preventing slave-dead
				end if;
			when WR =>
				avs_cntval_waitrequest			<= '0'; -- ack the master write
			when others =>
		end case;
	end process proc_avs_interface_comb;
	
	
	proc_mux_comb	: process (all)
	begin
		avs_cntval_readdata				<= counter_value_selected;
		if (to_integer(unsigned(address_in))  < N_COUNTER) then
			counter_value_selected		<= overflow_flag_last(to_integer(unsigned(address_in))) & counter_value_last(to_integer(unsigned(address_in)));
		else -- decode err?
			counter_value_selected		<= overflow_flag_last(0) & counter_value_last(0);
		end if;
	end process proc_mux_comb;
	

	
	
	
	

	


	
end architecture rtl;