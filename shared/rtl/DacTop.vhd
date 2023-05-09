-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Wrapper for DAC modules
-------------------------------------------------------------------------------
-- This file is part of 'Simple-PGPv4-KCU105-Example'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'Simple-PGPv4-KCU105-Example', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.AxiLitePkg.all;

library epix_hr_core;
use epix_hr_core.DacModelsPkg.all;

library unisim;
use unisim.vcomponents.all;

use work.CorePkg.all;

entity DacTop is
   generic (
      TPD_G            : time    := 1 ns;
      SIMULATION_G     : boolean := false;
      AXIL_BASE_ADDR_G : slv(31 downto 0);
      SHARED_PORTS_G   : boolean := false);
   port (
      dacTrig         : in  sl;
      -- AXI-Lite Interface (axilClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      -- Fast Dac Ports
      fastDacCsL          : out sl;
      fastDacSclk         : out sl;
      fastDacDin          : out sl;
      fastDacLoadL        : out sl;

      -- Slow Dac Ports
      slowDacDin         : out sl;
      slowDacSclk        : out sl;
      slowDacCsL         : out sl;
      slowDacClrL        : out sl);
end DacTop;

architecture mapping of DacTop is

   constant NUM_AXIL_MASTERS_C : positive := 3;

   constant XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXIL_MASTERS_C-1 downto 0) := genAxiLiteConfig(NUM_AXIL_MASTERS_C, AXIL_BASE_ADDR_G, 20, 16);

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C);
   signal axilReadMasters  : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0)  := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);

   signal WFDacDin  : sl;
   signal WFDacSclk : sl;
   signal WFDacClrL : sl;
   signal WFdacCsL  : sl;

   signal sDacSclk : sl;
   signal sDacDin  : sl;
      

begin

   ---------------------------
   -- AXI-Lite Crossbar Module
   ---------------------------
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => NUM_AXIL_MASTERS_C,
         MASTERS_CONFIG_G   => XBAR_CONFIG_C)
      port map (
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves(0)   => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves,
         axiClk              => axilClk,
         axiClkRst           => axilRst);

   --------------------------------------
   -- Routing DAC signals to external IOs
   --------------------------------------
   fastDacCsL <= WFdacCsL;
   slowDacDin  <= sDacDin;
   slowDacSclk <= sDacSclk;   


   G_SHARED : if (SHARED_PORTS_G = true) generate
      -- shared DAC signal
      fastDacSclk <= WFDacSclk when (WFdacCsL = '0') else sDacSclk;
      fastDacDin  <=  WFDacDin when (WFdacCsL = '0') else sDacDin;
   end generate G_SHARED;


   G_NOT_SHARED : if (SHARED_PORTS_G = false) generate
      -- shared DAC signal
      fastDacSclk <= WFDacSclk;
      fastDacDin  <=  WFDacDin;
   end generate G_NOT_SHARED;

   ----------------------------
   -- High speed DAC (DAC8812C)
   ----------------------------
   U_HS_DAC : entity epix_hr_core.DacWaveformGenAxi
      generic map (
         TPD_G => TPD_G,
         DAC_MODEL_G => DAC5719,
         DAC_DATA_WIDTH_G => 20 )
      port map (
         -- Master system clock
         sysClk           => axilClk,
         sysClkRst        => axilRst,
         -- DAC Control Signals
         dacDin           => WFDacDin,
         dacSclk          => WFDacSclk,
         dacCsL           => WFdacCsL,
         dacLdacL         => fastDacLoadL,
         dacClrL          => WFDacClrL,
         -- external trigger
         externalTrigger  => dacTrig,
         -- AXI lite slave port for register access
         axilClk          => axilClk,
         axilRst          => axilRst,
         sAxilWriteMaster => axilWriteMasters(2 downto 1),
         sAxilWriteSlave  => axilWriteSlaves(2 downto 1),
         sAxilReadMaster  => axilReadMasters(2 downto 1),
         sAxilReadSlave   => axilReadSlaves(2 downto 1));

   --------------------------
   -- Low Speed DAC (MAX5443)
   --------------------------
   U_LS_DAC : entity surf.Max5443
      generic map (
         TPD_G        => TPD_G,
         CLK_PERIOD_G => AXIL_CLK_PERIOD_C,
         NUM_CHIPS_G  => 1)
      port map (
         -- Global Signals
         axilClk         => axilClk,
         axilRst         => axilRst,
         -- AXI-Lite Register Interface (axiClk domain)
         axilReadMaster  => axilReadMasters(0),
         axilReadSlave   => axilReadSlaves(0),
         axilWriteMaster => axilWriteMasters(0),
         axilWriteSlave  => axilWriteSlaves(0),
         -- Guard ring DAC interfaces
         dacSclk         => sDacSclk,
         dacDin          => sDacDin,
         dacCsb(0)       => slowDacCsL,
         dacClrb         => slowDacClrL);

end mapping;