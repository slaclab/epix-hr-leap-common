-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: ASIC Deserializer Top-Level
-------------------------------------------------------------------------------
-- This file is part of 'epix-hr-leap-common' submodule.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'epix-hr-leap-common', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.AxiLitePkg.all;

library work;
use work.AppPkg.all;

library unisim;
use unisim.vcomponents.all;

entity AppDeser is
   generic (
      TPD_G            : time    := 1 ns;
      SIMULATION_G     : boolean := false;
      AXIL_BASE_ADDR_G : slv(31 downto 0);
      NUM_OF_LANES_G  : integer := 5);
   port (
      -- Clocks and Resets
      sspClk4x        : in  sl;
      -- ASIC Ports
      asicDataP       : in    Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      asicDataM       : in    Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      -- AXI-Lite Interface (axilClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      -- SSP Interfaces (sspClk domain)
      sspClk          : in  sl;
      sspRst          : in  sl;
      -- Ssp data outputs
      sspLinkUp       : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspValid        : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspData         : out Slv16Array(((NUM_OF_LANES_G - 1) * 24)-1 downto 0);
      sspSof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEofe         : out Slv24Array(NUM_OF_LANES_G - 1 downto 0));
end AppDeser;

architecture mapping of AppDeser is
  
   constant NUM_AXIL_MASTERS_C : positive := NUM_OF_LANES_G;

   constant XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXIL_MASTERS_C-1 downto 0) := genAxiLiteConfig(NUM_AXIL_MASTERS_C, AXIL_BASE_ADDR_G, 16, 12);

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0) := (others => AXI_LITE_WRITE_SLAVE_EMPTY_SLVERR_C);
   signal axilReadMasters  : AxiLiteReadMasterArray(NUM_AXIL_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray(NUM_AXIL_MASTERS_C-1 downto 0)  := (others => AXI_LITE_READ_SLAVE_EMPTY_SLVERR_C);

   signal sspReset         : slv(NUM_OF_LANES_G-1 downto 0);
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

   GEN_VEC : for i in NUM_OF_LANES_G - 1 downto 0 generate

      U_Deser_Group : entity work.AppDeserGroup
         generic map (
            TPD_G          => TPD_G,
            SIMULATION_G   => SIMULATION_G)
         port map (

            -- Asic Ports
            asicDataP        => asicDataP(i),
            asicDataM        => asicDataM(i),

            -- AXI-Lite Interface (axilClk domain)
            axilClk          => axilClk,
            axilRst          => axilRst,
            axilReadMaster   => axilReadMasters(i),
            axilReadSlave    => axilReadSlaves(i),
            axilWriteMaster  => axilWriteMasters(i),
            axilWriteSlave   => axilWriteSlaves(i),


            sspClk4x         => sspClk4x,
            sspClk           => sspClk,
            sspRst           => sspReset(i),

            sspLinkUp         => sspLinkUp(i),
            sspValid          => sspValid(i),
            sspData           => sspData(24*i+23 downto 24*i),
            sspSof            => sspSof(i),
            sspEof            => sspEof(i),
            sspEofe           => sspEofe(i)
         );

      U_reset : entity surf.RstPipeline
         generic map (
            TPD_G => TPD_G)
         port map (
            clk    => sspClk,
            rstIn  => sspRst,
            rstOut => sspReset(i));

   end generate GEN_VEC;

end mapping;
