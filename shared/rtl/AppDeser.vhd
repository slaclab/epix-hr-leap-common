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

      axilWriteMasters : in  AxiLiteWriteMasterArray(NUM_OF_LANES_G-1 downto 0);
      axilWriteSlaves  : out AxiLiteWriteSlaveArray(NUM_OF_LANES_G-1 downto 0);
      axilReadMasters  : in  AxiLiteReadMasterArray(NUM_OF_LANES_G-1 downto 0);
      axilReadSlaves   : out AxiLiteReadSlaveArray(NUM_OF_LANES_G-1 downto 0);

      -- SSP Interfaces (sspClk domain)
      sspClk          : in  sl;
      sspRst          : in  sl;
      -- Ssp data outputs
      sspLinkUp       : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspValid        : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspData         : out Slv16Array((NUM_OF_LANES_G * 24)-1 downto 0);
      sspSof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEof          : out Slv24Array(NUM_OF_LANES_G - 1 downto 0);
      sspEofe         : out Slv24Array(NUM_OF_LANES_G - 1 downto 0));
end AppDeser;

architecture mapping of AppDeser is
  
   signal sspReset         : slv(NUM_OF_LANES_G-1 downto 0);
begin

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
