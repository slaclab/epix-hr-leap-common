-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Deserializer Group
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

entity AppDeserGroup is
   generic (
       TPD_G            : time    := 1 ns;
       SIMULATION_G     : boolean := false;
       NUM_OF_LANES_G   : integer := 24
   );
   port (
      -- Asic Ports
      asicDataP         : in  slv(NUM_OF_LANES_G - 1  downto 0);
      asicDataM         : in  slv(NUM_OF_LANES_G - 1  downto 0);
      -- AXI-Lite Interface (axilClk domain)
      axilClk           : in  sl;
      axilRst           : in  sl;
      axilReadMaster    : in  AxiLiteReadMasterType;
      axilReadSlave     : out AxiLiteReadSlaveType;
      axilWriteMaster   : in  AxiLiteWriteMasterType;
      axilWriteSlave    : out AxiLiteWriteSlaveType;
      -- Ref Clocks MMCM Out (0)
      sspClk4x          : in sl;
      sspClk            : in sl;
      sspRst            : in sl;
      -- ssp outputs
      sspLinkUp         : out slv(NUM_OF_LANES_G - 1 downto 0);
      sspValid          : out slv(NUM_OF_LANES_G - 1 downto 0);
      sspData           : out Slv16Array(NUM_OF_LANES_G - 1 downto 0);
      sspSof            : out slv(NUM_OF_LANES_G - 1 downto 0);
      sspEof            : out slv(NUM_OF_LANES_G - 1 downto 0);
      sspEofe           : out slv(NUM_OF_LANES_G - 1 downto 0)
   );
end entity AppDeserGroup;

architecture mapping of AppDeserGroup is

   signal deserData : Slv8Array(23 downto 0);
   signal dlyLoad   : slv(23 downto 0);
   signal dlyCfg    : Slv9Array(23 downto 0);

begin
    
   -------------------------------------------------------
   -- ASIC Deserializers
   -------------------------------------------------------
   GEN_VEC :
   for i in NUM_OF_LANES_G - 1 downto 0 generate 
      U_Lane : entity surf.SelectioDeserLaneUltraScale
         generic map(
            TPD_G        => TPD_G,
            SIM_DEVICE_G => XIL_DEVICE_C
            )
         port map (
            -- SELECTIO Ports
            rxP     => asicDataP(i),
            rxN     => asicDataM(i),
            -- Clock and Reset Interface
            clkx4   => sspClk4x,
            clkx1   => sspClk,
            rstx1   => sspRst,
            -- Delay Configuration
            dlyLoad => dlyLoad(i),
            dlyCfg  => dlyCfg(i),
            -- Output
            dataOut => deserData(i)
         );

         end generate GEN_VEC;

   -------------------------------------------------------
   -- ASIC Gearboxes and SSP decoders
   -------------------------------------------------------
   U_SspDecoder : entity surf.SspLowSpeedDecoder8b10bWrapper
      generic map (
         TPD_G            => TPD_G,
         SIMULATION_G     => SIMULATION_G,
         NUM_LANE_G       => 24)
      port map (
         -- Deserialization Interface (deserClk domain)
         deserClk        => sspClk,
         deserRst        => sspRst,
         deserData       => deserData,
         dlyLoad         => dlyLoad,
         dlyCfg          => dlyCfg,
         -- SSP Frame Output
         rxLinkUp        => sspLinkUp,
         rxValid         => sspValid,
         rxData          => sspData,
         rxSof           => sspSof,
         rxEof           => sspEof,
         rxEofe          => sspEofe,
         -- AXI-Lite Interface (axilClk domain)
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => axilReadMaster,
         axilReadSlave   => axilReadSlave,
         axilWriteMaster => axilWriteMaster,
         axilWriteSlave  => axilWriteSlave
      );

end architecture;
