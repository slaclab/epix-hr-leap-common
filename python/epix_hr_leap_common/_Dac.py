#-----------------------------------------------------------------------------
# This file is part of the 'epix-hr-leap-common'. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the 'epix-hr-leap-common', including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------

import pyrogue as pr

import surf.devices.maxim as maxim
import epix_hr_core       as epixHr

class Dac(pr.Device):
    def __init__( self,**kwargs):
        super().__init__(**kwargs)

        self.add(maxim.Max5443(
            offset  = 0*0x0001_0000,
            numChip = 1,
        ))

        self.add(epixHr.HighSpeedDacRegisters(
            name    = 'FastDac',
            offset  = 1*0x0001_0000,
            DacModel= 'Max5719a'
        ))

        self.add(pr.MemoryDevice(
            name        = 'WaveformMem',
            offset      = 2*0x0001_0000,
            wordBitSize = 16,
            stride      = 4,
            size        = 1024*4,
            hidden      = True,
        ))
