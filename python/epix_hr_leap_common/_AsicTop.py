#-----------------------------------------------------------------------------
# This file is part of the 'Simple-PGPv4-KCU105-Example'. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the 'Simple-PGPv4-KCU105-Example', including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------

import pyrogue as pr

import ePix320kM as fpga
import surf.protocols.batcher as batcher
import epix_hr_core as ePixHrCore
import epix_hr_leap_common as ePixHrleapCommon

class AsicTop(pr.Device):
    def __init__( self,**kwargs):
        super().__init__(**kwargs)

        self.add(ePixHrleapCommon.RegisterControlDualClock(
            offset = 0x0000_0000,
        ))

        self.add(ePixHrCore.TriggerRegisters(
            offset = 0x0010_0000,
        ))

        # DigitalAsicStreamAxi 4 instances, 1 for each asic
        self.add((
            ePixHrleapCommon.DigitalAsicStreamAxi(
                name="DigAsicStrmRegisters0",
                offset=0x0020_0000,
                expand=False,
                enabled=True,
                numberLanes=24),
            ePixHrleapCommon.DigitalAsicStreamAxi(
                name="DigAsicStrmRegisters1",
                offset=0x0030_0000,
                expand=False,
                enabled=True,
                numberLanes=24),
            ePixHrleapCommon.DigitalAsicStreamAxi(
                name="DigAsicStrmRegisters2",
                offset=0x0040_0000,
                expand=False,
                enabled=True,
                numberLanes=24),
            ePixHrleapCommon.DigitalAsicStreamAxi(
                name="DigAsicStrmRegisters3",
                offset=0x0050_0000,
                expand=False,
                enabled=True,
                numberLanes=24),           
        ))

        self.add((
            batcher.AxiStreamBatcherEventBuilder(
                name="BatcherEventBuilder0",
                offset=0x0060_0000,
                expand=False,
                numberSlaves = 2),
            batcher.AxiStreamBatcherEventBuilder(
                name="BatcherEventBuilder1",
                offset=0x0070_0000,
                expand=False,
                numberSlaves = 2),
            batcher.AxiStreamBatcherEventBuilder(
                name="BatcherEventBuilder2",
                offset=0x0080_0000,
                expand=False,
                numberSlaves = 2),
            batcher.AxiStreamBatcherEventBuilder(
                name="BatcherEventBuilder3",
                offset=0x0090_0000,
                expand=False,
                numberSlaves = 2),                                           
        ))
