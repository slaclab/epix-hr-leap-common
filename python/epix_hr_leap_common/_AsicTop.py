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

import surf.protocols.batcher as batcher
import epix_hr_core as ePixHrCore
import epix_hr_leap_common as ePixHrleapCommon

class AsicTop(pr.Device):
    def __init__( self,asicStreams=5, debugChEnum=[], snEnum={}, **kwargs):
        super().__init__(**kwargs)

        DigitalAsicStreamAxiOffset = 0x0020_0000
        AxiStreamBatcherEventBuilderOffset = DigitalAsicStreamAxiOffset + 0x0010_0000 * asicStreams

        self.add(ePixHrleapCommon.RegisterControlDualClock(
            offset = 0x0000_0000,
            debugChEnum=debugChEnum,
            snEnum=snEnum
        ))

        self.add(ePixHrCore.TriggerRegisters(
            offset = 0x0010_0000,
            triggerFreq = 156.25e6,
            axiFreq     = 156.25e6,
        ))

        for indexAsicStreams in range(asicStreams):
            self.add(
                ePixHrleapCommon.DigitalAsicStreamAxi(
                    name="DigAsicStrmRegisters{}".format(indexAsicStreams),
                    offset=DigitalAsicStreamAxiOffset + 0x0010_0000 * indexAsicStreams,
                    expand=False,
                    enabled=True,
                    numberLanes=24))

        for indexAsicStreams in range(asicStreams):
            self.add(
                batcher.AxiStreamBatcherEventBuilder(
                    name="BatcherEventBuilder{}".format(indexAsicStreams),
                    offset=AxiStreamBatcherEventBuilderOffset + 0x0010_0000 * indexAsicStreams,
                    expand=False,
                    numberSlaves = 2))

