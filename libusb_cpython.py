#!/usr/bin/python

# This file is part of AutoMateHome.

# AutoMateHome is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# AutoMateHome is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AutoMateHome.  If not, see <http://www.gnu.org/licenses/>.

"""Libusb abstraction based on the libusb-cpython package. This is
   really only useful for debugging - in general one should use the
   CM15_libusb one instead."""


import LibUSB

import logging
import utils
import threading

LOGGER = logging.getLogger(__name__)

TRANSFER_COMPLETED = LibUSB.TRANSFER_COMPLETED
TRANSFER_TIMED_OUT = LibUSB.TRANSFER_TIMED_OUT
TRANSFER_TYPE_INTERRUPT = LibUSB.TRANSFER_TYPE_INTERRUPT


def getHandleToLowSpeedDevice(vencor_id, device_id):

  # find the device
  device = LibUSB.get_device_with_vid_pid(vendor_id, device_id)
  if device is None:
    raise Exception("unable to find X10 device")
  LOGGER.debug("found X10 device")

  # the cm15 is a low speed device (1.5MBit/s) which means that its
  # maximum data payload is 8 bytes per transfer
  assert(device.device_speed() == LibUSB.SPEED_LOW)

  # open the device:
  dh = device.open()
  LOGGER.debug("opened a handle to the device")

  return dh

def claimInterfaceAndGetEndpoints(dh):

  # Get the interface descriptor (there should only be one
  # alternative) and the interface number:
  desc = dh.active_config_descriptor()
  assert(len(desc["interfaces"]) == 1)
  assert(len(desc["interfaces"][0]) == 1)
  interface0Setting0 = desc["interfaces"][0][0]
  interfaceNumber = interface0Setting0["bInterfaceNumber"]

  # Claim the interface
  if dh.kernel_driver_active(interfaceNumber):
    LOGGER.debug("X10 device has kernel driver, detaching")
    dh.detach_kernel_driver(interfaceNumber)
  dh.claim_interface(interfaceNumber)
  LOGGER.debug("claimed the device interface %d" % interfaceNumber)

  # get the endpoints for the interface, there should only be two (input and output):
  assert(interface0Setting0["bNumEndpoints"] == 2)
  endpoints = interface0Setting0["endpoints"]
  assert(2 == len(endpoints))
  inputEndPoint = None
  outputEndPoint = None

  for ep in endpoints:
    address = ep["bEndpointAddress"]
    if LibUSB.ENDPOINT_IN == (address & LibUSB.ENDPOINT_DIR_MASK):
      inputEndPoint = address # data from cm15 arrives on this endpoint
    else:
      assert(LibUSB.ENDPOINT_OUT == (address & LibUSB.ENDPOINT_DIR_MASK))
      outputEndPoint = address # write to cm15 on this one

  assert(inputEndPoint is not None)
  assert(outputEndPoint is not None)

  return (inputEndPoint, outputEndPoint)

def createInterruptTransfer(dh, endpoint, bufferSize, callback, timeoutInMillis):
  th = LibUSB.transfer(0)
  th.fill_interrupt_transfer(dh, endpoint, "", callback, timeoutInMillis)
  th.zero_transfer_buffer(bufferSize)
  return th

def submitTransfer(th, data=None):
  if data is not None:
    th.set_transfer_buffer(data)
  th.submit_transfer()

def startPolling():
  devicePollLatch = utils.Latch()
  pollThread = threading.Thread(name="USBPollThread", target=lambda: __pollDevice(devicePollLatch))
  pollThread.daemon = True
  pollThread.start()
  devicePollLatch.waitForIt(5)
  LOGGER.debug("poll thread started, waiting for input")

def __pollDevice(latch):

  # wait for data from the device using the asynchronous API. See
  # http://libusb.sourceforge.net/api-1.0/
  pollobj = select.poll()
  pollfds = LibUSB.get_pollfds()
  LOGGER.debug("retreived poll file descriptors: %s" % pollfds)
  for (fd, events) in pollfds:
    pollobj.register(fd, events)

  while True:
    latch.open()
    try:
      timeoutInMillis = None
      t0 = datetime.datetime.now()
      nextTimeout = LibUSB.get_next_timeout()
      if nextTimeout is not None:
        (secs, usecs) = nextTimeout
        timeoutInMillis = secs * 1000.0 + usecs * 0.001
        if LOGGER.isEnabledFor(logging.DEBUG):
          LOGGER.debug("next timeout: %(timeoutInMillis)f" % locals())
      pollresult = pollobj.poll(timeoutInMillis) # milliseconds
      t1 = datetime.datetime.now()
      dt = t1 - t0
      elapsedTimeInMillis = dt.seconds * 1000.0 + dt.microseconds * 0.001
      if LOGGER.isEnabledFor(logging.DEBUG):
        LOGGER.debug("poll result: %(pollresult)s, timeout: %(timeoutInMillis)s, elapsed: %(elapsedTimeInMillis)f" % locals())
      if (len(pollresult) > 0) or (timeoutInMillis is None) or (elapsedTimeInMillis > timeoutInMillis):
        LibUSB.handle_events_timeout()
    except Exception,e:
      exc_type, exc_value, exc_traceback = sys.exc_info()
#        msg = traceback.format_exception_only(exc_type, exc_value);
      msg = traceback.format_exception(exc_type, exc_value, exc_traceback);
      LOGGER.error("\n".join(msg))

  
def transferStatusToString(status):
  return LibUSB.transfer_status_str(status)
