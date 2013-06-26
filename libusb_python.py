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

"""Libusb abstraction based on the python-libusb library. This is the
   preferred implementation, as python-libusb is widely used and
   implemented purely in python, so much more convenient to install."""

import usb1
import libusb1

import logging
import utils
import select

LOGGER = logging.getLogger(__name__)

class Poller:
  def __init__(self):
    self.pollobj = select.poll()

  def poll(self, timeoutInSeconds):
    if timeoutInSeconds is not None:
      timeoutInMillis = int(timeoutInSeconds * 1000.0)
    else:
      timeoutInMillis = None
    return self.pollobj.poll(timeoutInMillis)

  def register(self, fd, events):
    self.pollobj.register(fd, events)

  def unregister(self, fd):
    self.pollobj.unregister(fd)

class USBPollerThreadWithStartNotify(usb1.USBPollerThread):
  def __init__(self, context, poller, latch):
    self.latch = latch
    usb1.USBPollerThread.__init__(self, context, poller, self.excCallback)

  def run(self):
    self.latch.open()
    usb1.USBPollerThread.run(self)
    
  def excCallback(self, exc):
      exc_type, exc_value, exc_traceback = sys.exc_info()
#        msg = traceback.format_exception_only(exc_type, exc_value);
      msg = traceback.format_exception(exc_type, exc_value, exc_traceback);
      LOGGER.error("\n".join(msg))
    
class USBContext:

  def __init__(self):
    self.context = usb1.USBContext()

 
  def getHandleToLowSpeedDevice(self, vendor_id, device_id):

    # find the device
    device = self.context.getByVendorIDAndProductID(vendor_id, device_id, skip_on_access_error=True, skip_on_error=True)
    if device is None:
      raise Exception("unable to find X10 device")
    LOGGER.debug("found X10 device")

    # the cm15 is a low speed device (1.5MBit/s) which means that its
    # maximum data payload is 8 bytes per transfer
    assert(device.getDeviceSpeed() == libusb1.LIBUSB_SPEED_LOW)

    # open the device:
    dh = device.open()
    LOGGER.debug("opened a handle to the device")

    return dh

  def claimInterfaceAndGetEndpoints(self, dh):
  
    # Get the interface descriptor (there should only be one
    # alternative) and the interface number:
    device = dh.getDevice()
    assert(device.getNumConfigurations() == 1)
    config = device.iterConfiguations().next()
    assert(config.getNumInterfaces() == 1)
    interface0 = config.iterInterfaces().next()
    assert(interface0.getNumSettings() == 1)
    interface0Setting0 = interface0.iterSettings().next()
    bInterfaceNumber = 0
    assert(interface0Setting0.getNumber() == 0)
  
    # Claim the interface
    if dh.kernelDriverActive(bInterfaceNumber):
      LOGGER.debug("X10 device has kernel driver, detaching")
      dh.detachKernelDriver(bInterfaceNumber)
    dh.claimInterface(bInterfaceNumber)
    LOGGER.debug("claimed the device interface")
  
    # get the endpoints for the interface, there should only be two (input and output):
    assert(interface0Setting0.getNumEndpoints() == 2)
    inputEndPoint = None
    outputEndPoint = None
  
    for ep in interface0Setting0:
      address = ep.getAddress()
      if libusb1.LIBUSB_ENDPOINT_IN == (address & libusb1.USB_ENDPOINT_DIR_MASK):
        inputEndPoint = address # data from cm15 arrives on this endpoint
      else:
        assert(libusb1.LIBUSB_ENDPOINT_OUT == (address & libusb1.USB_ENDPOINT_DIR_MASK))
        outputEndPoint = address # write to cm15 on this one
  
    assert(inputEndPoint is not None)
    assert(outputEndPoint is not None)
  
    return (inputEndPoint, outputEndPoint)
  
  def createInterruptTransfer(self, dh, endpoint, bufferSize, callback, timeoutInMillis):
    th = dh.getTransfer()
    th.setInterrupt(endpoint, bufferSize, callback, timeout=timeoutInMillis)
    return th

  def submitTransfer(self, th, data=None):
    if data is not None:
      th.setBuffer(data)
    th.submit()

  def getTransferType(self, th):
    return th.getType()

  def getTransferEndpoint(self, th):
    return th.getEndpoint()

  def getTransferStatus(self, th):
    return th.getStatus()

  def getTransferData(self, th):
    buf = th.getBuffer()
    return buf[:th.getActualLength()]

  def startPolling(self):
    devicePollLatch = utils.Latch()
    poller = Poller()
    pollThread = USBPollerThreadWithStartNotify(self.context, poller, devicePollLatch)
    pollThread.name="USBPollThread"
    pollThread.daemon = True
    pollThread.start()
    devicePollLatch.waitForIt(5)
    LOGGER.debug("poll thread started, waiting for input")

  def transferStatusStr(self, code):
    return libusb1.libusb_transfer_status.get(code)
    
def __addGloals():
 for (k,v) in vars(libusb1).iteritems():
   if k.startswith("LIBUSB_") and isinstance(v, int):
     name = k[7:]
     vars(USBContext)[name] = v

__addGloals()

