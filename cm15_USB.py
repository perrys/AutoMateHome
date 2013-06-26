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


import Queue
import datetime
import logging
import select
import sys
import threading
import traceback
import getopt

import usb_lib_factory

USB_LIB = usb_lib_factory.create()

LOGGER = logging.getLogger(__name__)

class CM15:
  """Duplex USB connection to the CM15 device. Exposes a binary file
  API (read,write). Reads from and writes to the device are thread-safe."""

  VENDOR_ID = 0xBC7
  DEVICE_ID = 1

  def __init__(self):
    """Locate and initialize the CM15 device for I/O."""

    self.responseQueue = Queue.Queue()

    self.dh = USB_LIB.getHandleToLowSpeedDevice(CM15.VENDOR_ID, CM15.DEVICE_ID)
    device = self.dh.getDevice()
    LOGGER.info("Found device: %s [%s] serial # %s" % (device.getProduct(), device.getManufacturer(), device.getSerialNumber()))

    (self.inputEndPoint, self.outputEndPoint) = USB_LIB.claimInterfaceAndGetEndpoints(self.dh)
    LOGGER.info("Claimed the interface for this device")

    # prepare the output transfer. This is only submitted when we want
    # to send data to the device.
    self.outputTBuffer = USB_LIB.createInterruptTransfer(self.dh, self.outputEndPoint, 32, self.__outputEndpointCallback, 10 * 1000)
    self.outputTBufferLock = threading.Lock()

    # setup the input transfer and submit it so we are ready to
    # receive information from the device:
    self.inputTBuffer = USB_LIB.createInterruptTransfer(self.dh, self.inputEndPoint, 8, self.__inputEndpointCallback, 10 * 1000)
    USB_LIB.submitTransfer(self.inputTBuffer)
    LOGGER.info("Finished setting up transfer buffers")

    # start the polling thread:
    USB_LIB.startPolling()
    LOGGER.info("Poll thread started, waiting for input from device")

  def write(self, data):
    """Send data to the device."""
    if len(data) > 8:
      raise Exception("Cannot send more than 8 bytes per transfer to a low-speed USB device")
    with self.outputTBufferLock:
      USB_LIB.submitTransfer(self.outputTBuffer, data)

  def read(self, timeoutInSeconds=None):
    """Read the next data transfer from the device, blocking until it
    is available. The TIMEOUTINSECONDS parameter, if supplied,
    specifies the length of time to wait for data - if this time is
    exceeded the method returns None.

    If the response from the device is other than
    LIBUSB_TRANSFER_COMPLETED, and exception is raised."""

    
    try:
      while True:
        (data, status) = self.responseQueue.get(True, timeoutInSeconds)
        self.responseQueue.task_done()
        if USB_LIB.TRANSFER_COMPLETED == status:
          return data
        elif USB_LIB.TRANSFER_TIMED_OUT == status:
          return None
        LOGGER.error("incomplete response from device - status: %s" % USB_LIB.transferStatusStr(status))
    except Queue.Empty:
      return None


  def __outputEndpointCallback(self, transfer):
    assert(USB_LIB.TRANSFER_TYPE_INTERRUPT == USB_LIB.getTransferType(transfer))
    assert(self.outputEndPoint == USB_LIB.getTransferEndpoint(transfer))
    status = USB_LIB.getTransferStatus(transfer) 
    data = USB_LIB.getTransferData(transfer)
    LOGGER.debug("output [%s]: %s" % (USB_LIB.transferStatusStr(status), ", ".join(["0x%02x" % ord(c) for c in data])))

  def __inputEndpointCallback(self, transfer):
    assert(USB_LIB.TRANSFER_TYPE_INTERRUPT == USB_LIB.getTransferType(transfer))
    assert(self.inputEndPoint == USB_LIB.getTransferEndpoint(transfer))
    status = USB_LIB.getTransferStatus(transfer) 
    data = USB_LIB.getTransferData(transfer)
    LOGGER.debug("input [%s]: %s" % (USB_LIB.transferStatusStr(status), ", ".join(["0x%02x" % ord(c) for c in data])))
    self.responseQueue.put((data, status))
    USB_LIB.submitTransfer(self.inputTBuffer)


