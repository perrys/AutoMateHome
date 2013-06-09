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

import LibUSB

import utils

import Queue
import datetime
import logging
import select
import sys
import threading
import traceback

LOGGER = logging.getLogger(__name__)

class CM15:
  """Duplex USB connection to the CM15 device. Exposes a binary file
  API (read,write). Reads from and writes to the device are thread-safe.

  This implementation uses the libusb-bindings package."""

  VENDOR_ID = 0xBC7
  DEVICE_ID = 1

  def __init__(self):
    """Locate and initialize the CM15 device for I/O."""

    self.responseQueue = Queue.Queue()

    # find the device
    cm15 = LibUSB.get_device_with_vid_pid(CM15.VENDOR_ID, CM15.DEVICE_ID)
    if cm15 is None:
      raise Exception("unable to find CM15 device")
    LOGGER.debug("found cm15 device")

    # the cm15 is a low speed device (1.5MBit/s) which means that its
    # maximum data payload is 8 bytes per transfer
    assert(cm15.device_speed() == LibUSB.SPEED_LOW)

    # open the device:
    self.dh = cm15.open()
    LOGGER.debug("opened a handle to the device")

    # Get the interface descriptor (there should only be one
    # alternative) and the interface number:
    desc = cm15.active_config_descriptor()
    assert(len(desc["interfaces"]) == 1)
    assert(len(desc["interfaces"][0]) == 1)
    interface0Setting0 = desc["interfaces"][0][0]
    interfaceNumber = interface0Setting0["bInterfaceNumber"]

    # Claim the interface
    if self.dh.kernel_driver_active(interfaceNumber):
      LOGGER.debug("cm15 has kernel driver, detaching")
      self.dh.detach_kernel_driver(interfaceNumber)
    self.dh.claim_interface(interfaceNumber)
    LOGGER.debug("claimed the cm15 interface %d" % interfaceNumber)

    # get the endpoints for the interface, there should only be two (input and output):
    assert(interface0Setting0["bNumEndpoints"] == 2)
    endpoints = interface0Setting0["endpoints"]
    assert(2 == len(endpoints))
    self.inputEndPoint = None
    self.outputEndPoint = None

    for ep in endpoints:
      address = ep["bEndpointAddress"]
      if LibUSB.ENDPOINT_IN == (address & LibUSB.ENDPOINT_DIR_MASK):
        self.inputEndPoint = address # data from cm15 arrives on this endpoint
      else:
        assert(LibUSB.ENDPOINT_OUT == (address & LibUSB.ENDPOINT_DIR_MASK))
        self.outputEndPoint = address # write to cm15 on this one

    assert(self.inputEndPoint is not None)
    assert(self.outputEndPoint is not None)

    # prepare the output transfer. This is only submitted when we want
    # to send data to the device.
    self.outputTBuffer = LibUSB.transfer(0)
    self.outputTBuffer.fill_interrupt_transfer(self.dh, self.outputEndPoint, "", self.__outputEndpointCallback, 10 * 1000)
    self.outputTBufferLock = threading.Lock()

    # setup the input transfer and submit it so we are ready to
    # receive information from the device:
    self.inputTBuffer = LibUSB.transfer(0)
    self.inputTBuffer.fill_interrupt_transfer(self.dh, self.inputEndPoint, "", self.__inputEndpointCallback, 10 * 1000)
    self.inputTBuffer.zero_transfer_buffer(8) # reserve 8 bytes per transfer -- this is enough for low speed devices
    self.inputTBuffer.submit_transfer()

    # start the polling thread:
    devicePollLatch = utils.Latch()
    self.pollThread = threading.Thread(name="USBPollThread", target=lambda: self.__pollDevice(devicePollLatch))
    self.pollThread.daemon = True
    self.pollThread.start()
    devicePollLatch.waitForIt(5)
    LOGGER.debug("poll thread started, waiting for input")

  def write(self, data):
    """Send data to the device."""
    if len(data) > 8:
      raise Exception("Cannot send more than 8 bytes per transfer to a low-speed USB device")
    with self.outputTBufferLock:
      self.outputTBuffer.set_transfer_buffer(data)
      self.outputTBuffer.submit_transfer()

  def read(self, timeoutInSeconds=None):
    """Read the next data transfer from the device, blocking until it
    is available. The TIMEOUTINSECONDS parameter, if supplied,
    specifies the length of time to wait for data - if this time is
    exceeded the method returns None.

    If the response from the device is other than
    LIBUSB_TRANSFER_COMPLETED, and exception is raised.
    """
    
    try:
      while True:
        (data, status) = self.responseQueue.get(True, timeoutInSeconds)
        self.responseQueue.task_done()
        if LibUSB.TRANSFER_COMPLETED == status:
          return data
        elif LibUSB.TRANSFER_TIMED_OUT == status:
          return None
        LOGGER.error("incomplete response from device - status: %s" % LibUSB.transfer_status_str(status))
    except Queue.Empty:
      return None

  def __pollDevice(self, latch):

    latch.open()
    
    # wait for data from the device using the asynchronous API. See
    # http://libusb.sourceforge.net/api-1.0/
    pollobj = select.poll()
    pollfds = LibUSB.get_pollfds()
    LOGGER.debug("retreived poll file descriptors: %s" % pollfds)
    for (fd, events) in pollfds:
      pollobj.register(fd, events)

    while True:
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

  def __outputEndpointCallback(self, data, status, endpoint, endpoint_type):
    assert(LibUSB.TRANSFER_TYPE_INTERRUPT == endpoint_type)
    assert(self.outputEndPoint == endpoint)
    LOGGER.debug("output [%s]: %s" % (LibUSB.transfer_status_str(status), ", ".join(["0x%02x" % ord(c) for c in data])))

  def __inputEndpointCallback(self, data, status, endpoint, endpoint_type):
    assert(LibUSB.TRANSFER_TYPE_INTERRUPT == endpoint_type)
    assert(self.inputEndPoint == endpoint)
    LOGGER.debug("input [%s]: %s" % (LibUSB.transfer_status_str(status), ", ".join(["0x%02x" % ord(c) for c in data])))
    self.responseQueue.put((data, status))
    self.inputTBuffer.submit_transfer()


