/*******************************************************************************
 * 
 * RapidIO IP Library Core
 * 
 * This file is part of the RapidIO IP library project
 * http://www.opencores.org/cores/rio/
 * 
 * Description:
 * This file contains the function prototypes and types that are needed to be 
 * able to use the riostack.c module.
 *
 * To Do:
 * -
 * 
 * Author(s): 
 * - Magnus Rosenius, magro732@opencores.org 
 * 
 *******************************************************************************
 * 
 * Copyright (C) 2013 Authors and OPENCORES.ORG 
 * 
 * This source file may be used and distributed without 
 * restriction provided that this copyright statement is not 
 * removed from the file and that any derivative work contains 
 * the original copyright notice and the associated disclaimer. 
 * 
 * This source file is free software; you can redistribute it 
 * and/or modify it under the terms of the GNU Lesser General 
 * Public License as published by the Free Software Foundation; 
 * either version 2.1 of the License, or (at your option) any 
 * later version. 
 * 
 * This source is distributed in the hope that it will be 
 * useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
 * PURPOSE. See the GNU Lesser General Public License for more 
 * details. 
 * 
 * You should have received a copy of the GNU Lesser General 
 * Public License along with this source; if not, download it 
 * from http://www.opencores.org/lgpl.shtml 
 * 
 *******************************************************************************/

/** 
 * \file riostack.h
 */

#ifndef _RIOSTACK_H
#define _RIOSTACK_H

/*******************************************************************************
 * Includes
 *******************************************************************************/

#include "rioconfig.h"


/*******************************************************************************
 * Global typedefs
 *******************************************************************************/

/* The maximum size of a RapidIO packet. */
#define RIO_PACKET_SIZE ((uint8_t)69u)

/* The size of a maximum sized RapidIO packet when stored in memory. */
/* One entry contains a header with the used buffer size. */
#define RIO_BUFFER_SIZE (RIO_PACKET_SIZE+1u)

/* Configuration space offsets. */
#define DEVICE_IDENTITY_CAR ((uint32_t)0x00000000ul)
#define DEVICE_INFORMATION_CAR ((uint32_t)0x00000004ul)
#define ASSEMBLY_IDENTITY_CAR ((uint32_t)0x00000008ul)
#define ASSEMBLY_INFORMATION_CAR ((uint32_t)0x0000000cul)
#define PROCESSING_ELEMENT_FEATURES_CAR ((uint32_t)0x00000010ul)
#define SWITCH_PORT_INFORMATION_CAR ((uint32_t)0x00000014ul)
#define SOURCE_OPERATIONS_CAR ((uint32_t)0x00000018ul)
#define DESTINATION_OPERATIONS_CAR ((uint32_t)0x0000001cul)
#define SWITCH_ROUTE_TABLE_DESTINATION_ID_LIMIT_CAR ((uint32_t)0x00000034ul)
#define PROCESSING_ELEMENT_LOGICAL_LAYER_CONTROL_CSR ((uint32_t)0x0000004cul)
#define BASE_DEVICE_ID_CSR ((uint32_t)0x00000060ul)
#define HOST_BASE_DEVICE_ID_LOCK_CSR ((uint32_t)0x00000068ul)
#define COMPONENT_TAG_CSR ((uint32_t)0x0000006cul)
#define STANDARD_ROUTE_CONFIGURATION_DESTINATION_ID_SELECT_CSR ((uint32_t)0x00000070ul)
#define STANDARD_ROUTE_CONFIGURATION_PORT_SELECT_CSR ((uint32_t)0x00000074ul)
#define STANDARD_ROUTE_DEFAULT_PORT_CSR ((uint32_t)0x00000078ul)
#define EXTENDED_FEATURES_OFFSET ((uint32_t)0x00000100ul)
#define IMPLEMENTATION_DEFINED_OFFSET ((uint32_t)0x00010000ul)
#define LP_SERIAL_REGISTER_BLOCK_HEADER(offset) (offset)
#define PORT_LINK_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000020ul)
#define PORT_RESPONSE_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000024ul)
#define PORT_GENERAL_CONTROL_CSR(offset) ((offset) + 0x0000003cul)
#define PORT_N_LOCAL_ACKID_CSR(offset, n) ((offset) + (0x00000048ul+((n)*0x00000020ul)))
#define PORT_N_ERROR_AND_STATUS_CSR(offset, n) ((offset) + (0x00000058ul+((n)*0x00000020ul)))
#define PORT_N_CONTROL_CSR(offset, n) ((offset) + (0x0000005cul+((n)*0x00000020ul)))


/* Define the different types of RioSymbols. */
typedef enum 
{
  RIO_SYMBOL_TYPE_IDLE, RIO_SYMBOL_TYPE_CONTROL, 
  RIO_SYMBOL_TYPE_DATA, RIO_SYMBOL_TYPE_ERROR
} RioSymbolType;


/*
 * RapidIO symbol definition.
 * Idle symbol: Sent when nothing else to send. Does not use the data field.
 * Control symbol: Sent when starting, ending and acknowleding a packet. Data 
 * is right aligned, (Unused, C0, C1, C2) where C0 is transmitted/received first.
 * Data symbol: Sent to transfer packets. Uses the full data field, (D0, D1, 
 * D2, D3) where D0 is transmitted/received first.
 * Error symbols are created when a symbols could not be created and the stack 
 * should know about it.
 */
typedef struct
{
  RioSymbolType type;
  uint32_t data;
} RioSymbol;
 

/* Define different events that may happen in the stack. */
typedef enum
{
  RIO_EVENT_NONE, 
  RIO_EVENT_NREAD, RIO_EVENT_NWRITE, RIO_EVENT_NWRITE_R,
  RIO_EVENT_DOORBELL, RIO_EVENT_MESSAGE, 
  RIO_EVENT_MAINT_READ_REQUEST, RIO_EVENT_MAINT_WRITE_REQUEST, 
  RIO_EVENT_MAINT_READ_RESPONSE, RIO_EVENT_MAINT_WRITE_RESPONSE, 
  RIO_EVENT_RESPONSE_DONE, RIO_EVENT_RESPONSE_DONE_PAYLOAD,
  RIO_EVENT_RESPONSE_RETRY, RIO_EVENT_RESPONSE_ERROR,
  RIO_EVENT_MESSAGE_RESPONSE_DONE, RIO_EVENT_MESSAGE_RESPONSE_RETRY, 
  RIO_EVENT_MESSAGE_RESPONSE_ERROR
} RioEventType;


/* Define different states the link may be in. */
typedef enum
{
  RIO_STATUS_UNINITIALIZED, 
  RIO_STATUS_ENUMERATION,
  RIO_STATUS_OPERATIONAL
} RioStatusType;


/* Receiver states. */
typedef enum 
{
  RX_STATE_UNINITIALIZED, RX_STATE_PORT_INITIALIZED, RX_STATE_LINK_INITIALIZED,
  RX_STATE_INPUT_RETRY_STOPPED, RX_STATE_INPUT_ERROR_STOPPED
} RioReceiverState;


/* Transmitter states. */
typedef enum 
{
  TX_STATE_UNINITIALIZED, TX_STATE_PORT_INITIALIZED, TX_STATE_LINK_INITIALIZED,
  TX_STATE_SEND_PACKET_RETRY, TX_STATE_SEND_PACKET_NOT_ACCEPTED, TX_STATE_SEND_LINK_RESPONSE, 
  TX_STATE_OUTPUT_RETRY_STOPPED, TX_STATE_OUTPUT_ERROR_STOPPED
} RioTransmitterState;


/* Queue definition. */
typedef struct 
{
  uint8_t size;
  uint8_t available;
  uint8_t windowSize;
  uint8_t windowIndex;
  uint8_t frontIndex;
  uint8_t backIndex;
  uint32_t *buffer_p;
} Queue_t;

/* Forward declaration for the RioStack-structure. */
struct RioStack_t;

/* Structure to enter callback function pointers in. */
typedef struct
{
  uint32_t (*configRead)(struct RioStack_t *stack, uint32_t offset);
  void (*configWrite)(struct RioStack_t *stack, uint32_t offset, uint32_t data);
} RioStackObserver_t;


/* Define the structure to keep all the RapidIO stack variables. */
typedef struct RioStack_t
{
  /* Receiver variables. */
  RioReceiverState rxState;
  uint8_t rxCounter;
  uint16_t rxCrc;
  uint8_t rxStatusReceived;
  uint8_t rxAckId;
  uint8_t rxAckIdAcked;
  uint8_t rxErrorCause;
  Queue_t rxQueue;

  /* Transmitter variables. */
  RioTransmitterState txState;
  uint8_t txCounter;
  uint16_t txStatusCounter;
  uint8_t txFrameState;
  uint32_t txFrameTimeout[32];
  uint8_t txAckId;
  uint8_t txAckIdWindow;
  uint8_t txBufferStatus;
  Queue_t txQueue;

  /* Common protocol stack variables. */
  uint32_t portTime;
  uint32_t portTimeout;

  /* Common protocol stack variables updated visible via the configuration space. */
  uint16_t deviceIdentity;
  uint16_t deviceVendorIdentity;
  uint32_t deviceRev;
  uint16_t assyIdentity;
  uint16_t assyVendorIdentity;
  uint16_t assyRev;
  uint16_t baseDeviceId;
  uint32_t hostBaseDeviceIdLock;
  uint32_t componentTag;
  uint8_t host;
  uint8_t masterEnable;
  uint8_t discovered;

  /** The number of successfully received packets. */
  uint32_t statusInboundPacketComplete;

  /** The number of retried received packets. 
      This will happen if the receiver does not have resources available when an inbound packet is received. */
  uint32_t statusInboundPacketRetry;

  /** The number of received erronous control symbols. 
      This may happen if the inbound link has a high bit-error-rate. */
  uint32_t statusInboundErrorControlCrc;

  /** The number of received packets with an unexpected ackId. 
      This may happen if the inbound link has a high bit-error-rate. */
  uint32_t statusInboundErrorPacketAckId;

  /** The number of received packets with a checksum error. 
      This may happen if the inbound link has a high bit-error-rate. */
  uint32_t statusInboundErrorPacketCrc;

  /** The number of received symbols that contains an illegals character. 
      This may happen if the inbound link has a high bit-error-rate or if characters are missing in the 
      inbound character stream. */
  uint32_t statusInboundErrorIllegalCharacter;

  /** The number of general errors encountered at the receiver that does not fit into the other categories. 
      This happens if too short or too long packets are received. */
  uint32_t statusInboundErrorGeneral;

  /** The number of received packets that were discarded since they were unsupported by the stack. 
      This will happen if an inbound packet contains information that cannot be accessed using the function API 
      of the stack. */
  uint32_t statusInboundErrorPacketUnsupported;

  /** The number of successfully transmitted packets. */
  uint32_t statusOutboundPacketComplete;

  /** The number of retried transmitted packets. 
      This will happen if the receiver at the link-partner does not have resources available when an outbound
      packet is received. */
  uint32_t statusOutboundPacketRetry;

  /** The number of outbound packets that has had its retransmission timer expired. 
      This happens if the latency of the system is too high or if a packet is corrupted due to a high 
      bit-error-rate on the outbound link. */
  uint32_t statusOutboundErrorTimeout;

  /** The number of packet-accepted that was received that contained an unexpected ackId. 
      This happens if the transmitter and the link-partner is out of synchronization, probably due 
      to a software error. */
  uint32_t statusOutboundErrorPacketAccepted;

  /** The number of packet-retry that was received that contained an unexpected ackId. 
      This happens if the transmitter and the link-partner is out of synchronization, probably due to
      a software error. */
  uint32_t statusOutboundErrorPacketRetry;

  /** The number of received link-requests. 
      This happens if the link-partner transmitter has found an error and need to resynchronize itself 
      to the receiver. */
  uint32_t statusPartnerLinkRequest;

  /** The number of received erronous control symbols at the link-partner receiver. 
      This may happen if the outbound link has a high bit-error-rate. */
  uint32_t statusPartnerErrorControlCrc;

  /** The number of received packets with an unexpected ackId at the link-partner receiver. 
      This may happen if the outbound link has a high bit-error-rate. */
  uint32_t statusPartnerErrorPacketAckId;

  /** The number of received packets with a checksum error at the link-partner receiver. 
      This may happen if the outbound link has a high bit-error-rate. */
  uint32_t statusPartnerErrorPacketCrc;

  /** The number of received symbols that contains an illegals character at the link-parter receiver. 
      This may happen if the outbound link has a high bit-error-rate or if characters are missing in the 
      outbound character stream. */
  uint32_t statusPartnerErrorIllegalCharacter;

  /** The number of general errors encountered at the receiver that does not fit into the other categories. 
      This happens depending on the link-partner implementation. */
  uint32_t statusPartnerErrorGeneral;

  /* Callback structure. */
  const RioStackObserver_t *observer;

  /* Private user data. */
  const void *private;
} RioStack_t;


/*******************************************************************************
 * Global function prototypes
 *******************************************************************************/

/**
 * \brief Open the RapidIO stack for operation.
 *
 * \param[in] stack Stack instance to operate on.
 * \param[in] observer Callback structure to use when events happen.
 * \param[in] private Pointer to an opaque data area containing private user data.
 * \param[in] rxPacketBufferSize Number of words to use as reception buffer. This 
 *            argument specifies the size of rxPacketBuffer.
 * \param[in] rxPacketBuffer Pointer to buffer to store inbound packets in.
 * \param[in] txPacketBufferSize Number of words to use as transmission buffer. This 
 *            argument specifies the size of txPacketBuffer.
 * \param[in] txPacketBuffer Pointer to buffer to store outbound packets in.
 * \param[in] configDeviceVendorId Constant to use as deviceVendorIdentity when 
 *            accessed in configuration space. (See Part 1, chapter 5.4.1)
 * \param[in] configDeviceId Constant to use as deviceIdentity when accessed in 
 *            configuration space. (See Part 1, chapter 5.4.1)
 * \param[in] configDeviceRevisionId Constant to use as deviceRev when accessed in 
 *            configuration space. (See Part 1, chapter 5.4.2)
 * \param[in] configAssyVendorId Constant to use as assyVendorIdentity when accessed in 
 *            configuration space. (See Part 1, chapter 5.4.3)
 * \param[in] configAssyId Constant to use as assyIdentity when accessed in 
 *            configuration space. (See Part 1, chapter 5.4.3)
 * \param[in] configAssyRevisionId Constant to use as assyRev when accessed in 
 *            configuration space. (See Part 1, chapter 5.4.4)
 * \param[in] configBaseDeviceId The deviceId (source address) to use at startup when 
 *            sending packets.
 *
 * This function initializes all internally used variables in the stack. The stack will 
 * however not be operational until the transcoder has signalled that it is ready for
 * other symbols than idle. This is done using the function RIO_setPortStatus(). Once 
 * this function has been called it is possible to get and set symbols and to issue
 * requests. The requests will be transmitted once the link initialization has 
 * been completed.
 * 
 * The rxPacket/txPacket arguments are word buffers that are used internally to store the 
 * inbound and outbound packet queues. 
 *
 * The config argument constants are used as identification when maintenance packets 
 * are received and replied to. They should be set to make the device where the stack 
 * is used easily identifiable on the net.
 *
 * \note The reception buffers can only support maximum 31 buffers.
 */
void RIO_open( RioStack_t *stack, const RioStackObserver_t *observer, const void *private, 
               const uint32_t rxPacketBufferSize, uint32_t *rxPacketBuffer, 
               const uint32_t txPacketBufferSize, uint32_t *txPacketBuffer, 
               const uint16_t configDeviceVendorId, const uint16_t configDeviceId,
               const uint32_t configDeviceRevisionId, const uint16_t configAssyVendorId,
               const uint16_t configAssyId, const uint16_t configAssyRevisionId,
               const uint16_t configBaseDeviceId );

/*******************************************************************************************
 * Stack status functions.
 * Note that status counters are access directly in the stack-structure.
 *******************************************************************************************/

/**
 * \brief Get the status of the link.
 *
 * \param[in] stack The stack to operate on.
 * \return Returns the status of the link.
 *
 * This function indicates if the link is up and ready to relay packets.
 */
RioStatusType RIO_getStatus( RioStack_t *stack );

/**
 * \brief Get the number of pending outbound packets.
 *
 * \param[in] stack The stack to operate on.
 * \return Returns the number of pending outbound packets.
 *
 * This function checks the outbound queue and returns the number of packets 
 * that are pending to be transmitted onto the link.
 */
uint8_t RIO_outboundQueueLength( RioStack_t *stack );

/**
 * \brief Get the number of pending inbound packets.
 *
 * \param[in] stack The stack to operate on.
 * \return Returns the number of pending inbound packets.
 *
 * This function checks the inbound queue and returns the number of packets 
 * that has been received but not read by the user yet.
 */
uint8_t RIO_inboundQueueLength( RioStack_t *stack );

/*******************************************************************************************
 * Packet reception functions.
 *******************************************************************************************/

/**
 * \brief Check for new events.
 *
 * \param[in] stack The stack to operate on.
 * \return Returns the value RIO_EVENT_NONE if no event is pending and
 * something else if there are pending events.
 *
 * This function polls the incoming queue of packets and returns the
 * type of packet present there. The return value from this function
 * indicates which access functions that should be used to read the
 * received packet.
 * 
 * \note When a packet has been processed, RIO_packetRemove() must be called to free the 
 * used resources in the inbound queue.
 */
RioEventType RIO_eventPoll( RioStack_t *stack );

/**
 * \brief Remove a packet from the stack.
 * \param[in] stack The stack to operate on.
 *
 * Remove a pending packet from the stack. The incoming packet queue is updated
 * to remove the received packet.
 */
void RIO_packetRemove( RioStack_t *stack );

/**
 * \brief Check transmission buffers.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] size The size of the buffer that is about to be sent.
 * \return Returns non-zero if a buffer with the specified size fits into the
 * internal transmission buffers.
 *
 * Return if there are buffers available to send a packet of a specified size.
 *
 * \note If the response is negative, it might be positive later if outbound
 * packets has been sent and new buffers becomes available.
 *
 * \note Set size to zero if there is no user definded payload.
 */
bool_t RIO_sendAvailable( RioStack_t *stack, const uint16_t size );

/**
 * \brief Get a raw packet from inbound queue.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] length The size of the buffer to write the packet to.
 * \param[in] dest Pointer to where to copy the raw packet.
 * \return The number of words copied.
 *
 * This function copies a raw packet in the inbound queue into a word buffer. An assert 
 * will occur if the packet does not fit into the provided buffer.
 *
 * \note The packet is automatically removed from the inbound queue. Do not use 
 * RIO_packetRemove() to remove it.
 */
uint32_t RIO_packetGet( RioStack_t *stack, uint32_t length, uint32_t *dest);

/**
 * \brief Set a raw packet in outbound queue.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] length The size of the packet to write.
 * \param[in] src Pointer to where to copy the raw packet from.
 *
 * This function copies a raw packet from a source buffer into the outbound queue. 
 * An assert will occur if the packet does not fit into the internal buffer.
 *
 * \note Calling this function resembles a send-function, the packet will be placed in the 
 * outbound queue for transmission.
 *
 * \note If the copied packet does not have a correct CRC it might lock the stack since 
 * retransmissions will be done until forever.
 */
void RIO_packetSet( RioStack_t *stack, uint32_t length, uint32_t *src);

/*******************************************************************************************
 * Configuration-space access methods.
 *******************************************************************************************/

/**
 * \brief Read configuration space.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] offset The configuration space address to read.
 * \return The data read on the configuration space address specified.
 *
 * This function reads a configuration space offset and returns the content of
 * the entry.
 */
uint32_t RIO_readConfig( RioStack_t *stack, const uint32_t offset );

/**
 * \brief Write configuration space.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] offset The configuration space address to write to.
 * \param[in] data The data to write to the configuration space.
 *
 * This function writes to a configuration space offset and sets the
 * content to the specified data.
 */
void RIO_writeConfig( RioStack_t *stack, const uint32_t offset, const uint32_t data);

/*******************************************************************************************
 * Logical I/O MAINTENANCE-READ functions.
 *******************************************************************************************/

/**
 * \brief Send a maintenance read request.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] hopCount The hop_count to set in the read request.
 * \param[in] offset The byte address in the configuration space to read.
 *
 * This function creates and sends a maintenance read request packet. 
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMaintenanceReadRequest(RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                                    const uint8_t hopCount, const uint32_t offset );
#else
void RIO_sendMaintenanceReadRequest(RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                                    const uint8_t hopCount, const uint32_t offset );
#endif

/**
 * \brief Read a received maintenance read request.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the destination end point.
 * \param[out] tid The transaction id to be returned in the response.
 * \param[out] hopCount The hop_count to set in the read request.
 * \param[out] offset The byte address in the configuration space to read.
 *
 * This function reads a received maintenance read request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMaintenanceReadRequest(RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                                       uint8_t *hopCount, uint32_t *offset);
#else
void RIO_receiveMaintenanceReadRequest(RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                                       uint8_t *hopCount, uint32_t *offset);
#endif

/**
 * \brief Send a maintenance read response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] hopCount The hop_count to set in the read request.
 * \param[in] data The data to send in the response.
 *
 * This function creates a maintanance read response packet that should be 
 * sent when a request is received.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMaintenanceReadResponse( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                                      const uint8_t hopCount, const uint32_t data);
#else
void RIO_sendMaintenanceReadResponse( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                                      const uint8_t hopCount, const uint32_t data);
#endif

/**
 * \brief Read a received maintenance read response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the source end point.
 * \param[out] tid The transaction id in the response.
 * \param[out] hopCount The hop_count set in the read response.
 * \param[out] data The data in the response.
 *
 * This function reads a received maintanance read response packet.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMaintenanceReadResponse( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                                         uint8_t *hopCount, uint32_t *data);
#else
void RIO_receiveMaintenanceReadResponse( RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                                         uint8_t *hopCount, uint32_t *data);
#endif

/*******************************************************************************************
 * Logical I/O MAINTENANCE-WRITE functions.
 *******************************************************************************************/

/**
 * \brief Send a maintenance write request.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] hopCount The hop_count to set in the write request.
 * \param[in] offset The byte address in the configuration space to write to.
 * \param[in] data The data to write in configuration space.
 *
 * This function creates and sends a maintenance write request packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMaintenanceWriteRequest( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                                      const uint8_t hopCount, const uint32_t offset, const uint32_t data );
#else
void RIO_sendMaintenanceWriteRequest( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                                      const uint8_t hopCount, const uint32_t offset, const uint32_t data );
#endif

/**
 * \brief Read a received maintenance write request.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] destid The device id of the destination end point.
 * \param[out] tid The transaction id to be returned in the response.
 * \param[out] hopCount The hop_count to set in the write request.
 * \param[out] offset The byte address in the configuration space to write to.
 * \param[out] data The data to write in configuration space.
 *
 * This function creates and sends a maintenance write request packet. The reply
 * is received using RIO_eventPoll-function and RIO_packetTid together with the return
 * value from this function.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMaintenanceWriteRequest( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                                         uint8_t *hopCount, uint32_t *offset, uint32_t *data );
#else
void RIO_receiveMaintenanceWriteRequest( RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                                         uint8_t *hopCount, uint32_t *offset, uint32_t *data );
#endif

/**
 * \brief Send a maintenance write response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned.
 * \param[in] hopCount The hop_count to set in the write response.
 *
 * This function creates a maintanance write response packet from a pending
 * maintenance read request. The generated packet are placed in the outbound
 * packet queue.
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMaintenanceWriteResponse( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                                       const uint8_t hopCount);
#else
void RIO_sendMaintenanceWriteResponse( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                                       const uint8_t hopCount);
#endif

/**
 * \brief Read a received maintenance write response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the source end point.
 * \param[out] tid The transaction id read in the response.
 * \param[out] hopCount The hop_count read in the write response.
 *
 * This function creates a maintanance write response packet from a pending
 * maintenance read request. The generated packet are placed in the outbound
 * packet queue.
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMaintenanceWriteResponse( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid,
                                          uint8_t *hopCount);
#else
void RIO_receiveMaintenanceWriteResponse( RioStack_t *stack, uint16_t *srcid, uint8_t *tid,
                                          uint8_t *hopCount);
#endif

/*******************************************************************************************
 * Logical I/O NWRITE/NWRITER functions.
 *******************************************************************************************/

/**
 * \brief Send an NWRITE request to write a byte array.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] address The byte address to write to.
 * \param[in] dataLength The number of bytes to write. The largest allowed size is 256 bytes.
 * \param[in] data A pointer to the array of bytes to write.
 *
 * This function creates and sends an NWRITE request packet to write a number 
 * of bytes to a specified address. No reply will be received.
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 *
 * \note Not all combinations of addresses and sizes are allowed. See table below:
 * -------------------------------------------------------------------------------------
 * size | valid byte in double-word
 * -------------------------------------------------------------------------------------
 *   1  | 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
 *   2  | 11000000, 00110000, 00001100, 00000011
 *   3  | 11100000, 00000111
 *   4  | 11110000, 00001111
 *   5  | 11111000, 00011111
 *   6  | 11111100, 00111111
 *   7  | 11111110, 01111111
 *  8*N | 11111111 (N={1...32})
 * --------------------------------------------------------------------------------------
 * See RapidIO 2.2 part1 table 4-4 for more details. Asserts will occurr if an invalid
 * combination is detected. 
 */
#ifdef RIO_TRANSPARENT
void RIO_sendNwrite( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, 
                     const uint32_t address,  const uint16_t dataLength, const uint8_t *data );
#else
void RIO_sendNwrite( RioStack_t *stack, const uint16_t destid, 
                     const uint32_t address,  const uint16_t dataLength, const uint8_t *data );
#endif

/**
 * \brief Send an NWRITER request to write a byte array.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to set in the response.
 * \param[in] address The byte address to write to.
 * \param[in] dataLength The number of bytes to write. The largest allowed size is 256 bytes.
 * \param[in] data A pointer to the array of bytes to write.
 *
 * This function creates and sends an NWRITE request packet to write a number 
 * of bytes to a specified address. A reply will be received when the write has been completed.
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 *
 * \note Not all combinations of addresses and sizes are allowed. See table below:
 * -------------------------------------------------------------------------------------
 * size | valid byte in double-word
 * -------------------------------------------------------------------------------------
 *   1  | 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
 *   2  | 11000000, 00110000, 00001100, 00000011
 *   3  | 11100000, 00000111
 *   4  | 11110000, 00001111
 *   5  | 11111000, 00011111
 *   6  | 11111100, 00111111
 *   7  | 11111110, 01111111
 *  8*N | 11111111 (N={1...32})
 * --------------------------------------------------------------------------------------
 * See RapidIO 2.2 part1 table 4-4 for more details. Asserts will occurr if an invalid
 * combination is detected. 
 */
#ifdef RIO_TRANSPARENT
void RIO_sendNwriteR( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                      const uint32_t address, const uint16_t dataLength, const uint8_t *data );
#else
void RIO_sendNwriteR( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                      const uint32_t address, const uint16_t dataLength, const uint8_t *data );
#endif

/**
 * \brief Read a received  NWRITE/NWRITER request.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the destination end point.
 * \param[out] tid The transaction id in the response. Undefined value when NWRITE is read.
 * \param[out] address The byte address to write to.
 * \param[in] dataLength The number of bytes allocated in data.
 * \param[in] data A pointer to the array of bytes to copy to.
 * \return The number of bytes copied into data.
 *
 * This function reads a received NWRITE/NWRITER request packet to write a number 
 * of bytes to a specified address. Used to receive both NWRITE and NWRITER. The payload 
 * of the packet is copied into the provided buffer pointed to by data.
 *
 * \note The address is a byte address, not a word address.
 */
#ifdef RIO_TRANSPARENT
uint16_t RIO_receiveNwrite( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                            uint32_t *address, const uint16_t dataLength, uint8_t *data );
#else
uint16_t RIO_receiveNwrite( RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                            uint32_t *address, const uint16_t dataLength, uint8_t *data );
#endif

/*******************************************************************************************
 * Logical I/O NREAD functions.
 *******************************************************************************************/

/**
 * \brief Send an NREAD request to read a byte array.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to set in the response.
 * \param[in] address The byte address to write to.
 * \param[in] dataLength The number of bytes to write. The largest allowed size is 256 bytes.
 *
 * This function creates and sends an NWRITE request packet to write a number 
 * of bytes to a specified address. A reply will be received when the write has been completed.
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 *
 * \note Not all combinations of address and length are allowed. See table below:
 * -------------------------------------------------------------------------------------
 * size | valid byte in double-word
 * -------------------------------------------------------------------------------------
 *   1  | 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
 *   2  | 11000000, 00110000, 00001100, 00000011
 *   3  | 11100000, 00000111
 *   4  | 11110000, 00001111
 *   5  | 11111000, 00011111
 *   6  | 11111100, 00111111
 *   7  | 11111110, 01111111
 * -------------------------------------------------------------------------------------
 * For full double-words the following byte sizes are allowed:
 *  8, 16, 32, 64, 96, 128, 160, 192, 224, 256
 * --------------------------------------------------------------------------------------
 * See RapidIO 2.2 part1 table 4-4 for more details. Asserts will occurr if an invalid
 * combination is detected. 
 */
#ifdef RIO_TRANSPARENT
void RIO_sendNread( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                    const uint32_t address, const uint16_t dataLength);
#else
void RIO_sendNread( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                    const uint32_t address, const uint16_t dataLength);
#endif

/**
 * \brief Read a received  NREAD request.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the destination end point.
 * \param[out] tid The transaction id in the response.
 * \param[out] address The byte address to write to.
 * \param[in] dataLength The number of bytes allocated in data.
 *
 * This function reads a received NREAD request packet to read a number 
 * of bytes at a specified address.
 *
 * \note The address is a byte address, not a word address.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveNread( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                       uint32_t *address, uint16_t *dataLength);
#else
void RIO_receiveNread( RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                       uint32_t *address, uint16_t *dataLength);
#endif

/*******************************************************************************************
 * Logical I/O RESPONSE-DONE-PAYLOAD, RESPONSE-DONE, RESPONSE-RETRY and RESPONSE-ERROR 
 * functions.
 *******************************************************************************************/

/**
 * \brief Send a response with data payload.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The destination identifier of the response.
 * \param[in] tid The transaction identifier for the reply. It should correspond to
 * the tid in the received packet for which this response is sent.
 * \param[in] address The byte address that was read. It should correspond to
 * the address in the received packet for which this response is sent.
 * \param[in] dataLength The size of the data buffer to return in the reply. It 
 * should correspond to the dataLength in the received packet for which this response
 * is sent.
 * \param[in] data The data buffer to return in the reply.
 *
 * This function creates a response packet with the specified destination
 * identifier, transaction id and data payload. The generated packet are placed
 * in the outbound packet queue.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendResponseDonePayload( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                                  const uint32_t address, const uint16_t dataLength, const uint8_t *data);
#else
void RIO_sendResponseDonePayload( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                                  const uint32_t address, const uint16_t dataLength, const uint8_t *data);
#endif

/**
 * \brief Read payload from a response with payload.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the destination end point.
 * \param[out] tid The transaction id in the response.
 * \param[in] address The byte address that was read. It should correspond to
 * the address in the received packet for which this response is sent.
 * \param[in] dataLength The size of the data buffer to return in the reply. It 
 * should correspond to the dataLength in the received packet for which this response
 * is sent.
 * \param[in] data Pointer to a buffer to where the data in the response will be copied.
 * \return The number of bytes copied from the data payload contained in the response.
 *
 * This function reads a response packet and returns a the byte payload contained within.
 */
#ifdef RIO_TRANSPARENT
uint16_t RIO_receiveResponseDonePayload( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid,
                                         const uint32_t address, const uint16_t dataLength, uint8_t *data );
#else
uint16_t RIO_receiveResponseDonePayload( RioStack_t *stack, uint16_t *srcid, uint8_t *tid,
                                         const uint32_t address, const uint16_t dataLength, uint8_t *data );
#endif

/**
 * \brief Send a response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] tid The transaction id to send the response for. This should be the
 * same value as the packet that this response was received with.
 *
 * This function is used to send a response indicating a successfull
 * completion in reply to a previously received packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendResponseDone( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid );

#else
void RIO_sendResponseDone( RioStack_t *stack, const uint16_t destid, const uint8_t tid );

#endif
/**
 * \brief Read a received response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source of the response.
 * \param[out] tid The transaction id in the response.
 *
 * This function is used to read a received response indicating a successfull
 * completion in reply to a previously sent packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveResponseDone( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid );

#else
void RIO_receiveResponseDone( RioStack_t *stack, uint16_t *srcid, uint8_t *tid );

#endif
/**
 * \brief Send a retry response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] tid The transaction id to send the response for. This should be the
 * same value as the packet that this response was received with.
 *
 * This function is used to send a response indicating a busy resource
 * in reply to a previously received packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendResponseRetry( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid );

#else
void RIO_sendResponseRetry( RioStack_t *stack, const uint16_t destid, const uint8_t tid );

#endif
/**
 * \brief Read a received retry response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source of the response.
 * \param[out] tid The transaction id in the response.
 *
 * This function is used to read a received response indicating a retry condition
 * in reply to a previously sent packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveResponseRetry( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid );

#else
void RIO_receiveResponseRetry( RioStack_t *stack, uint16_t *srcid, uint8_t *tid );

#endif
/**
 * \brief Send a error response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] tid The transaction id to send the response for. This should be the
 * same value as the packet that this response was received with.
 *
 * This function is used to send a response indicating a busy resource
 * in reply to a previously received packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendResponseError( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid );
#else
void RIO_sendResponseError( RioStack_t *stack, const uint16_t destid, const uint8_t tid );
#endif

/**
 * \brief Read a received error response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source of the response.
 * \param[out] tid The transaction id in the response.
 *
 * This function is used to read a received response indicating an error condition
 * in reply to a previously sent packet.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveResponseError( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid );
#else
void RIO_receiveResponseError( RioStack_t *stack, uint16_t *srcid, uint8_t *tid );
#endif

/*******************************************************************************************
 * Logical message passing DOORBELL and MESSAGE functions.
 *******************************************************************************************/

/**
 * \brief Send a doorbell.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] info The information to send with the doorbell.
 * \return An identifier that maps to the doorbell response that are received using
 * RIO_packetTid.
 *
 * This function is used to send a doorbell to a remote endpoint. A response
 * should be sent when the doorbell has been processed using the RIO_sendResponseDone(),
 * RIO_sendResponseRetry() or RIO_sendResponseError() functions.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendDoorbell( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t tid, 
                       const uint16_t info);
#else
void RIO_sendDoorbell( RioStack_t *stack, const uint16_t destid, const uint8_t tid, 
                       const uint16_t info);
#endif

/**
 * \brief Read a received a doorbell.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the source end point.
 * \param[out] tid The transaction id to be returned in the response.
 * \param[out] info The information to send with the doorbell.
 *
 * This function is used to read a received doorbell from a remote endpoint.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveDoorbell( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *tid, 
                          uint16_t *info);
#else
void RIO_receiveDoorbell( RioStack_t *stack, uint16_t *srcid, uint8_t *tid, 
                          uint16_t *info);
#endif

/**
 * \brief Send a message.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination endpoint.
 * \param[in] mailbox The mailbox to send the message to.
 * \param[in] dataLength The size of the buffer to copy to.
 * \param[in] buffer A byte pointer to the message payload to send.
 *
 * This function sends a single packet message to a destination mailbox. 
 *
 * \note Mailbox 0-15 can support multipacket (when support is enabled) 
 * messages and 16-255 can only handle singlepacket messages. Dont use mailbox 
 * 0-15 unless you know that there will be large packets transmitted on it.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMessage( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t mailbox, 
                      const uint16_t dataLength, const uint8_t *data );
#else
void RIO_sendMessage( RioStack_t *stack, const uint16_t destid, const uint8_t mailbox, 
                      const uint16_t dataLength, const uint8_t *data );
#endif

/**
 * \brief Read a received message.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device id of the source endpoint.
 * \param[out] mailbox The mailbox the message is received on.
 * \param[in] dataLength The size of the buffer to copy to.
 * \param[in] data A byte pointer to the message payload to read.
 * \return The number of bytes copied. A zero will be returned if unable to copy.
 *
 * This function reads a single packet message to a destination mailbox. 
 */
#ifdef RIO_TRANSPARENT
uint16_t RIO_receiveMessage( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *mailbox, 
                             const uint16_t dataLength, uint8_t *data );
#else
uint16_t RIO_receiveMessage( RioStack_t *stack, uint16_t *srcid, uint8_t *mailbox, 
                             const uint16_t dataLength, uint8_t *data );
#endif

/*******************************************************************************************
 * Logical message passing MESSAGE-RESPONSE functions.
 *******************************************************************************************/

/**
 * \brief Send a message response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] mailbox The mailbox to send the response to.
 *
 * This function is used to send a message response indicating a successfull
 * completion in reply to a previously received message.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMessageResponseDone( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t mailbox );
#else
void RIO_sendMessageResponseDone( RioStack_t *stack, const uint16_t destid, const uint8_t mailbox );
#endif

/**
 * \brief Read a received message response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source endpoint.
 * \param[out] mailbox The mailbox the response is for.
 *
 * This function is used to read a received message response indicating a successfull
 * completion in reply to a previously sent message.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMessageResponseDone( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *mailbox );
#else
void RIO_receiveMessageResponseDone( RioStack_t *stack, uint16_t *srcid, uint8_t *mailbox );
#endif

/**
 * \brief Send a message retry response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] mailbox The mailbox to send the response to.
 *
 * This function is used to send a message response indicating a busy resource
 * in reply to a previously received message.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMessageResponseRetry( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t mailbox );
#else
void RIO_sendMessageResponseRetry( RioStack_t *stack, const uint16_t destid, const uint8_t mailbox );
#endif

/**
 * \brief Read a received message retry response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source endpoint.
 * \param[out] mailbox The mailbox the response is for.
 *
 * This function is used to read a received message retry response indicating a retry 
 * condition in reply to a previously sent message.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMessageResponseRetry( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *mailbox );
#else
void RIO_receiveMessageResponseRetry( RioStack_t *stack, uint16_t *srcid, uint8_t *mailbox );
#endif

/**
 * \brief Send a message error response.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the response to.
 * \param[in] mailbox The mailbox to send the response to.
 *
 * This function is used to send a message response indicating a busy resource
 * in reply to a previously received message.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
#ifdef RIO_TRANSPARENT
void RIO_sendMessageResponseError( RioStack_t *stack, const uint16_t destid, const uint16_t srcid, const uint8_t mailbox );
#else
void RIO_sendMessageResponseError( RioStack_t *stack, const uint16_t destid, const uint8_t mailbox );
#endif

/**
 * \brief Read a received message error response.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] srcid The device identifier of the source endpoint.
 * \param[out] mailbox The mailbox the response is for.
 *
 * This function is used to read a received message error response indicating an error
 * condition in reply to a previously sent message.
 */
#ifdef RIO_TRANSPARENT
void RIO_receiveMessageResponseError( RioStack_t *stack, uint16_t *destid, uint16_t *srcid, uint8_t *mailbox );
#else
void RIO_receiveMessageResponseError( RioStack_t *stack, uint16_t *srcid, uint8_t *mailbox );
#endif

/*******************************************************************************************
 * Port functions (backend API towards physical device)
 *******************************************************************************************/

/**
 * \brief Set a port current time.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] time The current time without unit.
 *
 * This function indicates to the stack the current time and this is used internally 
 * to calculate when a packet timeout should be triggered. Use this together with RIO_setPortTimeout() 
 * to allow for the stack to handle timeouts.
 */
void RIO_portSetTime( RioStack_t *stack, const uint32_t time);

/**
 * \brief Set a port timeout limit.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] time The time out threshold.
 *
 * The time to wait for a response from the link partner. The unit of the 
 * timeout value should be the same as the time used in RIO_setPortTime().
 *
 * This function is used to set a timeout threshold value and is used to know when 
 * an acknowledge should have been received from a link partner.
 */
void RIO_portSetTimeout( RioStack_t *stack, const uint32_t time);

/**
 * \brief Set a ports status.
 * 
 * \param[in] stack The stack to operate on.
 * \param[in] initialized The state of the port.
 *
 * If set to non-zero, the symbol encoder/decoder indicates to the stack that
 * it is successfully encoding/decoding symbol, i.e. synchronized to the link.
 *
 * This function indicates to the stack if the port that are encoding/decoding
 * symbols are ready to accept other symbols than idle-symbols. If the
 * encoding/decoding loses synchronization then this function should be called
 * with an argument equal to zero to force the stack to resynchronize the link.
 */
void RIO_portSetStatus( RioStack_t *stack, const uint8_t initialized );

/**
 * \brief Add a new symbol to the RapidIO stack.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] s A symbol received from a port.
 *
 * This function is used to insert new data, read from a port, into the stack. The
 * symbols will be concatenated to form packets that can be accessed using other
 * functions.
 */
void RIO_portAddSymbol( RioStack_t *stack, const RioSymbol s );

/**
 * \brief Get the next symbol to transmit on a port.
 *
 * \param[in] stack The stack to operate on.
 * \return A symbol that should be sent on a port.
 *
 * This function is used to fetch new symbols to transmit on a port. Packets that
 * are inserted are split into symbols that are accessed with this function.
 */
RioSymbol RIO_portGetSymbol( RioStack_t *stack );


/*******************************************************************************************
 * DEPRECATED
 * Will be removed.
 *******************************************************************************************/

/**
 * \brief Read packet transaction identification.
 * \param[in] stack The stack to operate on.
 * \return The identification of the packet. This matches a transaction identifier
 * that are set as argument when a send-function are called.
 *
 * This function is used to correlate a sent packet to a received response.
 *
 * \note This function cannot be used when receiving messages on mailboxes. Use
 * RIO_readMessageMailbox() instead.
 */
uint8_t RIO_packetTid( RioStack_t *stack );

/**
 * \brief Read packet destination identification.
 * \param[in] stack The stack to operate on.
 * \return The destination device identifier of the packet.
 *
 * This function is used to get the destination device identifier of a received packet.
 */
uint16_t RIO_packetDestination( RioStack_t *stack );

/**
 * \brief Read packet source identification.
 * \param[in] stack The stack to operate on.
 * \return The source device identifier of the packet.
 *
 * This function is used to get the source device identifier of a received packet.
 */
uint16_t RIO_packetSource( RioStack_t *stack );

/**
 * \brief Read a maintenance read request hop count value.
 *
 * \param[in] stack The stack to operate on.
 * \return The hopcount value of the packet.
 *
 * This function returns the hop count value of a received maintenance read
 * request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint8_t RIO_readMaintenanceReadRequestHop( RioStack_t *stack );

/**
 * \brief Read a maintenance read request offset value.
 *
 * \param[in] stack The stack to operate on.
 * \return The offset value of the packet.
 *
 * This function returns the offset value of a received maintenance read
 * request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint32_t RIO_readMaintenanceReadRequestOffset( RioStack_t *stack );

/**
 * \brief Read a maintenance read response hop count value.
 *
 * \param[in] stack The stack to operate on.
 * \return The hop count value of the packet.
 *
 * This function returns the hop count value of a received maintenance read
 * response packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint8_t RIO_readMaintenanceReadResponseHop( RioStack_t *stack );

/**
 * \brief Read a maintenance read response offset value.
 *
 * \param[in] stack The stack to operate on.
 * \return The offset value of the packet.
 *
 * This function returns the offset value of a received maintenance read
 * response packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint32_t RIO_readMaintenanceReadResponse( RioStack_t *stack );

/**
 * \brief Read a maintenance write request hop count value.
 *
 * \param[in] stack The stack to operate on.
 * \return The hopcount value of the packet.
 *
 * This function returns the hop count value of a received maintenance write
 * request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint8_t RIO_readMaintenanceWriteRequestHop( RioStack_t *stack );

/**
 * \brief Read a maintenance write request offset value.
 *
 * \param[in] stack The stack to operate on.
 * \return The offset value of the packet.
 *
 * This function returns the offset value of a received maintenance write
 * request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint32_t RIO_readMaintenanceWriteRequestOffset( RioStack_t *stack );

/**
 * \brief Read a maintenance write request data value.
 *
 * \param[in] stack The stack to operate on.
 * \return The data value of the packet.
 *
 * This function returns the data value of a received maintenance write
 * request packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint32_t RIO_readMaintenanceWriteRequestData( RioStack_t *stack );

/**
 * \brief Read a maintenance write response hop count value.
 *
 * \param[in] stack The stack to operate on.
 * \return The hopcount value of the packet.
 *
 * This function returns the hop count value of a received maintenance write
 * response packet. 
 *
 * \note In normal operational mode, the stack answers maintenance requests 
 * automatically without user intervention. This function should only be 
 * called if the stack is compiled in transparent mode.
 */
uint8_t RIO_readMaintenanceWriteResponseHop( RioStack_t *stack );

/**
 * \brief Send an NWRITE request to write a byte.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] address The byte address to write to.
 * \param[in] data The byte data to write.
 * This function creates and sends an NWRITE request packet. No reply will be received.
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
void RIO_sendNwrite8( RioStack_t *stack, const uint16_t destid, const uint32_t address, const uint8_t data );

/**
 * \brief Send an NWRITE_R request to write a byte.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] address The byte address to write to.
 * \param[in] data The byte data to write.
 * \return An identifier that maps to the packet transaction identifier that are received using
 * RIO_packetTid.
 *
 * This function creates and sends an NWRITE_R request packet. A reply should be received
 * when the write has been completed.
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
void RIO_sendNwriteR8( RioStack_t *stack, const uint16_t destid, const uint8_t tid, const uint32_t address, const uint8_t data );

/**
 * \brief Get the byte address from an NWRITE or NWRITE_R packet.
 *
 * \param[in] stack The stack to operate on.
 * \return The byte address contained in the NWRITE or NWRITE_R packet.
 *
 * This function reads a received an NWRITE or NWRITE_R request packet and fetches the address
 * contained within.
 *
 * \note The address is a byte address, not a word address.
 */
uint32_t RIO_readNwriteAddress8( RioStack_t *stack );

/**
 * \brief Get the number of bytes to write from an NWRITE or NWRITE_R packet.
 *
 * \param[in] stack The stack to operate on.
 * \return The number of bytes requested to be written in a NWRITE or NWRITE_R packet.
 *
 * This function reads a received an NWRITE or NWRITE_R request packet and fetches the
 * number of bytes requested to be written.
 *
 * \note The returned size is the number of bytes.
 */
uint8_t RIO_readNwriteSize8( RioStack_t *stack );

/**
 * \brief Get the byte to write from a NWRITE or NWRITE_R packet.
 *
 * \param[in] stack The stack to operate on.
 * \return The byte requested to be written in a NWRITE or NWRITE_R packet.
 *
 * This function reads a received an NWRITE or NWRITE_R request packet and fetches the
 * byte to write.
 */
uint8_t RIO_readNwritePayload8( RioStack_t *stack );


/**
 * \brief Send a NREAD request to read a byte.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device id of the destination end point.
 * \param[in] tid The transaction id to be returned in the response.
 * \param[in] address The byte address to read.
 * \return An identifier that maps to the packet identifier that are received using
 * RIO_packetTid.
 *
 * This function creates and sends an NREAD request packet. The response packet will
 * contain one byte of data. The reply is received using RIO_readResponseDone8().
 *
 * \note The address is a byte address, not a word address.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
void RIO_sendNread8( RioStack_t *stack, const uint16_t destid, const uint8_t tid, const uint32_t address );


/**
 * \brief Get the byte address from a NREAD packet.
 *
 * \param[in] stack The stack to operate on.
 * \return The byte address contained in a NREAD packet.
 *
 * This function reads a received an NREAD request packet and fetches the address
 * contained within.
 *
 * \note The address is a byte address, not a word address.
 */
uint32_t RIO_readNreadAddress8( RioStack_t *stack );

/**
 * \brief Get the number of bytes to read from a NREAD packet.
 *
 * \param[in] stack The stack to operate on.
 * \return The number of bytes requested to read in a NREAD packet.
 *
 * This function reads a received an NREAD request packet and fetches the
 * number of bytes to return in the reply.
 *
 * \note The returned size is the number of bytes.
 */
uint8_t RIO_readNreadSize8( RioStack_t *stack );

/**
 * \brief Get a doorbell info field.
 *
 * \param[in] stack The stack to operate on.
 * \return The info field of a doorbell.
 *
 * This function is used to read and return the info field of a received doorbell.
 */
uint16_t RIO_readDoorbellInfo( RioStack_t *stack );

/**
 * \brief Send a byte message.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The device identifier of the target to send the message to.
 * \param[in] mbox The mailbox to send the message to.
 * \param[in] size The number of bytes to send.
 * \param[in] data A pointer to an array of bytes to send. The parameter size indicates
 * the number of bytes to send.
 *
 * This functions sends a message to a mailbox in an end-point target. A message response
 * should be sent when the message has been processed using the RIO_sendMessageResponseDone(),
 * RIO_sendMessageResponseRetry() or RIO_sendMessageResponseError() functions.
 *
 * \note All sizes that are transmitted are even double words. If 48-bits are
 * sent then 64 bits will be received on the other side with the last bits
 * padded to zero.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
void RIO_sendMessage8( RioStack_t *stack, const uint16_t destid, const uint8_t mbox, const uint16_t size, const uint8_t *data );

/**
 * \brief Read a received message mailbox.
 *
 * \param[in] stack The stack to operate on.
 * \return The mailbox of a received message.
 *
 * This function returns the mailbox of an incoming message.
 */
uint8_t RIO_readMessageMbox( RioStack_t *stack );

/**
 * \brief Read a received message payload size in bytes.
 *
 * \param[in] stack The stack to operate on.
 * \return The size in bytes of the received message payload.
 *
 * This function returns the number of bytes that was received in
 * an incoming message.
 *
 * \note All sizes that are actually transmitted are even double words. If
 * 48-bits are sent then 64 bits will be received on the other side with
 * the last bits padded to zero.
 */
uint16_t RIO_readMessageSize8( RioStack_t *stack );

/**
 * \brief Get the payload of a received message.
 *
 * \param[in] stack The stack to operate on.
 * \param[out] buffer A byte pointer to where to place the payload.
 * \return None
 * This function is used to copy the byte content of a received message.
 */
void RIO_readMessagePayload8( RioStack_t *stack, uint8_t *buffer );

/**
 * \brief Send a response with one byte data payload.
 *
 * \param[in] stack The stack to operate on.
 * \param[in] destid The destination identifier of the response.
 * \param[in] tid The transaction identifier for the reply. It should correspond to
 * the tid in the received packet for which this response is sent.
 * \param[in] address The byte address that was read.
 * \param[in] data The data to return in the reply.
 *
 * This function creates a response packet with the specified destination
 * identifier, transaction id and data payload. The generated packet are placed
 * in the outbound packet queue.
 *
 * \note Call RIO_sendAvailable() before this function is called to make sure
 * the outbound queue has transmission buffers available.
 */
void RIO_sendResponseDone8( RioStack_t *stack, const uint16_t destid, const uint8_t tid, const uint32_t address, const uint8_t data );

/**
 * \brief Read a one byte payload from a response.
 *
 * \param[in] stack The stack to operate on.
 * \return The byte data payload contained in the response.
 *
 * This function reads a response packet and returns a one byte payload
 * contained within.
 */
uint8_t RIO_readResponseDone8( RioStack_t *stack );

/**
 * \brief Get the target mailbox.
 *
 * \param[in] stack The stack to operate on.
 * \return The target mailbox.
 *
 * This function is used to get the target mailbox of a message response.
 */
uint8_t RIO_readMessageResponseMbox( RioStack_t *stack );

#endif /* _RIO_STACK_H */
 
/*************************** end of file **************************************/
