/******************************************************************************
 * (C) Copyright 2013-2015 Magnus Rosenius and the Free Software Foundation.
 *
 * This file is part of OpenRIO.
 *
 * OpenRIO is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * OpenRIO is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Lesser Public License
 * along with OpenRIO.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

/******************************************************************************
 * Description:
 * This file contains a software implementation of a RapidIO stack according to 
 * the 2.2 version, part 6, of the standard. 
 * A number of limitations exist:
 * - Only short control symbols (24-bit) are supported.
 * - No multicast symbols.
 * - No timestamp symbols.
 * - No VC.
 * - No priority.
 *
 * Any application specific tailoring needed to compile properly should be done 
 * in rioconfig.h.
 ******************************************************************************/

/*******************************************************************************
 * Includes
 *******************************************************************************/

#include "riostack.h"

/* Get definitions of ASSERT0(), may or may not return. */
#include "rioconfig.h"

/*******************************************************************************
 * Local macro definitions
 *******************************************************************************/

/*lint -save */
/*lint -e961 Macros are needed due to performance reasons. They are also 
  trivial and clarifies the code. */
/*lint -e750 Allow unused macros for future usage. */

/** The number of times a packet should be _retried_ when the link partner keeps 
    sending PACKET_NOT_ACCEPTED. A value of X will result in the packet being send 
    a maximum number X+1 times (retried X times). 
    This limit exists to avoid a link being locked-up if for example a non-
    maintenance packet is sent when not allowed. */
#define MAX_PACKET_ERROR_RETRIES 3u

/* Macro to update 5-bit ackId counters. */
#define MASK_5BITS(x) ((uint8_t) ((x) & 0x1fu))

/* Macros to get entries from a control symbol. */
#define STYPE0_GET(data) ((uint8_t) (((data) >> 21) & 0x00000007u))
#define PARAMETER0_GET(data) ((uint8_t) (((data) >> 16) & 0x00000001fu))
#define PARAMETER1_GET(data) ((uint8_t) (((data) >> 11) & 0x00000001fu))
#define STYPE1_GET(data) ((uint8_t) (((data) >> 8) & 0x00000007u))
#define CMD_GET(data) ((uint8_t) (((data) >> 5) & 0x00000007u))
#define CRC5_GET(data) ((uint8_t) ((data) & 0x0000001fu))

/*lint -restore */

/* Transmitter frame states. */
#define TX_FRAME_START                  ((uint8_t)0u)
#define TX_FRAME_BODY                   ((uint8_t)1u)

/* Control symbol constants. */
typedef enum
{
  STYPE0_PACKET_ACCEPTED=0,
  STYPE0_PACKET_RETRY=1,
  STYPE0_PACKET_NOT_ACCEPTED=2,
  STYPE0_RESERVED=3,
  STYPE0_STATUS=4,
  STYPE0_VC_STATUS=5,
  STYPE0_LINK_RESPONSE=6,
  STYPE0_IMPLEMENTATION_DEFINED=7
} stype0_t;

typedef enum
{
  STYPE1_START_OF_PACKET=0,
  STYPE1_STOMP=1,
  STYPE1_END_OF_PACKET=2,
  STYPE1_RESTART_FROM_RETRY=3,
  STYPE1_LINK_REQUEST=4,
  STYPE1_MULTICAST_EVENT=5,
  STYPE1_RESERVED=6,
  STYPE1_NOP=7
} stype1_t;

/* Constants used to request link-responses. */
typedef enum
{
  LINK_REQUEST_RESET_DEVICE=3u,
  LINK_REQUEST_INPUT_STATUS=4u
} LinkRequestCmd_t;

/* Constants used to forward a port status in a link-response. */
/*#define LINK_RESPONSE_PORT_STATUS_ERROR 2u*/
/*#define LINK_RESPONSE_PORT_STATUS_RETRY_STOPPED 4u*/
/*#define LINK_RESPONSE_PORT_STATUS_ERROR_STOPPED 5u*/
#define LINK_RESPONSE_PORT_STATUS_OK 16u

/*******************************************************************************
 * Local typedefs
 *******************************************************************************/



/*******************************************************************************
 * Global declarations
 *******************************************************************************/



/*******************************************************************************
 * Local declarations
 *******************************************************************************/



/*******************************************************************************
 * Local function prototypes
 *******************************************************************************/

/* Helper functions for protocol events. */
static void handleStype0(RioStack_t *stack, stype0_t stype0, uint8_t parameter0, uint8_t parameter1);
static void handleStype1(RioStack_t *stack, stype1_t stype1, uint8_t cmd);
static void handleDataSymbol(RioStack_t *stack, uint32_t symbol);
static void handleErrorSymbol(RioStack_t *stack);
static void handleErrorPacketCrc(RioStack_t *stack);

static void handleStatus(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus);
static void handlePacketAccepted(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus);
static void handlePacketRetry(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus);
static void handlePacketNotAccepted(RioStack_t *stack, const uint8_t arbitrary, const uint8_t cause);
static void handleLinkResponse(RioStack_t *stack, const uint8_t ackId, const uint8_t portStatus);
static void handleStartOfPacket(RioStack_t *stack);
static void handleEndOfPacket(RioStack_t *stack);
static void handleLinkRequest(RioStack_t *stack, LinkRequestCmd_t cmd);
static void handleNewPacketStart(RioStack_t *stack);
static void handleNewPacketEnd(RioStack_t *stack);

/**
 * \brief Create a control symbol.
 *
 * \param[in] stype0 The stype0 value.
 * \param[in] parameter0 The parameter0 value.
 * \param[in] parameter1 The parameter1 value.
 * \param[in] stype1 The stype1 value.
 * \param[in] cmd The cmd value.
 * \return The control symbol that were created from the input parameters.
 *
 * This function creates a control symbol with the specified arguments and
 * calculates a CRC-5 checksum according to the standard specification.
 */
static RioSymbol_t createControlSymbol(const stype0_t stype0,
                                       const uint8_t parameter0, const uint8_t parameter1,
                                       const stype1_t stype1, const uint8_t cmd);

/**
 * \brief Function to calculate ITU-CRC5, polynomial=0x15.
 *
 * \param[in] data The data of a control symbol.
 * \param[in] crc The CRC to initiate the result with.
 * \return A new CRC-5 value.
 */
static uint8_t crc5(const uint32_t data, const uint8_t crc);

/**
 * \brief Get status of the receiver queue to report to peer.
 *
 * \param[in] stack The stack to work on.
 * \return Available size of queue, limited to 5 bits.
 */
static uint8_t getBufferStatus(const RioStack_t *stack);

/**
 * \brief Create a queue with a specified size and a buffer attached to it.
 *
 * \param[in] size The number of entries in the queue.
 * \param[in] buffer A pointer to the buffer to store the content in.
 * \return A queue with the specified size and where new data will be stored 
 * to the specified buffer.
 */
static RioQueue_t queueCreate(const uint8_t size, uint32_t *buffer);

/**
 * \brief Get number of available elements.
 *
 * \param[in] q The queue to operate on.
 * \return The number of free packet buffers in the queue.
 */
static uint8_t queueAvailable(const RioQueue_t q);

/**
 * \brief Get if the queue is empty or not.
 *
 * \param[in] q The queue to operate on.
 * \return Non-zero if the queue is empty.
 */
static uint8_t queueEmpty(const RioQueue_t q);

/**
 * \brief Get the length of a queue.
 *
 * \param[in] q The queue to operate on.
 * \return The number of elements in the queue.
 */
static uint8_t queueLength(const RioQueue_t q);

/**
 * \brief Add a new element to the queue.
 *
 * \param[in] q The queue to operate on.
 * \return A queue with one added element.
 */
static RioQueue_t queueEnqueue(RioQueue_t q);

/**
 * \brief Remove an element from the queue.
 *
 * \param[in] q The queue to operate on.
 * \return A queue with on removed element.
 */
static RioQueue_t queueDequeue(RioQueue_t q);

/**
 * \brief Check if the readout window is empty.
 *
 * \param[in] q The queue to operate on.
 * \return If the readout window is empty.
 */
static uint8_t queueWindowEmpty(const RioQueue_t q);

/**
 * \brief Reset the window to none.
 *
 * \param[in] q The queue to operate on.
 * \return The updated RioQueue_t structure.
 */
static RioQueue_t queueWindowReset(RioQueue_t q);

/**
 * \brief Increase the window to the next pending element.
 *
 * \param[in] q The queue to operate on.
 * \return The updated RioQueue_t structure.
 */
static RioQueue_t queueWindowNext(RioQueue_t q);

/**
 * \brief Set actual size of the newest element.
 *
 * \param[in] q The queue to operate on.
 * \param[in] size The size to set the newest content size to.
 */
static void queueBackSetSize(RioQueue_t q, const uint32_t size);

/**
 * \brief Set content at a specified index in the newest element.
 *
 * \param[in] q The queue to operate on.
 * \param[in] index position into the element
 * \param[in] content The content to set at the specified index in the newest queue element.
 */
static void queueBackSetContent(RioQueue_t q, const uint32_t index, const uint32_t content);

/**
 * \brief Get a pointer to the buffer of the newest element.
 *
 * \param[in] q The queue to operate on.
 * \return A pointer to the content.
 */
static uint32_t *queueGetBackBuffer(RioQueue_t q );

/**
 * \brief Get the size of the oldest element.
 * \param[in] q The queue to operate on.
 * \return The size of the element.
 */
static uint32_t queueFrontGetSize(RioQueue_t q );

/**
 * \brief Get the content of the oldest element at specified index.
 * \param[in] q The queue to operate on.
 * \param[in] index The index into the element to get the content from.
 * \return content of element at index position.
 */
static uint32_t queueGetFrontContent(RioQueue_t q, const uint32_t index);

/**
 * \brief Get a pointer to the buffer of the oldest element.
 *
 * \param[in] q The queue to operate on.
 * \return A pointer to the content.
 */
static uint32_t *queueGetFrontBuffer(RioQueue_t q );

/*******************************************************************************
 * Global functions
 *******************************************************************************/

void RIOSTACK_open(RioStack_t *stack, void *private,
                   const uint32_t rxPacketBufferSize, uint32_t *rxPacketBuffer, 
                   const uint32_t txPacketBufferSize, uint32_t *txPacketBuffer)
{
  /* Port time and timeout limit. */
  stack->portTime = 0u;
  stack->portTimeout = 0u;

  /* Setup the receiver. */
  stack->rxState = RX_STATE_UNINITIALIZED;
  stack->rxCounter = 0u;
  stack->rxCrc = 0xffffu;
  stack->rxStatusReceived = 0u;
  stack->rxAckId = 0u;
  stack->rxAckIdAcked = 0u;
  stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_RESERVED;
  stack->rxQueue = queueCreate((uint8_t) (rxPacketBufferSize/RIOSTACK_BUFFER_SIZE), rxPacketBuffer);

  /* Setup the transmitter. */
  stack->txState = TX_STATE_UNINITIALIZED;
  stack->txCounter = 0u;
  stack->txStatusCounter = 0u;
  stack->txFrameState = TX_FRAME_START;
  stack->txAckId = 0u;
  stack->txAckIdWindow = 0u;
  stack->txPacketErrorCounter = 0u;
  stack->txQueue = queueCreate((uint8_t) (txPacketBufferSize/RIOSTACK_BUFFER_SIZE), txPacketBuffer);

  /* Setup status counters for inbound direction. */
  stack->statusInboundPacketComplete = 0ul;
  stack->statusInboundPacketRetry = 0ul;
  stack->statusInboundErrorControlCrc = 0ul;
  stack->statusInboundErrorPacketAckId = 0ul;
  stack->statusInboundErrorPacketCrc = 0ul;
  stack->statusInboundErrorIllegalCharacter = 0ul;
  stack->statusInboundErrorGeneral = 0ul;
  stack->statusInboundErrorPacketUnsupported = 0ul;

  /* Setup status counters for outbound direction. */
  stack->statusOutboundPacketComplete = 0ul;
  stack->statusOutboundLinkLatencyMax = 0ul;
  stack->statusOutboundPacketRetry = 0ul;
  stack->statusOutboundErrorTimeout = 0ul;
  stack->statusOutboundErrorPacketAccepted = 0ul;
  stack->statusOutboundErrorPacketRetry = 0ul;

  /* Setup status counters for potential problems on the link-partner. */
  stack->statusPartnerLinkRequest = 0ul;
  stack->statusPartnerErrorControlCrc = 0ul;
  stack->statusPartnerErrorPacketAckId = 0ul;
  stack->statusPartnerErrorPacketCrc = 0ul;
  stack->statusPartnerErrorIllegalCharacter = 0ul;
  stack->statusPartnerErrorGeneral = 0ul;

  /* Set pointer to user private data. */
  stack->private = private;
}



/*******************************************************************************************
 * Stack status and queue access functions.
 * Note that status counters are accessed directly in the stack-structure.
 *******************************************************************************************/

uint8_t RIOSTACK_getLinkIsInitialized(const RioStack_t *stack)
{
  uint8_t status;


  status = (uint8_t) !(((stack->rxState == RX_STATE_UNINITIALIZED) || 
                        (stack->rxState == RX_STATE_PORT_INITIALIZED)) &&
                       ((stack->txState == TX_STATE_UNINITIALIZED) || 
                        (stack->txState == TX_STATE_PORT_INITIALIZED)));
  
  return status;
}

uint8_t RIOSTACK_getStatus(const RioStack_t *stack)
{
  return RIOSTACK_getLinkIsInitialized(stack);
}



uint8_t RIOSTACK_getOutboundQueueLength(const RioStack_t *stack)
{
  return queueLength(stack->txQueue);
}



uint8_t RIOSTACK_getOutboundQueueAvailable(const RioStack_t *stack)
{
  return queueAvailable(stack->txQueue);
}



void RIOSTACK_setOutboundPacket(RioStack_t *stack, RioPacket_t *packet)
{
  uint32_t *src, *dst;
  uint32_t size;
  uint32_t i;

  if(queueAvailable(stack->txQueue) > 0u)
  {
    src = &packet->payload[0];
    dst = queueGetBackBuffer(stack->txQueue);
    size = packet->size;
    for(i = 0u; i < size; i++)
    {
      dst[i] = src[i]; /*lint !e960 This is not pointer arithmetics. */
    }

    queueBackSetSize(stack->txQueue, size);
    stack->txQueue = queueEnqueue(stack->txQueue);
  }
  else
  {
    ASSERT0("Transmission queue packet overflow.");
  }
}



uint8_t RIOSTACK_getInboundQueueLength(const RioStack_t *stack)
{
  return queueLength(stack->rxQueue);
}



uint8_t RIOSTACK_getInboundQueueAvailable(const RioStack_t *stack)
{
  return queueAvailable(stack->rxQueue);
}



void RIOSTACK_getInboundPacket(RioStack_t *stack, RioPacket_t *packet)
{
  uint32_t *src, *dst;
  uint8_t size;
  uint8_t i;


  if(!queueEmpty(stack->rxQueue)) /*lint !e961 This is a boolean expression. */
  {
    src = queueGetFrontBuffer(stack->rxQueue);
    dst = &packet->payload[0];
    size = (uint8_t) queueFrontGetSize(stack->rxQueue);
    for(i = 0u; i < size; i++)
    {
      dst[i] = src[i]; /*lint !e960 This is not pointer arithmetics. */
    }

    packet->size = size;
    stack->rxQueue = queueDequeue(stack->rxQueue);
  }
  else
  {
    ASSERT0("Reading from empty reception queue.");
  }
}



/*******************************************************************************************
 * Packet port functions.
 *******************************************************************************************/

void RIOSTACK_portSetTime(RioStack_t *stack, const uint32_t timer)
{
  stack->portTime = timer;
}



void RIOSTACK_portSetTimeout(RioStack_t *stack, const uint32_t timer)
{
  stack->portTimeout = timer;
}



void RIOSTACK_portSetStatus(RioStack_t *stack, const uint8_t initialized)
{
  /* REMARK: Clean the queues here as well??? */
  if (initialized)
  {
    stack->rxState = RX_STATE_PORT_INITIALIZED;
    stack->rxCounter = 0u;
    stack->rxCrc = 0xffffu;
    stack->rxStatusReceived = 0u;
    stack->rxAckId = 0u;
    stack->rxAckIdAcked = 0u;
    stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_RESERVED;

    stack->txState = TX_STATE_PORT_INITIALIZED;
    stack->txCounter = 0u;
    stack->txStatusCounter = 0u;
    stack->txFrameState = TX_FRAME_START;
    stack->txAckId = 0u;
    stack->txAckIdWindow = 0u;
  }
  else
  {
    stack->rxState = RX_STATE_UNINITIALIZED;
    stack->txState = TX_STATE_UNINITIALIZED;
  }
}



void RIOSTACK_portAddSymbol(RioStack_t *stack, const RioSymbol_t symbol)
{
  stype0_t stype0;
  uint8_t parameter0;
  uint8_t parameter1;
  stype1_t stype1;
  uint8_t cmd;


  /* Check the receiver state. */
  if(stack->rxState != RX_STATE_UNINITIALIZED)
  {
    /* The receiver is not uninitialied. */

    /* Check the type of symbol. */
    if(symbol.type == RIOSTACK_SYMBOL_TYPE_DATA)
    {
      /* This is a data symbol. */
      handleDataSymbol(stack, symbol.data);
    }
    else if(symbol.type == RIOSTACK_SYMBOL_TYPE_CONTROL)
    {
      /* This is a control symbol. */

      /* Check if the CRC is correct. */
      if(crc5(symbol.data, 0x1fu) == CRC5_GET(symbol.data))
      {
        /* The CRC is correct. */

        /* Get the content of the control symbol. */
        stype0 = (stype0_t) STYPE0_GET(symbol.data);
        parameter0 = PARAMETER0_GET(symbol.data);
        parameter1 = PARAMETER1_GET(symbol.data);
        stype1 = (stype1_t) STYPE1_GET(symbol.data);            
        cmd = CMD_GET(symbol.data);

        /* Check the stype0 part of the symbol. */
        handleStype0(stack, stype0, parameter0, parameter1);

        /* Check the stype1 part of the symbol. */
        handleStype1(stack, stype1, cmd);
      }
      else
      {
        /* The CRC is not correct. */
        handleErrorPacketCrc(stack);
      }
    }
    else if(symbol.type == RIOSTACK_SYMBOL_TYPE_ERROR)
    {
      /* The decoder has received a erroneous symbol. */
      handleErrorSymbol(stack);
    }
    else
    {
      /* Idle symbol or unsupported symbol. */
      /* Discard these for now. */
    }
  }
  else
  {
    /* The receiver is uninitialied. */
    /* Discard all incoming symbols. */
  }
}



RioSymbol_t RIOSTACK_portGetSymbol(RioStack_t *stack )
{
  RioSymbol_t s;
  uint8_t bufferStatus;


  switch(stack->txState)
  {
    case TX_STATE_PORT_INITIALIZED:
      /******************************************************************************
       * PORT_INITIALIZED
       * This state is entered to initialize the link. Send status-control-symbols 
       * once in a while until the receiver has received enough error-free status-
       * control-symbols and we have transmitted enough status-control-symbols. Once 
       * an error-free status-control-symbol has been received, the statuses are
       * transmitted more frequently to decrease the time for the link to be 
       * initialized.
       ******************************************************************************/
      
      /* Check if an idle symbol or a status control symbol should be sent. */
      if(((stack->rxStatusReceived == 0u) && (stack->txCounter == 255u)) ||
         ((stack->rxStatusReceived == 1u) && (stack->txCounter >= 15u)))
      {
        /* A control symbol should be sent. */

        /* Create a new status symbol and reset the transmission counter. */
        stack->txCounter = 0u;
        bufferStatus = getBufferStatus(stack);
        s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_NOP, 0u);

        /* Check if the receiver has received any error-free status and that we 
           have sent at least 15 status control symbols. */
        if((stack->rxStatusReceived == 1u) && (stack->txStatusCounter < 15u))
        {
          /* Has not sent enough status control symbols. */
          stack->txStatusCounter++;
        }
        else
        {
          /* Has sent enough status control symbols. */
          /* Don't do anything. */
        }
      }
      else
      {
        /* Idle symbol should be sent. */
        s.type = RIOSTACK_SYMBOL_TYPE_IDLE;
        stack->txCounter++;
      }

      /* Check if we are ready to set the transmitter in a link initialized state. */
      if ((stack->rxState == RX_STATE_LINK_INITIALIZED) && (stack->txStatusCounter == 15u))
      {
        /* Ready to go to link initialized. */
        stack->txState = TX_STATE_LINK_INITIALIZED;
        stack->txFrameState = TX_FRAME_START;
        stack->txStatusCounter = 0u;
      }
      else
      {
        /* Not ready to go to link initialized. */
        /* Don't do anything. */
      }

      break;

    case TX_STATE_LINK_INITIALIZED:
      /******************************************************************************
       * LINK_INITIALIZED
       * The normal state. Accept packets and forward them. Send acknowledges when 
       * the receiver has received complete packets.
       ******************************************************************************/

      /* Check if the receiver wants to acknowledge a packet. */
      if(stack->rxAckId == stack->rxAckIdAcked)
      {
        /* The receiver does not want to acknowledge a packet. */

        /* Check if there are any outstanding packets and if it has timed out. */
        if((stack->txAckId == stack->txAckIdWindow) ||
           ((stack->portTime - stack->txFrameTimeout[stack->txAckId]) < stack->portTimeout))
        {
          /* There are no outstanding packets or there has been no timeout. */

          /* Check if a packet is ongoing. */
          if(stack->txFrameState == TX_FRAME_BODY)
          {
            /* A packet transmission is ongoing. */

            /* Check if the packet has been completly sent. */
            if(stack->txCounter != queueFrontGetSize(stack->txQueue))
            {
              /* The packet has not been completly sent. */

              /* Create a new data symbol to transmit. */
              s.type = RIOSTACK_SYMBOL_TYPE_DATA;
              s.data = queueGetFrontContent(stack->txQueue, (uint32_t)stack->txCounter);
              
              /* Check if this is the first symbol in a packet. */
              if (stack->txCounter == 0u)
              {
                /* Place the correct ackId in the right place. */
                s.data |= (uint32_t)stack->txAckIdWindow << 27;
              }
              else
              {
                /* Don't do anything. */
              }

              /* Update the transmission counter. */
              stack->txCounter++;

              /* A status control symbol was not sent. Update the status counter. */
              stack->txStatusCounter++;
            }
            else 
            {
              /* The packet has been sent. */

              /* Save the timeout time and update to the next ackId. */
              stack->txFrameTimeout[stack->txAckIdWindow] = stack->portTime;
              stack->txAckIdWindow = MASK_5BITS(stack->txAckIdWindow + 1u);
              stack->txQueue = queueWindowNext(stack->txQueue);

              /* Check if there are more packets pending to be sent. */
              /* Also check that there are buffer available at the receiver and that not too many 
                 packets are outstanding. */
              if((!queueWindowEmpty(stack->txQueue)) &&  /*lint !e961 Boolean expression. */
                 (stack->txBufferStatus > 0u) &&
                 (MASK_5BITS(stack->txAckIdWindow - stack->txAckId) != 31u))
              {
                /* More pending packets. */

                /* Create a control symbol to signal that the new packet has started. */
                bufferStatus = getBufferStatus(stack);
                s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_START_OF_PACKET, 0u);

                /* Restart transmission counter. */
                stack->txCounter = 0u;
              }
              else
              {
                /* No more pending packets. */

                /* Create a control symbol to signal that the packet has ended. */
                bufferStatus = getBufferStatus(stack);
                s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_END_OF_PACKET, 0u);

                /* Go back to wait for a new frame. */
                stack->txFrameState = TX_FRAME_START;
              }

              /* A status control symbol has been sent. Reset the status counter. */
              stack->txStatusCounter = 0u;
            }
          }
          else
          {
            /* No packet is being sent. */

            /* Check if there are any pending packets to start sending. */
            /* Also check that there are buffer available at the receiver and that not too many 
               packets are outstanding. */
            if((!queueWindowEmpty(stack->txQueue)) && /*lint !e961 Boolean expression. */
               (stack->txBufferStatus > 0u) &&
               (MASK_5BITS(stack->txAckIdWindow - stack->txAckId) != 31u))
            {
              /* There is a pending packet to send. */

              /* Send a start-of-packet control symbol to start to send the packet. */
              bufferStatus = getBufferStatus(stack);
              s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_START_OF_PACKET, 0u);
              stack->txFrameState = TX_FRAME_BODY;
              stack->txCounter = 0u;

              /* A status control symbol has been sent. Reset the status counter. */
              stack->txStatusCounter = 0u;
            }
            else
            {
              /* There are no pending packets to send. */

              /* Check if a status control symbol must be transmitted. */
              if(stack->txStatusCounter < 255u)
              {
                /* Not required to send a status control symbol. */

                /* Send an idle-symbol. */
                s.type = RIOSTACK_SYMBOL_TYPE_IDLE;
                stack->txStatusCounter++;
              }
              else
              {
                /* Must send a status control symbol. */

                /* Create a status control symbol. */
                bufferStatus = getBufferStatus(stack);
                s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_NOP, 0u);

                /* A status control symbol has been sent. Reset the status counter. */
                stack->txStatusCounter = 0u;
              }
            }
          }
        }
        else
        {
          /* There has been a timeout. */
          /* A packet has been sent but no packet-accepted has been received. */

          /* Send link-request-symbol (input-status). */
          bufferStatus = getBufferStatus(stack);
          s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_LINK_REQUEST, (uint8_t) LINK_REQUEST_INPUT_STATUS);

          /* Save the time when this was transmitted. */
          stack->txFrameTimeout[stack->txAckId] = stack->portTime;

          /* Remember that this symbol has been transmitted. */
          stack->txCounter = 1u;

          /* Go into the output error stopped state. */
          stack->txState = TX_STATE_OUTPUT_ERROR_STOPPED;
          stack->statusOutboundErrorTimeout++;
        }
      }
      else
      {
        /* The receiver wants us to send an acknowledgment. */
        bufferStatus = getBufferStatus(stack);
        s = createControlSymbol(STYPE0_PACKET_ACCEPTED, stack->rxAckIdAcked, bufferStatus, STYPE1_NOP, 0u);
        stack->rxAckIdAcked = MASK_5BITS(stack->rxAckIdAcked + 1u);

        /* A status control symbol was not sent. Update the status counter. */
        stack->txStatusCounter++;
      }
      break;

    case TX_STATE_SEND_PACKET_RETRY:
      /******************************************************************************
       * SEND_PACKET_RETRY
       * This state is set by the receiver to force a packet-retry-symbol to be 
       * transmitted.
       ******************************************************************************/
     
      /* Check if the receiver wants to acknowledge a packet. */
      /* This must be done first or we will get an error for a mismatching ackId in the link-partner. */
      if(stack->rxAckId == stack->rxAckIdAcked)
      {
        /* No pending acknowledge. */

        /* Send a packet-retry symbol to tell the link partner to retry the last frame. */
        bufferStatus = getBufferStatus(stack);
        s = createControlSymbol(STYPE0_PACKET_RETRY, stack->rxAckId, bufferStatus, STYPE1_NOP, 0u);

        /* Proceed with normal transmission. */
        stack->txState = TX_STATE_LINK_INITIALIZED;

        /* A status control symbol was not sent. Update the status counter. */
        stack->txStatusCounter++;
      }
      else
      {
        /* The receiver wants us to send an acknowledgment. */
        bufferStatus = getBufferStatus(stack);
        s = createControlSymbol(STYPE0_PACKET_ACCEPTED, stack->rxAckIdAcked, bufferStatus, STYPE1_NOP, 0u);
        stack->rxAckIdAcked = MASK_5BITS(stack->rxAckIdAcked + 1u);

        /* A status control symbol was not sent. Update the status counter. */
        stack->txStatusCounter++;
      }
      break;

    case TX_STATE_SEND_PACKET_NOT_ACCEPTED:
      /******************************************************************************
       * SEND_PACKET_NOT_ACCEPTED
       * This state is set by the receiver to force a packet-not-accepted-symbol to be 
       * transmitted.
       ******************************************************************************/

      /* Send a packet-not-accepted symbol to indicate an error on the link. */
      s = createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0u, (uint8_t) stack->rxErrorCause, 
                              STYPE1_NOP, 0u);

      /* Proceed with normal transmission. */
      stack->txState = TX_STATE_LINK_INITIALIZED;

      /* A status control symbol was not sent. Update the status counter. */
      stack->txStatusCounter++;
      break;

    case TX_STATE_SEND_LINK_RESPONSE:
      /******************************************************************************
       * SEND_LINK_RESPONSE
       * This state is set by the receiver to force a link-response-symbol to be 
       * transmitted.
       * Note that the link-request that caused this response also makes the receiver enter the normal operational state.
       ******************************************************************************/

      s = createControlSymbol(STYPE0_LINK_RESPONSE, stack->rxAckId, LINK_RESPONSE_PORT_STATUS_OK, STYPE1_NOP, 0u);

      /* Proceed with normal transmission. */
      stack->txState = TX_STATE_LINK_INITIALIZED;

      /* Force a status to be transmitted the next time to comply to the input-error-stopped 
         state rules. */
      stack->txStatusCounter = 255u;
      break;

    case TX_STATE_OUTPUT_RETRY_STOPPED:
      /******************************************************************************
       * OUTPUT_RETRY_STOPPED
       * This state is entered when the link-partner has transmitted a 
       * packet-retry-symbol. The packet-retry-symbol is acknowledged by sending a 
       * restart-from-retry-symbol.
       * This state follows 5.9.1.5 in part 6. 
       ******************************************************************************/

      /* Send a restart-from-retry symbol to acknowledge. */
      bufferStatus = getBufferStatus(stack);
      s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus, STYPE1_RESTART_FROM_RETRY, 0u);

      /* Restart the current frame and proceed with normal operation. */
      stack->txFrameState = TX_FRAME_START;
      stack->txState = TX_STATE_LINK_INITIALIZED;
      stack->txCounter = 0u;

      /* Discard all packets that has not received a matching packet-accepted. */
      stack->txAckIdWindow = stack->txAckId;
      stack->txQueue = queueWindowReset(stack->txQueue);

      /* A status control symbol was sent. Reset the status counter. */
      stack->txStatusCounter = 0u;
      break;

    case TX_STATE_OUTPUT_ERROR_STOPPED:
      /******************************************************************************
       * OUTPUT_ERROR_STOPPED
       * This state is entered when the link partner has encountered any problem 
       * which is indicated by sending a packet-not-accepted symbol or if a packet 
       * timeout has expired. The error condition is acknowledged by sending a 
       * link-request-symbol and then wait for a link-response reply.
       * This state follows 5.13.2.7 in part 6. 
       ******************************************************************************/

      /* Check if a link-request-symbol has been transmitted. */
      if(stack->txCounter == 0u)
      {
        /* A link-request-symbol has not been transmitted. */

        /* Send link-request-symbol (input-status). */
        bufferStatus = getBufferStatus(stack);
        s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus,
                                STYPE1_LINK_REQUEST, (uint8_t) LINK_REQUEST_INPUT_STATUS);

        /* Save the time when this was transmitted. */
        stack->txFrameTimeout[stack->txAckId] = stack->portTime;

        /* Remember that this symbol has been transmitted. */
        stack->txCounter = 1u;
      }
      else
      {
        /* A link-request-symbol has been transmitted. */

        /* Check if the link partner reply has timed out. */
        if((stack->portTime - stack->txFrameTimeout[stack->txAckId]) < stack->portTimeout)
        {
          /* No timeout. */

          /* A link-request-symbol has been transmitted. */
          /* Send only idle-symbols until the link-response is received. */
          s.type = RIOSTACK_SYMBOL_TYPE_IDLE;
        }
        else
        {
          /* Link response timeout. */

          /* Check if the link-partner has not responded for too many times. */
          if(stack->txCounter < 5u)
          {
            /* Not too many timeouts. */
            /* Retry and send a new link-request. */

            /* Send link-request-symbol (input-status). */
            bufferStatus = getBufferStatus(stack);
            s = createControlSymbol(STYPE0_STATUS, stack->rxAckId, bufferStatus,
                                    STYPE1_LINK_REQUEST, (uint8_t) LINK_REQUEST_INPUT_STATUS);
            
            /* Save the time when this was transmitted. */
            stack->txFrameTimeout[stack->txAckId] = stack->portTime;
            
            /* Increment the number of times we have retransmitted the link-request. */
            stack->txCounter++;
          }
          else
          {
            /* The link partner has not answered for too many times. */
            /* Give up and set the state to uninitialized. */
            stack->txState = TX_STATE_UNINITIALIZED;
            s.type = RIOSTACK_SYMBOL_TYPE_IDLE;
            ASSERT0("No link-response received, giving up.");
          }
        }
      }
      break;

    case TX_STATE_UNINITIALIZED:
    default:
      /******************************************************************************
       * Wait for the port to be initialized.
       ******************************************************************************/

      /* Send only idle symbols. */
      s.type = RIOSTACK_SYMBOL_TYPE_IDLE;
      break;
  }

  /* Return the created symbol. */
  return s;
}



/*******************************************************************************
 * Local RapidIO stack helper functions.
 *******************************************************************************/


static void handleStype0(RioStack_t *stack, stype0_t stype0, uint8_t parameter0, uint8_t parameter1)
{
  /* Check if the receiver port is initialized. */
  if(stack->rxState != RX_STATE_PORT_INITIALIZED)
  {
    /* The receiver port is initialized. */

    /* Check the stype0 part of the symbol. */
    switch(stype0)
    {
      case STYPE0_STATUS:
        /* A status containing the current ackId and the buffer status has been 
           received. */
        handleStatus(stack, parameter0, parameter1);
        break;

      case STYPE0_PACKET_ACCEPTED:
        /* A packet has been accepted by the link partner. */
        handlePacketAccepted(stack, parameter0, parameter1);
        break;

      case STYPE0_PACKET_RETRY:
        /* The link partner wants us to initiate a restart of the received ackId. */
        handlePacketRetry(stack, parameter0, parameter1);
        break;

      case STYPE0_PACKET_NOT_ACCEPTED:
        /* The link partner indicates that a packet has been rejected. */
        handlePacketNotAccepted(stack, parameter0, parameter1);
        break;

      case STYPE0_LINK_RESPONSE:
        /* The link partner has sent a response to a link-request. */
        handleLinkResponse(stack, parameter0, parameter1);
        break;

      case STYPE0_VC_STATUS:
      case STYPE0_RESERVED:
      case STYPE0_IMPLEMENTATION_DEFINED:
      default:
        /* Unsupported symbol received. */
        /* Discard them. */
        break;
    }
  }
  else
  {
    /* The receiver port is not initialized. */

    /* Check if the symbol is a status symbol. */
    /* Only status symbols are accepted in this state. */
    if(stype0 == STYPE0_STATUS) 
    {
      /* Status symbol received. */
            
      /* Indicate an error-free status has been received. */
      stack->rxStatusReceived = 1u;
            
      /* Check if enough status control symbols has been received. */
      if(stack->rxCounter == 7u)
      {
        /* Enough correct status control symbols has been received without 
           errors in between. */

        /* Setup the transmitter with the content of the symbol. */
        stack->txAckId = parameter0;
        stack->txAckIdWindow = stack->txAckId;
        stack->txBufferStatus = parameter1;

        /* Set the transmitter in its normal operational mode. */
        stack->rxState = RX_STATE_LINK_INITIALIZED;
        stack->rxCounter = 0u;
      }
      else
      {
        /* Count the number of consecutive error-free status control symbols
           that has been received. */
        stack->rxCounter++;
      }
    }
    else
    {
      /* The received symbol is not a status symbol. */
      /* Discard it. */
    }
  }
}


static void handleStype1(RioStack_t *stack, stype1_t stype1, uint8_t cmd)
{
  switch(stype1)
  {
    case STYPE1_START_OF_PACKET:
      if(stack->rxState == RX_STATE_LINK_INITIALIZED)
      {
        /* Start of a new packet. */
        handleStartOfPacket(stack);
      }
      break;

    case STYPE1_END_OF_PACKET:
      if(stack->rxState == RX_STATE_LINK_INITIALIZED)
      {
        /* Ending a packet. */
        handleEndOfPacket(stack);
      }
      break;

    case STYPE1_STOMP:
      if(stack->rxState == RX_STATE_LINK_INITIALIZED)
      {
        /* Cancel the currently received frame. */
        stack->rxCounter = 0u;
      }
      break;

    case STYPE1_RESTART_FROM_RETRY:
      if(stack->rxState == RX_STATE_LINK_INITIALIZED)
      {
        /* Cancel the currently received frame when in this state. */
        stack->rxCounter = 0u;
      }
      else if(stack->rxState == RX_STATE_INPUT_RETRY_STOPPED)
      {
        /* The link partner has confirmed our packet-retry-symbol. */
        /* Go back to the normal state and reset the frame reception. */
        stack->rxState = RX_STATE_LINK_INITIALIZED;
        stack->rxCounter = 0u;
      }
      else
      {
        /* Discard in other states. */
      }
      break;

    case STYPE1_LINK_REQUEST:
      /* A link-request has been received. */
      handleLinkRequest(stack, (LinkRequestCmd_t) cmd);
      stack->rxState = RX_STATE_LINK_INITIALIZED;
      
      break;

    case STYPE1_NOP:
      /* No operation symbol. */
      /* Discard these. */
      break;

    case STYPE1_MULTICAST_EVENT:
    case STYPE1_RESERVED:
    default:
      /* Unsupported symbol received. */
      /* Discard them. */
      break;
  }
}


static void handleDataSymbol(RioStack_t *stack, uint32_t symbol)
{
  /* Check the state. A data symbol is only processed in the operational state. */
  if(stack->rxState == RX_STATE_LINK_INITIALIZED)
  {
    /* We are in operational state. */

    /* Check if a packet has been started and that it is not too long. */
    if((stack->rxCounter >= 1u) && (stack->rxCounter <= RIOPACKET_SIZE_MAX))
    {
      /* A packet has been started. */

      /* Check if the ackId is correct on the first part of the packet. */
      if((stack->rxCounter > 1u) || (((uint8_t)(symbol >> 27)) == stack->rxAckId))
      {
        /* The ackId is the expected one. */

        /* Check if this is the first symbol of a packet. */
        if(stack->rxCounter == 1u)
        {
          /* This is the first symbol of the packet. */
          /* Start to calculate the CRC of the packet. */
          /* Note that the ackId should not be included in the CRC calculation. */
          stack->rxCrc = RIOPACKET_crc32(symbol & (uint32_t)0x03fffffful, 0xffffu);
        }
        else
        {
          /* This is not the first symbol. */
          /* Continue to calculate the CRC of the packet. */
          stack->rxCrc = RIOPACKET_crc32(symbol, stack->rxCrc);
        }

        /* Save the new data in the packet queue and update the reception counter. */
        queueBackSetContent(stack->rxQueue, (uint32_t)stack->rxCounter - (uint32_t)1ul, symbol);
        stack->rxCounter++;
      }
      else
      {
        /* The ackId is not correct. */
        /* Packet error. Enter input-error-stopped state. */
        stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
        stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
        stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID;
        stack->statusInboundErrorPacketAckId++;
      }
    }
    else
    {
      /* No packet has been started or the packet is too long. */
      /* Packet error. Enter input-error-stopped state. */
      stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
      stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
      stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_GENERAL;
      stack->statusInboundErrorGeneral++;
    }
  }
  else
  {
    /* We are not in operational state. */
    /* Discard all data symbols. */
  }
}


static void handleErrorSymbol(RioStack_t *stack)
{
  if(stack->rxState != RX_STATE_INPUT_ERROR_STOPPED)
  {
    /* Idle symbol error. Place the receiver in input-error-stopped state. */
    stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
    stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
    stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_ILLEGAL_CHARACTER;
    stack->statusInboundErrorIllegalCharacter++;
  }
}


static void handleErrorPacketCrc(RioStack_t *stack)
{
  if(stack->rxState != RX_STATE_PORT_INITIALIZED)
  {
    /* The control symbol CRC is incorrect. */
    /* Corrupted control symbol. Discard the symbol and enter the input-error-stopped state. */
    stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
    stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
    stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC;
    stack->statusInboundErrorControlCrc++;
  }
  else
  {
    /* CRC error in control symbol. */
    /* Restart counting error-free status-control-symbols. */
    stack->rxCounter = 0u;
  }
}


static void handleStatus(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus)
{
  /* Update the buffer status of the link partner. */
  (void) ackId;
  stack->txBufferStatus = bufferStatus;
}



static void handlePacketAccepted(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus)
{
  uint32_t linkLatency;


  /* Check if an acknowledge is expected and that it is for a transmitted packet. */
  if((stack->txAckId != stack->txAckIdWindow) && (ackId == stack->txAckId))
  {
    /* Acknowledge for a recently transmitted packet received. */

    /* Check if the latency of this packet is larger than the largest encountered so far. */
    linkLatency = stack->portTime - stack->txFrameTimeout[ackId];
    if(linkLatency > stack->statusOutboundLinkLatencyMax)
    {
      stack->statusOutboundLinkLatencyMax = linkLatency;
    }

    /* Remove the packet from the outbound queue and restart the transmission for
       a new packet. */
    stack->txQueue = queueDequeue(stack->txQueue);
    stack->txAckId = MASK_5BITS(stack->txAckId + 1u);
    stack->txPacketErrorCounter = 0;
    stack->statusOutboundPacketComplete++;
  }
  else
  {
    /* Acknowledge for an unexpected ackId or not waiting for an acknowledge. */
    /* Link protocol violation. Discard the symbol and enter the output-error-stopped state. */
    stack->txState = TX_STATE_OUTPUT_ERROR_STOPPED;
    stack->txCounter = 0u;
    stack->statusOutboundErrorPacketAccepted++;
  }

  /* Update the buffer status of the link partner. */
  stack->txBufferStatus = bufferStatus;
}



static void handlePacketRetry(RioStack_t *stack, const uint8_t ackId, const uint8_t bufferStatus)
{
  /* Check if the retried packet ackId is acceptable. */
  if(ackId == stack->txAckId)
  {
    /* The request for retry is for the current packet. */
    /* Force the transmitter to send a RESTART-FROM-RETRY symbol. */
    stack->txState = TX_STATE_OUTPUT_RETRY_STOPPED;
    stack->statusOutboundPacketRetry++;
  }
  else
  {
    /* Link protocol violation. Discard the symbol and enter the output-error-stopped state. */
    stack->txState = TX_STATE_OUTPUT_ERROR_STOPPED;
    stack->txCounter = 0u;
    stack->statusOutboundErrorPacketRetry++;
  }

  /* Update the buffer status of the link partner. */
  stack->txBufferStatus = bufferStatus;
}



static void handlePacketNotAccepted(RioStack_t *stack, const uint8_t arbitrary, const uint8_t cause)
{
  (void) arbitrary;

  /* Force the transmitter to enter output-error-stopped state. */
  stack->txState = TX_STATE_OUTPUT_ERROR_STOPPED;
  stack->txCounter = 0u;

  /* Record the type of error that caused the packet to not being accepted. */
  switch(cause)
  {
    case PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID:
      stack->statusPartnerErrorPacketAckId++;
      break;
    case PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC:
      stack->statusPartnerErrorControlCrc++;
      break;
    case PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC:
      stack->statusPartnerErrorPacketCrc++;
      break;
    case PACKET_NOT_ACCEPTED_CAUSE_ILLEGAL_CHARACTER:
      stack->statusPartnerErrorIllegalCharacter++;
      break;
    default:
      stack->statusPartnerErrorGeneral++;
      break;
  }
}



static void handleLinkResponse(RioStack_t *stack, const uint8_t ackId, const uint8_t portStatus)
{
  uint8_t window;
  uint8_t windowReceived;


  (void) portStatus;

  /* Check if this symbols is expected. */
  if(stack->txState == TX_STATE_OUTPUT_ERROR_STOPPED)
  {
    /* A link-response is expected. */

    /* Calculate the number of packets that has not received an acknowledge on our side and 
       on the link-partner side. */
    window = MASK_5BITS(stack->txAckIdWindow - stack->txAckId);
    windowReceived = MASK_5BITS(ackId - stack->txAckId);

    /* Check if the link-partners response is acceptable. */
    if(windowReceived <= window)
    {
      /* The link-partner response is acceptable. */
      
      /* Remove entries in the queue that the link-partner has sent acknowledges for that has been lost. */
      while(stack->txAckId != ackId)
      {
        stack->txQueue = queueDequeue(stack->txQueue);
        stack->txAckId = MASK_5BITS(stack->txAckId + 1u);
        stack->txPacketErrorCounter = 0;
        stack->statusOutboundPacketComplete++;
      }

      /* Check if this packet has been rejected too many times. */
      if(stack->txPacketErrorCounter < MAX_PACKET_ERROR_RETRIES)
      {
        /* Not rejected too many times. */
        /* Just increase the count of the number of rejections. */
        stack->txPacketErrorCounter++;
      }
      else
      {
        /* Rejected too many times. */
        /* Remove the packet but dont update the ackId. */
        stack->txQueue = queueDequeue(stack->txQueue);
        stack->txPacketErrorCounter = 0;
      }
      
      /* Set the transmission window to the resend packets that has not been received. */
      stack->txQueue = queueWindowReset(stack->txQueue);
      stack->txAckIdWindow = ackId;
      stack->txFrameState = TX_FRAME_START;

      /* Set the transmitter back into normal operation. */
      stack->txState = TX_STATE_LINK_INITIALIZED;
    }
    else
    {
      /* The link-partner response is unacceptable. */
      /* Recovery is not possible. */
      stack->txState = TX_STATE_UNINITIALIZED;
      ASSERT0("Unrecoverable protocol error.");
    }
  }
  else
  {
    /* Not expecting a link-response. */
    /* Just discard this symbol. */
    /* REMARK: Add status counter here??? */
  }
}



static void handleStartOfPacket(RioStack_t *stack)
{
  /* Check if a packet is already started. */
  if(stack->rxCounter != 0u)
  {
    /* Packet has already started. */
    /* This indicates an implicit end-of-packet symbol and signals the previous packet as ready. */

    /* Check if the packet is long enough to contain a complete packet. */
    if(stack->rxCounter > 3u)
    {
      /* Packet long enough to process. */
    
      /* Check the packet CRC. */
      if(stack->rxCrc == 0x0000u)
      {
        /* The packet has a correct CRC. */

        /* Process the newly received packet and start a new one. */
        handleNewPacketEnd(stack);
        handleNewPacketStart(stack);
      }
      else
      {
        /* The packet has an invalid CRC. */
        /* Packet error. Enter input-error-stopped state. */
        stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
        stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
        stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC;
        stack->statusInboundErrorPacketCrc++;
      }
    }
    else
    {
      /* The packet is too short. */
      /* Packet error. Enter input-error-stopped state. */
      stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
      stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
      stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_GENERAL;
      stack->statusInboundErrorGeneral++;
    }
  }
  else
  {
    /* Packet has not already started. */
    handleNewPacketStart(stack);
  }
}



static void handleEndOfPacket(RioStack_t *stack)
{
  /* Check if the packet is long enough to contain a complete packet. */
  if(stack->rxCounter > 3u)
  {
    /* Packet long enough to process. */
    
    /* Check if the CRC is correct. */
    if(stack->rxCrc == 0x0000u)
    {
      /* The packet has a correct CRC. */

      /* Process the newly received packet. */
      handleNewPacketEnd(stack);
    }
    else
    {
      /* The packet has an invalid CRC. */
      /* Packet error. Enter input-error-stopped state. */
      stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
      stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
      stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC;
      stack->statusInboundErrorPacketCrc++;
    }
  }
  else
  {
    /* The packet is too short. */
    /* Packet error. Enter input-error-stopped state. */
    stack->txState = TX_STATE_SEND_PACKET_NOT_ACCEPTED;
    stack->rxState = RX_STATE_INPUT_ERROR_STOPPED;
    stack->rxErrorCause = PACKET_NOT_ACCEPTED_CAUSE_GENERAL;
    stack->statusInboundErrorGeneral++;
  }
}



static void handleNewPacketStart(RioStack_t *stack)
{
  /* Check if there are buffers available to store the new frame. */
  if (queueAvailable(stack->rxQueue) > 0u)
  {
    /* There are buffers available to accept the new packet. */

    /* Update the reception counter to indicate the frame has started. */
    stack->rxCounter = 1u;
  }
  else
  {
    /* There are no buffers available. */
    /* Go to input retry stopped state. */
    stack->statusInboundPacketRetry++;
    stack->txState = TX_STATE_SEND_PACKET_RETRY;
    stack->rxState = RX_STATE_INPUT_RETRY_STOPPED;
  }
}



static void handleNewPacketEnd(RioStack_t *stack)
{
  /* Save the size of the packet. */
  queueBackSetSize(stack->rxQueue, (uint32_t)stack->rxCounter - (uint32_t)1ul);

  /* Always forward the packet to the top of the stack. */
  stack->rxQueue = queueEnqueue(stack->rxQueue);

  /* Make sure the CRC is reset to an invalid value to avoid a packet 
     accidentally being accepted. */
  stack->rxCrc = 0xffffu;

  /* Reset the reception counter. */
  stack->rxCounter = 0u;

  /* Update the ackId for the receiver. */
  stack->rxAckId = MASK_5BITS(stack->rxAckId + 1u);

  /* Update status counter. */
  stack->statusInboundPacketComplete++;
}



static void handleLinkRequest(RioStack_t *stack, LinkRequestCmd_t cmd)
{
  /* Check the command of the link-request. */
  if(cmd == LINK_REQUEST_INPUT_STATUS)
  {
    /* Input-status requested. */
    /* Return input port status. */

    /* Force the transmitter to send a link-response-symbol. */
    stack->txState = TX_STATE_SEND_LINK_RESPONSE;
  }
  else if(cmd == LINK_REQUEST_RESET_DEVICE)
  {
    /* Reset-device requested. */
    /* REMARK: Support this??? */
  }
  else
  {
    /* Unrecognized command. */
    /* Don't do anything. */
  }

  /* Always cancel an ongoing frame when a link-request has been received. */
  stack->rxCounter = 0u;

  /* Receiving this indicates the link partner having encountered a potential problem. */
  /* Count the number of times this happens. */
  stack->statusPartnerLinkRequest++;
}



/*
 * Create a 24-bit control symbol. The format is:
 * stype0(2:0)|parameter0(4:0)|parameter1(4:0)|stype1(2:0)|cmd(2:0)|crc5(4:0)
 * See RapidIO 3.1, part6, chapter 3 for more details.
 */
static RioSymbol_t createControlSymbol(const stype0_t stype0,
                                       const uint8_t parameter0, const uint8_t parameter1,
                                       const stype1_t stype1, const uint8_t cmd)
{
  RioSymbol_t s;

  s.type = RIOSTACK_SYMBOL_TYPE_CONTROL;

  s.data = ((uint32_t)stype0 & (uint32_t)0x07ul) << 21;
  s.data |= ((uint32_t)parameter0 & (uint32_t)0x1ful) << 16;
  s.data |= ((uint32_t)parameter1 & (uint32_t)0x1ful) << 11;
  s.data |= ((uint32_t)stype1 & (uint32_t)0x07ul) << 8;
  s.data |= ((uint32_t)cmd & (uint32_t)0x7ul) << 5;
  s.data |= crc5(s.data, 0x1fu);

  return s;
}



static uint8_t crc5(const uint32_t data, const uint8_t crc)
{
  static const uint8_t crcTable[] = {
    0x00u, 0x15u, 0x1fu, 0x0au, 0x0bu, 0x1eu, 0x14u, 0x01u,
    0x16u, 0x03u, 0x09u, 0x1cu, 0x1du, 0x08u, 0x02u, 0x17u,
    0x19u, 0x0cu, 0x06u, 0x13u, 0x12u, 0x07u, 0x0du, 0x18u,
    0x0fu, 0x1au, 0x10u, 0x05u, 0x04u, 0x11u, 0x1bu, 0x0eu
  };

  uint8_t result;
  uint8_t index;
  
  result = crc;
  index  = (uint8_t)((data >> 19) & (uint32_t)0x1ful) ^ result;
  result = crcTable[index];
  index  = (uint8_t)((data >> 14) & (uint32_t)0x1ful) ^ result;
  result = crcTable[index];
  index  = (uint8_t)((data >> 9) & (uint32_t)0x1ful) ^ result;
  result = crcTable[index];
  index  = (uint8_t)((data >> 4) & (uint32_t)0x1eul) ^ result;
  result = crcTable[index];

  return result;
}



static uint8_t getBufferStatus(const RioStack_t *stack)
{
  uint8_t status;


  status = queueAvailable(stack->rxQueue);
  if(status > 31u)
  {
    status = 31u;
  }

  return status;
}



/*******************************************************************************************
 * Internal queue functions.
 *******************************************************************************************/

static RioQueue_t queueCreate(const uint8_t size, uint32_t *buffer)
{
  RioQueue_t q;

  q.size = size;
  q.available = size;
  q.windowSize = 0u;
  q.windowIndex = 0u;
  q.frontIndex = 0u;
  q.backIndex = 0u;
  q.buffer_p = buffer;

  return q;
}



static uint8_t queueAvailable(const RioQueue_t q)
{
  return q.available;
}



static uint8_t queueEmpty(const RioQueue_t q)
{
  return (uint8_t) (q.available == q.size);
}



static uint8_t queueLength(const RioQueue_t q)
{
  return q.size - q.available;
}



static RioQueue_t queueEnqueue(RioQueue_t q)
{
  q.backIndex++;
  if(q.backIndex == q.size)
  {
    q.backIndex = 0u;
  }
  q.available--;
  return q;
}



static RioQueue_t queueDequeue(RioQueue_t q)
{
  q.frontIndex++;
  if(q.frontIndex == q.size)
  {
    q.frontIndex = 0u;
  }
  q.available++;
  if(q.windowSize == 0u)
  {
    q.windowIndex = q.frontIndex;
  }
  else
  {
    q.windowSize--;
  }
  return q;
}



static uint8_t queueWindowEmpty(const RioQueue_t q)
{
  return (uint8_t) ((q.available + q.windowSize) == q.size);
}



static RioQueue_t queueWindowReset(RioQueue_t q)
{
  q.windowIndex = q.frontIndex;
  q.windowSize = 0u;
  return q;
}



static RioQueue_t queueWindowNext(RioQueue_t q)
{
  q.windowIndex++;
  if(q.windowIndex == q.size)
  {
    q.windowIndex = 0u;
  }
  q.windowSize++;
  return q;
}



static void queueBackSetSize(RioQueue_t q, const uint32_t size)
{
  (q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.backIndex))[0u] = size; /*lint !e960 The buffer_p acts as an array of packets. */
}



static void queueBackSetContent(RioQueue_t q, const uint32_t index, const uint32_t content)
{
  (q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.backIndex))[index+1u] = content; /*lint !e960 The buffer_p acts as an array of packets. */
}



static uint32_t queueFrontGetSize(RioQueue_t q)
{
  return (q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.windowIndex))[0u]; /*lint !e960 The buffer_p acts as an array of packets. */
}



static uint32_t queueGetFrontContent(const RioQueue_t q, const uint32_t index)
{
  return (q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.windowIndex))[index+1u]; /*lint !e960 The buffer_p acts as an array of packets. */
}



static uint32_t *queueGetFrontBuffer(const RioQueue_t q )
{
  return &((q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.windowIndex))[1u]); /*lint !e960 The buffer_p acts as an array of packets. */
}



static uint32_t *queueGetBackBuffer(const RioQueue_t q )
{
  return &((q.buffer_p+(RIOSTACK_BUFFER_SIZE*q.backIndex))[1u]); /*lint !e960 The buffer_p acts as an array of packets. */
}
 
/*************************** end of file **************************************/
