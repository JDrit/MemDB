/* Generated by the protocol buffer compiler.  DO NOT EDIT! */
/* Generated from: messages.proto */

/* Do not generate deprecated warnings for self */
#ifndef PROTOBUF_C__NO_DEPRECATED
#define PROTOBUF_C__NO_DEPRECATED
#endif

#include "messages.pb-c.h"
void   messages__get_request__init
                     (Messages__GetRequest         *message)
{
  static Messages__GetRequest init_value = MESSAGES__GET_REQUEST__INIT;
  *message = init_value;
}
size_t messages__get_request__get_packed_size
                     (const Messages__GetRequest *message)
{
  assert(message->base.descriptor == &messages__get_request__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__get_request__pack
                     (const Messages__GetRequest *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__get_request__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__get_request__pack_to_buffer
                     (const Messages__GetRequest *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__get_request__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__GetRequest *
       messages__get_request__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__GetRequest *)
     protobuf_c_message_unpack (&messages__get_request__descriptor,
                                allocator, len, data);
}
void   messages__get_request__free_unpacked
                     (Messages__GetRequest *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__get_request__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__get_response__init
                     (Messages__GetResponse         *message)
{
  static Messages__GetResponse init_value = MESSAGES__GET_RESPONSE__INIT;
  *message = init_value;
}
size_t messages__get_response__get_packed_size
                     (const Messages__GetResponse *message)
{
  assert(message->base.descriptor == &messages__get_response__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__get_response__pack
                     (const Messages__GetResponse *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__get_response__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__get_response__pack_to_buffer
                     (const Messages__GetResponse *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__get_response__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__GetResponse *
       messages__get_response__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__GetResponse *)
     protobuf_c_message_unpack (&messages__get_response__descriptor,
                                allocator, len, data);
}
void   messages__get_response__free_unpacked
                     (Messages__GetResponse *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__get_response__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__put_request__init
                     (Messages__PutRequest         *message)
{
  static Messages__PutRequest init_value = MESSAGES__PUT_REQUEST__INIT;
  *message = init_value;
}
size_t messages__put_request__get_packed_size
                     (const Messages__PutRequest *message)
{
  assert(message->base.descriptor == &messages__put_request__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__put_request__pack
                     (const Messages__PutRequest *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__put_request__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__put_request__pack_to_buffer
                     (const Messages__PutRequest *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__put_request__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__PutRequest *
       messages__put_request__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__PutRequest *)
     protobuf_c_message_unpack (&messages__put_request__descriptor,
                                allocator, len, data);
}
void   messages__put_request__free_unpacked
                     (Messages__PutRequest *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__put_request__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__put_response__init
                     (Messages__PutResponse         *message)
{
  static Messages__PutResponse init_value = MESSAGES__PUT_RESPONSE__INIT;
  *message = init_value;
}
size_t messages__put_response__get_packed_size
                     (const Messages__PutResponse *message)
{
  assert(message->base.descriptor == &messages__put_response__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__put_response__pack
                     (const Messages__PutResponse *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__put_response__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__put_response__pack_to_buffer
                     (const Messages__PutResponse *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__put_response__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__PutResponse *
       messages__put_response__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__PutResponse *)
     protobuf_c_message_unpack (&messages__put_response__descriptor,
                                allocator, len, data);
}
void   messages__put_response__free_unpacked
                     (Messages__PutResponse *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__put_response__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__remove_request__init
                     (Messages__RemoveRequest         *message)
{
  static Messages__RemoveRequest init_value = MESSAGES__REMOVE_REQUEST__INIT;
  *message = init_value;
}
size_t messages__remove_request__get_packed_size
                     (const Messages__RemoveRequest *message)
{
  assert(message->base.descriptor == &messages__remove_request__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__remove_request__pack
                     (const Messages__RemoveRequest *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__remove_request__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__remove_request__pack_to_buffer
                     (const Messages__RemoveRequest *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__remove_request__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__RemoveRequest *
       messages__remove_request__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__RemoveRequest *)
     protobuf_c_message_unpack (&messages__remove_request__descriptor,
                                allocator, len, data);
}
void   messages__remove_request__free_unpacked
                     (Messages__RemoveRequest *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__remove_request__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__remove_response__init
                     (Messages__RemoveResponse         *message)
{
  static Messages__RemoveResponse init_value = MESSAGES__REMOVE_RESPONSE__INIT;
  *message = init_value;
}
size_t messages__remove_response__get_packed_size
                     (const Messages__RemoveResponse *message)
{
  assert(message->base.descriptor == &messages__remove_response__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__remove_response__pack
                     (const Messages__RemoveResponse *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__remove_response__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__remove_response__pack_to_buffer
                     (const Messages__RemoveResponse *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__remove_response__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__RemoveResponse *
       messages__remove_response__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__RemoveResponse *)
     protobuf_c_message_unpack (&messages__remove_response__descriptor,
                                allocator, len, data);
}
void   messages__remove_response__free_unpacked
                     (Messages__RemoveResponse *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__remove_response__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__client_request__init
                     (Messages__ClientRequest         *message)
{
  static Messages__ClientRequest init_value = MESSAGES__CLIENT_REQUEST__INIT;
  *message = init_value;
}
size_t messages__client_request__get_packed_size
                     (const Messages__ClientRequest *message)
{
  assert(message->base.descriptor == &messages__client_request__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__client_request__pack
                     (const Messages__ClientRequest *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__client_request__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__client_request__pack_to_buffer
                     (const Messages__ClientRequest *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__client_request__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__ClientRequest *
       messages__client_request__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__ClientRequest *)
     protobuf_c_message_unpack (&messages__client_request__descriptor,
                                allocator, len, data);
}
void   messages__client_request__free_unpacked
                     (Messages__ClientRequest *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__client_request__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   messages__client_response__init
                     (Messages__ClientResponse         *message)
{
  static Messages__ClientResponse init_value = MESSAGES__CLIENT_RESPONSE__INIT;
  *message = init_value;
}
size_t messages__client_response__get_packed_size
                     (const Messages__ClientResponse *message)
{
  assert(message->base.descriptor == &messages__client_response__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t messages__client_response__pack
                     (const Messages__ClientResponse *message,
                      uint8_t       *out)
{
  assert(message->base.descriptor == &messages__client_response__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t messages__client_response__pack_to_buffer
                     (const Messages__ClientResponse *message,
                      ProtobufCBuffer *buffer)
{
  assert(message->base.descriptor == &messages__client_response__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
Messages__ClientResponse *
       messages__client_response__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (Messages__ClientResponse *)
     protobuf_c_message_unpack (&messages__client_response__descriptor,
                                allocator, len, data);
}
void   messages__client_response__free_unpacked
                     (Messages__ClientResponse *message,
                      ProtobufCAllocator *allocator)
{
  assert(message->base.descriptor == &messages__client_response__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
static const ProtobufCFieldDescriptor messages__get_request__field_descriptors[1] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__GetRequest, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__get_request__field_indices_by_name[] = {
  0,   /* field[0] = key */
};
static const ProtobufCIntRange messages__get_request__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 1 }
};
const ProtobufCMessageDescriptor messages__get_request__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.GetRequest",
  "GetRequest",
  "Messages__GetRequest",
  "messages",
  sizeof(Messages__GetRequest),
  1,
  messages__get_request__field_descriptors,
  messages__get_request__field_indices_by_name,
  1,  messages__get_request__number_ranges,
  (ProtobufCMessageInit) messages__get_request__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__get_response__field_descriptors[3] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__GetResponse, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "success",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_BOOL,
    0,   /* quantifier_offset */
    offsetof(Messages__GetResponse, success),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "value",
    3,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__GetResponse, value),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__get_response__field_indices_by_name[] = {
  0,   /* field[0] = key */
  1,   /* field[1] = success */
  2,   /* field[2] = value */
};
static const ProtobufCIntRange messages__get_response__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 3 }
};
const ProtobufCMessageDescriptor messages__get_response__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.GetResponse",
  "GetResponse",
  "Messages__GetResponse",
  "messages",
  sizeof(Messages__GetResponse),
  3,
  messages__get_response__field_descriptors,
  messages__get_response__field_indices_by_name,
  1,  messages__get_response__number_ranges,
  (ProtobufCMessageInit) messages__get_response__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__put_request__field_descriptors[2] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__PutRequest, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "value",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__PutRequest, value),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__put_request__field_indices_by_name[] = {
  0,   /* field[0] = key */
  1,   /* field[1] = value */
};
static const ProtobufCIntRange messages__put_request__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 2 }
};
const ProtobufCMessageDescriptor messages__put_request__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.PutRequest",
  "PutRequest",
  "Messages__PutRequest",
  "messages",
  sizeof(Messages__PutRequest),
  2,
  messages__put_request__field_descriptors,
  messages__put_request__field_indices_by_name,
  1,  messages__put_request__number_ranges,
  (ProtobufCMessageInit) messages__put_request__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__put_response__field_descriptors[2] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__PutResponse, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "success",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_BOOL,
    0,   /* quantifier_offset */
    offsetof(Messages__PutResponse, success),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__put_response__field_indices_by_name[] = {
  0,   /* field[0] = key */
  1,   /* field[1] = success */
};
static const ProtobufCIntRange messages__put_response__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 2 }
};
const ProtobufCMessageDescriptor messages__put_response__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.PutResponse",
  "PutResponse",
  "Messages__PutResponse",
  "messages",
  sizeof(Messages__PutResponse),
  2,
  messages__put_response__field_descriptors,
  messages__put_response__field_indices_by_name,
  1,  messages__put_response__number_ranges,
  (ProtobufCMessageInit) messages__put_response__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__remove_request__field_descriptors[1] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__RemoveRequest, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__remove_request__field_indices_by_name[] = {
  0,   /* field[0] = key */
};
static const ProtobufCIntRange messages__remove_request__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 1 }
};
const ProtobufCMessageDescriptor messages__remove_request__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.RemoveRequest",
  "RemoveRequest",
  "Messages__RemoveRequest",
  "messages",
  sizeof(Messages__RemoveRequest),
  1,
  messages__remove_request__field_descriptors,
  messages__remove_request__field_indices_by_name,
  1,  messages__remove_request__number_ranges,
  (ProtobufCMessageInit) messages__remove_request__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__remove_response__field_descriptors[2] =
{
  {
    "key",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_STRING,
    0,   /* quantifier_offset */
    offsetof(Messages__RemoveResponse, key),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "success",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_BOOL,
    0,   /* quantifier_offset */
    offsetof(Messages__RemoveResponse, success),
    NULL,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__remove_response__field_indices_by_name[] = {
  0,   /* field[0] = key */
  1,   /* field[1] = success */
};
static const ProtobufCIntRange messages__remove_response__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 2 }
};
const ProtobufCMessageDescriptor messages__remove_response__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.RemoveResponse",
  "RemoveResponse",
  "Messages__RemoveResponse",
  "messages",
  sizeof(Messages__RemoveResponse),
  2,
  messages__remove_response__field_descriptors,
  messages__remove_response__field_indices_by_name,
  1,  messages__remove_response__number_ranges,
  (ProtobufCMessageInit) messages__remove_response__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__client_request__field_descriptors[4] =
{
  {
    "type",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_ENUM,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientRequest, type),
    &messages__type__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "get",
    2,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientRequest, get),
    &messages__get_request__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "put",
    3,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientRequest, put),
    &messages__put_request__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "remove",
    4,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientRequest, remove),
    &messages__remove_request__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__client_request__field_indices_by_name[] = {
  1,   /* field[1] = get */
  2,   /* field[2] = put */
  3,   /* field[3] = remove */
  0,   /* field[0] = type */
};
static const ProtobufCIntRange messages__client_request__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 4 }
};
const ProtobufCMessageDescriptor messages__client_request__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.ClientRequest",
  "ClientRequest",
  "Messages__ClientRequest",
  "messages",
  sizeof(Messages__ClientRequest),
  4,
  messages__client_request__field_descriptors,
  messages__client_request__field_indices_by_name,
  1,  messages__client_request__number_ranges,
  (ProtobufCMessageInit) messages__client_request__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor messages__client_response__field_descriptors[4] =
{
  {
    "type",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_ENUM,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientResponse, type),
    &messages__type__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "get",
    2,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientResponse, get),
    &messages__get_response__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "put",
    3,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientResponse, put),
    &messages__put_response__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "remove",
    4,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_MESSAGE,
    0,   /* quantifier_offset */
    offsetof(Messages__ClientResponse, remove),
    &messages__remove_response__descriptor,
    NULL,
    0,             /* flags */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned messages__client_response__field_indices_by_name[] = {
  1,   /* field[1] = get */
  2,   /* field[2] = put */
  3,   /* field[3] = remove */
  0,   /* field[0] = type */
};
static const ProtobufCIntRange messages__client_response__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 4 }
};
const ProtobufCMessageDescriptor messages__client_response__descriptor =
{
  PROTOBUF_C__MESSAGE_DESCRIPTOR_MAGIC,
  "messages.ClientResponse",
  "ClientResponse",
  "Messages__ClientResponse",
  "messages",
  sizeof(Messages__ClientResponse),
  4,
  messages__client_response__field_descriptors,
  messages__client_response__field_indices_by_name,
  1,  messages__client_response__number_ranges,
  (ProtobufCMessageInit) messages__client_response__init,
  NULL,NULL,NULL    /* reserved[123] */
};
const ProtobufCEnumValue messages__type__enum_values_by_number[3] =
{
  { "GET", "MESSAGES__TYPE__GET", 1 },
  { "PUT", "MESSAGES__TYPE__PUT", 2 },
  { "REMOVE", "MESSAGES__TYPE__REMOVE", 3 },
};
static const ProtobufCIntRange messages__type__value_ranges[] = {
{1, 0},{0, 3}
};
const ProtobufCEnumValueIndex messages__type__enum_values_by_name[3] =
{
  { "GET", 0 },
  { "PUT", 1 },
  { "REMOVE", 2 },
};
const ProtobufCEnumDescriptor messages__type__descriptor =
{
  PROTOBUF_C__ENUM_DESCRIPTOR_MAGIC,
  "messages.Type",
  "Type",
  "Messages__Type",
  "messages",
  3,
  messages__type__enum_values_by_number,
  3,
  messages__type__enum_values_by_name,
  1,
  messages__type__value_ranges,
  NULL,NULL,NULL,NULL   /* reserved[1234] */
};
