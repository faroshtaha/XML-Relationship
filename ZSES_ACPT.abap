DATA: lv_msg_guid TYPE sxmsmguid,
      msg_guid    TYPE bdsloio20-loio_id.
      
swc_get_element container 'msg_guid' msg_guid.
lv_msg_guid = object-key-msg_guid.

SUBMIT rsxmb_display_msg_vers_new
  WITH msgguid = lv_msg_guid
  WITH version = '000'
  WITH pipeline = 'SENDER'
  WITH kind = 'C'
  WITH msgmandt = sy-mandt
  WITH msgpid = 'SENDER'.
