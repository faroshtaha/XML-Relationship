
*----------------------------------------------------------------------*
*       CLASS lcl_relation DEFINITION
*----------------------------------------------------------------------*
*   Declare Definition of class
*----------------------------------------------------------------------*
CLASS lcl_relation DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_related.
            INCLUDE TYPE sibflporb.
    TYPES:
        relation TYPE oblreltype,
      END   OF ty_related.

    TYPES:
      ty_lt_related TYPE STANDARD TABLE OF ty_related.

    METHODS:
      constructor
        IMPORTING
          obj_no   TYPE sibfboriid
          obj_type TYPE sibftypeid
          obj_cat  TYPE sibfcatid.
    METHODS:
      add_relation
        IMPORTING
          obj_no   TYPE sibfboriid
          obj_type TYPE sibftypeid
          obj_cat  TYPE sibfcatid
          relation TYPE oblreltype.
    METHODS:
      create_relations.

  PRIVATE SECTION.
    DATA:
      lt_related TYPE ty_lt_related,  "sibflporbt,
      lt_my_prop    TYPE sibflporb.

ENDCLASS.                    "lcl_relation DEFINITION

FUNCTION ZPTP_GOS_RELATIONSHIP .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOC_NO) TYPE  SIBFBORIID
*"     VALUE(IV_BUS_OBJ) TYPE  SIBFTYPEID
*"     VALUE(IV_OBJ_CAT) TYPE  SIBFCATID
*"     VALUE(IV_OBJ_NO) TYPE  SIBFBORIID
*"     VALUE(IV_OBJ_TYPE) TYPE  SIBFTYPEID
*"     VALUE(IV_RELATION) TYPE  OBLRELTYPE
*"  EXPORTING
*"     REFERENCE(EV_RETURN) TYPE  C
*"--------------------------------------------------------------------

  DATA: lo_relation TYPE REF TO lcl_relation.

* Sales Order as the Referent
  CREATE OBJECT lo_relation
    EXPORTING
      obj_no   = iv_doc_no         " Document number in which IDOC has to be attached
      obj_type = iv_bus_obj       " Business Object Type ( ex. - VTTK for Shipment )
      obj_cat  = iv_obj_cat.       " Business Object i.e 'BO'

* Add IDOC to Sales Order
  lo_relation->add_relation(
    obj_no   = iv_obj_no     " IDOC Number to be attached
    obj_type = iv_obj_type   " IDOC ( Type of Document being attached ex. 'IDOC' )
    obj_cat  = iv_obj_cat         " Business Object i.e 'BO'
    relation = iv_relation ).     " Type of IDOC being attached i.e 'IDC1' for inbound idoc and 'IDC0' for outbound
*
* Create Relations
  lo_relation->create_relations( ).
  " Get the status  whether the operation is successfull or not . i.e ev_return = 'S" for success and Blank for not success.
  GET PARAMETER ID 'GOS' FIELD ev_return."Get the value of lv_return from the memory id 'GOS' and assign it to exporting parameter ev_return .
ENDFUNCTION.

*----------------------------------------------------------------------*
*       CLASS lcl_relation IMPLEMENTATION
*----------------------------------------------------------------------*
*   Implementation of the class
*----------------------------------------------------------------------*
CLASS lcl_relation IMPLEMENTATION.
  METHOD constructor.
*   Set Properties of Referent
    me->lt_my_prop-instid = obj_no.
    me->lt_my_prop-typeid = obj_type.
    me->lt_my_prop-catid  = obj_cat.
  ENDMETHOD.                    "constructor
  METHOD add_relation.
*   Add Referenc
    FIELD-SYMBOLS: <lfs_relat> LIKE LINE OF me->lt_related.
    APPEND INITIAL LINE TO me->lt_related ASSIGNING <lfs_relat>.
    <lfs_relat>-instid = obj_no.
    <lfs_relat>-typeid = obj_type.
    <lfs_relat>-catid  = obj_cat.
    <lfs_relat>-relation = relation.
  ENDMETHOD.                    "add_relation
*
  METHOD create_relations.
    DATA: lwa_relate_key TYPE sibflporb.
    FIELD-SYMBOLS: <lfs_relat> LIKE LINE OF me->lt_related.
*
    DATA:
      ls_parent   TYPE borident,
      ls_related  TYPE borident,
      lv_relation TYPE binreltyp,
      lx_obl      TYPE REF TO cx_obl,
      lv_errstr   TYPE string,
      lv_done     TYPE flag,
      lv_return   TYPE c.
    CONSTANTS : c_x TYPE c VALUE 'X',
                c_s TYPE c VALUE 'S'.
*   for each relation
    LOOP AT me->lt_related ASSIGNING <lfs_relat>.
      MOVE-CORRESPONDING <lfs_relat> TO lwa_relate_key.
      TRY.
*      First try with new method to create the Link
          CALL METHOD cl_binary_relation=>create_link
            EXPORTING
              is_object_a = lt_my_prop
              is_object_b = lwa_relate_key
              ip_reltype  = <lfs_relat>-relation.

          lv_done = c_x.

*       If the Link can not be handled by this class, call the
*       FM to create the link

        CATCH cx_obl_model_error.

          ls_parent-objkey  = me->lt_my_prop-instid.
          ls_parent-objtype = me->lt_my_prop-typeid.
          ls_related-objkey  = <lfs_relat>-instid.
          ls_related-objtype = <lfs_relat>-typeid.
          lv_relation = <lfs_relat>-relation.

          CALL FUNCTION 'BINARY_RELATION_CREATE'
            EXPORTING
              obj_rolea      = ls_parent
              obj_roleb      = ls_related
              relationtype   = lv_relation
            EXCEPTIONS
              no_model       = 1
              internal_error = 2
              unknown        = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE c_s NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            lv_done = c_x.
          ENDIF.
        CATCH cx_obl INTO lx_obl.
          lv_errstr = lx_obl->get_text( ).
          MESSAGE lv_errstr TYPE c_s.
      ENDTRY.

      FREE MEMORY ID 'GOS'.
      IF lv_done = c_x.
        "Set the value of lv_return = 'S" which shows the operation is successful
        lv_return = c_s.
        "Set the value of lv_return to the memory id 'GOS' and read in the calling function
        SET PARAMETER ID 'GOS' FIELD lv_return.
      ENDIF.
    ENDLOOP.
    COMMIT WORK.
  ENDMETHOD.                    "create_relations
ENDCLASS.                    "lcl_relation IMPLEMENTATION
