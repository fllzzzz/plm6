import common from './common'
import wms from './wms'
import mes from './mes'

const WMS_IN_WAREHOUSE_VOUCHER = wms.valueFormat
const WMS_OUT_WAREHOUSE_RECORD_SUMMARY = wms.valueFormat
const WMS_RETURN_WAREHOUSE_VOUCHER = wms.valueFormat
const WMS_QUIT_WAREHOUSE_VOUCHER = wms.valueFormat
const WMS_MATERIAL_MOVE_HOUSE_VOUCHER = wms.valueFormat
const WMS_PREPARES_CUSTOM_SUMMARY = wms.preparesCustomSummary
const WMS_REPORT_IN_SUMMARY = wms.valueFormat
const WMS_REPORT_OUT_SUMMARY = wms.valueFormat
const WMS_REPORT_INVENTORY_SUMMARY = wms.valueFormat

const WMS_IN_RECORD_SUMMARY_DETAIL = wms.valueFormat
const WMS_OUT_RECORD_SUMMARY_DETAIL = wms.valueFormat

const PROJECT_MATERIAL_CONT = wms.checkUnitFormat
const PROJECT_MATERIAL_RETURN_CONT = wms.checkUnitFormat
const PROJECT_MATERIAL_COSTS_DETAIL = common.meteWithUnit

const WMS_INVENTORY_MANAGE_SUMMARY = common.filter
const WMS_INVENTORY_MANAGE_END_SUMMARY_DETAIL = common.meteWithUnit
const WMS_INVENTORY_MANAGE_INBOUND_SUMMARY_DETAIL = common.meteWithUnit
const WMS_INVENTORY_MANAGE_OUTBOUND_SUMMARY_DETAIL = common.meteWithUnit
const WMS_INVENTORY_MANAGE_BEGIN_SUMMARY = common.meteWithUnit
const WMS_INVENTORY_MANAGE_IN_SUMMARY = common.meteWithUnit
const WMS_INVENTORY_MANAGE_OUT_SUMMARY = common.meteWithUnit
const WMS_INVENTORY_MANAGE_END_SUMMARY = common.meteWithUnit

const WMS_LOGISTICS_DELIVERY = common.meteWithUnit
const WMS_LOGISTICS_ORDER_MATERIAL_DETAIL = common.meteWithUnit

const STEEL_MES_BUSINESS_TRIP_TRACKING = mes.materialType
const STEEL_MES_PACK = mes.materialType
const STEEL_MES_BUSINESS_OUTBOUND_PROJECT = mes.materialType
const STEEL_MES_BUSINESS_OUTBOUND_MONOMER = mes.materialType
const STEEL_MES_BUSINESS_INSTALLATION_MONOMER = mes.materialType

const STEEL_MES_REPORT_STEEL_DOSAGE = mes.steelDosageFormat

export default {
  WMS_IN_WAREHOUSE_VOUCHER,
  WMS_OUT_WAREHOUSE_RECORD_SUMMARY,
  WMS_RETURN_WAREHOUSE_VOUCHER,
  WMS_QUIT_WAREHOUSE_VOUCHER,
  WMS_MATERIAL_MOVE_HOUSE_VOUCHER,
  WMS_PREPARES_CUSTOM_SUMMARY,
  WMS_REPORT_IN_SUMMARY,
  WMS_REPORT_OUT_SUMMARY,
  WMS_REPORT_INVENTORY_SUMMARY,
  WMS_IN_RECORD_SUMMARY_DETAIL,
  WMS_OUT_RECORD_SUMMARY_DETAIL,
  PROJECT_MATERIAL_CONT,
  PROJECT_MATERIAL_RETURN_CONT,
  PROJECT_MATERIAL_COSTS_DETAIL,
  WMS_INVENTORY_MANAGE_END_SUMMARY_DETAIL,
  WMS_INVENTORY_MANAGE_INBOUND_SUMMARY_DETAIL,
  WMS_INVENTORY_MANAGE_OUTBOUND_SUMMARY_DETAIL,
  WMS_INVENTORY_MANAGE_BEGIN_SUMMARY,
  WMS_INVENTORY_MANAGE_IN_SUMMARY,
  WMS_INVENTORY_MANAGE_OUT_SUMMARY,
  WMS_INVENTORY_MANAGE_END_SUMMARY,
  WMS_LOGISTICS_DELIVERY,
  WMS_LOGISTICS_ORDER_MATERIAL_DETAIL,
  STEEL_MES_BUSINESS_TRIP_TRACKING,
  STEEL_MES_PACK,
  STEEL_MES_BUSINESS_OUTBOUND_PROJECT,
  STEEL_MES_BUSINESS_OUTBOUND_MONOMER,
  STEEL_MES_BUSINESS_INSTALLATION_MONOMER,
  STEEL_MES_REPORT_STEEL_DOSAGE,
  WMS_INVENTORY_MANAGE_SUMMARY
}
