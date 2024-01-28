import request from '@/utils/request'

/**
 * 合同台账（合同登记表）
 * @param {number} year 年份
 */
export function contractLedger(params) {
  return request({
    module: 'contract',
    url: 'project/pageProjectLedger/print',
    method: 'get',
    params
  })
}

/**
 * 收款台账/收款记录
 * @param {number} projectId 项目id
 * @param {number} startDate 开始时间
 * @param {number} endDate 结束时间
 */
export function collectionLedger(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/print',
    method: 'get',
    params
  })
}

// 项目台账/收款记录
export function collectionProject(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/project/print',
    method: 'get',
    params
  })
}

/**
 * 开票台账/开票记录
 * @param {number} projectId 项目id
 * @param {number} startDate 开始时间
 * @param {number} endDate 结束时间
 */
export function invoiceLedger(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/print',
    method: 'get',
    params
  })
}

// 收款台账开票记录
export function invoiceProject(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/project/print',
    method: 'get',
    params
  })
}

/**
 * 欠款清单
 * @param {number} projectId 项目id
 * @param {number} year 年份
 */
export function arrearsList(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/listCollectionWarning/print',
    method: 'get',
    params
  })
}

/**
 * 供应商付款/物流台账
 */
export function logisticsLedger(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/detail/print',
    method: 'get',
    params
  })
}

/**
 * 供应商付款/应付汇总
 */
export function payableSummary(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/payable/summary/print',
    method: 'get',
    params
  })
}

/**
 * 供应商付款/付款台账
 */
export function supplierPayableLedger(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/payment/ledger/print',
    method: 'get',
    params
  })
}

/**
 * 供应商付款/收票台账
 */
export function supplierInvoiceLedger(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/payment/ledger/invoice/print',
    method: 'get',
    params
  })
}

/**
 * 我的项目
 * @param {number} year 年份
 */
export function myProject(params) {
  return request({
    module: 'contract',
    url: 'project/listMy/print',
    method: 'get',
    params: params
  })
}

/**
 * 项目列表
 * @param {number} year 年份
 */
export function projectList(params) {
  return request({
    module: 'contract',
    url: 'project/listAllProject/print',
    method: 'get',
    params
  })
}

/**
 * 结构计价表
 */
export function structurePrice(params) {
  return request({
    module: 'contract',
    url: 'business/artifact/print',
    method: 'get',
    params
  })
}

/**
 * 散发制品计价表
 */
export function machinePartPrice(params) {
  return request({
    module: 'contract',
    url: 'business/machine-part/print',
    method: 'get',
    params
  })
}

/**
 * 散发制品详情打印
 */
export function machinePartDetail(businessId) {
  return request({
    module: 'contract',
    url: `business/machine-part/${businessId}/print`,
    method: 'get'
  })
}

/**
 * 围护件计价表
 */
export function enclosurePrice(params) {
  return request({
    module: 'contract',
    url: 'business/enclosure/print',
    method: 'get',
    params
  })
}

/**
 * 配套件计价表
 */
export function auxiliaryMaterialPrice(params) {
  return request({
    module: 'contract',
    url: 'business/standardPart/print',
    method: 'get',
    params
  })
}

/**
 * 项目收款详情
 */
export function collectionDetail(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/print',
    method: 'get',
    params
  })
}

/**
 * 项目开票详情
 */
export function invoiceDetail(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/print',
    method: 'get',
    params
  })
}

/**
 * 项目发运详情
 */
export function happenedDetail(params) {
  return request({
    module: 'contract',
    url: 'project/tracking/record/print',
    method: 'get',
    params
  })
}

/**
 * 客户交易记录
 */
export function transactionRecord(params) {
  return request({
    module: 'contract',
    url: 'business/check/detail/print',
    method: 'get',
    params
  })
}

/**
 * 订单跟踪列表
 */
export function saleOrderTracking(params) {
  return request({
    module: 'contract',
    url: 'project/tracking/print',
    method: 'get',
    params
  })
}

/**
 * 建刚发运跟踪
 */
export function shipmentTracking(params) {
  return request({
    module: 'contract',
    url: 'business/ship/detail/print',
    method: 'get',
    params
  })
}

/**
 * 桥梁发运跟踪
 */
export function bridgeShipmentTracking(params) {
  return request({
    module: 'contract',
    url: 'business/ship/bridge/detail/print',
    method: 'get',
    params
  })
}

/**
 * 入库记录
 */
export function warehouseRecord(params) {
  return request({
    url: '/api/mes/building/warehouse/list-record/print',
    method: 'get',
    params
  })
}

/**
 * 工业用电电费
 */
export function industryElectricRecord(params) {
  return request({
    url: '/api/contract/water-electricity/print',
    method: 'get',
    params
  })
}

/**
 * 民用用电电费
 */
export function civilElectricRecord(params) {
  return request({
    url: '/api/contract/water-electricity/print',
    method: 'get',
    params
  })
}

/**
 * 水费
 */
export function waterRecord(params) {
  return request({
    url: '/api/contract/water-electricity/print',
    method: 'get',
    params
  })
}

/**
 * 气体统计
 */
export function gasRecord(params) {
  return request({
    url: '/api/contract/gas-summary/print',
    method: 'get',
    params
  })
}

/**
 * 费用报销
 */
export function expenseReimburseList(params) {
  return request({
    url: '/api/contract/expense-reimburse/print',
    method: 'get',
    params
  })
}

/**
 * 管理人员工资
 */
export function managementSalaryList(params) {
  return request({
    url: '/api/contract/wage-summary/print',
    method: 'get',
    params
  })
}

/**
 * 生产人员工资
 */
export function productionSalaryList(params) {
  return request({
    url: '/api/contract/wage-summary/print',
    method: 'get',
    params
  })
}

/**
 * 物业费用
 */
export function propertyFeeList(params) {
  return request({
    url: '/api/contract/property-fee/print',
    method: 'get',
    params
  })
}

// 业财报表/主材费清单
export function mainMaterialList(params) {
  return request({
    url: '/api/contract/project-finance/list-outbound/print',
    method: 'get',
    params
  })
}
// 业财报表/人工费清单
export function manualList(params) {
  return request({
    url: '/api/contract/project-finance/list-wage/print',
    method: 'get',
    params
  })
}

// 业财报表/运输费
export function shippingFeeList(id) {
  return request({
    url: `/api/mes/building/cargo/shipment/list/print/${id}`,
    method: 'get'
  })
}

// 业财报表/检测费
export function testingFee(params) {
  return request({
    url: '/api/contract/project-finance/list-testing/print',
    method: 'get',
    params
  })
}

// 业财报表/分包费
export function subcontractFee(params) {
  return request({
    url: '/api/contract/project-finance/list-sub/print',
    method: 'get',
    params
  })
}

// 业财报表/项目管理费
export function projectManagementFee(params) {
  return request({
    url: ``,
    method: 'get',
    params
  })
}

// 业财报表/管理费
export function managementFee(params) {
  return request({
    url: `/api/contract/project-finance/list-manage/print`,
    method: 'get',
    params
  })
}

// 业财报表/水电费
export function waterElectricFee(params) {
  return request({
    url: `/api/contract/project-finance/list-water/print`,
    method: 'get',
    params
  })
}

// 业财报表/折旧费
export function depreciationFee(params) {
  return request({
    url: '/api/contract/project-finance/list-depreciation/print',
    method: 'get',
    params
  })
}

// 业财报表
export function fortuneReportList(params) {
  return request({
    url: '/api/contract/project-finance/print',
    method: 'get',
    params
  })
}

// 销售管理 分段制品计价表
export function contractBoxPrice(params) {
  return request({
    url: '/api/business/bridge-box/print',
    method: 'get',
    params
  })
}

// 废料台账-开票
export function scrapInvoice(params) {
  return request({
    url: '/api/contract/invoice/waste/print',
    method: 'get',
    params
  })
}

// 废料台账-收款
export function scrapCollection(params) {
  return request({
    url: '/api/contract/collection/waste/print',
    method: 'get',
    params
  })
}

// 废料台账-日期列表
export function scrapDate(params) {
  return request({
    url: '/api/contract/contractWaste/saleDetail/print',
    method: 'get',
    params
  })
}

// 废料台账-购买方列表
export function scrapPurchaser(params) {
  return request({
    url: '/api/contract/contractWaste/summary/ledger/print',
    method: 'get',
    params
  })
}

// 废料台账-累计出售额详情
export function totalScrapDetaile(params) {
  return request({
    url: '/api/contract/contractWaste/saleAmount/print',
    method: 'get',
    params
  })
}

export default {
  contractLedger, // 合同台账（合同登记表）
  collectionLedger, // 收款记录
  collectionProject, // 项目台账收款记录
  invoiceLedger, // 开票记录
  invoiceProject, // 项目台账开票记录
  arrearsList, // 欠款清单
  myProject, // 我的项目
  projectList, // 项目列表
  structurePrice, // 结构计价表
  enclosurePrice, // 围护计价表
  machinePartPrice, // 散发件计价表
  machinePartDetail, // 散发件详情打印
  auxiliaryMaterialPrice, // 配套件计价表
  logisticsLedger, // 物流台账
  payableSummary, // 应付汇总
  supplierPayableLedger, // 供应商付款台账
  supplierInvoiceLedger, // 供应商收票台账
  collectionDetail, // 项目收款详情
  invoiceDetail, // 项目开票详情
  happenedDetail, // 项目发运详情
  transactionRecord, // 客户交易记录
  saleOrderTracking, // 订单跟踪列表
  shipmentTracking, // 发运跟踪
  bridgeShipmentTracking, // 分段发运跟踪
  warehouseRecord, // 入库记录
  industryElectricRecord, // 工业用电电费
  civilElectricRecord, // 民用用电电费
  waterRecord, // 水费
  gasRecord, // 气体统计
  expenseReimburseList, // 费用报销
  managementSalaryList, // 管理人员工资清单
  productionSalaryList, // 生产人员工资
  propertyFeeList, // 物业费用清单

  mainMaterialList, // 主材费清单
  manualList, // 人工费
  shippingFeeList, // 运输费
  testingFee, // 检测费
  subcontractFee, // 分包费
  projectManagementFee, // 项目管理费
  managementFee, // 管理费
  waterElectricFee, // 水电费
  depreciationFee, // 折旧费
  fortuneReportList, // 业财报表
  contractBoxPrice, // 分段制品计价表
  scrapInvoice, // 废料开票记录
  totalScrapDetaile, // 废料台账累计出售额详情
  scrapCollection, // 废料收款记录
  scrapPurchaser, // 废料按购买方查
  scrapDate // 废料按日期查
}
