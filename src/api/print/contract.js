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
 * 我的项目
 * @param {number} year 年份
 */
export function myProject({ year }) {
  return request({
    module: 'contract',
    url: 'project/listMy/print',
    method: 'get',
    params: { year }
  })
}

/**
 * 项目列表
 * @param {number} year 年份
 */
export function projectList({ year }) {
  return request({
    module: 'contract',
    url: 'project/listAllProject/print',
    method: 'get',
    params: { year }
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
    url: 'business/auxiliary-material/print',
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

export default {
  contractLedger, // 合同台账（合同登记表）
  collectionLedger, // 收款记录
  invoiceLedger, // 开票记录
  arrearsList, // 欠款清单
  myProject, // 我的项目
  projectList, // 项目列表
  structurePrice, // 结构计价表
  enclosurePrice, // 围护计价表
  auxiliaryMaterialPrice, // 配套件计价表
  collectionDetail, // 项目收款详情
  invoiceDetail, // 项目开票详情
  happenedDetail, // 项目发运详情
  transactionRecord // 客户交易记录
}
