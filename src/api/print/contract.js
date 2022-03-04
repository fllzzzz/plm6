import request from '@/utils/request'

/**
 * 合同台账（合同登记表）
 * @param {number} year 年份
 */
export function contractLedger({ year }) {
  return request({
    module: 'contract',
    url: 'project/listAllProject/print',
    method: 'get',
    params: { year }
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
  myProject, // 我的项目
  structurePrice, // 结构计价表
  enclosurePrice, // 围护计价表
  auxiliaryMaterialPrice, // 配套件计价表
  collectionDetail, // 项目收款详情
  invoiceDetail, // 项目开票详情
  happenedDetail, // 项目发运详情
  transactionRecord // 客户交易记录
}
