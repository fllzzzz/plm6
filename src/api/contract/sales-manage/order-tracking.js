import request from '@/utils/request'

/**
 * 客户交易列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} businessType 商务类型
 * @param {number} endDate 结束时间
 * @param {number} startDate 开始时间
 * @param {number} projectId 项目id
 * @returns
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'project/tracking',
    method: 'get',
    params
  })
}

/**
 * 发运记录
 * @param {number} projectId|required 商务类型
 * @param {number} monomerId 单体id
 * @param {string} name 名称
 * @param {string} serialNumber 编号
 * @param {number} auditStartDate 发运开始时间
 * @param {number} auditEndDate 发运结束时间
 */
export function shipRecord(params) {
  return request({
    module: 'contract',
    url: `project/tracking/record`,
    method: 'get',
    params
  })
}

/**
 * 开票记录
 * @param {number} projectId|required 项目id
 */
export function invoiceRecord(params) {
  return request({
    module: 'contract',
    url: `contract/invoice/listPage`,
    method: 'get',
    params
  })
}

/**
 * 收款记录
 * @param {number} projectId|required 项目id
 */
export function collectionRecord(params) {
  return request({
    module: 'contract',
    url: `contract/collection/listPage`,
    method: 'get',
    params
  })
}

/**
 * 入库记录
 * @param {number} projectId|required 项目id
 */
export function warehouseRecord(params) {
  return request({
    url: `/api/mes/building/warehouse/list-record`,
    method: 'get',
    params
  })
}

export default { get }
