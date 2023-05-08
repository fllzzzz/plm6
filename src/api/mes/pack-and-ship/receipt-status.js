import request from '@/utils/request'

/**
 * 收货状态-列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} projectId 项目id
 * @param {number} receiptStatus 收货状态
 * @param {number} serialNumber 车次
 * @param {string} licensePlate 车牌
 * @param {string} productType 装载材料类型
 * @param {string} manufactureType 制造类型
 * @param {string} driverName 司机
 * @param {string} auditUserName 发运人
 * @param {string} auditReceiptName 实际收货人
 * @param {string} receiptName 收货人
 * @param {number} auditStartDate 发运开始时间
 * @param {number} auditEndDate 发运结束时间
 * @param {number} auditReceiptStartDate 收货开始时间
 * @param {number} auditReceiptEndDate 结束结束时间
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'cargo/receipt',
    method: 'get',
    params
  })
}

// 围护： 收货状态-列表
export function getEnclosure(params) {
  return request({
    url: '/api/mes/enclosure/cargo/receipt',
    method: 'get',
    params
  })
}

/**
 * 下载物流详单
 * @param {number} id|required 物流信息id
 */
export function download({ id }) {
  return request({
    url: `api/mes/receipt/status/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

/**
 * 下载物流汇总信息
 */
export function downloadLogistics(params) {
  return request({
    url: `api/mes/receipt/status/export`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

/**
 * 收货状态-详情
 */
export function detail(id) {
  return request({
    module: 'mes',
    url: `cargo/receipt/${id}`,
    method: 'get'
  })
}

// 取消送货
export function deliveryCancel(data) {
  return request({
    module: 'mes',
    url: 'cargo/cancel',
    method: 'put',
    data
  })
}

// 到场签收
export function deliverySign(id) {
  return request({
    module: 'mes',
    url: `cargo/sign/${id}`,
    method: 'put'
  })
}
export default { get, download }
