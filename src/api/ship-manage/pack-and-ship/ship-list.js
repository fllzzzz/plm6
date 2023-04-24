import request from '@/utils/request'

/**
 * 发运记录-列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} projectId 项目id
 * @param {number} auditEndDate 发运结束时间
 * @param {number} auditStartDate 发运开始时间
 * @param {number} auditUserName 办理人(发运人)
 * @param {number} blurry 名称编号
 * @param {string} driverName 司机
 * @param {string} licensePlate 车牌
 * @param {string} manufactureType 制造类型
 * @param {string} productType 装载材料类型
 * @param {string} serialNumber 车次编号
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'cargo/ship',
    method: 'get',
    params
  })
}

/**
 * 发运记录-发运量汇总
 */
export function getSummaryShipMete(params) {
  return request({
    module: 'mes',
    url: 'cargo/ship/summary',
    method: 'get',
    params
  })
}
/**
 * 当年/当月发运记录汇总
 */
export function getSummaryMonthMete(params) {
  return request({
    module: 'mes',
    url: 'cargo/yearAndMonth/summary',
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
    url: `api/mes/ship/${id}/export`,
    method: 'get',
    responseType: 'blob'
    // params: { id }
  })
}

/**
 * 下载物流汇总信息
 */
export function downloadLogistics(params) {
  return request({
    url: `api/mes/ship/logistics/export`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

/**
 * 发运记录详情
 * @param {number} id
 */
export function detail(id) {
  return request({
    module: 'mes',
    url: `cargo/ship/${id}`,
    method: 'get'
  })
}

export default { get, download }
