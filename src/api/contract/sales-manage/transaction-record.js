import request from '@/utils/request'

/**
 * 客户交易列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} businessType 商务类型
 * @param {number} endDate 结束时间
 * @param {number} startDate 开始时间
 * @param {string} name 客户名称
 * @returns
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'business/check/summary',
    method: 'get',
    params
  })
}

/**
 * 交易详情
 * @param {number} businessType|required 商务类型
 * @param {string} name|required 客户名称
 * @param {number} endDate 结束时间
 * @param {number} startDate 开始时间
 */
export function detail(params) {
  return request({
    module: 'contract',
    url: `business/check/detail`,
    method: 'get',
    params
  })
}

export default { get, detail }
