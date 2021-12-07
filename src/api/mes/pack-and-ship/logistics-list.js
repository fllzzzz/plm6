import request from '@/utils/request'

/**
 * 物流记录
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'cargo/logistics',
    method: 'get',
    params
  })
}

export default { get }

/**
 * 分页获取物流单位列表
 */
export function getLogisticsPrice(params) {
  return request({
    module: 'mes',
    url: 'supper/price',
    method: 'get',
    params
  })
}

/**
 * 新增物流单位
 */
export function addLogisticsPrice(data) {
  return request({
    module: 'mes',
    url: 'supper/price',
    method: 'post',
    data
  })
}

/**
 * 编辑物流单位
 */
export function editLogisticsPrice(data) {
  return request({
    module: 'mes',
    url: 'supper/price',
    method: 'put',
    data
  })
}

/**
 * 删除物流单位
 */
export function delLogisticsPrice(id) {
  return request({
    module: 'mes',
    url: `supper/price/${id}`,
    method: 'delete'
  })
}

export const logisticsPrice = {
  get: getLogisticsPrice,
  add: addLogisticsPrice,
  edit: editLogisticsPrice,
  del: delLogisticsPrice
}

