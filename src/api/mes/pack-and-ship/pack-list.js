import request from '@/utils/request'

/**
 * 打包记录 / 打印标签列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} materialType 材料类型
 * @param {number} projectId 项目id
 * @param {number} bagSerial 包单号
 * @param {string} userName 打包人名称
 * @param {string} startDate 开始时间
 * @param {string} endDate 结束时间
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'package/page',
    method: 'get',
    params
  })
}

// 打包记录：围护
export function getEnclosure(params) {
  return request({
    url: '/api/mes/enclosure/package/page',
    method: 'get',
    params
  })
}

/**
 * 打包详情
 * @param {number} id|required 包id
 */
export function detail(id) {
  return request({
    module: 'mes',
    url: `package/${id}`,
    method: 'get'
  })
}

/**
 * 围护：打包详情
 * @param {number} id|required 包id
 */
export function enclosureDetail(id) {
  return request({
    url: `/api/mes/enclosure/package/${id}`,
    method: 'get'
  })
}

/**
 * 打包详情
 * @param {number} id|required 包id
 */
export function del(id) {
  return request({
    module: 'mes',
    url: 'package',
    method: 'delete',
    data: { ids: [id] }
  })
}

/**
 * 围护：打包详情
 * @param {number} id|required 包id
 */
export function enclosureDel(id) {
  return request({
    url: '/api/mes/enclosure/package',
    method: 'delete',
    data: { ids: [id] }
  })
}

export default { get, del }
