import request from '@/utils/request'

/**
 * 生产组列表
 */
export function get(params) {
  return request({
    url: '/api/mes/enclosure/groups/page',
    method: 'get',
    params
  })
}

/**
 * 新增生产组
 * @param {number} factoryId|required 工厂id
 * @param {number} productionLineId|required 生产线id
 * @param {number} workshopId|required 车间id
 * @param {string} name|required 名称
 */
export function add(data) {
  return request({
    url: '/api/mes/enclosure/groups',
    method: 'post',
    data
  })
}

/**
 * 编辑生产组
 * @param {number} id|required id
 * @param {number} factoryId|required 工厂id
 * @param {number} productionLineId|required 生产线id
 * @param {number} workshopId|required 车间id
 * @param {string} name|required 名称
 */
export function edit(data) {
  return request({
    url: '/api/mes/enclosure/groups',
    method: 'put',
    data
  })
}

/**
 * 删除生产组
 * @param {array} ids|required ids
 */
export function del(ids) {
  return request({
    url: '/api/mes/enclosure/groups',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
