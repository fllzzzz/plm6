import request from '@/utils/request'

/**
 * 班组列表（分页）
 */
export function get(params) {
  return request({
    url: '/api/mes/enclosure/team/page',
    method: 'get',
    params
  })
}

/**
 * 新增班组
 * @param {number} factoryId|required 工厂id
 * @param {number} leaderId|required 组长id
 * @param {array} memberIds|required 组员ids
 */
export function add(data) {
  return request({
    url: '/api/mes/enclosure/team',
    method: 'post',
    data
  })
}

/**
 * 编辑班组
 * @param {number} id|required id
 * @param {number} factoryId|required 工厂id
 * @param {number} leaderId|required 组长id
 * @param {array} memberIds|required 组员ids
 */
export function edit(data) {
  return request({
    url: '/api/mes/enclosure/team',
    method: 'put',
    data
  })
}

/**
 * 删除班组
 * @param {array} ids|required ids
 */
export function del(ids) {
  return request({
    url: '/api/mes/enclosure/team',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
