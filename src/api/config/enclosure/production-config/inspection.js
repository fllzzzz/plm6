import request from '@/utils/request'

/**
 * 质检班组列表
 */
export function get(params) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam/page',
    method: 'get',
    params
  })
}

/**
 * 新增质检班组
 * @param {number} factoryId|required 工厂id
 * @param {array} inspectorIds|required 质检组员ids
 */
export function add(data) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam',
    method: 'post',
    data
  })
}

/**
 * 编辑质检班组
 * @param {number} id|required id
 * @param {number} factoryId|required 工厂id
 * @param {array} inspectorIds|required 质检组员ids
 */
export function edit(data) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam',
    method: 'put',
    data
  })
}

/**
 * 删除质检班组
 * @param {array} ids|required ids
 */
export function del(ids) {
  return request({
    url: '/api/mes/enclosure/inspectionTeam',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
