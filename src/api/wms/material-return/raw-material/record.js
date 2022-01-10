import request from '@/utils/request'

/**
 * 原材料退库申请列表
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'return/application/record/raw-materials',
    method: 'get',
    params
  })
}

/**
 *修改订单
 *
 * @export
 * @returns
 */
export function edit(data) {
  return request({
    module: 'wms',
    url: 'return/application/record/raw-materials',
    method: 'put',
    data
  })
}

/**
 *删除订单
 *
 * @export
 * @param {Array} ids 要删除的id集合
 * @returns
 */
export function del(ids) {
  return request({
    module: 'wms',
    url: 'return/application/record/raw-materials',
    method: 'delete',
    data: ids
  })
}

/**
 * 订单详情
 * @param {*} id 详情id
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `return/application/record/raw-materials/${id}`,
    method: 'get'
  })
}

export default { get, edit, del, detail }
