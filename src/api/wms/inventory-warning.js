import request from '@/utils/request'

/**
 * 库存预警列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'material/inventory-warning',
    method: 'get',
    params
  })
}

/**
 * 新增物料库存预警
 *
 * @export
 * @param {Number} id id
 * @param {Number} minimumInventory 最低值
 * @returns
 */
export function batchAdd(data) {
  return request({
    module: 'wms',
    url: 'material/inventory-warning/batch',
    method: 'post',
    data
  })
}

/**
 *保存最低库存预警
 *
 * @export
 * @param {Number} id id
 * @param {Number} minimumInventory 最低值
 * @returns
 */
export function editMinimumInventory(data) {
  return request({
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: 'material/inventory-warning/minimumInventory',
    method: 'put',
    data
  })
}

/**
 * 修改启用状态
 * @export
 * @param {*} data
 * @returns
 */
export function editEnabled(data) {
  return request({
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `material/inventory-warning/enabled`,
    cancelKey: false,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'wms',
    url: 'material/inventory-warning',
    method: 'delete',
    data: ids
  })
}

export default { get, batchAdd, del }
