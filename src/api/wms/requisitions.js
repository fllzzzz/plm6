import request from '@/utils/request'

/**
 * 申购单（简要）
 */
export function getUnclosedRequisitionsBrief() {
  return request({
    module: 'wms',
    url: 'requisitions/unclosed/brief',
    method: 'get'
  })
}

/**
 * 获取申购单详情
 */
export function getRequisitionsDetailBySN(sn) {
  return request({
    module: 'wms',
    url: `requisitions/serial-number/${sn}`,
    method: 'get'
  })
}

/**
 * 申购单列表
 */
export function get() {
  return request({
    module: 'wms',
    url: 'requisitions',
    method: 'get'
  })
}

/**
 * 申购单详情
 * @param {number} id | required 获取申购单详情
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `requisitions/${id}`,
    method: 'get'
  })
}

/**
 * 添加申购单
 * @param {number} projectId 项目id
 * @param {string} purchaseNo 申购单号
 * @param {object} steelPlate 钢板
 * @param {object} sectionSteel 型钢
 * @param {object} steelCoil 钢卷
 * @param {object} enclosure 成品围护
 * @param {object} material 辅材
 */
export function add({ projectId, purchaseNo, steelPlate, sectionSteel, steelCoil, enclosure, material }) {
  return request({
    module: 'wms',
    url: 'requisitions',
    method: 'post',
    data: { projectId, purchaseNo, steelPlate, sectionSteel, steelCoil, enclosure, material }
  })
}

/**
 * 修改申购单
 * @param {number} id | required 申购单单id
 * @param {number} projectId 项目id
 * @param {string} purchaseNo 申购单号
 * @param {object} steelPlate 钢板
 * @param {object} sectionSteel 型钢
 * @param {object} steelCoil 钢卷
 * @param {object} enclosure 成品围护
 * @param {object} material 辅材
 */
export function edit({ id, projectId, purchaseNo, steelPlate, sectionSteel, steelCoil, enclosure, material }) {
  return request({
    module: 'wms',
    url: 'requisitions',
    method: 'put',
    data: { id, projectId, purchaseNo, steelPlate, sectionSteel, steelCoil, enclosure, material }
  })
}

/**
 * 修改申购单状态 【采购完毕/采购未完成】
 * @param {number} id | required 申购单id
 * @param {number} status | required 申购单状态【enum】
 */
export function editStatus(data) {
  return request({
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `requisitions/status`,
    method: 'put',
    data
  })
}

/**
 * 删除申购单单
 * @param {array} ids | required 申购单单id
 */
export function del(ids) {
  return request({
    module: 'wms',
    url: 'requisitions',
    method: 'delete',
    data: ids
  })
}

/**
 * 下载申购单单
 * @param {number} id  | required 申购单单id
 */
export function download(id) {
  return request({
    module: 'wms',
    url: `requisitions/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

/**
 * 申购单修改记录
 * @param {array} purchaseOrderId | required 申购单id
 * @param {array} purchaseOrderDetailId 申购单详情id
 * @param {array} page | required 页码
 * @param {array} size | required 每页数量
 */
export function log(params) {
  return request({
    module: 'wms',
    url: 'requisitions/edit/notes',
    method: 'get',
    params
  })
}

export default { get, add, edit, del, download }
