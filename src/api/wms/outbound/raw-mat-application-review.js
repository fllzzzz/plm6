import request from '@/utils/request'

/**
 * 获取出库清单列表
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'outbound/application/review/raw-materials',
    method: 'get',
    params
  })
}

/**
 * 获取出库清单详情
 * @returns
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `outbound/application/review/raw-materials/${id}`,
    method: 'get'
  })
}

/**
 * 获取待审核的ids
 */
export function getPendingReviewIdList() {
  return request({
    module: 'wms',
    url: `outbound/application/review/raw-materials/pending/ids`,
    method: 'get'
  })
}

/**
 * 删除/清空 出库单（审核退回）
 * @returns
 */
export function reviewReturned(id) {
  return request({
    module: 'wms',
    url: `outbound/application/review/raw-materials/${id}/returned`,
    method: 'put'
  })
}

/**
 * 出库单确认出库（审核通过）
 * @returns
 */
export function reviewPassed(id) {
  return request({
    module: 'wms',
    url: `outbound/application/review/raw-materials/${id}/passed`,
    method: 'put'
  })
}

/**
 * 获取当前用户的出库单
 * @returns
 */
export function getDetailByCurrentUser() {
  return request({
    module: 'wms',
    url: 'outbound/application/review/raw-materials/current-user',
    method: 'get'
  })
}

/**
 * 获取当前用户的出库单的中记录的数量
 * @returns
 */
export function getDetailNumberByCurrentUser() {
  return request({
    module: 'wms',
    url: 'outbound/application/review/raw-materials/current-user/detail-number',
    method: 'get'
  })
}

/**
 * 删除出库清单中的物料
 * @param {object} listId 出库清单id, materialId 单条物料id
 * @returns
 */
export function delMaterial(data) {
  return request({
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `outbound/application/review/raw-materials/material-del`,
    method: 'put',
    data
  })
}

// 清空出库单
export function clear(id) {
  return request({
    module: 'wms',
    url: `outbound/application/review/raw-materials/${id}/clear`,
    method: 'put'
  })
}

export default { get }
