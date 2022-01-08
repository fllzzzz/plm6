import request from '@/utils/request'

/**
 * 原材料退货申请列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'reject/application/review/raw-materials',
    method: 'get',
    params
  })
}

/**
 * 订单详情
 * @param {*} id 详情id
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `reject/application/review/raw-materials/${id}`,
    method: 'get'
  })
}

/**
 * 获取待审核的ids
 */
export function getPendingReviewIdList(params) {
  return request({
    module: 'wms',
    url: `reject/application/review/raw-materials/pending/ids`,
    method: 'get',
    params
  })
}

/**
 * 审核退回
 * @param {*} id 详情id
 */
export function reviewReturned(data) {
  return request({
    module: 'wms',
    url: `reject/application/review/raw-materials/returned`,
    method: 'put',
    data
  })
}

/**
 * 审核通过
 * @param {*} id 详情id
 */
export function reviewPassed(data) {
  return request({
    module: 'wms',
    url: `reject/application/review/raw-materials/passed`,
    method: 'put',
    data
  })
}

export default { get, detail }
