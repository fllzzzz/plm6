import request from '@/utils/request'

/**
 * 原材料退库申请列表
 *
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'return/application/review/raw-materials',
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
    url: `return/application/review/raw-materials/${id}`,
    method: 'get'
  })
}

/**
 * 获取待审核的ids
 */
export function getPendingReviewIdList() {
  return request({
    module: 'wms',
    url: `return/application/review/raw-materials/pending/ids`,
    method: 'get'
  })
}

/**
 * 审核退回
 * @param {*} id 详情id
 */
export function reviewReturned(data) {
  return request({
    module: 'wms',
    url: `return/application/review/raw-materials/returned`,
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
    url: `return/application/review/raw-materials/passed`,
    method: 'put',
    data
  })
}

export default { get, detail }
