import request from '@/utils/request'

/**
 * 原材料调拨申请列表
 *
 * @param {String} serialNumber 调拨单号
 * @param {Array} createTime  创建时间
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'transfer/application/review/raw-materials',
    method: 'get',
    params
  })
}

/**
 * 调拨详情
 * @param {*} id 详情id
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `transfer/application/review/raw-materials/${id}`,
    method: 'get'
  })
}

/**
 * 获取待审核的ids
 */
export function getPendingReviewIdList(params) {
  return request({
    module: 'wms',
    url: `transfer/application/review/raw-materials/pending/ids`,
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
    url: `transfer/application/review/raw-materials/returned`,
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
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
    url: `transfer/application/review/raw-materials/passed`,
    method: 'put',
    data
  })
}

export default { get, detail }
