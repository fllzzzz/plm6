import request from '@/utils/request'

/**
 * 获取可退库的入库清单
 *
 * @export
 * @returns
 */
export function getInboundListForRejectable(params) {
  return request({
    module: 'wms',
    url: 'reject/inbound-rejectable',
    method: 'get',
    params
  })
}

/**
 * 获取可退库的入库清单详情
 *
 * @export
 * @returns
 */
export function getInboundDetail(id) {
  return request({
    module: 'wms',
    url: `reject/inbound-rejectable/${id}`,
    method: 'get'
  })
}

// 退货物料匹配列表
export function getMatchList(id) {
  return request({
    module: 'wms',
    url: `reject/inbound-rejectable/material-match/${id}`,
    method: 'get'
  })
}

// 退货信息提交
export function rejectApplication(data) {
  return request({
    module: 'wms',
    url: `reject/inbound-rejectable`,
    method: 'post',
    data
  })
}

export default {
  get: getInboundListForRejectable,
  detail: getInboundDetail
}
