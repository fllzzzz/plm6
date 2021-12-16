import request from '@/utils/request'

/**
 *
 * 获取工序价格
 */
export function get({ monomerId, productType, projectId }) {
  return request({
    module: 'mes',
    url: 'wages',
    method: 'get',
    params: { monomerId, productType, projectId }
  })
}

/**
 *
 * 修改工序价格
 */
export function edit({ monomerId, price, processId, productProcessId, productType, projectId }) {
  return request({
    module: 'mes',
    url: 'wages',
    method: 'put',
    data: { monomerId, price, processId, productProcessId, productType, projectId }
  })
}

/**
 *
 * 审核工序价格列表
 */
export function checkList({ monomerId, processId, projectId, status, page, size }) {
  return request({
    module: 'mes',
    url: 'wages/audit/list',
    method: 'get',
    params: { monomerId, processId, projectId, status, page, size }
  })
}

/**
 *
 * 审核工序价格数量
 */
export function checkNumber({ monomerId, processId, projectId }) {
  return request({
    module: 'mes',
    url: 'wages/audit/number',
    method: 'get',
    params: { monomerId, processId, projectId }
  })
}

/**
 *
 * 审核工序价格
 */
export function check(data) {
  return request({
    module: 'mes',
    url: 'wages/status',
    method: 'put',
    data
  })
}

export default {
  get, edit
}
