import request from '@/utils/request'

/**
 *
 * 获取编外人员价格设置列表
 */
export function get({ areaName, monomerId, factoryId, leaderName, monomerName, processId, productType, projectId, serialNumber }) {
  return request({
    module: 'mes',
    url: 'wages/out_staff',
    method: 'get',
    params: { areaName, monomerId, factoryId, leaderName, monomerName, processId, productType, projectId, serialNumber }
  })
}

/**
 *
 * 调整编外人员价格设置列表
 */
export function edit(data) {
  return request({
    module: 'mes',
    url: 'wages/out_staff',
    method: 'put',
    data
  })
}

/**
 *
 * 审核工序价格列表
 */
export function checkList({ productType, status, page, size }) {
  return request({
    module: 'mes',
    url: 'wages/out_staff/auditList',
    method: 'get',
    params: { productType, status, page, size }
  })
}

/**
 *
 * 审核工序价格数量
 */
export function checkNumber({ areaName, monomerId, factoryId, leaderName, monomerName, processId, productType, projectId, serialNumber }) {
  return request({
    module: 'mes',
    url: 'wages/out_staff/auditList/count',
    method: 'get',
    params: { areaName, monomerId, factoryId, leaderName, monomerName, processId, productType, projectId, serialNumber }
  })
}

/**
 *
 * 审核工序价格
 */
export function check(data) {
  return request({
    module: 'mes',
    url: 'wages/out_staff/status',
    method: 'put',
    data
  })
}

export default {
  get, edit
}
