import request from '@/utils/request'

/**
 *
 * 获取班组报表-结构
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'team_form/artifact_assemble',
    method: 'get',
    params
  })
}

/**
 *
 * 获取班组报表-结构-工序详情
 */
export function processDetail({ startDate, endDate, factoryId, processId, productType, productionLineId, projectId }) {
  return request({
    module: 'mes',
    url: 'team_form/artifact_assemble/details',
    method: 'get',
    params: { startDate, endDate, factoryId, processId, productType, productionLineId, projectId }
  })
}

/**
 *
 * 获取班组报表-结构-工序详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'team_form/artifact_assemble/process',
    method: 'get',
    params
  })
}

export default {
  get
}
