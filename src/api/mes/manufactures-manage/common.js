import request from '@/utils/request'

/**
 *
 * 构件-出入库
 */
export function getBoardForArtifact(params) {
  return request({
    module: 'mes',
    url: 'warehouse/artifact/page',
    method: 'get',
    params
  })
}

/**
 *
 * 围护-出入库
 */
export function getBoardForEnclosure(params) {
  return request({
    module: 'mes',
    url: 'warehouse/enclosure/page',
    method: 'get',
    params
  })
}

/**
 *
 * 构件-出入库
 */
export function getBoardForArtifactSummary(params) {
  return request({
    module: 'mes',
    url: 'warehouse/artifact/summary',
    method: 'get',
    params
  })
}

/**
 *
 * 围护-出入库
 */
export function getBoardForEnclosureSummary(params) {
  return request({
    module: 'mes',
    url: 'warehouse/enclosure/summary',
    method: 'get',
    params
  })
}
