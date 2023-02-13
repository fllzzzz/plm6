import request from '@/utils/request'

/**
 * 辅材费
 */
export function getAuxAnalysis(params) {
  return request({
    url: `/api/wms/outbound/application/use-classify`,
    method: 'get',
    params
  })
}

export function getDetail(params) {
  return request({
    url: `/api/wms/outbound/application/use-classify-detail`,
    method: 'get',
    params
  })
}

