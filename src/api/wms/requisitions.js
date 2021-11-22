import request from '@/utils/request'

/**
 * 申购单（简要）
 */
export function getUnclosedRequisitionsBrief() {
  return request({
    module: 'wms',
    url: 'requisitions/unclosed/brief',
    method: 'get'
  })
}
