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

/**
 * 获取申购单详情
 */
export function getRequisitionsDetailBySN(sn) {
  return request({
    module: 'wms',
    url: `requisitions/serial-number/${sn}`,
    method: 'get'
  })
}
