import request from '@/utils/request'

/**
 * @description:新增工单
 */
export function save(data) {
  return request({
    module: 'mes',
    url: 'scheduling',
    method: 'post',
    data
  })
}

/**
 * @description:清空工单
 */
export function clear(data) {
  return request({
    module: 'mes',
    url: 'scheduling/clear',
    method: 'put',
    data
  })
}

