import request from '@/utils/request'

/**
 * @description:新增排产
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
 * @description:清空排产
 */
export function clear(data) {
  return request({
    module: 'mes',
    url: 'scheduling/clear',
    method: 'put',
    data
  })
}

