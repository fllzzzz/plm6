
import request from '@/utils/request'

/**
 *
 * 生产跟踪/构件
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/process/artifact/process/track',
    method: 'get',
    params
  })
}

/**
 *
 * 生产跟踪/部件
 */
export function assembleTrack(params) {
  return request({
    module: 'mes',
    url: 'task/process/assemble/process/track',
    method: 'get',
    params
  })
}

export default { get }

