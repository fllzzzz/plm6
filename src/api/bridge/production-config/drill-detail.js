import request from '@/utils/request'

export function add(data) {
  return request({
    module: 'bridge',
    url: 'borehole/link',
    method: 'post',
    data: data
  })
}
export function editGet(data) {
  return request({
    module: 'bridge',
    url: 'borehole/link',
    method: 'put',
    data
  })
}
export function delGet(data) {
  return request({
    module: 'bridge',
    url: 'borehole/link',
    method: 'delete',
    data
  })
}

export default { add }
