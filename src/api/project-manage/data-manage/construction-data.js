import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `construction-data`,
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'project',
    url: `construction-data`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

export function del(data) {
  return request({
    module: 'project',
    url: `construction-data`,
    method: 'delete',
    data
  })
}
export default { add, get, del }
