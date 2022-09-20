import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: 'sub-visa',
    method: 'get',
    params
  })
}

export function visaAudit(data) {
  return request({
    module: 'project',
    url: 'sub-visa/check',
    method: 'put',
    data
  })
}

export function download(id) {
  return request({
    module: 'project',
    url: `sub-visa/print/${id}`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get }
