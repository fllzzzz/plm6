import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'intelligentworkshop/getdata',
    method: 'get',
    params
  })
}

export function detail(params) {
  return request({
    module: 'mes',
    url: 'intelligentworkshop/getProjectData',
    method: 'get',
    params
  })
}

export function summaryData(params) {
  return request({
    module: 'mes',
    url: 'intelligentworkshop/summarizedata',
    method: 'get',
    params
  })
}

export default { get }
