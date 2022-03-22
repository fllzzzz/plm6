import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'abnormal/page',
    method: 'get',
    params
  })
}

export function exceptionList(params) {
  return request({
    module: 'mes',
    url: 'abnormal/report',
    method: 'get',
    params
  })
}

export function taskList({ productId, productType }) {
  return request({
    module: 'mes',
    url: 'abnormal/task',
    method: 'get',
    params: { productId, productType }
  })
}

export function exceptionChange(data) {
  return request({
    module: 'mes',
    url: 'abnormal/handle/abnormal',
    method: 'put',
    data
  })
}

export function taskChange(data) {
  return request({
    module: 'mes',
    url: 'abnormal/handle/task',
    method: 'put',
    data
  })
}

export function changePreview(data) {
  return request({
    module: 'mes',
    url: 'abnormal/handle/preview',
    method: 'put',
    data
  })
}

export function detailPreview(params) {
  return request({
    module: 'mes',
    url: 'abnormal/preview',
    method: 'get',
    params
  })
}

export function detail(params) {
  return request({
    module: 'mes',
    url: 'abnormal/change/report',
    method: 'get',
    params
  })
}

export function productInfo(params) {
  return request({
    module: 'mes',
    url: 'abnormal/product/info',
    method: 'get',
    params
  })
}

export default { get }
