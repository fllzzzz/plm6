import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/enclosure/entities',
    method: 'get',
    params
  })
}

export function enclosureChangeReview(data) {
  return request({
    url: '/api/enclosure/examine',
    method: 'post',
    data
  })
}

// export function enclosureChangeReview(data) {
//   return request({
//     url: '/api/enclosure/examine',
//     method: 'post',
//     data
//   })
// }

export default { get }
