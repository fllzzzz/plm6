import request from '@/utils/request'

export function enclosureListChange(data) {
  return request({
    url: '/api/enclosure/change',
    method: 'post',
    data
  })
}
