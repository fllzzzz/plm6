import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetail/all',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetailSave/type/3',
    method: 'post',
    data
  })
}

export default { get, edit }
