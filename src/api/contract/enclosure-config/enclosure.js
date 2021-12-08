import request from '@/utils/request'
import { TechnologyTypeEnum } from '@enum-ms/contract'

export function getDicts(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dict',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dict',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: `enclosure/dictionaries/dictDetailSave/type/${TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V}`,
    method: 'post',
    data
  })
}

export function detailDict(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetail/all',
    method: 'get',
    params
  })
}

export function editStatus({ type, data }) {
  return request({
    module: 'contract',
    url: `enclosure/dictionaries/dictDetailSave/type/${type}`,
    method: 'post',
    data: data
  })
}

export default { get, add }
