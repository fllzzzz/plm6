import request from '@/utils/externalRequest'

export function fetchArtifact(url, { id, factoryId, taskId }) {
  return request({
    url: `${url}/api/plan/qr/info/artifact/${id}`,
    method: 'get',
    params: { id, factoryId, taskId }
  })
}

export function fetchEnclosure(url, { id, factoryId, taskId }) {
  return request({
    url: `${url}/api/plan/qr/info/enclosure/${id}`,
    method: 'get',
    params: { id, factoryId, taskId }
  })
}

export function fetchAuxiliaryMaterial(url, { id, taskId }) {
  return request({
    url: `${url}/api/qr/info/mes/auxiliaryMaterial`,
    method: 'get',
    params: { id, taskId }
  })
}

export function fetchBox(url, { id, factoryId, taskId }) {
  return request({
    url: `${url}/api/bridge/qr/info/box/task`,
    method: 'get',
    params: { id, factoryId, taskId }
  })
}

export function fetchElement(url, { id, factoryId, taskId }) {
  return request({
    url: `${url}/api/bridge/qr/info/element/task`,
    method: 'get',
    params: { id, factoryId, taskId }
  })
}

export function fetchBridgeAuxiliaryMaterial(url, { id, factoryId, taskId }) {
  return request({
    url: `${url}/api/bridge/qr/info/auxiliaryMaterial`,
    method: 'get',
    params: { id, factoryId, taskId }
  })
}
