import { constantize } from '../base'

// 模型转换状态
const modelTranslateStatusEnum = {
  PROCESSING: { L: '正在转换', K: 'PROCESSING', V: 'processing', T: 'warning' },
  SUCCESS: { L: '转换成功', K: 'SUCCESS', V: 'success', T: 'success' },
  FAILED: { L: '转换失败', K: 'FAILED', V: 'failed', T: 'danger' },
  UPLOAD_PROCESSING: { L: '正在上传', K: 'UPLOAD_PROCESSING', V: 'uploadProcessing', T: 'warning' },
  UPLOAD_SUCCESS: { L: '未转换', K: 'UPLOAD_SUCCESS', V: 'uploadSuccess', T: 'success' },
  UPLOAD_FAILED: { L: '上传失败', K: 'UPLOAD_FAILED', V: 'uploadFailed', T: 'danger' }
}
constantize(modelTranslateStatusEnum)

// tekla常见版本
const bimTeklaEditionEnum = {
  SIXTEEN: { L: '16版本', K: 'SIXTEEN', V: 16 },
  SEVENTEEN: { L: '17版本', K: 'SEVENTEEN', V: 17 },
  EIGHTEEN: { L: '18版本', K: 'EIGHTEEN', V: 18 },
  NINETEEN: { L: '19版本', K: 'NINETEEN', V: 19 },
  TWENTY: { L: '20版本', K: 'TWENTY', V: 20 },
  TWENTY_ONE: { L: '21版本', K: 'TWENTY_ONE', V: 21 },
  TWENTY_TWO: { L: '22版本', K: 'TWENTY_TWO', V: 22 }
}
constantize(bimTeklaEditionEnum)

// 模型菜单
const modelMenuBarEnum = {
  PROJECT_TREE: { L: '项目树', K: 'PROJECT_TREE', V: 1, ICON: 'project-tree.png', COLORS: [] },
  COMPONENT_TREE: { L: '构件树', K: 'COMPONENT_TREE', V: 2, ICON: 'artifact-tree.png', COLORS: [
    { title: '无清单', color: '#999999', opacity: 1, value: 1 },
    { title: '有清单', color: '#f5f7fa', opacity: 1, value: 9 }
  ] },
  PRODUCTION_STATE: { L: '生产状态', K: 'PRODUCTION_STATE', ICON: 'production.png', V: 3, COLORS: [
    { title: '未生产', color: '#f5f7fa', opacity: 1, value: 2, qField: 'unProducedQuantity', wField: 'unProducedTotalGrossWeight' },
    { title: '在制品', color: '#ffba00', opacity: 1, value: 3, qField: 'producedQuantity', wField: 'producedTotalGrossWeight' },
    { title: '生产完毕', color: '#1682e6', opacity: 1, value: 4, qField: 'storageQuantity', wField: 'storageTotalGrossWeight' }
  ] },
  SHIPMENT_STATUS: { L: '发运状态', K: 'SHIPMENT_STATUS', ICON: 'shipment.png', V: 4, COLORS: [
    { title: '未入库', color: '#f5f7fa', opacity: 1, value: 5, qField: 'unProducedQuantity', wField: 'unProducedTotalGrossWeight' },
    { title: '已入库', color: '#ffba00', opacity: 1, value: 6, qField: 'storageQuantity', wField: 'storageTotalGrossWeight' },
    { title: '已发运', color: '#1682e6', opacity: 1, value: 7, qField: 'producedQuantity', wField: 'producedTotalGrossWeight' }
  ] },
  INSTALL_STATE: { L: '安装状态', K: 'INSTALL_STATE', V: 5, ICON: 'installation.png', COLORS: [
    { title: '未收货', color: '#f5f7fa', opacity: 1, value: 5, qField: 'unProducedQuantity', wField: 'unProducedTotalGrossWeight' },
    { title: '已收货', color: '#ffba00', opacity: 1, value: 6, qField: 'producedQuantity', wField: 'producedTotalGrossWeight' },
    { title: '已安装', color: '#32d3a6', opacity: 1, value: 7, qField: 'storageQuantity', wField: 'storageTotalGrossWeight' }
  ] }
}
constantize(modelMenuBarEnum)

export {
  modelTranslateStatusEnum,
  bimTeklaEditionEnum,
  modelMenuBarEnum
}

export default {
  modelTranslateStatusEnum, // 模型转换状态
  bimTeklaEditionEnum,
  modelMenuBarEnum
}
