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

export { modelTranslateStatusEnum, bimTeklaEditionEnum }

export default {
  modelTranslateStatusEnum, bimTeklaEditionEnum // 模型转换状态
}
