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

export { modelTranslateStatusEnum }

export default {
  modelTranslateStatusEnum // 模型转换状态
}
