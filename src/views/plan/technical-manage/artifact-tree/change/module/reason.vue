<template>
  <div class="reason-content" :style="heightStyle">
    <el-card style="height: 100%">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" class="reason-content-form" label-width="100px">
        <div style="flex: 1">
          <el-form-item label="变更原因" prop="changeReasonTypeEnum">
            <common-select
              v-model="form.changeReasonTypeEnum"
              :options="changeReasonTypeEnum.ENUM"
              type="enum"
              clearable
              placeholder="请选择变更原因"
            />
          </el-form-item>
          <el-form-item label="原因描述" prop="changeRemark">
            <el-input
              v-model.trim="form.changeRemark"
              type="textarea"
              :autosize="{ minRows: 6, maxRows: 10 }"
              :maxlength="600"
              placeholder="请填写原因描述"
              style="width: 100%"
            />
          </el-form-item>
        </div>
        <div class="divider"></div>
        <div style="flex: 1">
          <div class="title">变更附件上传</div>
          <upload-list
            show-download
            :file-classify="fileClassifyEnum.CHANGE_LIST_ATT.V"
            v-model:files="form.attachments"
            empty-text="暂未上传变更附件"
          />
        </div>
      </el-form>
    </el-card>
  </div>
</template>

<script setup>
import { defineProps, inject, defineExpose, ref } from 'vue'
import { fileClassifyEnum } from '@enum-ms/file'
import { changeReasonTypeEnum } from '@enum-ms/plan'
import UploadList from '@comp/file-upload/UploadList.vue'

defineProps({
  heightStyle: {
    type: String
  }
})

const form = inject('form')

const formRef = ref()
const rules = {
  changeReasonTypeEnum: [{ required: true, message: '请选择变更原因', trigger: 'change' }],
  changeRemark: [{ max: 200, message: '不能超过 200 个字符', trigger: 'blur' }]
}

// 验证
const validate = () => {
  return new Promise((resolve, reject) => {
    formRef.value.validate(valid => {
      if (valid) {
        resolve(valid)
      } else {
        reject()
      }
    })
  })
}

defineExpose({
  validate
})

</script>

<style lang="scss" scoped>
.reason-content {
  ::v-deep(.el-card__body) {
    height: 100%;
  }
}

.reason-content-form {
  display: flex;
  height: 100%;

  .divider {
    height: 100%;
    width: 1px;
    border-right: 1px dashed #ecedf0;
    margin: 0 30px;
  }

  .title {
    text-align: center;
    color: #606266;
    font-weight: bold;
    height: 48px;
    line-height: 48px;
    font-size: 18px;
  }
}
</style>
