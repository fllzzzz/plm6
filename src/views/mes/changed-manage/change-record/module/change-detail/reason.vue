<template>
  <div class="reason-content" :style="heightStyle">
    <el-card style="height: 100%">
      <el-form ref="formRef" :model="detail" size="small" class="reason-content-form">
        <div style="flex: 1">
          <el-form-item label="变更原因" prop="changeReasonTypeEnum">
            <span v-parse-enum="{ e: changeReasonTypeEnum, v: detail.changeReasonTypeEnum }" />
          </el-form-item>
          <el-form-item label="原因描述" prop="changeRemark">
            <span>{{ detail.changeRemark }}</span>
          </el-form-item>
        </div>
        <div class="divider"></div>
        <div style="flex: 1">
          <div class="title">变更附件上传</div>
          <upload-list
            show-download
            :uploadable="false"
            :file-classify="fileClassifyEnum.CHANGE_LIST_ATT.V"
            v-model:files="detail.attachments"
            empty-text="暂未上传变更附件"
          />
        </div>
      </el-form>
    </el-card>
  </div>
</template>

<script setup>
import { defineProps, inject } from 'vue'
import { fileClassifyEnum } from '@enum-ms/file'
import { changeReasonTypeEnum } from '@enum-ms/plan'
import UploadList from '@comp/file-upload/UploadList.vue'

defineProps({
  heightStyle: {
    type: String
  }
})

const detail = inject('changeInfo')
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
