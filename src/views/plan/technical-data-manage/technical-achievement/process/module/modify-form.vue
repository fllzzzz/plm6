<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="文件修改"
    :wrapper-closable="false"
    size="900px"
    custom-class="contract-change"
  >
    <template #titleRight>
      <common-button type="info" @click="versionVisible=true">修订版本</common-button>
      <common-button type="primary" v-loading="loading" size="small" @click="onSubmit">提交</common-button>
    </template>
    <template #content>
      <el-divider><span class="title">原文件信息</span></el-divider>
      <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="文件名称" :span="2">{{currentRow.fileName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="attachmentDTO" label="文件" :span="2">
          <template v-if="currentRow.attachmentDTO">
            <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(currentRow.attachmentDTO)">{{ currentRow.attachmentDTO.name }}</div>
          </template>
          <span v-else>-</span>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="processType" label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="boolSingleProject" label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="project" label="所属项目" :span="2">
          {{currentRow.project?projectNameFormatter(currentRow.project):'-'}}
        </el-descriptions-item>
         <el-descriptions-item label-class-name="userName" label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="uploadTime" label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
          {{currentRow.remark}}
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title">新文件信息</span></el-divider>
      <el-form ref="formRef" :model="form" :rules="rules" size="small">
      <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="*文件名称" :span="2">
          <el-input
            v-model="form.fileName"
            placeholder="名称"
            style="width: 100%;"
            size="small"
            maxlength="20"
            clearable
          />
        </el-descriptions-item>
        <el-descriptions-item label-class-name="attachmentDTO" label="*文件" :span="2">
         <template v-if="form.file">
            <span style="cursor: pointer; color: #409eff" @dblclick="updateAttachmentView(form)">{{ form.file }}</span>
          </template>
          <template v-else-if="form.attachmentDTO">
            <span style="cursor: pointer; color: #409eff" @dblclick="attachmentView(form.attachmentDTO)">{{ form.attachmentDTO.name }}</span>
          </template>
          <upload-btn ref="uploadRef" v-model:files="form.attachmentFiles" :file-classify="fileClassifyEnum.PLAN_ATT.V" :show-file-list="false" :limit="1" :accept="'.pdf'" @change="uploadFile" :btn-text="'变更文件'"/>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="processType" label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="boolSingleProject" label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="project" label="所属项目" :span="2">
          {{currentRow.project?projectNameFormatter(currentRow.project):'-'}}
        </el-descriptions-item>
         <el-descriptions-item label-class-name="userName" label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="uploadTime" label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
           <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 1, maxRows: 3 }"
            :maxlength="200"
            placeholder="备注"
            style="width:100%"/>
        </el-descriptions-item>
      </el-descriptions>
      </el-form>
      <historyVersion v-model="versionVisible" :currentRow="currentRow" />
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, ref, watch, nextTick } from 'vue'
import useVisible from '@compos/use-visible'

import { fileClassifyEnum } from '@enum-ms/file'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { ElMessage } from 'element-plus'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'

import historyVersion from './history-version'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  fileName: undefined,
  file: undefined,
  attachmentId: undefined,
  remark: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const versionVisible = ref(false)

const pdfShow = ref(false)
const currentId = ref()
const loading = ref(false)

watch(
  () => visible.value,
  (val) => {
    if (formRef.value) {
      formRef.value.resetFields()
    }
    form.value = JSON.parse(JSON.stringify(props.currentRow))
    if (formRef.value) {
      nextTick(() => {
        formRef.value.clearValidate()
      })
    }
  },
  { deep: true, immediate: true }
)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function updateAttachmentView(item) {
  currentId.value = item.attachmentId
  pdfShow.value = true
}

function uploadFile() {
  form.value.fileName = form.value.attachmentFiles[0].name.split('.')[0]
  form.value.file = form.value.attachmentFiles[0].name
  form.value.attachmentId = form.value.attachmentFiles[0].id
  form.value.attachmentFiles = []
}

function handleSuccess() {
  emit('success')
  handleClose()
}

async function onSubmit() {
  if (judgeSameValue(props.currentRow, form.value)) {
    ElMessage.error('未改动，请修改后提交')
    return
  }
  if (!form.value.fileName) {
    ElMessage.error('新文件文件名不能为空')
    return
  }
  loading.value = true
  try {
    await crudApi.edit({
      attachmentId: form.value.attachmentId,
      fileName: form.value.fileName,
      id: form.value.id,
      remark: form.value.remark
    })
    ElMessage.success('修改成功')
    handleSuccess()
  } catch (error) {
    console.log('提交失败', error)
  } finally {
    loading.value = false
  }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
