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
      <common-button type="info" v-permission="permission.detail" @click="versionVisible=true">修订版本</common-button>
      <common-button type="primary" v-loading="loading" size="small" @click="onSubmit">提交</common-button>
    </template>
    <template #content>
      <el-divider><span class="title">原文件信息</span></el-divider>
      <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label="文件名称" :span="2">{{currentRow.fileName}}</el-descriptions-item>
        <el-descriptions-item label="文件" :span="2">
          <template v-if="currentRow.attachmentDTO">
            <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(currentRow.attachmentDTO)">{{ currentRow.attachmentDTO.name }}</div>
          </template>
          <span v-else>-</span>
        </el-descriptions-item>
        <el-descriptions-item label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label="所属项目" :span="2">
           <el-row>
            <template v-if="isNotBlank(currentRow.projectList && currentRow.boolSingleProject)">
              <el-col v-for="item in currentRow.projectList" :key="item.id" :span="12">
                {{projectNameFormatter(item)}}
              </el-col>
            </template>
            <template v-else>-</template>
          </el-row>
        </el-descriptions-item>
         <el-descriptions-item label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d} {h}:{i}:{s}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label="备注">
          <div style="word-break:break-all;width:100%;">{{currentRow.remark}}</div>
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title" style="background-color:#e6a23c;">新文件信息</span></el-divider>
      <el-form ref="formRef" :model="form" :rules="rules" size="small">
      <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label-class-name="modify-content" label="*文件名称" :span="2">
          <template #label>
            <span style="color:red;margin-right:5px;font-size:14px;">*</span>
            <span>文件名称</span>
          </template>
          <el-input
            class="input-border-none"
            v-model="form.fileName"
            placeholder="名称"
            style="width: 100%;"
            size="small"
            maxlength="20"
            clearable
          />
        </el-descriptions-item>
        <el-descriptions-item label-class-name="modify-content" label="*文件" :span="2">
          <template #label>
            <span style="color:red;margin-right:5px;font-size:14px;">*</span>
            <span>文件</span>
          </template>
          <div style="display:flex;">
            <div style="flex:1;">
               <template v-if="form.file">
                <span style="cursor: pointer; color: #409eff" @dblclick="updateAttachmentView(form)">{{ form.file }}</span>
              </template>
              <template v-else-if="form.attachmentDTO">
                <span style="cursor: pointer; color: #409eff" @dblclick="attachmentView(form.attachmentDTO)">{{ form.attachmentDTO.name }}</span>
              </template>
            </div>
            <div style="flex:1;text-align:right;">
              <upload-btn ref="uploadRef" btnType="warning" v-model:files="form.attachmentFiles" :file-classify="fileClassifyEnum.PLAN_ATT.V" :show-file-list="false" :limit="1" :accept="'.pdf'" @change="uploadFile" :btnName="'变更文件'"/>
            </div>
          </div>
        </el-descriptions-item>
        <el-descriptions-item label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label="所属项目" :span="2">
           <el-row>
            <template v-if="isNotBlank(currentRow.projectList && currentRow.boolSingleProject)">
              <el-col v-for="item in currentRow.projectList" :key="item.id" :span="12">
                {{projectNameFormatter(item)}}
              </el-col>
            </template>
            <template v-else>-</template>
          </el-row>
        </el-descriptions-item>
         <el-descriptions-item label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d} {h}:{i}:{s}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label-class-name="modify-content" label="备注">
           <el-input
            class="input-border-none"
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 1, maxRows: 3 }"
            :maxlength="200"
            placeholder="备注"
            style="width:100%"/>
        </el-descriptions-item>
      </el-descriptions>
      <div style="color:#e6a23c;margin-top:10px;font-size:13px;">*  1. 深色框内的信息可进行修改，其它信息不可变更</div>
      <div style="color:#e6a23c;font-size:13px;margin-top:3px;">*  2. 更换工艺文件会生成新的修订版本</div>
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

import { isNotBlank } from '@data-type/index'
import { fileClassifyEnum } from '@enum-ms/file'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { ElMessage } from 'element-plus'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'
import { planProcessListPM as permission } from '@/page-permission/plan'

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
const uploadRef = ref()

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
    if (uploadRef.value) {
      uploadRef.value.clearFiles()
    }
    form.value.attachmentFiles = []
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
  const file = form.value.attachmentFiles[0].name
  const fileNameArr = file.split('.')
  let fileName = ''
  for (let i = 0; i < fileNameArr.length; i++) {
    if (i !== (fileNameArr.length - 1)) {
      if (i !== (fileNameArr.length - 2)) {
        fileName = fileName + fileNameArr[i] + '.'
      } else {
        fileName = fileName + fileNameArr[i]
      }
    }
  }
  form.value.fileName = fileName
  form.value.file = file
  form.value.attachmentId = form.value.attachmentFiles[0].id
  if (uploadRef.value) {
    uploadRef.value.clearFiles()
  }
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
::v-deep(.el-descriptions__label.el-descriptions__cell.is-bordered-label){
  width:110px;
}
::v-deep(.modify-content){
  background:#999 !important;
  color:#fff !important;
}
</style>
