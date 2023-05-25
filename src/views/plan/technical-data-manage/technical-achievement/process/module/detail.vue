<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="文件详情"
    :wrapper-closable="false"
    size="900px"
    custom-class="contract-change"
  >
    <template #titleRight>
      <common-button type="info" @click="versionVisible=true">修订版本</common-button>
      <common-button type="primary" @click="bindVisible=true">绑定列表</common-button>
    </template>
    <template #content>
      <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="文件名称" :span="2">
          <div style="word-break:break-all;">{{currentRow.fileName}}</div>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="attachmentDTO" label="文件" :span="2">
          <template v-if="currentRow.attachmentDTO">
            <div style="cursor: pointer; color: #409eff;word-break:break-all;" @dblclick="attachmentView(currentRow.attachmentDTO)">{{ currentRow.attachmentDTO.name }}</div>
          </template>
          <span v-else>-</span>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="processType" label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="boolSingleProject" label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="project" :label="currentRow.boolSingleProject?'所属项目':'关联项目'" :span="2">
          <el-row>
            <template v-if="isNotBlank(currentRow.projectList)">
              <el-col v-for="item in currentRow.projectList" :key="item.id" :span="12">
                【{{projectNameFormatter(item)}}】
              </el-col>
            </template>
            <template v-else>-</template>
          </el-row>
        </el-descriptions-item>
         <el-descriptions-item label-class-name="userName" label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="uploadTime" label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d} {h}:{i}:{s}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
          <div style="word-break:break-all;">{{currentRow.remark}}</div>
        </el-descriptions-item>
      </el-descriptions>
      <historyVersion v-model="versionVisible" :currentRow="currentRow" />
      <artifactBindDetail v-model="bindVisible" :currentRow="currentRow" @success="emit('success')" />
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'

import { isNotBlank } from '@data-type/index'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'

import historyVersion from './history-version'
import artifactBindDetail from './artifact-bind-detail'
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

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const versionVisible = ref(false)
const bindVisible = ref(false)

const pdfShow = ref(false)
const currentId = ref()

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-descriptions__label.el-descriptions__cell.is-bordered-label){
  width:110px;
}
</style>
