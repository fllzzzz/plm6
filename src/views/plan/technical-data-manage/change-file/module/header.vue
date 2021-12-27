<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.fileName"
        placeholder="输入文件名搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          ref="changeFileRef"
          v-permission="crud.permission.import"
          :upload-fun="upload"
          :data="carryParam"
          :btn-name="'文件上传'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :data-type="crud.query.type"
          :icon="'el-icon-upload'"
          :accept="'.zip'"
          class="filter-item"
          @success="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <common-button
          v-if="projectId"
          v-permission="crud.permission.download"
          :loading="downloadLoading"
          type="warning"
          icon="el-icon-download"
          size="mini"
          :disabled="!projectId"
          @click="downloadAll()"
        >下载项目下所有文件</common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import uploadBtn from '../../components/drawing-upload-btn'
import { upload, downloadByProject } from '@/api/plan/technical-data-manage/other'
import { fileDownload } from '@/utils/file'

const defaultQuery = {
  type: technicalDataTypeEnum.CHANGE_FILE.V // 类型 1蓝图 2变更文件 3模型 4其他文件
}

const { crud, query } = regHeader(defaultQuery)
const downloadLoading = ref(false)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const carryParam = computed(() => {
  return { projectId: props.projectId, type: crud.query.type }
})
const changeFileRef = ref()
async function downloadAll() {
  try {
    downloadLoading.value = true
    await fileDownload(downloadByProject, crud.query.projectId, crud.query.type)
  } catch (error) {
    console.log('根据单体下载', error)
  } finally {
    downloadLoading.value = false
  }
}
</script>
